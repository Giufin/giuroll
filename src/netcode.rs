#[cfg(feature = "logtofile")]
use log::info;
use std::{
    collections::{HashMap, HashSet},
    sync::atomic::Ordering::Relaxed,
    time::{Duration, Instant},
};
use windows::Win32::Networking::WinSock::{sendto, SOCKADDR, SOCKET};

use crate::{input_to_accum, rollback::Rollbacker, SOKU_FRAMECOUNT, TARGET_OFFSET};

#[derive(Clone, Debug)]
pub struct NetworkPacket {
    id: usize,
    desyncdetect: u8,

    delay: u8,
    max_rollback: u8,

    inputs: Vec<u16>, //also u8 in size? starts out at id + delay
    //confirms: Vec<bool>,
    last_confirm: usize,
    sync: Option<i32>,
}

impl NetworkPacket {
    fn encode(&self) -> Box<[u8]> {
        let mut buf = [0; 400];
        buf[4..8].copy_from_slice(&self.id.to_le_bytes()); //0
        buf[8] = self.desyncdetect;
        buf[9] = self.delay;
        buf[10] = self.max_rollback;

        buf[11] = self.inputs.len() as u8; //inputs, confirms are the same length

        for a in 0..self.inputs.len() {
            buf[(12 + a * 2)..(14 + a * 2)].copy_from_slice(&self.inputs[a].to_le_bytes());
        }

        let next = 12 + self.inputs.len() * 2;

        buf[next..next + 4].copy_from_slice(&self.last_confirm.to_le_bytes());
        let next = next + 4;
        buf[next..next + 4].copy_from_slice(&self.sync.unwrap_or(i32::MAX).to_le_bytes());

        let last = next + 4;

        buf[0..last].to_vec().into_boxed_slice()
    }

    pub fn decode(d: &[u8]) -> Self {
        let id = usize::from_le_bytes(d[4..8].try_into().unwrap());
        let desyncdetect = d[8];
        let delay = d[9];
        let max_rollback = d[10];
        let inputsize = d[11];
        let inputs = (0..inputsize as usize)
            .map(|x| u16::from_le_bytes(d[12 + x * 2..12 + (x + 1) * 2].try_into().unwrap()))
            .collect();
        let lastend = 12 + inputsize as usize * 2;
        let last_confirm = usize::from_le_bytes(d[lastend..lastend + 4].try_into().unwrap());

        let lastend = lastend + 4 as usize;
        let syncraw = i32::from_le_bytes(d[lastend..lastend + 4].try_into().unwrap());

        let sync = match syncraw {
            i32::MAX => None,
            x => Some(x),
        };

        Self {
            id,
            desyncdetect,
            delay,
            max_rollback,
            inputs,
            last_confirm,
            sync,
        }
    }
}

#[derive(Clone, Debug)]
pub enum FrameTimeData {
    Empty,
    LocalFirst(Instant),
    RemoteFirst(Instant),
    Done(i32),
}

pub struct Netcoder {
    last_opponent_confirm: usize,

    id: usize,

    //ideally we shouldn't be keeping a separate input stack from the Rollbacker but for now it's what I have
    opponent_inputs: Vec<Option<u16>>,
    last_opponent_input: usize,

    inputs: Vec<u16>,

    send_times: HashMap<usize, Instant>,
    recv_delays: HashMap<usize, Duration>,

    pub delay: usize,
    pub max_rollback: usize,
    pub display_stats: bool,
    pub last_opponent_delay: usize,

    past_frame_starts: Vec<FrameTimeData>,

    pub receiver: std::sync::mpsc::Receiver<(NetworkPacket, Instant)>,
    time_syncs: Vec<i32>,
    last_median_sync: i32,

    pub autodelay_enabled: bool,
}

/// The packets are only sent once per frame; a packet contains all previous unconfirmed inputs; a lost "main" packet is not recovered whenever it's not neccesseary
impl Netcoder {
    pub fn new(receiver: std::sync::mpsc::Receiver<(NetworkPacket, Instant)>) -> Self {
        Self {
            last_opponent_confirm: 0,
            inputs: Vec::new(),

            opponent_inputs: Vec::new(),

            send_times: HashMap::new(),
            recv_delays: HashMap::new(),

            last_opponent_delay: 0,
            last_opponent_input: 0,
            id: 0,
            delay: 0,
            max_rollback: 0,
            display_stats: false,

            past_frame_starts: Vec::new(),
            receiver,

            time_syncs: vec![],
            last_median_sync: 0,
            autodelay_enabled: false,
        }
    }

    /// returns whether or not we are allowed to proceed based on the confirmations we received
    /// and sends the following frame to the opponent
    pub fn process_and_send(
        &mut self,
        rollbacker: &mut Rollbacker,
        current_input: [bool; 10],
    ) -> u32 {
        let function_start_time = Instant::now();

        // self.id is lower than real framecount by 1, this is because we don't process frame 0
        while self.past_frame_starts.len() <= self.id {
            self.past_frame_starts.push(FrameTimeData::Empty);
        }

        let is_p1;
        unsafe {
            // todo: take out to it's own function
            let netmanager = *(0x8986a0 as *const usize);

            //host only
            let delay_display = (netmanager + 0x80) as *mut u8;
            *delay_display = self.delay as u8;

            //client only
            let delay_display = (netmanager + 0x81) as *mut u8;
            *delay_display = self.delay as u8;

            is_p1 = netmanager != 0 && *(netmanager as *const usize) == 0x858cac;
        }

        //because it looks like soku locks the netcode untill the start of a new frame, we sometimes reach this point before the netcode has finished processing it's packet, for that reason:
        std::thread::sleep(Duration::from_millis(1));

        while let Ok((packet, time)) = self.receiver.try_recv() {
            if packet.id > self.id + 20 {
                //these are probably packets comming from the last round, we better avoid them

                continue;
            }

            // time how long it took us to handlne that frame.
            // If we did not handle it in time we just send a -1000, meaning the opponent will slow down by a 1000 microseconds,
            // later on it should be worth to send information about frames ariving way too late,
            // that would make the opponent pause, or severely slow down for multiple frames

            //todo, handle time data packets not ariving at all, by taking the time of arrival of the subsequent packet

            if packet.id >= self.opponent_inputs.len() {
                if !is_p1 {
                    //self.delay = packet.delay as usize;
                    self.max_rollback = packet.max_rollback as usize;
                }

                if self.display_stats {
                    unsafe { crate::NEXT_DRAW_ENEMY_DELAY = Some(packet.delay as i32) };
                } else {
                    unsafe { crate::NEXT_DRAW_ENEMY_DELAY = None };
                }

                self.last_opponent_delay = packet.delay as usize;

                // is the first arrival of the newest packet
                let last = self
                    .past_frame_starts
                    .get(packet.id)
                    .cloned()
                    .unwrap_or(FrameTimeData::Empty);

                match last {
                    //bug! this value is set to -1000 even if we are less than 1000 microseconds from completing out frame, which is possible only for targets with
                    // less than 1000 microsecond ping. nevertheless it should be fixed at some point
                    FrameTimeData::Empty => {
                        //let r = if self.id + 1 < packet.id {
                        //    -((time.elapsed().as_micros()) as i128 / 100)
                        //} else {
                        //    -((time.elapsed().as_micros()) as i128 / 1000)
                        //};

                        while self.past_frame_starts.len() <= packet.id {
                            self.past_frame_starts.push(FrameTimeData::Empty);
                        }

                        self.past_frame_starts[packet.id] = FrameTimeData::RemoteFirst(time);
                        //Some(r)
                    }
                    FrameTimeData::LocalFirst(x) => {
                        let r = time
                            .checked_duration_since(x)
                            .unwrap_or_else(|| {
                                {
                                    {
                                        x.checked_duration_since(time)
                                            .expect("either of these opperation should succeed")
                                    }
                                }
                            })
                            .as_micros() as i128;
                        //info!("time passed: {}", r);

                        self.past_frame_starts[packet.id] = FrameTimeData::Done(r as i32);

                        //Some(r)
                    }

                    FrameTimeData::RemoteFirst(_) => {
                        //info!("same frame received twice");
                        ()
                    }
                    FrameTimeData::Done(_) => (),
                };

                //if let Some(my_diff) = my_diff {
                //    while self.past_frame_starts.len() <= packet.id {
                //        self.past_frame_starts.push(FrameTimeData::Empty);
                //    }
                //    self.past_frame_starts[packet.id] = FrameTimeData::Done(my_diff as i32);
                //}

                // handle opponents timing data
                if let Some(remote) = packet.sync {
                    //info!("frame diff {}", remote);
                    if remote < 0 {
                        TARGET_OFFSET.fetch_add(-remote.max(-5000), Relaxed);
                    } else {
                        match self
                            .past_frame_starts
                            .get(packet.id.saturating_sub((packet.inputs.len()) as usize))
                        {
                            Some(FrameTimeData::Done(local)) => {
                                let diff = *local - remote;

                                while packet.id > self.time_syncs.len() {
                                    self.time_syncs.push(0);
                                }
                                self.time_syncs.push(diff);

                                //TARGET_OFFSET.fetch_add(diff, Relaxed);
                            }
                            Some(FrameTimeData::RemoteFirst(_)) => {
                                //println!("frame diff: remote first");
                                TARGET_OFFSET.fetch_add(-200, Relaxed);
                            }
                            Some(_) => (),
                            None => (), //info!("no time packet"),
                        }
                    }
                    //info!("packet sync data: {:?}", x)
                }

                let weather_remote = packet.desyncdetect;
                let weather_local = rollbacker
                    .weathers
                    .get(&(packet.id.saturating_sub(20)))
                    .cloned()
                    .unwrap_or(0);
                if weather_remote != weather_local {
                    #[cfg(feature = "allocconsole")]
                    println!("desync");

                    //todo, add different desync indication !
                    #[cfg(feature = "logtofile")]
                    info!(
                        "DESYNC: local: {}, remote: {}",
                        weather_local, weather_remote
                    )
                }
            }

            let latest = packet.id as usize; //last delay
            while self.opponent_inputs.len() <= latest as usize {
                self.opponent_inputs.push(None);
            }
            let mut fr = latest;

            self.last_opponent_input = self.last_opponent_input.max(packet.id);

            for a in self.last_opponent_confirm..packet.last_confirm {
                let el = self.send_times.get(&a);
                if let Some(&x) = el {
                    let x = time.saturating_duration_since(x);
                    self.recv_delays.insert(fr, x);
                }
            }

            self.last_opponent_confirm = self.last_opponent_confirm.max(packet.last_confirm);

            for a in packet.inputs {
                if self.opponent_inputs[fr].is_none() {
                    //println!("{:?}", self.send_times[fr].elapsed());

                    let inp_a = a;

                    self.opponent_inputs[fr] = Some(inp_a);

                    // todo: move into it's own function

                    let inp = (0..10)
                        .into_iter()
                        .map(|x| (inp_a & (1 << x)) > 0)
                        .collect::<Vec<_>>()
                        .try_into()
                        .unwrap();
                    rollbacker.enemy_inputs.insert(inp, fr);
                }

                if fr == 0 {
                    break;
                }
                fr -= 1;
            }
        }

        let input_head = self.id;

        let input_range = self.last_opponent_confirm..=input_head;

        // do not override existing inputs; this can happen when delay is changed
        while rollbacker.self_inputs.len() <= input_head {
            rollbacker.self_inputs.push(current_input);
        }

        while self.inputs.len() <= input_head {
            self.inputs.push(input_to_accum(&current_input));
        }

        let mut ivec = self.inputs[input_range.clone()].to_vec();
        ivec.reverse();

        let past = match self.past_frame_starts.get(self.id.saturating_sub(30)) {
            Some(FrameTimeData::Done(x)) => Some(*x),
            _ => None,
        };

        let to_be_sent = NetworkPacket {
            id: self.id,
            desyncdetect: rollbacker
                .weathers
                .get(&(self.id.saturating_sub(20)))
                .cloned()
                .unwrap_or(0),
            delay: self.delay as u8,
            max_rollback: self.max_rollback as u8,
            inputs: ivec,
            last_confirm: (self.last_opponent_input).min(self.id + 30),
            sync: past,
        };

        unsafe { send_packet(to_be_sent.encode()) };
        self.send_times.insert(input_head, Instant::now());

        let m = rollbacker.start();

        let diff = (self.id - unsafe { *SOKU_FRAMECOUNT }) as i64;
        let m = if diff < (self.delay as i64) {
            m.saturating_sub(1)
        } else if diff > (self.delay as i64) {
            m + 1
        } else {
            m
        };

        //println!("m: {m}");

        //if rollbacker.guessed.len() > 13 {
        //    panic!("WHAT 13");
        //}

        unsafe {
            let m2 = rollbacker.guessed.len();

            if self.display_stats {
                if self.id % 60 == 0 && self.id > 0 {
                    let mut sum = 0;
                    for a in (self.id - 60)..(self.id) {
                        if let Some(x) = self.recv_delays.get(&a) {
                            sum += x.as_micros();
                        }
                    }
                    crate::NEXT_DRAW_PING = Some((sum / (60_000 * 2)) as i32);
                    crate::NEXT_DRAW_ROLLBACK = Some(m2 as i32);
                } else if crate::NEXT_DRAW_PING.is_none() {
                    crate::NEXT_DRAW_PING = Some(0);
                    crate::NEXT_DRAW_ROLLBACK = Some(m2 as i32);
                }
            } else {
                crate::NEXT_DRAW_PING = None;
                crate::NEXT_DRAW_ROLLBACK = None;
            }

            if self.autodelay_enabled && self.id == 300 {
                let id = self.id - 60;
                let iter = (id - 200..id)
                    .map(|x| self.recv_delays.get(&x))
                    .filter_map(|x| x)
                    .map(|x| x.as_micros());

                let (count, sum) = iter.fold((0, 0), |x, y| (x.0 + 1, x.1 + y));
                let avg = sum / count;
                println!("avg: {}", avg);
                self.delay = ((avg / (1_000_000 / 30)) as usize).min(9);
            }
        }

        //time sync
        const TIME_SYNC_MEDIAN_INTERVAL: usize = 50;
        if self.id % TIME_SYNC_MEDIAN_INTERVAL == 0 && self.id > (TIME_SYNC_MEDIAN_INTERVAL + 30) {
            match self
                .time_syncs
                .get((self.id - 30 - TIME_SYNC_MEDIAN_INTERVAL)..(self.id - 30))
                .map(|x| {
                    let ret: Result<[i32; TIME_SYNC_MEDIAN_INTERVAL], _> = x.try_into();
                    ret.ok()
                })
                .flatten()
            {
                Some(mut av) => {
                    av.sort();

                    //let median = (av[TIME_SYNC_MEDIAN_INTERVAL / 2 - 1]
                    //    + av[TIME_SYNC_MEDIAN_INTERVAL / 2])
                    //    / 2;
                    //println!("median: {median}");
                    let sum: i32 = av[3..TIME_SYNC_MEDIAN_INTERVAL - 3].iter().sum();
                    let average = sum / (TIME_SYNC_MEDIAN_INTERVAL as i32 - 6);
                    println!("average: {average}");

                    self.last_median_sync = average;
                }
                None => (),
            }
        }
        if self.last_median_sync.abs() > 20000 {
            TARGET_OFFSET.fetch_add(self.last_median_sync / 700, Relaxed);
        } else if self.last_median_sync.abs() > 10000 {
            TARGET_OFFSET.fetch_add(self.last_median_sync / 1400, Relaxed);
        } else if self.last_median_sync.abs() > 2000 {
            TARGET_OFFSET.fetch_add(self.last_median_sync / 2000, Relaxed);
        } else {
            let res = if self.last_median_sync.abs() > 500 {
                self.last_median_sync.clamp(-1, 1)
            } else {
                0
            };
            TARGET_OFFSET.fetch_add(res, Relaxed);
        }

        if self.id > self.last_opponent_confirm + 30 {
            //crate::TARGET_OFFSET.fetch_add(1000 * m as i32, Relaxed);
            println!(
                "frame is missing: m: {m}, id: {}, confirm: {}",
                self.id, self.last_opponent_confirm
            );
            0
        } else if self.id
            > self.last_opponent_input
                + ((self.max_rollback * 2) + self.delay.min(self.last_opponent_delay)).min(30)
        {
            crate::TARGET_OFFSET.fetch_add(1000 * m as i32, Relaxed);
            println!(
                "frame is missing for reason 2: m: {m}, id: {}, confirm: {}",
                self.id, self.last_opponent_confirm
            );
            0
        } else {
            //no pause, perform additional operations here

            //todo: consider moving to it's own function
            match self.past_frame_starts[self.id].clone() {
                FrameTimeData::Empty => {
                    self.past_frame_starts[self.id] = FrameTimeData::LocalFirst(function_start_time)
                }
                FrameTimeData::LocalFirst(_) => todo!("should be unreachable"),
                FrameTimeData::RemoteFirst(x) => {
                    self.past_frame_starts[self.id] = FrameTimeData::Done(
                        x.saturating_duration_since(function_start_time).as_micros() as i32,
                    )
                }
                FrameTimeData::Done(_) => (),
            }

            self.id += 1;

            m as u32
        }
    }
}

pub unsafe fn send_packet(mut data: Box<[u8]>) {
    //info!("sending packet");
    data[0] = 0x6b;

    let netmanager = *(0x8986a0 as *const usize);

    let socket = netmanager + 0x3e4;

    let to;
    if *(netmanager as *const usize) == 0x858cac {
        let it = (netmanager + 0x4c8) as *const usize;
        data[1] = 1;

        if *it == 0 {
            panic!();
        }
        to = *(it as *const *const SOCKADDR);
    } else {
        data[1] = 2;

        if *(netmanager as *const usize) != 0x858d14 {
            panic!();
        }
        to = (netmanager + 0x47c) as *const SOCKADDR
    }

    let rse = sendto(*(socket as *const SOCKET), &data, 0, to, data.len() as i32);

    if rse == -1 {
        //to do, change error handling for sockets

        //#[cfg(feature = "logtofile")]
        //info!("socket err: {:?}", WSAGetLastError());
    }
}
