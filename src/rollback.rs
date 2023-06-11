use std::{
    arch::asm,
    collections::{BTreeMap, HashMap, HashSet},
    ffi::c_void,
};
#[cfg(any(NO))]
use log::info;

use windows::{
    imp::HeapFree,
    Win32::System::Memory::{HeapHandle, HeapValidate},
};

use crate::{
    force_sound_skip, set_input_buffer, ISDEBUG, MEMORY_RECEIVER, SOKU_FRAMECOUNT,
    SOUND_DELET_MUTEX, SOUND_MUTEX,
};

type RInput = [bool; 10];

const CHARSIZEDATA_A: [usize; 20] = [
    2236, 2220, 2208, 2244, 2216, 2284, 2196, 2220, 2260, 2200, 2232, 2200, 2200, 2216, 2352, 2224,
    2196, 2196, 2216, 2216, /* 0, 2208, */
];

const CHARSIZEDATA_B: [usize; 20] = [
    940, 940, 940, 944, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940,
    940, /* 0, 940, */
];

pub enum MemoryManip {
    Alloc(usize),
    Free(usize),
}
pub struct EnemyInputHolder {
    pub i: Vec<Option<RInput>>,
}

impl EnemyInputHolder {
    fn new() -> Self {
        Self { i: Vec::new() }
    }
    fn get(&self, count: usize) -> RInput {
        match self.get_result(count) {
            Ok(x) => x,
            Err(x) => x,
        }
    }

    pub fn insert(&mut self, input: RInput, frame: usize) {
        while frame >= self.i.len() {
            self.i.push(None);
        }
        if let Some(x) = self.i[frame].replace(input) {
            //doubled input
            if x != input {
                panic!("replacing existing input");
            }
        }
    }

    fn get_result(&self, frame: usize) -> Result<RInput, RInput> {
        match self.i.get(frame) {
            Some(Some(x)) => Ok(*x),
            None if frame == 0 => Err([false; 10]),
            Some(None) | None => {
                /*
                               // here we need to guess what the input will be. to prevent guessing wrong on mashes we should be very conservative about our guesses
                               let mut w = (1..3)
                                   .map(|x| self.get(frame.saturating_sub(x)))
                                   .reduce(|x, y| {
                                       (0..10)
                                           .map(|idx| x[idx] & y[idx])
                                           .collect::<Vec<_>>()
                                           .try_into()
                                           .unwrap()
                                   })
                                   .unwrap();

                               w[0..4].copy_from_slice(&self.get(frame - 1)[0..4]);
                */

                Err(self.get(frame - 1))
            }
        }
    }
}
#[derive(Debug, Clone)]
enum PlanFrame {
    ApplyInput(RInput, RInput),
    GuessInput(RInput, RInput, bool), //bool is "should save before"
    Save,
}

pub struct Rollbacker {
    pub guessed: Vec<RollFrame>,

    current: usize,
    rolling_back: bool,

    pub future_sound: HashMap<usize, usize>,
    // first element is the sound, second is the frame it occured at, whenever a frame comes true we can delete all future sounds with that value

    // stores all the sounds that happened in "guessed" frames. Will also need to be topped up *after* last frame.
    // every frame we can store this as "past_sounds", and if any sound in future sounds did not appear in past_sounds, we can then cancell that sound by force calling 0x401d50
    // which we hook, and set a static to ignore the sound.

    //also, while rolling back, we should not play sounds that already did appear in past_sounds (and instead remove them, so we can see what is)
    pub enemy_inputs: EnemyInputHolder,
    pub self_inputs: Vec<RInput>,

    pub weathers: HashMap<usize, u8>,
    //pub consumer: std::sync::mpsc::Receiver<(usize, RInput)>,
}

impl Rollbacker {
    pub fn guesslen(&self) -> usize {
        self.guessed.len()
    }
    pub fn new(/*consumer: std::sync::mpsc::Receiver<(usize, RInput)> */) -> Self {
        Self {
            guessed: Vec::new(),
            current: 0,
            rolling_back: false,
            enemy_inputs: EnemyInputHolder::new(),
            self_inputs: Vec::new(),
            weathers: HashMap::new(),
            future_sound: HashMap::new(),
            //consumer,
        }
    }
    /// fill in inputs before calling this function
    pub fn start(&mut self) -> usize {
        //this should only be called on the 0th iteration.
        self.current = unsafe { *SOKU_FRAMECOUNT }; //following
        let mut newsound = std::mem::replace(&mut *SOUND_MUTEX.lock().unwrap(), BTreeMap::new());

        //while let Ok((pos, input)) = self.consumer.try_recv() {
        //    //info!("NTC received {:?}", (pos, input));
        //    self.enemy_inputs.insert(input, pos);
        //}

        while self.guessed.len() > 0
            && (self
                .enemy_inputs
                .get_result(self.guessed[0].prev_state.number)
                .map(|x| x == self.guessed[0].enemy_input)
                .unwrap_or(false))
        {
            let m = self.guessed.remove(0);

            //newsound.remove(&m.prev_state.number);

            self.weathers
                .insert(m.prev_state.number, m.prev_state.weather_sync_check);
            m.prev_state.did_happen();
        }

        //info!("{:?}", newsound.remove(&(self.current - self.guessed.len())));

        *SOUND_DELET_MUTEX.lock().unwrap() = newsound;
        if self.guessed.len() > 0 {

            //info!(
            //    "{:?} {:?}",
            //    self.enemy_inputs
            //        .get_result(self.guessed[0].prev_state.number),
            //    self.guessed[0].enemy_input
            //);
        }

        self.rolling_back = false;
        self.guessed.len() + 1
    }

    fn apply_input(input: RInput, opponent_input: RInput) {
        let is_p1 = unsafe {
            let netmanager = *(0x8986a0 as *const usize);
            *(netmanager as *const usize) == 0x858cac
        };

        if is_p1 {
            unsafe { set_input_buffer(input, opponent_input) };
            //LATEST_1.lock().unwrap().0 = input;
            //LATEST_2.lock().unwrap().0 = opponent_input;
        } else {
            unsafe { set_input_buffer(opponent_input, input) };
            //LATEST_1.lock().unwrap().0 = opponent_input;
            //LATEST_2.lock().unwrap().0 = input;
        }
    }

    pub fn step(&mut self, iteration_number: usize) -> Option<()> {
        unsafe {
            if self.guessed.len() > 0 && false {
                //disabled for now because caused crashes
                let last = if iteration_number == 0 {
                    self.guessed.len() - 1
                } else {
                    iteration_number - 1
                };

                while let Ok(man) = MEMORY_RECEIVER.as_ref().unwrap().try_recv() {
                    match man {
                        MemoryManip::Alloc(pos) => self.guessed[last].prev_state.allocs.push(pos), /* */
                        MemoryManip::Free(pos) => self.guessed[last].prev_state.frees.push(pos),
                    }
                }
            }
        }

        if self.guessed.len() == iteration_number {
            //last iteration for this frame, handle sound here
            {
                let mut to_be_skipped = vec![];
                {
                    let new_sounds = &mut *SOUND_MUTEX.lock().unwrap();
                    let old_sounds = &mut *SOUND_DELET_MUTEX.lock().unwrap();

                    let new_col = new_sounds
                        .values()
                        .map(|x| x.into_iter())
                        .flatten()
                        .collect::<HashSet<_>>();

                    //info!("new: {:?} old: {:?}", new_sounds, old_sounds);

                    for idx in (self.current).saturating_sub(5)..(self.current) {
                        if !new_sounds.contains_key(&idx) {
                            //info!("key: {idx} not found");
                            continue;
                        }
                        if let Some(x) = old_sounds.get(&idx) {
                            for a in x {
                                if !new_col.contains(a) {
                                    to_be_skipped.push(*a);
                                    //info!("{} shouldn't happen", a);
                                }
                            }
                        }
                    }

                    std::mem::swap(old_sounds, new_sounds);

                    //let old_col = old_sounds
                    //    .values()
                    //    .map(|x| x.into_iter())
                    //    .flatten()
                    //    .collect::<HashSet<_>>();
                    //
                    //old_col.difference(&new_col).map(|x| **x).collect()
                };
                for a in to_be_skipped {
                    force_sound_skip(a);
                }

                //*old_sounds = std::mem::replace(new_sounds, BTreeMap::new());
            }

            let si = self.self_inputs[self.current];
            let ei = self.enemy_inputs.get(self.current);
            Self::apply_input(si, ei);
            self.guessed.push(RollFrame::dump_with_guess(si, ei));
            return Some(());
        }

        let fr = &mut self.guessed[iteration_number];

        if self.rolling_back {
            std::mem::replace(&mut fr.prev_state, unsafe { dump_frame() }).never_happened();
            fr.enemy_input = self.enemy_inputs.get(fr.prev_state.number);
            Self::apply_input(fr.player_input, fr.enemy_input);
            Some(())
        } else if fr.enemy_input != self.enemy_inputs.get(fr.prev_state.number) {
            //info!("ROLLBACK");
            self.rolling_back = true;
            fr.prev_state.clone().restore();
            fr.enemy_input = self.enemy_inputs.get(fr.prev_state.number);
            Self::apply_input(fr.player_input, fr.enemy_input);
            Some(())
        } else {
            None
        }
    }

    pub(crate) fn write_self(&mut self, input: [bool; 10]) {
        self.self_inputs.push(input);
    }
}

pub struct RollFrame {
    pub prev_state: Frame,
    pub player_input: RInput,
    pub enemy_input: RInput,
}

impl RollFrame {
    fn apply(&mut self, enemy_input: RInput, player_input: RInput) {}

    fn count(&self) -> usize {
        self.prev_state.number
    }

    fn dump_with_guess(player_input: RInput, guess: RInput) -> Self {
        Self {
            prev_state: unsafe { dump_frame() },
            player_input: player_input,
            enemy_input: guess,
        }
    }
}
static mut FPST: [u8; 108] = [0u8; 108];
pub unsafe fn dump_frame() -> Frame {
    let w = unsafe {
        //let b = 3;
        asm!(
            "FSAVE {fpst}",
            "FRSTOR {fpst}",
            fpst = sym FPST
        );
        FPST
    };

    let mut m = vec![];

    //m.push(read_addr(mystcountpos as usize, 4));
    //player1? object [2023-05-17T13:47:55.614854700Z INFO giuroll] eax: 0x4282f0
    //0x895ec
    /*
     */
    #[cfg(any(NO))]
    if ISDEBUG {
        info!("0x895ec")
    };
    let ptr1 = read_addr(0x8985ec, 0x4);
    let first = get_ptr(&ptr1.content[0..4], 0);
    m.push(read_addr(first, 0xec));

    //if ISDEBUG { info!("o1: {:?} ", t) };
    {
        let t = read_vec(first + 0x1c);

        m.push(t.read_underlying());

        m.push(t.to_addr());
    }

    //if ISDEBUG { info!("o2: {:?} ", t) };
    {
        let t = read_vec(first + 0x68);
        if t.start != 0 {
            m.push(t.read_underlying());
            //info!("5ec+x68 was not 0");
        }
        //m.push(t.to_addr());
    }

    //if ISDEBUG { info!("o3: {:?} ", t) };
    {
        let t = read_linked_list(first + 0x78, 0);

        m.extend(t.read_all().to_vec(/*needed to consume */).into_iter());
    }

    {
        let t = read_linked_list(first + 0xa4, 0x180);

        m.extend(t.read_all().to_vec().into_iter());
    }

    {
        m.extend(
            read_deque(first + 0x28)
                .read_whole(0x10)
                .to_vec()
                .into_iter(),
        );
    }
    #[cfg(any(NO))]
    //0x8985e0
    if ISDEBUG {
        info!("0x8985e0")
    };
    let ptr1 = read_addr(0x8985e0, 0x4);
    let first = get_ptr(&ptr1.content[0..4], 0);
    m.push(read_addr(first, 0x118));

    m.extend(
        read_linked_list(first + 0x4, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );
    //    info!("hir");
    let llautosize = read_linked_list(first + 0x2c, 0);
    m.push(llautosize.clone().to_addr());
    m.push(read_addr(first + 0x2c + 0xc, 4));
    //info!("hi2 {}", .usize_align()[0]);

    let mut lit = llautosize.read_underlying().to_vec().into_iter();
    m.push(lit.next().unwrap().to_addr());

    for a in lit {
        let p = a.additional_data;
        if p != 0 {
            let size = match read_heap(p) {
                0 => 0x70, //very weird, but I have no clue what's going on and if I don't check this it crashes
                x => x,
            };

            m.push(read_addr(p, size));
            m.push(a.clone().to_addr());
        }
    }

    //m.push(read_) to do ReadSizeDetect(param_1,(LinkedListTheThird *)(ppvVar1 + 0xb));
    m.extend(
        read_linked_list(first + 0x38, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );

    #[cfg(any(NO))]
    //0x8985f0
    if ISDEBUG {
        info!("0x8985f0")
    };
    let ptr1 = read_addr(0x8985f0, 0x4);
    let first = get_ptr(&ptr1.content[0..4], 0);

    m.push(read_addr(first, 0x94));
    //info!("{:?}", read_deque(first));
    //info!("{:?}", read_vec(first).read_underlying().usize_align());

    m.push(read_vec(first + 0x10).read_underlying());
    //info!("{:?}", read_linked_list(first + 0x10, 0x178).read_all());

    m.push(read_vec(first + 0x20).read_underlying());

    m.extend(
        read_linked_list(first + 0x30, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );

    let effect_linked_list = read_linked_list(first + 0x5c, 0x178).read_all().to_vec();
    //info!("len {}", effect_linked_list.len());
    m.extend(effect_linked_list.into_iter());

    #[cfg(any(NO))]
    //0x8985e8
    if ISDEBUG {
        info!("0x8985e8")
    };
    let read_weird = |m: &mut Vec<_>, pos: usize, size: usize| {
        let dat = read_addr(pos, 0x14);
        let n = dat.usize_align();

        //names taken from ghidra, thank you ghidra, very cool
        let v1 = n[2];
        let v2 = n[3];
        let read_from = n[1];
        let v3 = n[4];

        if read_from == 0 {
            if n[2] != 0 || n[3] != 0 || n[4] != 0 {
                #[cfg(any(NO))]
                if ISDEBUG {
                    info!("read_from is zero {:?}", n)
                };
            }
            //return;
        } else {
            m.push(read_addr(read_from, v1 * 4));
        }
        for a in 0..v3 {
            let addr = read_from + ((a + v2) % v1) * 4;
            //if addr == 0 {
            //    if ISDEBUG { info!("addr0: {}, {}", a, v3) };
            //}
            m.push(read_addr(addr, size));
        }
    };

    /*
     */
    let ptr1 = read_addr(0x8985e8, 0x4);
    let first = get_ptr(&ptr1.content[0..4], 0);

    m.push(read_addr(first, 0x688));

    m.push(read_vec(first + 0x14).read_underlying());
    m.push(read_vec(first + 0x24).read_underlying());

    m.extend(
        read_linked_list(first + 0x34, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );

    m.extend(
        read_linked_list(first + 0x60, 0x178)
            .read_all()
            .to_vec()
            .into_iter(),
    );
    /*
     */
    read_weird(&mut m, first + 0x18c, 0xc);
    read_weird(&mut m, first + 0x1c0, 0xc);

    #[cfg(any(NO))]
    //0x8985e4
    if ISDEBUG {
        info!("0x8985e4")
    };

    let ptr1 = read_addr(0x8985e4, 0x4);
    let first = get_ptr(&ptr1.content[0..4], 0);
    m.push(read_addr(first, 0x908));
    m.extend(
        read_linked_list(first + 0x30, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );
    m.extend(
        read_linked_list(first + 0x3c, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );
    m.extend(
        read_linked_list(first + 0x48, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );
    m.extend(
        read_linked_list(first + 0x54, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );
    m.extend(
        read_linked_list(first + 0x60, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );
    m.extend(
        read_linked_list(first + 0x6c, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );

    {
        let w = read_vec(first + 0x9c);
        if w.start != 0 {
            m.push(w.read_underlying());
            #[cfg(any(NO))]
            info!("battle+x9c wasn't 0");
        }
        let w = read_vec(first + 0xac);

        if w.start != 0 {
            m.push(w.read_underlying());
            #[cfg(any(NO))]
            //seems to have never triggered, same as the one above
            info!("battle+xac wasn't 0");
        }
    }
    m.extend(
        read_linked_list(first + 0xbc, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );

    m.extend(
        read_linked_list(first + 0xe8, 0)
            .read_all()
            .to_vec()
            .into_iter(),
    );

    //0x8985dc
    if ISDEBUG {
        #[cfg(any(NO))]
        info!("0x8985dc")
    };

    let ptr1 = read_addr(0x8985dc, 0x4);
    let first = get_ptr(&ptr1.content[0..4], 0);

    m.push(read_addr(first, 0x58));
    m.push(read_vec(first + 0x40).read_underlying());

    //todo 180f0

    //0x8986a0
    #[cfg(any(NO))]
    if ISDEBUG {
        info!("0x8986a0")
    };

    //it wants a mutex from here on out, which is sus

    let ptr1 = read_addr(0x8986a0, 0x4);
    ////if ISDEBUG { info!("ptr1: {}", get_ptr(&ptr1.content[0..4], 0)) };
    let first = get_ptr(&ptr1.content[0..4], 0);
    //
    if first != 0 {
        m.push(read_addr(first + 0xf8, 0x68));
        m.push(read_addr(first + 0x174, 0x68));
    }

    //weird from here on out
    //read some deque with first
    #[cfg(any(NO))]
    if ISDEBUG {
        info!("0x8985e4")
    };

    let funky = |p: usize, offset: usize, m: &mut Vec<_>| {
        let bullets = |pos: usize, char: u8, m: &mut Vec<_>| {
            let list = read_linked_list(pos, 0);

            m.extend(list.clone().read_all().to_vec().into_iter());

            let und = list.read_underlying();

            //let mut w = und.to_vec().into_iter();
            //m.push(w.next().unwrap().to_addr());
            for a in und.iter().skip(1) {
                //I fucked up the organization here, it is what it is
                m.push(a.clone().to_addr());
                let d = a.additional_data;
                if d != 0 {
                    let z = CHARSIZEDATA_B[char as usize];
                    //let asize = read_heap(d);
                    //
                    //if z != asize {
                    //    info!("unequal: {} {}", z, asize);
                    //}

                    let bullet = read_addr(d, z);
                    m.push(bullet.clone());
                    let p1 = get_ptr(&bullet.content, 0x3a4);

                    if p1 != 0 {
                        let ll = read_linked_list(d + 0x3a4, 0);

                        m.extend(ll.read_all().to_vec().into_iter());
                    }

                    let p1 = get_ptr(&bullet.content, 0x17c);
                    if p1 != 0 {
                        let ll = read_linked_list(d + 0x17c, 0);
                        m.extend(ll.read_all().to_vec().into_iter());
                    }

                    let p3 = get_ptr(&bullet.content, 0x35c);
                    if p3 != 0 {
                        let s = read_heap(p3);
                        if s > 4000 {
                            #[cfg(any(NO))]
                            info!("bullet data too big! {}", s)
                        } else {
                            m.push(read_addr(p3, s));
                        }
                    }

                    let p4 = get_ptr(&bullet.content, 0x354);
                    if p4 != 0 {
                        let nd = read_addr(p4, 0x54);
                        m.push(nd.clone());

                        let size = usize::from_le_bytes(nd.content[0x30..0x34].try_into().unwrap());
                        let ptr = usize::from_le_bytes(nd.content[0x2c..0x30].try_into().unwrap());

                        let n2 = read_addr(ptr, size * 4);
                        m.push(n2.clone());

                        for a in 0..size {
                            let p = get_ptr(&n2.content, a * 4);
                            if p != 0 {
                                m.push(read_addr(p, 0x10));
                            }
                        }

                        let size = usize::from_le_bytes(nd.content[0x44..0x48].try_into().unwrap());
                        let ptr = usize::from_le_bytes(nd.content[0x40..0x44].try_into().unwrap());

                        let n2 = read_addr(ptr, size * 4);
                        m.push(n2.clone());

                        for a in 0..size {
                            let p = get_ptr(&n2.content, a * 4);
                            if p != 0 {
                                m.push(read_addr(p, 0x10));
                            }
                        }

                        let size = usize::from_le_bytes(nd.content[0x8..0xc].try_into().unwrap())
                            * usize::from_le_bytes(nd.content[0x4..0x8].try_into().unwrap())
                            * 2
                            + 2;

                        let ptr = usize::from_le_bytes(nd.content[0x50..0x54].try_into().unwrap());

                        m.push(read_addr(ptr, size));
                    }
                }
            }
        };

        let old = *((p + 0xc + offset * 4) as *const usize);
        let char = old + 0x34c;
        let char = *(char as *const u8);

        let cdat = read_addr(old, CHARSIZEDATA_A[char as usize]);
        m.push(cdat.clone());

        let todobullets = old + 0x17c;
        bullets(todobullets, char, m);

        if char == 5 {
            //youmu
            read_weird(m, old + 0x8bc, 0x2c);
        }

        let mut z = vec![];
        let ll = read_linked_list(old + 0x718, 0x0);

        z.push(read_addr(ll.ll4, 0xf4));

        for _ in 0..ll.listcount {
            let zcop = z.last().unwrap();
            let ptr = get_ptr(&zcop.content, 0);
            z.push(read_addr(ptr, 0xf4));
        }

        m.extend(z.into_iter());

        let new = get_ptr(&cdat.content, 0x6f8);
        m.push(read_addr(new, 0x68));

        //sokuroll does some scuff here, reading the values one by one, I'm just not gonna bother
        let p4 = read_vec(new + 0x10);
        let w = p4.read_underlying();

        let i = p4.maybecapacity - p4.start;
        let i = (((i >> 0x1f) & 3) + i) >> 2;
        //info!("fsiz {}, {}, {}", p4.end - p4.start, read_heap(p4.start), i);

        for a in 0..i {
            let p = get_ptr(&w.content, a * 4);

            if p != 0 {
                let o = read_addr(p, 4);
                let o2 = read_addr(p + 0x154, 4);

                m.push(o);
                m.push(o2);
            }
        }

        m.push(w.clone());

        m.push(p4.to_addr());

        let p5 = read_vec(new + 0x20);
        m.push(p5.read_underlying());
        m.push(p5.to_addr());

        let p6 = read_linked_list(new + 0x30, 0);
        m.extend(p6.read_all().to_vec().into_iter());

        //let p7 = bulletsagain

        bullets(new + 0x5c, char, m);

        let p8 = read_deque(old + 0x7b0);
        m.extend(p8.read_whole(0x10).to_vec().into_iter());

        let p9 = read_deque(old + 0x5e8);
        m.extend(p9.read_whole(0x98).to_vec().into_iter());

        let p10 = read_deque(old + 0x5b0);
        m.extend(p10.read_whole(0x10).to_vec().into_iter());

        let p11 = read_deque(old + 0x5fc);
        m.extend(p11.read_whole(0x10).to_vec().into_iter());
    };

    let i3 = read_addr(0x8985e4, 4);

    let p3 = get_ptr(&i3.content, 0);

    funky(p3, 0, &mut m);

    funky(p3, 1, &mut m);

    #[cfg(any(NO))]
    if ISDEBUG {
        info!("bullets done");
    }

    //let size = get_ptr(&a.content, 0x34c) as *const usize;

    //m.push(read_addr(0x898718, 0x128));
    //scuffed 0x89881c

    let sc1 = *(0x89881c as *const usize);
    if sc1 != 0 {
        m.push(read_addr(sc1, 0x50));

        //100026f0

        let sc2 = read_deque(sc1 + 0x3c);
        let z = sc2.obj_s as i32;

        #[cfg(any(NO))]
        if ISDEBUG {
            info!("weird deque done");
        }

        if z != 0 {
            let size = sc2.size as i32;
            let ptr = sc2.data as i32;

            //info!("{:0x}", ptr);

            let z = {
                //if is not gonna happen;
                let y = (sc2.f3 as i32 - 1 + z) % (size * 8);
                (ptr + ((y + (((y >> 0x1f) * 7) & 7)) >> 3)) as i32
            };

            let w = if ptr <= z - 0x50 { z - 0x50 } else { ptr };

            let x = (ptr + size).min(w + 0x28);

            m.push(read_addr(w as usize, (((x - w) >> 2) * 4) as usize));

            /*let x = if(w + 0x28 <= x) {
                w+0x28
            } else {
                x
            };*/
        }
    }
    //

    // 0x1c, 0x68
    //let p1c = read_vec(first + 0x1c, 12);
    //let p68 = read_vec(first + 0x68, 12);

    let to_be_read = vec![
        (0x898600, 0x6c),
        (0x8985d8, 4),
        (0x8985d4, 4),
        (0x8971b8, 0x20),
        (0x883cc8, 4),
        (0x89a88c, 4),
        (0x89a454, 4),
        (0x896d64, 8),
        (0x896b20, 4),
        (0x89b65c, 4),
        (0x89b660, 0x9c0),
        (0x89c01c, 4),
        (0x89aaf8, 4),
        (0x88526c, 4),
        (0x8971c0, 0x14),
        (0x8971c8, 0x4c),
    ];
    //if *(0x8985d8 as *const usize) != fc2 {
    //    panic!();
    //}

    for (pos, size) in to_be_read {
        let x = read_addr(pos, size);

        m.push(x);
    }

    Frame {
        number: *SOKU_FRAMECOUNT,
        adresses: m.into_boxed_slice(),
        fp: w,
        frees: vec![],
        allocs: vec![],
        weather_sync_check: ((*(0x8971c4 as *const usize) * 16) + (*(0x8971c4 as *const usize) * 1)
            & 0xFF) as u8,
    }
}

fn read_heap(pos: usize) -> usize {
    unsafe {
        windows::Win32::System::Memory::HeapSize(
            *(0x89b404 as *const HeapHandle),
            windows::Win32::System::Memory::HEAP_FLAGS(0),
            pos as *const c_void,
        )
    }
}

#[derive(Debug, Clone)]
pub struct ReadAddr {
    pub pos: usize,
    pub content: Box<[u8]>,
}

impl ReadAddr {
    fn usize_align(&self) -> Box<[usize]> {
        self.content
            .chunks(4)
            .map(|x| usize::from_le_bytes(x.try_into().unwrap()))
            .collect()
    }

    pub fn restore(self) {
        if self.pos == 0 || self.content.len() == 0 {
            return; //
        }
        let slice =
            unsafe { std::slice::from_raw_parts_mut(self.pos as *mut u8, self.content.len()) };
        slice.copy_from_slice(&self.content);
    }
}

#[derive(Debug, Clone)]
struct VecAddr {
    pub pos: usize,
    pub start: usize,
    pub maybecapacity: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
struct LL4 {
    pub pos: usize,
    pub next: usize, //12 size, self
    pub field2: usize,
    pub additional_data: usize,
}

impl LL4 {
    fn to_addr(self) -> ReadAddr {
        ReadAddr {
            pos: self.pos,
            content: [
                self.next.to_le_bytes(),
                self.field2.to_le_bytes(),
                self.additional_data.to_le_bytes(),
            ]
            .concat()
            .into_boxed_slice(),
        }
    }

    fn read_underlying_additional(&self, size: usize) -> ReadAddr {
        // if size == 0 {panic!()}

        let ret = read_addr(self.additional_data, size);

        ret
    }
}

#[derive(Debug, Clone)]
struct LL3Holder {
    pub pos: usize,
    pub ll4: usize, //12 size, self
    pub additional_size: usize,
    pub listcount: usize,
    pub add_data: usize,
}

impl LL3Holder {
    fn read_underlying(&self) -> Box<[LL4]> {
        if self.ll4 == 0 {
            #[cfg(any(NO))]
            info!("ll4 is 0 ,painc");
            panic!("ll4 is 0");
        }

        let mut b = vec![read_ll4(self.ll4)];

        if self.listcount > 100000 {
            panic!("list too big");
        }

        for a in 0..self.listcount {
            let last = b.last().unwrap();
            let next = last.next;
            if next == 0 {
                #[cfg(any(NO))]
                info!(
                    "next was equal to zero, pos {}, out of {}",
                    a,
                    self.listcount - 1
                );
                panic!();
            };
            b.push(read_ll4(next));
        }

        b.into_boxed_slice()
    }

    fn read_all(self) -> Box<[ReadAddr]> {
        //I think that readLL3 does not read itself, however, I will leave this here because it cannot hurt
        //if ISDEBUG { info!("funnierhere") };
        if self.listcount == 0 {
            Box::new([read_ll4(self.ll4).to_addr()])
        } else {
            let size = self.additional_size;
            if size == 0 {
                self.read_underlying()
                    .to_vec()
                    .into_iter()
                    .map(|x| x.to_addr())
                    .collect()
            } else {
                let mut uv = self.read_underlying().to_vec();
                let first = uv.remove(0);

                uv.into_iter()
                    .map(|x| {
                        if x.additional_data == 0 {
                            vec![x.to_addr()].into_iter()
                        } else {
                            let f = x.read_underlying_additional(size);
                            let sec = x.to_addr();
                            vec![f, sec].into_iter()
                        }
                    })
                    .flatten()
                    .chain([first.to_addr()].into_iter())
                    .collect()
            }
        }
    }

    fn to_addr(self) -> ReadAddr {
        ReadAddr {
            pos: self.pos,
            content: [
                self.ll4.to_le_bytes(),
                self.listcount.to_le_bytes(),
                self.add_data.to_le_bytes(),
            ]
            .concat()
            .into_boxed_slice(),
        }
    }
}

impl VecAddr {
    fn read_underlying(&self) -> ReadAddr {
        read_addr(self.start, self.end - self.start)
    }

    fn to_addr(self) -> ReadAddr {
        ReadAddr {
            pos: self.pos,
            content: [
                self.start.to_le_bytes(),
                self.maybecapacity.to_le_bytes(),
                self.end.to_le_bytes(),
            ]
            .concat()
            .into_boxed_slice(),
        }
    }
}
#[derive(Debug, Clone)]
struct Deque {
    #[allow(unused)]
    pos: usize,
    #[allow(unused)]
    f0: usize,
    data: usize,
    size: usize,
    f3: usize,
    obj_s: usize,
}

impl Deque {
    #[allow(unused)]
    fn to_addr(self) -> ReadAddr {
        ReadAddr {
            pos: self.pos,
            content: [
                self.f0.to_le_bytes(),
                self.data.to_le_bytes(),
                self.size.to_le_bytes(),
                self.f3.to_le_bytes(),
                self.obj_s.to_le_bytes(),
            ]
            .concat()
            .into_boxed_slice(),
        }
    }

    fn read_underlying(&self, size: usize) -> Box<[ReadAddr]> {
        let wobbest: ReadAddr = read_addr(self.data, self.size * 4);

        wobbest
            .content
            .clone()
            .chunks(4)
            .map(|x| usize::from_le_bytes(x.try_into().unwrap()))
            .filter(|x| *x != 0)
            .map(|x| read_addr(x, size))
            .chain([wobbest].into_iter())
            .collect()
    }

    fn read_whole(self, size: usize) -> Box<[ReadAddr]> {
        if self.obj_s == 0 {
            return Box::new([]);
        }

        self.read_underlying(size)
    }
}

#[must_use]
fn read_addr(pos: usize, size: usize) -> ReadAddr {
    if size > 10000 {
        panic!("size too big {}", size);
    }
    if pos == 0 || size == 0 {
        #[cfg(any(NO))]
        if ISDEBUG {
            info!("unchecked 0 :(")
        };
        return ReadAddr {
            pos: 0,
            content: Box::new([]),
        };
    }
    #[cfg(any(NO))]
    if false {
        //info!("here2 {} {}", pos, unsafe { *(0x89b404 as *const usize) });

        unsafe {
            if HeapValidate(
                *(0x89b404 as *const HeapHandle),
                windows::Win32::System::Memory::HEAP_FLAGS(0),
                Some(pos as *const c_void),
            )
            .into()
            {
                let m = read_heap(pos);
                //info!("here1 {}", pos);
                if m != size && m != usize::MAX && m != 940 && m != 944 {
                    
                    info!("values unequal, {} {}", m, size);
                }
            }
        }
    }
    let ptr = pos as *const u8;
    ReadAddr {
        pos: pos,
        content: unsafe { std::slice::from_raw_parts(ptr, size) }.into(),
    }
}
#[must_use]
fn read_vec(pos: usize) -> VecAddr {
    let ptr = pos as *const u8;
    let content = unsafe { std::slice::from_raw_parts(ptr, 12) };

    VecAddr {
        pos: pos,
        start: usize::from_le_bytes(content[0..4].try_into().unwrap()),
        maybecapacity: usize::from_le_bytes(content[4..8].try_into().unwrap()),
        end: usize::from_le_bytes(content[8..12].try_into().unwrap()),
    }
}
#[must_use]
fn read_linked_list(pos: usize, add_size: usize) -> LL3Holder {
    let w = read_addr(pos, 12);

    LL3Holder {
        pos: pos,
        additional_size: add_size,
        ll4: usize::from_le_bytes(w.content[0..4].try_into().unwrap()),
        listcount: usize::from_le_bytes(w.content[4..8].try_into().unwrap()),
        add_data: usize::from_le_bytes(w.content[8..12].try_into().unwrap()),
    }
}
#[must_use]
fn read_ll4(pos: usize) -> LL4 {
    let w = read_addr(pos, 12);
    LL4 {
        pos: pos,
        next: usize::from_le_bytes(w.content[0..4].try_into().unwrap()),
        field2: usize::from_le_bytes(w.content[4..8].try_into().unwrap()),
        additional_data: usize::from_le_bytes(w.content[8..12].try_into().unwrap()),
    }
}

#[must_use]
fn read_deque(pos: usize) -> Deque {
    let m = read_addr(pos, 20);

    let w = m
        .content
        .chunks(4)
        .map(|x| usize::from_le_bytes(x.try_into().unwrap()))
        .collect::<Vec<_>>();

    Deque {
        pos: pos,
        f0: w[0],
        data: w[1],
        size: w[2],
        f3: w[3],
        obj_s: w[4],
    }
}

fn get_ptr(from: &[u8], offset: usize) -> usize {
    usize::from_le_bytes(from[offset..offset + 4].try_into().unwrap())
}

#[derive(Clone, Debug)]
pub struct Frame {
    pub number: usize,
    pub adresses: Box<[ReadAddr]>,
    pub fp: [u8; 108],
    pub frees: Vec<usize>,
    pub allocs: Vec<usize>,

    pub weather_sync_check: u8,
    //charge_buffers: (Option<[i32; 6]>, Option<[i32; 6]>),
}

impl Frame {
    pub fn never_happened(self) {
        for a in self.allocs {
            let heap = unsafe { *(0x89b404 as *const isize) };
            #[cfg(any(NO))]
            if ISDEBUG {
                info!("alloc: {}", a)
            };
            unsafe { HeapFree(heap, 0, a as *const c_void) };
        }
    }

    pub fn did_happen(self) {
        for a in self.frees {
            let heap = unsafe { *(0x89b404 as *const isize) };
            unsafe { HeapFree(heap, 0, a as *const c_void) };
        }
    }

    fn size_data(&self) -> String {
        let addr_total = self.adresses.iter().fold(0, |a, x| a + x.content.len());
        let freetotal = self.frees.len() * 4;
        let alloctotal = self.allocs.len() * 4;
        format!("addr: {addr_total} frees: {freetotal} allocs: {alloctotal}")
    }

    fn redundency_data(&self) -> String {
        let mut w = HashSet::new();
        let mut counter = 0;
        for a in self.adresses.iter() {
            for b in 0..a.content.len() {
                if !w.insert(a.pos + b) {
                    counter += 1;
                }
            }
        }

        format!("reduntant bytes: {}", counter)
    }

    pub fn restore(self) {
        unsafe {
            FPST = self.fp;
            asm!(
                "FRSTOR {fpst}",
                fpst = sym FPST
            )
        }

        for a in self.adresses.clone().to_vec().into_iter() {
            //if ISDEBUG { info!("trying to restore {}", a.pos) };
            a.restore();
            //if ISDEBUG { info!("success") };
        }

        //*PREVIOUS_1.lock().unwrap() = self.charge_buffers.0;
        //*PREVIOUS_2.lock().unwrap() = self.charge_buffers.1;

        //self.did_happen();
    }
}
