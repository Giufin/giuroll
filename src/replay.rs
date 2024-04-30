use crate::{
    pause, read_current_input, read_key_better, resume,
    rollback::{dump_frame, Frame},
    ISDEBUG, LAST_STATE, MEMORY_RECEIVER_ALLOC, MEMORY_RECEIVER_FREE, REAL_INPUT, REAL_INPUT2,
    SOKU_FRAMECOUNT,
};
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicI32, AtomicU32, AtomicU8, Ordering::Relaxed},
        Mutex,
    },
};

struct RePlayRePlay {
    frame: usize,
    p1_inputs: HashMap<usize, [bool; 10]>,
    p2_inputs: HashMap<usize, [bool; 10]>,
    is_p2: bool,
}

impl RePlayRePlay {
    fn new(frame: usize, is_p1: bool) -> Self {
        Self {
            frame,
            p1_inputs: HashMap::new(),
            p2_inputs: HashMap::new(),
            is_p2: is_p1,
        }
    }

    fn read_input(&mut self) {
        let map = if self.is_p2 {
            &mut self.p1_inputs
        } else {
            &mut self.p2_inputs
        };

        unsafe { map.insert(*SOKU_FRAMECOUNT, read_current_input()) };
    }

    fn apply_input(&self) {
        unsafe {
            let fc = *SOKU_FRAMECOUNT;
            REAL_INPUT2 = self.p1_inputs.get(&fc).copied();

            REAL_INPUT = self.p2_inputs.get(&fc).copied();
        }
    }
}

static mut RE_PLAY_PAUSE: usize = 0;

static FRAMES: Mutex<Vec<Frame>> = Mutex::new(Vec::new());
static mut RE_PLAY: Option<RePlayRePlay> = None;

static PAUSESTATE: AtomicU8 = AtomicU8::new(0);
static mut DISABLE_PAUSE: bool = false;

static IS_REWINDING: AtomicU8 = AtomicU8::new(0);
static mut REWIND_PRESSED_LAST_FRAME: bool = false;

pub unsafe extern "cdecl" fn apause(_a: *mut ilhook::x86::Registers, _b: usize) {
    //let pinput = 0x89a248;
    //let input = read_addr(0x89a248, 0x58).usize_align();
    let pstate = PAUSESTATE.load(Relaxed);

    const ABUTTON: *mut usize = (0x89a248 + 0x40) as *mut usize;
    let a_input = *ABUTTON;
    *ABUTTON = 0;
    if DISABLE_PAUSE {
    } else {
        match (a_input, pstate) {
            (0, 1) => PAUSESTATE.store(2, Relaxed),
            (0, _) => (),
            (1, 0) => PAUSESTATE.store(1, Relaxed),
            (1, 1) => (),
            (1, 2) => PAUSESTATE.store(0, Relaxed),
            _ => (),
        }
    }
    //if ISDEBUG { info!("input: {:?}", input[16]) };
}

pub unsafe extern "cdecl" fn is_replay_over(
    a: *mut ilhook::x86::Registers,
    _b: usize,
    _c: usize,
) -> usize {
    // https://stackoverflow.com/a/46134764
    let ori_fun: unsafe extern "fastcall" fn(u32) -> bool =
        unsafe { std::mem::transmute(0x00480860) };
    (*a).eax = (ori_fun((*a).ecx) && RE_PLAY.is_none()) as u32;
    return 0x00482689 + 5;
}

pub unsafe fn clean_replay_statics() {
    for a in std::mem::replace(&mut *FRAMES.lock().unwrap(), vec![]) {
        a.did_happen();
    }

    RE_PLAY = None;
    DISABLE_PAUSE = false;
    set_keys_availability_in_takeover(true);
}

pub unsafe extern "cdecl" fn disable_x_in_takeover(
    a: *mut ilhook::x86::Registers,
    _b: usize,
    _c: usize,
) -> usize {
    let should_jump = (*a).ebp == *(((*a).eax + 0x44) as *const u32);

    if RE_PLAY.is_some() || !should_jump {
        0x4826bb
    } else {
        0x4825e5
    }
}

unsafe fn set_keys_availability_in_takeover(enable: bool) {
    for n in 0..=1 {
        let input_manager = *((0x00898680 as *const *mut u32).offset(n));
        if input_manager != 0 as *mut u32 {
            if !enable {
                *input_manager.offset(0x18) = 0; // clear InputManager.inKeys
            }
            *input_manager.offset(0x19) = !enable as u32; // InputManager.readInKeys
        }
    }
}

unsafe fn set_keybinding_by_index(
    p_profile_info_src: *const i8,
    p_profile_info_dst: *mut i8,
    index: i8,
) {
    // reimplement Soku function 0x00434bf0
    *p_profile_info_dst.offset(0x1a8) = index;
    if index == -1 {
        // keyboard
        // apply keybinding
        p_profile_info_src
            .offset(0x140)
            .copy_to_nonoverlapping(p_profile_info_dst.offset(0xd0).offset(4), 0x34);
    } else if *(0x008a02b8 as *const i8) <= index {
        // if controller counter <= index
        *p_profile_info_dst.offset(0xd0).offset(4) = -2;
        p_profile_info_dst
            .offset(0xd0)
            .offset(4 + 1)
            .write_bytes(0, 0x34 - 1);
    } else {
        p_profile_info_src
            .offset(0x174 + 1)
            .copy_to_nonoverlapping(p_profile_info_dst.offset(0xd0).offset(4 + 1), 0x34 - 1);
        *p_profile_info_dst.offset(0xd0).offset(4) = index;
    }
}

unsafe fn load_keybinding() {
    // (partially) reimplement Soku 0x0043f0eb ~ 0x0043f123
    let p1_controller_index = *(0x00898678 as *const i8);
    set_keybinding_by_index(
        0x00898868 as *const i8,
        0x00898868 as *mut i8,
        p1_controller_index,
    );
    // set p2:
    // let mut p2_controller_index = *(0x00898679 as *const i8);
    // if (p1_controller_index == -1 && p2_controller_index == -2) {
    //     p2_controller_index = -1;
    // }
    // set_keybinding_by_index(
    //     0x00899054 as *const i8,
    //     0x00899054 as *mut i8,
    //     p2_controller_index,
    // );
}

pub unsafe fn handle_replay(
    framecount: usize,
    battle_state: &mut u32,
    cur_speed: &mut u32,
    cur_speed_iter: &mut u32,
    weird_counter: &mut u32,
) {
    if let Some(x) = FRAMES.lock().unwrap().last_mut() {
        //TODO
        while let Ok(man) = MEMORY_RECEIVER_ALLOC.as_ref().unwrap().try_recv() {
            x.allocs.push(man);
        }

        while let Ok(man) = MEMORY_RECEIVER_FREE.as_ref().unwrap().try_recv() {
            x.frees.push(man);
            x.allocs.retain(|x| *x != man);
        }
    }

    let scheme = [0x02, 0x03, 0x04, 0x05];
    //let scheme = [0x10, 0x11, 0x12, 0x13];

    let mut override_target_frame = None;
    let qdown = read_key_better(scheme[0]);
    if qdown {
        if let Some(rprp) = RE_PLAY.take() {
            override_target_frame = Some(rprp.frame as u32 - 1);
            DISABLE_PAUSE = false;
            set_keys_availability_in_takeover(true);
        }
    }

    let wdown = read_key_better(scheme[1]);
    if wdown {
        if let Some(x) = &mut RE_PLAY {
            x.is_p2 = false;
            override_target_frame = Some(x.frame as u32 - 1);
            RE_PLAY_PAUSE = 40;
        } else {
            RE_PLAY = Some(RePlayRePlay::new(framecount, false));
        }
    }

    let edown = read_key_better(scheme[2]);
    if edown {
        if let Some(x) = &mut RE_PLAY {
            x.is_p2 = true;
            override_target_frame = Some(x.frame as u32 - 1);
            RE_PLAY_PAUSE = 40;
        } else {
            RE_PLAY = Some(RePlayRePlay::new(framecount, true));
        }
    }

    if wdown || edown {
        RE_PLAY_PAUSE = 40;
        RE_PLAY_PAUSE = 40;
        DISABLE_PAUSE = true;
        set_keys_availability_in_takeover(false);
        load_keybinding();
    }

    let rdown = read_key_better(scheme[3]);
    if rdown {
        if let Some(rprp) = &RE_PLAY {
            override_target_frame = Some(rprp.frame as u32 - 1);
            RE_PLAY_PAUSE = 40;
        }
    }

    if let Some(rprp) = &mut RE_PLAY {
        if rprp.frame + 1 < *SOKU_FRAMECOUNT {
            *cur_speed = 1;

            rprp.read_input();
            rprp.apply_input()
        }
    }

    if RE_PLAY_PAUSE == 1 {
        RE_PLAY_PAUSE -= 1;
        PAUSESTATE.store(0, Relaxed);
    } else if RE_PLAY_PAUSE > 1 {
        RE_PLAY_PAUSE -= 1;
        PAUSESTATE.store(1, Relaxed);
    }

    resume(battle_state);
    if *cur_speed_iter == 0 && PAUSESTATE.load(Relaxed) != 0 && override_target_frame.is_none() {
        if RE_PLAY.is_some() {
            REWIND_PRESSED_LAST_FRAME = true
        }
        match (*cur_speed, REWIND_PRESSED_LAST_FRAME) {
            (16, false) => {
                REWIND_PRESSED_LAST_FRAME = true;
                override_target_frame = Some(framecount as u32 + 1);
            }
            (8, false) => {
                REWIND_PRESSED_LAST_FRAME = true;
                override_target_frame = Some(framecount as u32 - 2);
            }
            _ => {
                if *cur_speed == 1 {
                    REWIND_PRESSED_LAST_FRAME = false;
                }
                *cur_speed = 0;
                pause(battle_state, weird_counter);
                return;
            }
        }

        *cur_speed = 1;
    }

    /*
       let wthr = 0x8971d0;
       if *(wthr as *const usize) != 11 && read_key_better(0x18) {
           *cur_speed = 1024;
       }

       if *cur_speed == 1024 && read_key_better(0x18) && *(wthr as *const usize) == 11{
           *cur_speed_iter = 1024;
       }
    */
    if RE_PLAY.is_none() {
        let speed = 4096;

        let condition = matches!(battle_state, 5 | 3);
        let key = read_key_better(0x18);

        if !condition && key {
            *cur_speed = speed;
        }

        if *cur_speed == speed && key && condition {
            *cur_speed_iter = speed;
        }
    }

    if *cur_speed_iter == 0 {
        //"true" frame

        IS_REWINDING.store(0, Relaxed);
        let qdown = read_key_better(0x10);

        if (qdown && RE_PLAY.is_none()) || override_target_frame.is_some() {
            let target = if let Some(x) = override_target_frame {
                x
            } else {
                let target = (framecount).saturating_sub(*cur_speed as usize + 1);
                target as u32
            };

            let mutex = FRAMES.lock().unwrap();
            let mut map = mutex;
            let frames = &mut *map;
            let mut last: Option<Frame> = None;
            loop {
                let candidate = frames.pop();
                if let Some(x) = candidate {
                    if let Some(x) = last {
                        x.never_happened();
                    }

                    let framenum = x.number as u32;
                    if framenum <= target {
                        IS_REWINDING.store(1, Relaxed);
                        //good
                        let diff = target - framenum;

                        x.clone().restore();
                        //x.did_happen();
                        //dump_frame();
                        //unsafe {
                        //    FPST = x.fp;
                        //    asm!(
                        //        "FRSTOR {fpst}",
                        //        fpst = sym FPST
                        //    )
                        //}
                        //
                        //for a in x.adresses.clone().to_vec().into_iter() {
                        //    //if ISDEBUG { info!("trying to restore {}", a.pos) };
                        //    a.restore();
                        //    //if ISDEBUG { info!("success") };
                        //}

                        *cur_speed_iter = 1;
                        *cur_speed = 1 + diff;
                        //                        let diff = 1;

                        if diff <= 0 && false {
                            pause(battle_state, weird_counter);
                            *cur_speed_iter = *cur_speed;
                            return;
                        }

                        break;
                    } else {
                        last = Some(x);
                        continue;
                    }
                } else {
                    //nothing can be done ?

                    if let Some(last) = last {
                        last.restore();
                        //did it happen?
                    }
                    pause(battle_state, weird_counter);
                    *cur_speed_iter = *cur_speed;

                    return;
                }
            }
        }
    }

    let framecount = *SOKU_FRAMECOUNT;

    if framecount % 16 == 1 || IS_REWINDING.load(Relaxed) == 1 {
        let frame = dump_frame();

        let mut mutex = FRAMES.lock().unwrap();

        mutex.push(frame);
    }
}
