use crate::{
    pause, read_key_better, println, resume,
    rollback::{dump_frame, Frame},
    MEMORY_RECEIVER_ALLOC, MEMORY_RECEIVER_FREE, SOKU_FRAMECOUNT,
};
use std::sync::{
    atomic::{AtomicU8, Ordering::Relaxed},
    Mutex,
};

static FRAMES: Mutex<Vec<Frame>> = Mutex::new(Vec::new());

static PAUSESTATE: AtomicU8 = AtomicU8::new(0);

static IS_REWINDING: AtomicU8 = AtomicU8::new(0);
static mut REWIND_PRESSED_LAST_FRAME: bool = false;

pub unsafe extern "cdecl" fn apause(_a: *mut ilhook::x86::Registers, _b: usize) {
    //let pinput = 0x89a248;
    //let input = read_addr(0x89a248, 0x58).usize_align();
    let pstate = PAUSESTATE.load(Relaxed);

    const ABUTTON: *mut usize = (0x89a248 + 0x40) as *mut usize;
    let a_input = *ABUTTON;
    *ABUTTON = 0;

    match (a_input, pstate) {
        (0, 1) => PAUSESTATE.store(2, Relaxed),
        (0, _) => (),
        (1, 0) => PAUSESTATE.store(1, Relaxed),
        (1, 1) => (),
        (1, 2) => PAUSESTATE.store(0, Relaxed),
        _ => (),
    }

    //if ISDEBUG { info!("input: {:?}", input[16]) };
}

pub unsafe fn clean_replay_statics() {
    for a in std::mem::replace(&mut *FRAMES.lock().unwrap(), vec![]) {
        a.did_happen();
    }
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
    let mut override_target_frame = None;

    resume(battle_state);
    if *cur_speed_iter == 0 && PAUSESTATE.load(Relaxed) != 0 && override_target_frame.is_none() {
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

    if *cur_speed_iter == 0 {
        //"true" frame

        IS_REWINDING.store(0, Relaxed);
        let qdown = read_key_better(0x10);

        if qdown || override_target_frame.is_some() {
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
