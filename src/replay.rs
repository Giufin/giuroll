use crate::{
    change_delay_from_keys, draw_num, draw_num_x_center, get_num_length, pause, println, ptr_wrap,
    read_current_input, read_key_better, resume,
    rollback::{dump_frame, Frame, DUMP_FRAME_TIME},
    soku_heap_free, CENTER_X_P1, CENTER_X_P2, CENTER_Y_P1, CENTER_Y_P2, DISABLE_SOUND,
    ENABLE_CHECK_MODE, INSIDE_COLOR, INSIDE_HALF_HEIGHT, INSIDE_HALF_WIDTH, MEMORY_RECEIVER_ALLOC,
    MEMORY_RECEIVER_FREE, NEXT_DRAW_ROLLBACK, OUTER_COLOR, OUTER_HALF_HEIGHT, OUTER_HALF_WIDTH,
    PROGRESS_COLOR, REAL_INPUT, REAL_INPUT2, SOKU_FRAMECOUNT, TAKEOVER_COLOR,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    io::Write,
    iter::Empty,
    os::raw::c_void,
    sync::atomic::{AtomicU8, Ordering::Relaxed},
    time::Duration,
    u32,
};
use winapi::shared::{
    d3d9::IDirect3DDevice9,
    d3d9types::{D3DCLEAR_TARGET, D3DRECT},
};
use windows::Win32::System::Console::AllocConsole;

struct RePlayRePlay {
    frame: usize,
    p1_inputs: HashMap<usize, [bool; 10]>,
    p2_inputs: HashMap<usize, [bool; 10]>,
    is_p2: bool,
}

impl RePlayRePlay {
    fn new(frame: usize, is_p1: bool) -> Self {
        println!("new RePlayRePlay at {}", frame);
        Self {
            frame,
            p1_inputs: HashMap::new(),
            p2_inputs: HashMap::new(),
            is_p2: is_p1,
        }
    }

    fn read_input(&mut self) {
        // println!("read {}", unsafe { *SOKU_FRAMECOUNT });
        let map = if self.is_p2 {
            &mut self.p2_inputs
        } else {
            &mut self.p1_inputs
        };

        unsafe { map.insert(*SOKU_FRAMECOUNT, read_current_input()) };
    }

    fn apply_input(&self) {
        unsafe {
            let fc = *SOKU_FRAMECOUNT;
            // println!(
            //     "write {} {} {}",
            //     *SOKU_FRAMECOUNT,
            //     self.p1_inputs.get(&fc).is_some(),
            //     self.p2_inputs.get(&fc).is_some()
            // );

            REAL_INPUT = if self.p1_inputs.get(&fc).is_none()
                && REPLAY_KO_FRAMECOUNT == Some(*SOKU_FRAMECOUNT)
            {
                Some([false; 10])
            } else {
                self.p1_inputs.get(&fc).copied()
            };

            REAL_INPUT2 = self.p2_inputs.get(&fc).copied();
        }
    }
}

static mut RE_PLAY_PAUSE: usize = 0;

static mut FRAMES: VecDeque<Frame> = VecDeque::new();
static mut RE_PLAY: Option<RePlayRePlay> = None;

static PAUSESTATE: AtomicU8 = AtomicU8::new(0);
static mut DISABLE_PAUSE: bool = false;

static IS_REWINDING: AtomicU8 = AtomicU8::new(0);
static mut REWIND_PRESSED_LAST_FRAME: bool = false;

static mut REPLAY_KO_FRAMECOUNT: Option<usize> = None;

static mut LAST_TARGET: Option<usize> = None;

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

static mut D3D9_DEVICE: *mut *mut IDirect3DDevice9 = 0x008A0E30 as *mut *mut IDirect3DDevice9;

pub unsafe fn render_replay_progress_bar_and_numbers() {
    let gametype_main = *(0x898688 as *const u32);
    let is_netplay = *(0x8986a0 as *const usize) != 0;
    // assert!(ORI_BATTLE_WATCH_ON_RENDER.is_some());
    if is_netplay || gametype_main != 2 {
        return;
    }

    if RE_PLAY.is_none() {
        return;
    }

    // let crenderer_begin: unsafe extern "cdecl" fn() = std::mem::transmute(0x00401000);
    // let crenderer_end: unsafe extern "fastcall" fn(*const c_void) = std::mem::transmute(0x00401040);

    let mut center_x = CENTER_X_P1;
    let mut center_y = CENTER_Y_P1;
    if let Some(replay) = &RE_PLAY
        && replay.is_p2
    {
        center_x = CENTER_X_P2;
        center_y = CENTER_Y_P2;
    }

    // crenderer_begin();

    if RE_PLAY_PAUSE == 0
        && let Some(replay) = &RE_PLAY
    {
        let frame_count = (*SOKU_FRAMECOUNT - replay.frame) as i32;
        let frame_half_len = (get_num_length(frame_count, true) / 2.0) as i32;
        let outer = D3DRECT {
            x1: center_x - frame_half_len,
            x2: center_x + frame_half_len,
            y1: center_y - OUTER_HALF_HEIGHT,
            y2: center_y + OUTER_HALF_HEIGHT,
        };
        (**D3D9_DEVICE).Clear(1, &outer, D3DCLEAR_TARGET, TAKEOVER_COLOR, 0.0, 0);
        draw_num_x_center(
            (center_x as f32, (center_y - INSIDE_HALF_HEIGHT) as f32),
            frame_count,
        );
    } else {
        let outer = D3DRECT {
            x1: center_x - OUTER_HALF_WIDTH,
            x2: center_x + OUTER_HALF_WIDTH,
            y1: center_y - OUTER_HALF_HEIGHT,
            y2: center_y + OUTER_HALF_HEIGHT,
        };
        (**D3D9_DEVICE).Clear(1, &outer, D3DCLEAR_TARGET, OUTER_COLOR, 0.0, 0);

        let progress_length: i32 = 2 * INSIDE_HALF_WIDTH * (40 - RE_PLAY_PAUSE as i32) / 40;

        let inside = D3DRECT {
            x1: center_x - INSIDE_HALF_WIDTH,
            x2: center_x - INSIDE_HALF_WIDTH + progress_length,
            y1: center_y - INSIDE_HALF_HEIGHT,
            y2: center_y + INSIDE_HALF_HEIGHT,
        };
        (**D3D9_DEVICE).Clear(1, &inside, D3DCLEAR_TARGET, PROGRESS_COLOR, 0.0, 0);

        let progress = D3DRECT {
            x1: center_x - INSIDE_HALF_WIDTH + progress_length,
            x2: center_x + INSIDE_HALF_WIDTH,
            y1: center_y - INSIDE_HALF_HEIGHT,
            y2: center_y + INSIDE_HALF_HEIGHT,
        };
        (**D3D9_DEVICE).Clear(1, &progress, D3DCLEAR_TARGET, INSIDE_COLOR, 0.0, 0);

        draw_num_x_center(
            (center_x as f32, (center_y - INSIDE_HALF_HEIGHT) as f32),
            RE_PLAY_PAUSE as i32,
        );
    }

    // crenderer_end(0x896b4c as *const c_void);

    return;
}

pub unsafe extern "cdecl" fn is_replay_over(
    a: *mut ilhook::x86::Registers,
    _b: usize,
    _c: usize,
) -> usize {
    // https://stackoverflow.com/a/46134764
    let ori_fun: unsafe extern "fastcall" fn(u32) -> bool =
        unsafe { std::mem::transmute(0x00480860) };
    (*a).eax = (ori_fun((*a).ecx) && RE_PLAY.is_none() && CHECK.is_none()) as u32;
    // A workaround for removing the single (p1 only) extra input at the end of replays with KO
    // Check whether the deque saving inputs has only one element (size == 1):
    if *ptr_wrap!((*(0x0089881c as *const *const u32)).offset(0x4c / 4)) == 1 {
        // When the replay is over at this frame because of KO
        REPLAY_KO_FRAMECOUNT = Some(*SOKU_FRAMECOUNT);
    }
    return 0x00482689 + 5;
}

pub unsafe fn clean_replay_statics() {
    for mut a in std::mem::replace(&mut FRAMES, VecDeque::new()) {
        a.did_happen();
    }
    if let Some(frees) = FREES.take() {
        for a in frees {
            soku_heap_free!(a);
        }
    }
    ALLOCS = None;

    DISABLE_PAUSE = false;
    if RE_PLAY.take().is_some() || PERFORMANCE_TEST.take().is_some() {
        set_keys_availability_in_takeover(true);
    }
    NEXT_DRAW_ROLLBACK = None;
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

#[derive(Eq, PartialEq, Debug)]
enum CheckStep {
    TestPlay1,
    TestPlay2,
    TestRollback(u32, u32, bool),
}
#[derive(Debug)]
struct F32 {
    f: f32,
}

impl PartialEq for F32 {
    fn eq(&self, other: &Self) -> bool {
        return self.f.to_ne_bytes() == other.f.to_ne_bytes();
    }
}
impl Eq for F32 {}

#[derive(Eq, PartialEq, Debug)]
struct PlayerData {
    x_pos: F32,
    y_pos: F32,
    x_speed: F32,
    y_speed: F32,
    gravity: F32,
    dir: i8,
    health: i16,
    hit_state: i32,
    untech: i16,
}

impl PlayerData {
    pub unsafe fn from_player(p_player: *const c_void) -> Self {
        let p_player = p_player as *const u8;
        Self {
            x_pos: F32 {
                f: *(p_player.offset(0xEC) as *const f32),
            },
            y_pos: F32 {
                f: *(p_player.offset(0xF0) as *const f32),
            },
            x_speed: F32 {
                f: *(p_player.offset(0xF4) as *const f32),
            },
            y_speed: F32 {
                f: *(p_player.offset(0xF8) as *const f32),
            },
            gravity: F32 {
                f: *(p_player.offset(0x100) as *const f32),
            },
            dir: *(p_player.offset(0x104) as *const i8),
            health: *(p_player.offset(0x184) as *const i16),
            hit_state: *(p_player.offset(0x190) as *const i32),
            untech: *(p_player.offset(0x4BA) as *const i16),
        }
    }
}
#[derive(Eq, PartialEq, Debug)]
struct CheckData {
    p1_data: PlayerData,
    p2_data: PlayerData,
    battle_state: u32,
}

impl CheckData {
    pub unsafe fn from_battle() -> Self {
        let p_battle_manager = *(0x008985E4 as *const *const u8);
        Self {
            p1_data: PlayerData::from_player(*(p_battle_manager.offset(0xc) as *const _)),
            p2_data: PlayerData::from_player(*(p_battle_manager.offset(0x10) as *const _)),
            battle_state: *(p_battle_manager.offset(0x88) as *const u32),
        }
    }
}
pub struct Check {
    check_step: CheckStep,
    check_data: Vec<CheckData>,
    is_failed: bool,
}

pub struct PerformanceTest {
    base_framecount: usize,
    max_rollback: usize,
}

static mut ALLOCS: Option<HashSet<usize>> = None;
static mut FREES: Option<HashSet<usize>> = None;
pub static mut CHECK: Option<Check> = None;
pub static mut PERFORMANCE_TEST: Option<PerformanceTest> = None;
// const NO_ACCESS: bool = false;
const DEFAULT_TESTED_MAX_ROLLBACK: u32 = 6;
const TEST_ALL_SMALLER_ROLLBACK: bool = true;
pub unsafe fn handle_replay(
    framecount: usize,
    battle_state: &mut u32,
    cur_speed: &mut u32,
    cur_speed_iter: &mut u32,
    weird_counter: &mut u32,
    scheme: &[u8; 4],
) {
    if framecount == 0 {
        REPLAY_KO_FRAMECOUNT = None;
        LAST_TARGET = None;
        if ENABLE_CHECK_MODE && read_key_better(0x2E) {
            DUMP_FRAME_TIME = Some(Duration::ZERO);
            let _ = AllocConsole();
            println!("Enter check mode.");
            println!("Start step 1: playing the replay normally.");
            DISABLE_SOUND = true;
            CHECK = Some(Check {
                check_step: CheckStep::TestPlay1,
                check_data: Vec::new(),
                is_failed: false,
            });
            PAUSESTATE.store(0, Relaxed);
        } else if let Some(max_rb) = (1..=13).find_map(|x| read_key_better(x + 1).then_some(x)) {
            PAUSESTATE.store(0, Relaxed);
            NEXT_DRAW_ROLLBACK = Some(max_rb as i32);
            PERFORMANCE_TEST = Some(PerformanceTest {
                base_framecount: 0,
                max_rollback: max_rb as usize,
            });
            set_keys_availability_in_takeover(false);
        } else {
            DISABLE_SOUND = false;
            CHECK = None
        }; // press C
        ALLOCS = Some(HashSet::new());
        FREES = Some(HashSet::new());
    } else {
        if *cur_speed_iter == 0 && CHECK.is_some() {
            println!("WARNING: CHECK with cur_speed_iter == 0");
        }
    }

    for man in MEMORY_RECEIVER_ALLOC.as_ref().unwrap().try_iter() {
        ALLOCS.as_mut().unwrap().insert(man);
    }
    for a in MEMORY_RECEIVER_FREE.as_ref().unwrap().try_iter() {
        if ALLOCS.as_ref().unwrap().contains(&a) {
            ALLOCS.as_mut().unwrap().remove(&a);
            soku_heap_free!(a);
        } else {
            FREES.as_mut().unwrap().insert(a);
        }
    }

    unsafe fn read_key_if_no_test(key: u8) -> bool {
        match CHECK.is_some() || PERFORMANCE_TEST.is_some() {
            false => read_key_better(key),
            true => false,
        }
    }
    REAL_INPUT = None;
    REAL_INPUT2 = None;

    //let scheme = [0x02, 0x03, 0x04, 0x05];
    //let scheme = [0x10, 0x11, 0x12, 0x13];
    let mut override_target_frame: Option<u32> = None;
    // println!(
    //     "{}: cur_speed_iter {}, cur_speed {}, pause {}",
    //     framecount,
    //     *cur_speed_iter,
    //     *cur_speed,
    //     PAUSESTATE.load(Relaxed)
    // );

    if *cur_speed_iter + 1 >= *cur_speed
        && let Some(t) = LAST_TARGET.take()
        && t != framecount
    {
        println!("mistake frame {}, should be {}", framecount, t);
    }

    if *cur_speed_iter == 0 && framecount >= 2 {
        let qdown = read_key_if_no_test(scheme[0]);
        if qdown {
            if let Some(rprp) = RE_PLAY.take() {
                override_target_frame = Some(rprp.frame as u32 - 1);
                DISABLE_PAUSE = false;
                RE_PLAY_PAUSE = 1;
                set_keys_availability_in_takeover(true);
                // let mut frames = FRAMES.lock().unwrap();
                // let least_modify_frame = rprp
                //     .p1_inputs
                //     .keys()
                //     .chain(rprp.p2_inputs.keys())
                //     .min()
                //     .unwrap_or(&(rprp.frame - 1))
                //     + 1;
                // while let Some(x) = frames.last()
                //     && x.number >= least_modify_frame
                // {
                //     frames.pop();
                // }
            }
        }

        let wdown = read_key_if_no_test(scheme[1]);
        if wdown {
            if let Some(x) = &mut RE_PLAY {
                x.is_p2 = false;
                override_target_frame = Some(x.frame as u32 - 1);
                RE_PLAY_PAUSE = 40;
            } else {
                RE_PLAY = Some(RePlayRePlay::new(framecount + 1, false));
            }
        }

        let edown = read_key_if_no_test(scheme[2]);
        if edown {
            if let Some(x) = RE_PLAY.as_mut() {
                x.is_p2 = true;
                override_target_frame = Some(x.frame as u32 - 1);
                RE_PLAY_PAUSE = 40;
            } else {
                RE_PLAY = Some(RePlayRePlay::new(framecount + 1, true));
            }
        }

        if wdown || edown {
            RE_PLAY_PAUSE = 40;
            RE_PLAY_PAUSE = 40;
            DISABLE_PAUSE = true;
            set_keys_availability_in_takeover(false);
            load_keybinding();
        }

        let rdown = read_key_if_no_test(scheme[3]);
        if rdown {
            if let Some(rprp) = &RE_PLAY {
                override_target_frame = Some(rprp.frame as u32 - 1);
                RE_PLAY_PAUSE = 40;
            }
        }

        if RE_PLAY_PAUSE == 1 {
            RE_PLAY_PAUSE -= 1;
            PAUSESTATE.store(0, Relaxed);
        } else if RE_PLAY_PAUSE > 1 {
            RE_PLAY_PAUSE -= 1;
            PAUSESTATE.store(1, Relaxed);
        }
    }

    resume(battle_state);

    if let Some(check) = CHECK.as_mut() {
        unsafe fn check_failed() {
            println!(
                "Check doesn't pass! Failed at frame {}. Now paused there",
                *SOKU_FRAMECOUNT
            );
            CHECK = None;
            PAUSESTATE.store(1, Relaxed);
        }
        let mut check_different_frame = |old: &CheckData, new: &CheckData| -> bool {
            if *old != *new {
                check.is_failed = true;
                println!("Difference at frame {} is determined.", *SOKU_FRAMECOUNT);
                println!("old: {:?}", old);
                println!("new: {:?}", new);
                println!("Continue or not? (Y or N, default N.)");
                print!("> ");
                let _ = std::io::stdout().flush();
                loop {
                    let mut buf = String::new();
                    match std::io::stdin().read_line(&mut buf) {
                        Ok(_) => match buf.as_str().trim() {
                            "y" | "Y" => {
                                println!("Continue...");
                                return false;
                            }
                            "n" | "N" | _ => {
                                println!("The test has been failed.");
                                return true;
                            }
                        },
                        Err(e) => {
                            println!("Warning: can't read stdin: {}", e.to_string());
                            return true;
                        }
                    }
                }
            }
            return false;
        };
        let ori_is_replay_over: unsafe extern "fastcall" fn(u32) -> bool =
            unsafe { std::mem::transmute(0x00480860) };
        let is_over = ori_is_replay_over(0x898718) || *battle_state == 5;
        unsafe fn get_input(is_p2: bool) -> [bool; 10] {
            let p_battle_manager = *(0x008985E4 as *const *const u8);
            let player_addr: *const u8 = match is_p2 {
                true => *(p_battle_manager.offset(0x10) as *const _),
                false => *(p_battle_manager.offset(0xc) as *const _),
            };
            let addr: *const i32 = player_addr.offset(0x754) as *const i32;
            let mut ret = [false; 10];
            (ret[0], ret[1]) = (*addr.offset(1) < 0, *addr.offset(1) > 0);
            (ret[2], ret[3]) = (*addr.offset(0) < 0, *addr.offset(0) > 0);
            for i in 2..8 {
                ret[i - 2 + 4] = *addr.offset(i as isize) > 0;
            }
            ret
        }
        unsafe fn apply_old_input() {
            REAL_INPUT = Some(get_input(false));
            REAL_INPUT2 = Some(get_input(true));
            // println!("{:?} {:?}", REAL_INPUT, REAL_INPUT2);
        }
        match check.check_step {
            CheckStep::TestPlay1 => {
                check.check_data.push(CheckData::from_battle());
                if is_over {
                    override_target_frame = Some(1);
                    check.check_step = CheckStep::TestPlay2;
                    FRAMES.push_back(dump_frame(None::<Empty<_>>, None::<Empty<_>>));
                    println!("Step 1 got {} frames.", check.check_data.len());
                    println!("Start step 2: rollbacking to the beginning and replaying the replay.")
                } else {
                    override_target_frame = Some(*SOKU_FRAMECOUNT as u32 + 1);
                }
            }
            CheckStep::TestPlay2 => {
                if *SOKU_FRAMECOUNT >= check.check_data.len() {
                    println!(
                        "Step 2 got more frame then {} got by step 1, at frame {}.",
                        *SOKU_FRAMECOUNT + 1,
                        *SOKU_FRAMECOUNT
                    );
                    check_failed();
                } else if check_different_frame(
                    &check.check_data[*SOKU_FRAMECOUNT],
                    &CheckData::from_battle(),
                ) {
                    check_failed();
                } else if is_over {
                    if *SOKU_FRAMECOUNT + 1 < check.check_data.len() {
                        println!("Step 2 ends too early at frame {}.", *SOKU_FRAMECOUNT);
                        check_failed();
                    } else {
                        println!("Step 2 is consistent with step 1. Step 2 pass!");
                        println!("Step 3: replaying the replay with rollbacks everywhere");
                        override_target_frame = Some(1);
                        // apply_old_input();
                        check.check_step = CheckStep::TestRollback(1, 1, false);
                    }
                } else {
                    override_target_frame = Some(*SOKU_FRAMECOUNT as u32 + 1);
                }
            }
            CheckStep::TestRollback(being_tested_frame, cur_rollback, stepping) => {
                // println!("{}", *SOKU_FRAMECOUNT);
                if stepping
                    && *SOKU_FRAMECOUNT < check.check_data.len()
                    && check_different_frame(
                        &check.check_data[*SOKU_FRAMECOUNT],
                        &CheckData::from_battle(),
                    )
                {
                    check_failed();
                } else if stepping && *SOKU_FRAMECOUNT + 1 < check.check_data.len() && is_over {
                    println!("Step 3 ends too early at frame {}.", being_tested_frame);
                    check_failed();
                } else if *SOKU_FRAMECOUNT as u32 == being_tested_frame + cur_rollback {
                    let being_tested_frame_ = being_tested_frame as usize;
                    if being_tested_frame_ >= check.check_data.len() {
                        println!(
                            "Step 3 got more frame then {} got by step 1, at frame {}.",
                            being_tested_frame + 1,
                            being_tested_frame
                        );
                        check_failed();
                    } else if being_tested_frame_ + 1 == check.check_data.len() {
                        println!(
                            "Tests are complete! {} Time: {:?}",
                            match check.is_failed {
                                false => "All tests pass!",
                                true => "At least a test was failed.",
                            },
                            DUMP_FRAME_TIME.unwrap()
                        );
                        PAUSESTATE.store(1, Relaxed);
                        CHECK = None;
                    } else {
                        let (new_tested_frame, tested_rollback, step) =
                            match (cur_rollback == DEFAULT_TESTED_MAX_ROLLBACK, stepping) {
                                (_, false) => (being_tested_frame, cur_rollback, true),
                                (false, true) => (being_tested_frame, cur_rollback + 1, false),
                                (true, true) => {
                                    // println!("clear frame {}", being_tested_frame);
                                    while let Some(frame) = FRAMES.front_mut()
                                        && frame.number <= being_tested_frame_
                                    {
                                        frame.did_happen();
                                        FRAMES.pop_front();
                                    }
                                    if being_tested_frame % 300 == 0 {
                                        println!(
                                            "{} / {}",
                                            being_tested_frame + 1,
                                            check.check_data.len()
                                        );
                                        println!("{:?}", check.check_data[being_tested_frame_]);
                                    };
                                    (
                                        being_tested_frame + 1,
                                        match TEST_ALL_SMALLER_ROLLBACK {
                                            true => 1,
                                            false => DEFAULT_TESTED_MAX_ROLLBACK,
                                        },
                                        false,
                                    )
                                }
                            };
                        override_target_frame = Some(new_tested_frame);
                        check.check_step =
                            CheckStep::TestRollback(new_tested_frame, tested_rollback, step);
                        // if !step {
                        //     apply_old_input();
                        // }
                    }
                    // println!("{:?}", check.check_step);
                } else {
                    if !stepping {
                        apply_old_input();
                        // override_target_frame = Some(*SOKU_FRAMECOUNT as u32 + 1);
                    }
                    override_target_frame = Some(*SOKU_FRAMECOUNT as u32 + 1);
                }
            }
        }
        // println!("target {:?}", override_target_frame);
    }

    if let Some(test) = PERFORMANCE_TEST.as_mut() {
        if framecount == test.base_framecount {
            override_target_frame = Some(
                (test.base_framecount + test.max_rollback)
                    .try_into()
                    .unwrap(),
            );
            // println!("to {} + 1", override_target_frame.unwrap());
        } else if framecount >= test.base_framecount + test.max_rollback + 1 {
            // println!("now {}", framecount);
            test.base_framecount += 1;
            override_target_frame = Some(test.base_framecount.try_into().unwrap());
            while let Some(frame) = FRAMES.front_mut()
                && frame.number < test.base_framecount
            {
                frame.did_happen();
                FRAMES.pop_front();
            }
        }
    }

    if RE_PLAY.is_some() || PAUSESTATE.load(Relaxed) == 0 {
        REWIND_PRESSED_LAST_FRAME = false;
    }
    if *cur_speed_iter == 0
        && PAUSESTATE.load(Relaxed) != 0
        && override_target_frame.is_none()
        && RE_PLAY.is_none()
        && CHECK.is_none()
    {
        match (*cur_speed, REWIND_PRESSED_LAST_FRAME) {
            (16, false) => {
                REWIND_PRESSED_LAST_FRAME = true;
                override_target_frame = Some(framecount as u32 + 1);
            }
            (8, false) => {
                REWIND_PRESSED_LAST_FRAME = true;
                override_target_frame = Some(framecount as u32 - 1);
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
       if *(wthr as *const usize) != 11 && read_key_if_no_test(0x18) {
           *cur_speed = 1024;
       }

       if *cur_speed == 1024 && read_key_if_no_test(0x18) && *(wthr as *const usize) == 11{
           *cur_speed_iter = 1024;
       }
    */
    if RE_PLAY.is_none() {
        let speed = 4096;

        let condition = matches!(battle_state, 5 | 3);
        let key = read_key_if_no_test(0x18);

        if !condition && key {
            *cur_speed = speed;
        }

        if *cur_speed == speed && key && condition {
            *cur_speed_iter = speed;
        }
    }

    let mut pause_for_override_target_frame: bool = false;
    if *cur_speed_iter == 0 || override_target_frame.is_some() {
        //"true" frame

        IS_REWINDING.store(0, Relaxed);

        'to_target_frame: {
            let target = if let Some(x) = override_target_frame {
                if x < framecount as u32 {
                    x
                } else {
                    *cur_speed_iter = 1;
                    pause_for_override_target_frame = x == framecount as u32;
                    *cur_speed = match pause_for_override_target_frame {
                        true => 3,
                        false => 2 + x - framecount as u32,
                    };
                    break 'to_target_frame;
                }
            } else {
                let qdown = read_key_if_no_test(0x10);
                if qdown && RE_PLAY.is_none() {
                    let target = (framecount).saturating_sub(*cur_speed as usize + 1);
                    target as u32
                } else {
                    break 'to_target_frame;
                }
            };
            LAST_TARGET = Some(target as _);

            // println!("target {}", target);

            let mut dropped_frame: Vec<Frame> = vec![];
            loop {
                let candidate = FRAMES.back_mut();
                if let Some(x) = candidate {
                    let framenum = x.number as u32;
                    if framenum <= target {
                        IS_REWINDING.store(1, Relaxed);
                        //good
                        let diff = target - framenum;
                        x.restore(
                            Some(dropped_frame.iter_mut().rev()),
                            Some(ALLOCS.replace(HashSet::new()).unwrap().into_iter()),
                            Some(FREES.replace(HashSet::new()).unwrap().into_iter()),
                        );

                        *cur_speed_iter = 1;
                        if diff == 0 {
                            pause_for_override_target_frame = true;
                        }
                        *cur_speed = match pause_for_override_target_frame {
                            true => 3,
                            false => 2 + diff,
                        };
                        //                        let diff = 1;

                        if diff <= 0 && false {
                            pause(battle_state, weird_counter);
                            *cur_speed_iter = *cur_speed;
                            return;
                        }
                        break;
                    } else {
                        dropped_frame.push(FRAMES.pop_back().unwrap());
                        continue;
                    }
                } else {
                    //nothing can be done ?
                    println!("No such eailer frame to use");

                    if let Some(last) = dropped_frame.pop() {
                        // it can happen when rewind to frame 1
                        last.restore(
                            Some(dropped_frame.iter_mut().rev()),
                            Some(ALLOCS.replace(HashSet::new()).unwrap().into_iter()),
                            Some(FREES.replace(HashSet::new()).unwrap().into_iter()),
                        );
                        FRAMES.push_back(last);
                    }
                    pause(battle_state, weird_counter);
                    *cur_speed_iter = *cur_speed;

                    return;
                }
            }
        }
    }

    if let Some(rprp) = &mut RE_PLAY {
        if PAUSESTATE.load(Relaxed) == 0 && *cur_speed_iter + 1 >= *cur_speed
        /*&& !(*cur_speed_iter == 0 && qdown != 0)*/
        {
            rprp.read_input();
        }
        rprp.apply_input();
    }

    let framecount = *SOKU_FRAMECOUNT;

    if framecount == 1
        || framecount == 2
        || framecount % 16 == 1
        || IS_REWINDING.load(Relaxed) == 1
        || match &RE_PLAY {
            Some(rprp) => rprp.frame - 1 == framecount,
            _ => false,
        }
        || match &CHECK {
            Some(check) => matches!(check.check_step, CheckStep::TestRollback(_, __, ___)),
            _ => false,
        }
        || PERFORMANCE_TEST.is_some()
    {
        if framecount == 0 {
            println!("try to save frame 0! stop it!");
        } else if FRAMES.is_empty() || FRAMES.back().unwrap().number != *SOKU_FRAMECOUNT {
            let x = dump_frame(
                Some(ALLOCS.replace(HashSet::new()).unwrap().into_iter()),
                Some(FREES.replace(HashSet::new()).unwrap().into_iter()),
            );
            // println!("push {}", x.number);
            FRAMES.push_back(x);
        }
    } else {
        // println!("cannot push?");
    }

    if (PAUSESTATE.load(Relaxed) != 0 && *cur_speed_iter + 1 >= *cur_speed)
        || pause_for_override_target_frame
    {
        pause(battle_state, weird_counter);
    }
    return;

    // println!(
    //     "{} {} {}",
    //     cur_speed,
    //     cur_speed_iter,
    //     LAST_TARGET.unwrap_or(0)
    // );
}
