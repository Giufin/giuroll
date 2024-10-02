#![feature(pointer_is_aligned)]
#![feature(abi_thiscall)]
#![feature(let_chains)]
#![feature(coroutines)]
#![feature(iter_from_coroutine)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(panic_update_hook)]
#![feature(panic_info_message)]
// we should manually and carefully avoid undefined behavior about
// references to and any borrowing of static mut variables.
// shuold be solved before updating to 2024 edition?
#![allow(static_mut_refs)]

use std::panic;
use std::{
    any::type_name,
    collections::HashMap,
    ffi::{c_void, OsStr},
    mem::align_of,
    os::windows::prelude::OsStringExt,
    path::{Path, PathBuf},
    ptr::{addr_of_mut, null_mut},
    sync::{
        atomic::{AtomicI32, AtomicU32, Ordering::Relaxed},
        Mutex,
    },
    time::{Duration, Instant, SystemTime},
};
mod netcode;
mod replay;
mod rollback;
mod sound;

use ilhook::x86::{HookPoint, HookType};

//use libloadng::Library;
#[cfg(feature = "logtofile")]
use log::info;
use mininip::datas::{Identifier, Value};
use netcode::{Netcoder, NetworkPacket};

//use notify::{RecursiveMode, Watcher};
use rollback::{Rollbacker, DUMP_FRAME_TIME, LAST_M_LEN, MEMORY_LEAK};
use sound::RollbackSoundManager;
use windows::core::PCWSTR;
use windows::Win32::Foundation::HANDLE;
use windows::Win32::System::LibraryLoader::GetModuleFileNameW;
use windows::Win32::System::Memory::HEAP_FLAGS;
use windows::Win32::{
    Foundation::{GetLastError, HMODULE, HWND},
    Networking::WinSock::{closesocket, SOCKADDR, SOCKET},
    System::{
        Memory::{
            HeapAlloc, HeapFree, VirtualProtect, PAGE_EXECUTE_READWRITE, PAGE_PROTECTION_FLAGS,
        },
        Threading::WaitForSingleObject,
    },
};

//mod netcode;
// +83E1
// +2836D
//004083dc actually
//00407a21 game thread created here

//#[cfg(debug_assertions)]
const ISDEBUG: bool = false;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Callbacks {
    pub save_state: unsafe extern "C" fn() -> u32,
    pub load_state_pre: unsafe extern "C" fn(usize, u32),
    pub load_state_post: unsafe extern "C" fn(u32),
    pub free_state: unsafe extern "C" fn(u32, bool),
}

static mut CALLBACK_ARRAY: Vec<Callbacks> = Vec::new();

//#[cfg(not(debug_assertions))]
//const ISDEBUG: bool = false;
#[cfg(feature = "logtofile")]
pub fn set_up_fern() -> Result<(), fern::InitError> {
    fern::Dispatch::new()
        // Perform allocation-free log formatting
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{} {}] {}",
                //humantime::format_rfc3339(std::time::SystemTime::now()),
                record.level(),
                record.target(),
                message
            ))
        })
        // Add blanket level filter -
        .level(log::LevelFilter::Debug)
        .chain(fern::log_file("output.log")?)
        // Apply globally
        .apply()?;

    Ok(())
}

static mut ENABLE_PRINTLN: bool = false;

#[macro_export]
macro_rules! println {
    ($($arg:tt)*) => {{
        use crate::ENABLE_PRINTLN;
        use crate::replay::CHECK;
        #[allow(unused_unsafe)]
        if unsafe { ENABLE_PRINTLN } || unsafe { CHECK.is_some() } {
            std::println!($($arg)*);
        }
    }};
}

use winapi::shared::{
    d3d9::IDirect3DDevice9,
    d3d9types::{D3DCLEAR_TARGET, D3DCOLOR, D3DCOLOR_ARGB, D3DRECT},
};

fn warning_box(text: &str, title: &str) {
    let to_utf_16 = |s: &str| s.encode_utf16().chain([0]).collect::<Vec<u16>>();
    unsafe {
        use windows::Win32::UI::WindowsAndMessaging::{MessageBoxW, MB_ICONERROR, MB_OK};
        MessageBoxW(
            HWND(0),
            PCWSTR(to_utf_16(text).as_ptr()),
            PCWSTR(to_utf_16(title).as_ptr()),
            MB_OK | MB_ICONERROR,
        );
    }
}

#[allow(unused)]
fn pointer_debug_str<T>(p: *const T) -> String {
    return format!(
        "The alignment is {} ({}); the address is {:#010x}.",
        align_of::<T>(),
        type_name::<T>(),
        p as usize
    );
}

#[macro_export]
macro_rules! ptr_wrap {
    ($src:expr) => {{
        match ($src) {
            src_ => {
                // use crate::pointer_debug_str;
                // assert!(
                //     src_.is_aligned(),
                //     "Pointer unaligned at {} {} : {}",
                //     file!(),
                //     line!(),
                //     pointer_debug_str(src_)
                // );
                src_
            }
        }
    }};
}

static HOOK: Mutex<Option<Box<[HookPoint]>>> = Mutex::new(None);

unsafe fn tamper_memory<T: Sized>(dst: *mut T, src: T) -> T {
    let mut old_prot_ptr: PAGE_PROTECTION_FLAGS = PAGE_PROTECTION_FLAGS(0);
    assert_ne!(std::mem::size_of::<T>(), 0);
    VirtualProtect(
        dst as *const c_void,
        std::mem::size_of::<T>(),
        PAGE_EXECUTE_READWRITE,
        std::ptr::addr_of_mut!(old_prot_ptr),
    )
    .unwrap();
    let ori = dst.read_unaligned();
    dst.write_unaligned(src);
    VirtualProtect(
        dst as *const c_void,
        std::mem::size_of::<T>(),
        old_prot_ptr,
        std::ptr::addr_of_mut!(old_prot_ptr),
    )
    .unwrap();
    return ori;
}

unsafe fn jmp_relative_opt_to_pointer<T: Sized>(jmp_addr: *const c_void) -> T {
    let p_offset = (jmp_addr as *mut u8).offset(1) as *mut usize;
    let end = (jmp_addr as usize).wrapping_add(1 + std::mem::size_of::<usize>());
    let ret = end.wrapping_add(p_offset.read_unaligned());
    assert_eq!(core::mem::size_of::<T>(), std::mem::size_of::<usize>());
    return std::mem::transmute_copy(&ret);
}

unsafe fn tamper_jmp_relative_opr<T: Sized>(dst: *mut c_void, src: T) -> T {
    let mut old_prot_ptr: PAGE_PROTECTION_FLAGS = PAGE_PROTECTION_FLAGS(0);
    let p_offset = (dst as *mut u8).offset(1) as *mut usize;
    let end = (dst as usize).wrapping_add(1 + std::mem::size_of::<usize>());
    assert_eq!(std::mem::size_of::<T>(), std::mem::size_of::<usize>());
    let ret = jmp_relative_opt_to_pointer(dst);
    VirtualProtect(
        p_offset as *const c_void,
        std::mem::size_of::<usize>(),
        PAGE_EXECUTE_READWRITE,
        std::ptr::addr_of_mut!(old_prot_ptr),
    )
    .unwrap();
    p_offset.write_unaligned((std::mem::transmute_copy::<T, usize>(&src)).wrapping_sub(end));
    VirtualProtect(
        p_offset as *const c_void,
        std::mem::size_of::<usize>(),
        old_prot_ptr,
        std::ptr::addr_of_mut!(old_prot_ptr),
    )
    .unwrap();
    return ret;
}

// calling convention here was changed from cdecl to C because of the requirements of the new library. Thankfully they appear to be aliases in the current ABI
#[no_mangle]
pub unsafe extern "C" fn exeinit() {
    truer_exec(std::env::current_dir().unwrap());
}

//if I don't pass the path like this I get an "access violation". The library I'm using for the injection does mention that arguments must be Copy
static mut EXE_INIT_PATH: Vec<u8> = Vec::new();
#[no_mangle]
pub unsafe extern "C" fn better_exe_init_push_path(s: u8) {
    EXE_INIT_PATH.push(s);
}

#[no_mangle]
pub unsafe extern "C" fn better_exe_init() -> bool {
    let os = OsStr::from_encoded_bytes_unchecked(EXE_INIT_PATH.as_ref());

    truer_exec(PathBuf::from(os))
        .or_else(|| truer_exec(std::env::current_dir().unwrap()))
        .is_some()
}

#[no_mangle]
pub extern "C" fn getPriority() -> i32 {
    1000
}

#[no_mangle]
pub unsafe extern "C" fn addRollbackCb(cb: *const Callbacks) {
    CALLBACK_ARRAY.push(*cb);
}

#[no_mangle]
pub extern "C" fn Initialize(dllmodule: HMODULE) -> bool {
    let mut dat = [0u16; 1025];
    unsafe { GetModuleFileNameW(dllmodule, &mut dat) };

    let s = std::ffi::OsString::from_wide(&dat);

    //std::thread::sleep(Duration::from_millis(2000));
    //let m = init(0);
    let mut filepath = Path::new(&s).to_owned();
    filepath.pop();
    truer_exec(filepath);
    true
}
//687040 true real input buffer manipulation
// 85b8ec some related varible, 487040
#[no_mangle]
pub extern "cdecl" fn CheckVersion(a: *const [u8; 16]) -> bool {
    const HASH110A: [u8; 16] = [
        0xdf, 0x35, 0xd1, 0xfb, 0xc7, 0xb5, 0x83, 0x31, 0x7a, 0xda, 0xbe, 0x8c, 0xd9, 0xf5, 0x3b,
        0x2e,
    ];
    unsafe { *ptr_wrap!(a) == HASH110A }
}

static mut REAL_INPUT: Option<[bool; 10]> = None;
static mut REAL_INPUT2: Option<[bool; 10]> = None;

static mut UPDATE: Option<SystemTime> = None;
static mut TARGET: Option<u128> = None;

static mut WARNING_FRAME_MISSING_1_COUNTDOWN: usize = 0;
static mut WARNING_FRAME_MISSING_2_COUNTDOWN: usize = 0;
static SOKU_LOOP_EVENT: Mutex<Option<isize>> = Mutex::new(None);
static TARGET_OFFSET: AtomicI32 = AtomicI32::new(0);
//static TARGET_OFFSET_COUNT: AtomicI32 = AtomicI32::new(0);

static mut TITLE: &'static [u16] = &[];
const VER: &str = env!("CARGO_PKG_VERSION");

unsafe extern "cdecl" fn skip(_a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {}

//static SOUNDS_THAT_DID_HAPPEN: Mutex<BTreeMap<usize, Vec<usize>>> = Mutex::new(BTreeMap::new());

// set this mutex at the start of each frame. after each rollback you can see which sounds are left in this mutex. these sounds can and should be pasued
//static SOUND_THAT_MAYBE_HAPPEN: Mutex<BTreeMap<usize, Vec<usize>>> = Mutex::new(BTreeMap::new());
static mut SOUND_MANAGER: Option<RollbackSoundManager> = None;

static mut FORCE_SOUND_SKIP: bool = false;
//this is getting bad, fix the redundancy
//static INPUTS_RAW: Mutex<BTreeMap<usize, [u16; 2]>> = Mutex::new(BTreeMap::new());

static mut SPIN_TIME_MICROSECOND: i128 = 0;

static mut F62_ENABLED: bool = false;

const VERSION_BYTE_60: u8 = 0x6b;
const VERSION_BYTE_62: u8 = 0x6c;

static mut LAST_GAME_REQUEST: Option<[u8; 400]> = None;
static mut LAST_LOAD_ACK: Option<[u8; 400]> = None;
static mut LAST_MATCH_ACK: Option<[u8; 400]> = None;
static mut LAST_MATCH_LOAD: Option<[u8; 400]> = None;

static mut IS_FIRST_READ_INPUTS: bool = true;

static mut ORI_RECVFROM: Option<
    unsafe extern "stdcall" fn(SOCKET, *mut u8, i32, i32, *mut SOCKADDR, *mut i32) -> u32,
> = None;

static mut OUTER_COLOR: D3DCOLOR = 0;
static mut INSIDE_COLOR: D3DCOLOR = 0;
static mut PROGRESS_COLOR: D3DCOLOR = 0;
static mut TAKEOVER_COLOR: D3DCOLOR = 0;
static mut CENTER_X_P1: i32 = 224;
static mut CENTER_Y_P1: i32 = 428;
static mut CENTER_X_P2: i32 = 640 - 224;
static mut CENTER_Y_P2: i32 = 428;
static mut INSIDE_HALF_HEIGHT: i32 = 7;
static mut INSIDE_HALF_WIDTH: i32 = 58;
static mut OUTER_HALF_HEIGHT: i32 = 9;
static mut OUTER_HALF_WIDTH: i32 = 60;

static mut FREEZE_MITIGATION: bool = false;
static mut ENABLE_CHECK_MODE: bool = false;
static mut WARNING_WHEN_LAGGING: bool = true;

static mut MAX_ROLLBACK_PREFERENCE: u8 = 6;

static mut DISABLE_SOUND: bool = false;

#[repr(C)]
struct FakeBattleManagerForTsk {
    fake_left_win_count: u8,  // SWRS_ADDR_WINCNTOFS = 0x573
    fake_right_win_count: u8, // SWRS_ADDR_WINCNTOFS = 0x573
    _unused1: [u8; 0xa],
    fake_p_left_char: *mut u8,  // SWRS_ADDR_LCHAROFS = 0xC
    fake_p_right_char: *mut u8, // SWRS_ADDR_RCHAROFS = 0x10
    _unused2: [u8; 0x74],
    fake_battle_mode: u32, // SWRS_ADDR_BTLMODEOFS = 0x88
}
// SWRS_ADDR_PBATTLEMGR = 0x0047579c
const P_FAKE_BATTLE_MANAGER_FOR_TSK: *mut *mut FakeBattleManagerForTsk = 0x47579c as _;
static mut FAKE_BATTLE_MANAGER_FOR_TSK: Option<Box<FakeBattleManagerForTsk>> = None;

impl FakeBattleManagerForTsk {
    fn new_box() -> Box<Self> {
        let mut self_ = Box::new(Self {
            fake_battle_mode: 0,
            fake_p_left_char: 0 as *mut u8,
            fake_p_right_char: 0 as *mut u8,
            fake_left_win_count: 0,
            fake_right_win_count: 0,
            _unused1: [0; 0xa],
            _unused2: [0; 0x74],
        });
        self_.fake_p_left_char =
            (addr_of_mut!(self_.fake_left_win_count) as usize).wrapping_sub(0x573) as _;
        self_.fake_p_right_char =
            (addr_of_mut!(self_.fake_right_win_count) as usize).wrapping_sub(0x573) as _;
        assert_eq!(
            self_.fake_p_left_char.wrapping_add(0x573),
            addr_of_mut!(self_.fake_left_win_count)
        );
        assert_eq!(
            self_.fake_p_right_char.wrapping_add(0x573),
            addr_of_mut!(self_.fake_right_win_count)
        );
        assert_eq!(
            ((self_.as_ref() as *const _) as *const *mut u8).wrapping_byte_offset(0xc),
            addr_of_mut!(self_.fake_p_left_char)
        );
        assert_eq!(
            ((self_.as_ref() as *const _) as *const *mut u8).wrapping_byte_offset(0x10),
            addr_of_mut!(self_.fake_p_right_char)
        );
        assert_eq!(
            ((self_.as_ref() as *const _) as *const u32).wrapping_byte_offset(0x88),
            addr_of_mut!(self_.fake_battle_mode)
        );
        self_
    }
}

pub fn force_sound_skip(soundid: usize) {
    unsafe {
        let forcesound = std::mem::transmute::<usize, extern "stdcall" fn(u32)>(0x401d50);
        FORCE_SOUND_SKIP = true;

        forcesound(soundid as u32);

        FORCE_SOUND_SKIP = false;
    }
}

//returns None on .ini errors
fn truer_exec(filename: PathBuf) -> Option<()> {
    panic::update_hook(|prev, info| {
        let payload = if let Some(s) = info.payload().downcast_ref::<&str>() {
            format!("{s:}")
        } else if let Some(s) = info.payload().downcast_ref::<&String>() {
            format!("{s:}")
        } else if let Some(a) = info.message() {
            format!("{}", a)
        } else {
            "panic without message".to_string()
        };
        let location = if let Some(l) = info.location() {
            format!("{}", l)
        } else {
            "unknown".to_string()
        };
        // let backtrace = format!("{:}", std::backtrace::Backtrace::force_capture());
        warning_box(
            format!(
                "{}\n{}\n{payload:}\nLocation: {location:}",
                "Giuroll was panicked, which may or may not be caused by Giuroll.",
                concat!(
                    "Your feedback is important! ",
                    "Please take a screenshot and report the information to ",
                    "@hagb_ in public hisoutensoku Discord groups."
                )
            )
            .as_str(),
            "Panic!",
        );
        prev(info);
    });

    #[cfg(feature = "allocconsole")]
    unsafe {
        use windows::Win32::System::Console::AllocConsole;
        AllocConsole();
    }

    let mut filepath = filename;
    filepath.push("giuroll.ini");
    //println!("{:?}", filepath);

    let conf = mininip::parse::parse_file(filepath).ok()?;

    #[cfg(feature = "logtofile")]
    {
        set_up_fern().unwrap();
        info!("here");
        std::panic::set_hook(Box::new(|x| info!("panic! {:?}", x)));
        let _ = set_up_fern();
    }

    unsafe {
        let (s, r) = std::sync::mpsc::channel();
        DATA_RECEIVER = Some(r);
        DATA_SENDER = Some(s);

        let (s, r) = std::sync::mpsc::channel();
        MEMORY_RECEIVER_FREE = Some(r);
        MEMORY_SENDER_FREE = Some(s);

        let (s, r) = std::sync::mpsc::channel();
        MEMORY_RECEIVER_ALLOC = Some(r);
        MEMORY_SENDER_ALLOC = Some(s);
    }

    fn read_ini_bool(
        conf: &HashMap<Identifier, Value>,
        section: &str,
        key: &str,
        default: bool,
    ) -> bool {
        conf.get(&Identifier::new(Some(section.to_string()), key.to_string()))
            .map(|x| match x {
                Value::Bool(x) => *x,
                _ => todo!("non bool .ini entry"),
            })
            .unwrap_or(default)
    }

    fn read_ini_int_hex(
        conf: &HashMap<Identifier, Value>,
        section: &str,
        key: &str,
        default: i64,
    ) -> i64 {
        conf.get(&Identifier::new(Some(section.to_string()), key.to_string()))
            .map(|x| match x {
                Value::Int(x) => *x,
                Value::Raw(x) | Value::Str(x) => {
                    i64::from_str_radix(x.strip_prefix("0x").unwrap(), 16).unwrap()
                }
                _ => todo!("non integer .ini entry"),
            })
            .unwrap_or(default)
    }

    fn read_ini_string(
        conf: &HashMap<Identifier, Value>,
        section: &str,
        key: &str,
        default: String,
    ) -> String {
        conf.get(&Identifier::new(Some(section.to_string()), key.to_string()))
            .map(|x| match x {
                Value::Str(x) => x.clone(),
                _ => todo!("non string .ini entry"),
            })
            .unwrap_or(default)
    }

    let inc = read_ini_int_hex(&conf, "Keyboard", "increase_delay_key", 0);
    let dec = read_ini_int_hex(&conf, "Keyboard", "decrease_delay_key", 0);
    let rdec = read_ini_int_hex(&conf, "Keyboard", "decrease_max_rollback_key", 0x0a);
    let rinc = read_ini_int_hex(&conf, "Keyboard", "increase_max_rollback_key", 0x0b);
    let net = read_ini_int_hex(&conf, "Keyboard", "toggle_network_stats", 0);
    let exit_takeover = read_ini_int_hex(&conf, "Keyboard", "exit_takeover", 0x10);
    let p1_takeover = read_ini_int_hex(&conf, "Keyboard", "p1_takeover", 0x11);
    let p2_takeover = read_ini_int_hex(&conf, "Keyboard", "p2_takeover", 0x12);
    let set_or_retry_takeover = read_ini_int_hex(&conf, "Keyboard", "set_or_retry_takeover", 0x13);
    let spin = read_ini_int_hex(&conf, "FramerateFix", "spin_amount", 1500);
    let f62_enabled = read_ini_bool(&conf, "FramerateFix", "enable_f62", cfg!(feature = "f62"));
    let network_menu = read_ini_bool(&conf, "Netplay", "enable_network_stats_by_default", false);
    let default_delay = read_ini_int_hex(&conf, "Netplay", "default_delay", 2).clamp(0, 9);
    let autodelay_enabled = read_ini_bool(&conf, "Netplay", "enable_auto_delay", true);
    let freeze_mitigation = read_ini_bool(&conf, "Netplay", "freeze_mitigation__", false);
    let autodelay_rollback = read_ini_int_hex(&conf, "Netplay", "auto_delay_rollback", 0);
    let max_rollback_preference =
        read_ini_int_hex(&conf, "Netplay", "max_rollback_preference", 6).clamp(0, 15) as u8;
    let warning_when_lagging = read_ini_bool(&conf, "Misc", "warning_when_lagging", true);
    let soku2_compat_mode = read_ini_bool(&conf, "Misc", "soku2_compatibility_mode", false);
    let enable_println = read_ini_bool(
        &conf,
        "Misc",
        "enable_println",
        cfg!(feature = "allocconsole") || ISDEBUG,
    );
    let enable_check_mode = read_ini_bool(&conf, "Misc", "enable_check_mode", false);
    let outer_color: D3DCOLOR = read_ini_int_hex(
        &conf,
        "Takeover",
        "progress_bar_outer_color",
        D3DCOLOR_ARGB(0xff, 0xff, 0, 0) as i64,
    ) as D3DCOLOR;
    let inside_color: D3DCOLOR = read_ini_int_hex(
        &conf,
        "Takeover",
        "progress_bar_inside_color",
        D3DCOLOR_ARGB(0xff, 0, 0, 0xff) as i64,
    ) as D3DCOLOR;
    let progress_color: D3DCOLOR = read_ini_int_hex(
        &conf,
        "Takeover",
        "progress_bar_progress_color",
        D3DCOLOR_ARGB(0xff, 0xff, 0xff, 0) as i64,
    ) as D3DCOLOR;
    let takeover_color: D3DCOLOR = read_ini_int_hex(
        &conf,
        "Takeover",
        "takeover_color",
        D3DCOLOR_ARGB(0xff, 0, 0xff, 0) as i64,
    ) as D3DCOLOR;
    let center_x_p1 = read_ini_int_hex(&conf, "Takeover", "progress_bar_center_x_p1", 224);
    let center_y_p1 = read_ini_int_hex(&conf, "Takeover", "progress_bar_center_y_p1", 428);
    let center_x_p2 = read_ini_int_hex(&conf, "Takeover", "progress_bar_center_x_p2", 640 - 224);
    let center_y_p2 = read_ini_int_hex(&conf, "Takeover", "progress_bar_center_y_p2", 428);
    let inside_half_height =
        read_ini_int_hex(&conf, "Takeover", "progress_bar_inside_half_height", 7);
    let inside_half_width =
        read_ini_int_hex(&conf, "Takeover", "progress_bar_inside_half_width", 58);
    let outer_half_height =
        read_ini_int_hex(&conf, "Takeover", "progress_bar_outer_half_height", 9);
    let outer_half_width = read_ini_int_hex(&conf, "Takeover", "progress_bar_outer_half_width", 60);

    //soku2 compatibility. Mods should change character size data themselves using exported functions. This is a temporary solution until soku2 team can implement that functionality.
    unsafe {
        if soku2_compat_mode {
            const CHARSIZEDATA_A: [usize; 35] = [
                2236, 2220, 2208, 2244, 2216, 2284, 2196, 2220, 2260, 2200, 2232, 2200, 2200, 2216,
                2352, 2224, 2196, 2196, 2216, 2216, 0, 2208, 2236, 2232, 2196, 2196, 2216, 2216,
                2200, 2216, 2352, 2200, 2284, 2220, 2208,
            ];

            const CHARSIZEDATA_B: [usize; 35] = [
                940, 940, 940, 944, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940,
                940, 940, 940, 940, 0, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940,
                940, 940,
            ];

            CHARSIZEDATA = (0..35)
                .map(|i| (CHARSIZEDATA_A[i], CHARSIZEDATA_B[i]))
                .collect();
        } else {
            const CHARSIZEDATA_A: [usize; 20] = [
                2236, 2220, 2208, 2244, 2216, 2284, 2196, 2220, 2260, 2200, 2232, 2200, 2200, 2216,
                2352, 2224, 2196, 2196, 2216, 2216,
            ];

            const CHARSIZEDATA_B: [usize; 20] = [
                940, 940, 940, 944, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940, 940,
                940, 940, 940, 940,
            ];

            CHARSIZEDATA = (0..20)
                .map(|i| (CHARSIZEDATA_A[i], CHARSIZEDATA_B[i]))
                .collect();
        }
    }

    #[allow(unused_mut)]
    let mut verstr: String = if f62_enabled {
        format!("{}CN", VER)
    } else {
        format!("{}", VER)
    };
    #[cfg(feature = "lowframetest")]
    {
        verstr += "-low_frame_test"
    };
    let mut title = read_ini_string(
        &conf,
        "Misc",
        "game_title",
        format!("Soku with giuroll {} :YoumuSleep:", verstr),
    );
    title.push('\0');

    let verstr = format!("Giuroll {}", verstr);

    let title = title.replace('$', &verstr);

    let tleak = Box::leak(Box::new(title));

    unsafe {
        TITLE = Box::leak(tleak.encode_utf16().collect::<Box<_>>());
        F62_ENABLED = f62_enabled;
        SPIN_TIME_MICROSECOND = spin as i128;
        INCREASE_DELAY_KEY = inc as u8;
        DECREASE_DELAY_KEY = dec as u8;
        INCREASE_MAX_ROLLBACK_KEY = rinc as u8;
        DECREASE_MAX_ROLLBACK_KEY = rdec as u8;
        TOGGLE_STAT_KEY = net as u8;
        TAKEOVER_KEYS_SCHEME[0] = exit_takeover as u8;
        TAKEOVER_KEYS_SCHEME[1] = p1_takeover as u8;
        TAKEOVER_KEYS_SCHEME[2] = p2_takeover as u8;
        TAKEOVER_KEYS_SCHEME[3] = set_or_retry_takeover as u8;
        TOGGLE_STAT = network_menu;
        LAST_DELAY_VALUE = default_delay as usize;
        DEFAULT_DELAY_VALUE = default_delay as usize;
        AUTODELAY_ENABLED = autodelay_enabled;
        AUTODELAY_ROLLBACK = autodelay_rollback as i8;
        OUTER_COLOR = outer_color;
        INSIDE_COLOR = inside_color;
        PROGRESS_COLOR = progress_color;
        TAKEOVER_COLOR = takeover_color;
        CENTER_X_P1 = center_x_p1 as i32;
        CENTER_X_P2 = center_x_p2 as i32;
        CENTER_Y_P1 = center_y_p1 as i32;
        CENTER_Y_P2 = center_y_p2 as i32;
        INSIDE_HALF_HEIGHT = inside_half_height as i32;
        INSIDE_HALF_WIDTH = inside_half_width as i32;
        OUTER_HALF_HEIGHT = outer_half_height as i32;
        OUTER_HALF_WIDTH = outer_half_width as i32;
        FREEZE_MITIGATION = freeze_mitigation;
        ENABLE_PRINTLN = enable_println;
        ENABLE_CHECK_MODE = enable_check_mode;
        WARNING_WHEN_LAGGING = warning_when_lagging;
        MAX_ROLLBACK_PREFERENCE = max_rollback_preference;
    }

    unsafe {
        FAKE_BATTLE_MANAGER_FOR_TSK = Some(FakeBattleManagerForTsk::new_box());
        tamper_memory(
            P_FAKE_BATTLE_MANAGER_FOR_TSK,
            FAKE_BATTLE_MANAGER_FOR_TSK.as_mut().unwrap().as_mut() as *mut FakeBattleManagerForTsk,
        );
    }

    unsafe {
        tamper_memory(
            0x858b80 as *mut u8,
            if F62_ENABLED {
                VERSION_BYTE_62
            } else {
                VERSION_BYTE_60
            },
        );
    }

    //meiling d236 desync fix, original by PinkySmile, Slen, cc/delthas, Fear Nagae, PC_Volt
    unsafe {
        tamper_memory(0x724316 as *mut [u8; 4], [0x66, 0xB9, 0x0F, 0x00]);
    }

    // 9 digit font fix, by ichirin
    unsafe {
        for a in [0x43DC7D, 0x882954] {
            tamper_memory(a as *mut u8, 0x0A);
        }
    }

    let new =
        unsafe { ilhook::x86::Hooker::new(0x482701, HookType::JmpBack(main_hook), 0).hook(6) };
    std::mem::forget(new);

    //0x899d60 maybe sound manager?
    unsafe extern "cdecl" fn handle_sound_real(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) -> usize {
        //let sw = REQUESTED_THREAD_ID.swap(0, Relaxed);

        (*a).ecx = 0x89f9f8;
        // Soku2 unaligned (can be triggered with the b bullet of Flandre):
        // (*a).eax = *ptr_wrap!(((*a).esp + 4) as *const u32);
        (*a).eax = (((*a).esp + 4) as *const u32).read_unaligned();
        let soundid = (*a).eax as usize;

        if DISABLE_SOUND {
            return 0x401db7;
        }

        if !BATTLE_STARTED || soundid == 0 {
            return if soundid == 0 { 0x401db7 } else { 0x401d58 };
        }

        if let Some(manager) = SOUND_MANAGER.as_mut()
            && !FORCE_SOUND_SKIP
        {
            //println!(
            //    "trying to play sound {} at frame {} with rollback {}",
            //    soundid,
            //    *SOKU_FRAMECOUNT,
            //    manager.current_rollback.is_some()
            //);
            if manager.insert_sound(*SOKU_FRAMECOUNT, soundid) {
                //println!("sound {} accepted at frame {}", soundid, *SOKU_FRAMECOUNT);
                0x401d58
            } else {
                //println!("sound {} rejected at frame {} because it was already present", soundid, *SOKU_FRAMECOUNT);
                0x401db7
            }
        } else {
            0x401d58
        }
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x401d50, // 0x482820, //0x482532, sokuroll <-
            HookType::JmpToRet(handle_sound_real),
            0,
        )
        .hook(6)
    };
    std::mem::forget(new);

    unsafe extern "cdecl" fn soundskiphook1(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) -> usize {
        if FORCE_SOUND_SKIP {
            // force call it, the return to end of function

            // 0x401d81

            let eax = *ptr_wrap!(((*a).esi + 4) as *const u32);
            let ecx = *ptr_wrap!(eax as *const u32);
            let fun = *ptr_wrap!((ecx + 0x48) as *const u32);
            let true_fun = std::mem::transmute::<usize, extern "thiscall" fn(u32, u32 /* , u32*/)>(
                fun as usize,
            );

            true_fun(ecx, eax /*, *(((*a).esp + 0x8)  as *const u32)*/);

            0x401db6
        } else {
            //replicate the usual logic

            // Soku2 unaligned (can be triggered with the b bullet of Flandre):
            //if ((*ptr_wrap!(((*a).esp + 8) as *const usize)) & 1) == 0 {
            if ((((*a).esp + 8) as *const usize).read_unaligned() & 1) == 0 {
                0x401d8c
            } else {
                0x401d81
            }
        }
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x401d7a, // 0x482820, //0x482532, sokuroll <-
            HookType::JmpToRet(soundskiphook1),
            0,
        )
        .hook(5)
    };
    std::mem::forget(new);

    // println!("DEBUG: disable sound");
    // // disable sound
    // assert_eq!(unsafe { tamper_memory(0x00401d56 as *mut u8, 0xeb) }, 0x74);
    // assert_eq!(unsafe { *(0x00401d56 as *mut u8) }, 0xeb);

    unsafe extern "cdecl" fn on_exit(_: *mut ilhook::x86::Registers, _: usize) {
        println!("on exit");
        HAS_LOADED = false;
        AFTER_GAME_REQUEST_FROM_P1 = false;

        GIRLS_ARE_TALKING = false;
        LAST_LOAD_ACK = None;
        LAST_GAME_REQUEST = None;
        LAST_MATCH_ACK = None;
        LAST_MATCH_LOAD = None;
        LIKELY_DESYNCED = false;

        REQUESTED_THREAD_ID.store(0, Relaxed);
        NEXT_DRAW_PING = None;

        *(0x8971C0 as *mut usize) = 0; // reset wether to prevent desyncs
        ESC = 0;
        ESC2.store(0, Relaxed);
        BATTLE_STARTED = false;
        DISABLE_SEND.store(0, Relaxed);
        LAST_STATE.store(0, Relaxed);

        //SOUNDS_THAT_DID_HAPPEN.lock().unwrap().clear();
        //SOUND_THAT_MAYBE_HAPPEN.lock().unwrap().clear();

        //INPUTS_RAW.lock().unwrap().clear();

        // we should be removing allocations that happen during frames which were rolled back, but that somehow breaks it, possibly because of some null check initializations
        //let allocset = std::mem::replace(&mut *ALLOCMUTEX.lock().unwrap(), BTreeSet::new());
        //let freeset = std::mem::replace(&mut *FREEMUTEX.lock().unwrap(), BTreeSet::new());
        //
        //for a in allocset.difference(&freeset) {
        //    //    unsafe { HeapFree(heap, 0, *a as *const c_void) };
        //    println!("freed but not alloced: {}", a);
        //}

        if let Some(x) = NETCODER.take() {
            let r = x.receiver;
            while r.try_recv().is_ok() {}
            DATA_RECEIVER = Some(r);
        }

        // it cannot be used by any different thread now
        if let Some(x) = ROLLBACKER.take() {
            for mut a in x.guessed {
                if !a.prev_state.has_called_never_happened && !a.prev_state.has_happened {
                    a.prev_state.did_happen();
                }
            }
        }

        for a in MEMORY_RECEIVER_FREE.as_ref().unwrap().try_iter() {
            soku_heap_free!(a);
        }

        clean_replay_statics();

        GIRLSTALKED = false;
        NEXT_DRAW_ROLLBACK = None;
        NEXT_DRAW_ENEMY_DELAY = None;
        DUMP_FRAME_TIME = None;
        println!("Memory leak: {} bytes", MEMORY_LEAK);
        MEMORY_LEAK = 0;
        LAST_M_LEN = 0;
    }

    //no_ko_sound
    /*
    explanation:
    sometimes rollback falsely cancels the KO sound. I believe this is because it's triggered from two different sites, and one of them, 0x6dcc0c
    seems to be triggered from a destructor. ~~The object whose destructor is cleared up here is likely overriden, and sokuroll does not restore that particular reference, because
    it's usually not relevant to rollback~~. After some experimenting I cannot find a cause for why the sound is called from two callsites, but no matter which one
    I remove the issue persist. It is also possible that instead of incorrect rollback, the sound is called before the frame, which is highly unusual for a sound,
    but so is having 2 call sites, that's why I think that's the most likely explanation.
    Sokuroll likely had an exception for the KO sound since usually it would never roll back that particualar sound, but I couldn't find a reference to it
    in the decompiled code. Here we simply remove that sound from it's 2 separate, unrelated callsites, and call it once, from a callsite that makes more sense.
    that callsite (can be found by searching no_ko in this file) is triggered not on the first frame after a knockdown, but on the second one, which is how it seems to work
    in vanilla game
    */
    unsafe {
        unsafe extern "stdcall" fn override_play_sfx(sound_id: u32) {
            // refer to https://github.com/enebe-nb/SokuLib/blob/dev/src/BattleMode.cpp
            let is_story_or_result_mode = matches!(*(0x00898690 as *const u32), 0 | 7);
            if is_story_or_result_mode || sound_id != 0x2c {
                std::mem::transmute::<usize, extern "stdcall" fn(u32)>(0x439490)(sound_id);
            }
        }
        for addr in [0x6d828b, 0x6dcc0f] {
            tamper_jmp_relative_opr(
                addr as *mut c_void,
                override_play_sfx as unsafe extern "stdcall" fn(u32),
            );
        }
    };

    let new = unsafe { ilhook::x86::Hooker::new(0x481960, HookType::JmpBack(on_exit), 0).hook(6) };
    std::mem::forget(new);

    unsafe extern "cdecl" fn spectator_skip(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) -> usize {
        let framecount_cur = *ptr_wrap!(((*a).esi + 0x4c) as *const u32);
        let edi = (*a).edi;

        //println!("edi: {}, framecount: {}", edi, framecount_cur);
        let no_skip = edi + 16 < framecount_cur && BATTLE_STARTED;
        if no_skip {
            /*
            LAB_0042daa6                                    XREF[1]:     0042daa0(j)
            0042daa6 8b 5e 48        MOV        EBX,dword ptr [ESI + 0x48]
            0042daa9 8b 4e 4c        MOV        ECX,dword ptr [ESI + 0x4c]
            */

            (*a).ebx = *ptr_wrap!(((*a).esi + 0x48) as *const u32);
            (*a).ecx = framecount_cur;

            0x42daac
        } else {
            //println!("here 3");
            /*
            0042db1d 8b 5c 24 1c     MOV        EBX,dword ptr [ESP + local_10]
             */
            // probably Soku2 unaligned
            // (*a).ebx = *ptr_wrap!(((*a).esp + 0x1c) as *const u32);
            (*a).ebx = (((*a).esp + 0x1c) as *const u32).read_unaligned();
            0x42db21
        }
    }

    // changes the spectator logic to only send frame if there are at least 10 frames in the buffer. this prevent spectator from desyncing
    let new = unsafe {
        ilhook::x86::Hooker::new(0x42daa6, HookType::JmpToRet(spectator_skip), 0).hook(6)
    };
    std::mem::forget(new);

    unsafe fn draw_block(device: *const IDirect3DDevice9, inner: &D3DRECT, color: D3DCOLOR) {
        let border = D3DRECT {
            x1: inner.x1.min(inner.x2) - 2,
            x2: inner.x1.max(inner.x2) + 2,
            y1: inner.y1.min(inner.y2) - 2,
            y2: inner.y1.max(inner.y2) + 2,
        };
        (*device).Clear(
            1,
            &border,
            D3DCLEAR_TARGET,
            D3DCOLOR_ARGB(0xff, 0, 0, 0),
            0.0,
            0,
        );
        (*device).Clear(1, inner, D3DCLEAR_TARGET, color, 0.0, 0);
    }

    static mut WARNING_FRAME_LOST_COUNTDOWN: AtomicU32 = AtomicU32::new(0);
    unsafe extern "cdecl" fn drawnumbers(_a: *mut ilhook::x86::Registers, _b: usize) {
        let d3d9_devic3 = 0x008A0E30 as *const *const IDirect3DDevice9;
        let yellow = D3DCOLOR_ARGB(0xff, 0xff, 0xff, 0);
        let red = D3DCOLOR_ARGB(0xff, 0xff, 0, 0);

        WARNING_FRAME_MISSING_1_COUNTDOWN = WARNING_FRAME_MISSING_1_COUNTDOWN.saturating_sub(1);
        // (**d3d9_devic3).drawText
        if let Some(x) = NEXT_DRAW_PING {
            if WARNING_FRAME_MISSING_1_COUNTDOWN != 0
                && WARNING_WHEN_LAGGING
                && *SOKU_FRAMECOUNT >= 120
            {
                let inner = D3DRECT {
                    x1: 300 - get_num_length(NEXT_DRAW_PING.unwrap_or(10), false) as i32,
                    x2: 300 + 2,
                    y1: 466,
                    y2: 480 - 2,
                };
                draw_block(*d3d9_devic3, &inner, yellow);
            }
            draw_num((300.0, 466.0), x);
        }

        WARNING_FRAME_MISSING_2_COUNTDOWN = WARNING_FRAME_MISSING_2_COUNTDOWN.saturating_sub(1);
        if let Some(x) = NEXT_DRAW_ROLLBACK {
            if WARNING_FRAME_MISSING_2_COUNTDOWN != 0
                && WARNING_WHEN_LAGGING
                && *SOKU_FRAMECOUNT >= 120
            {
                let inner = D3DRECT {
                    x1: 325 - get_num_length(NEXT_DRAW_ROLLBACK.unwrap_or(1), false) as i32,
                    x2: 325 + 2,
                    y1: 466,
                    y2: 480 - 2,
                };
                draw_block(*d3d9_devic3, &inner, yellow);
            }
            draw_num((325.0, 466.0), x);
            if let Some(netcoder) = NETCODER.as_ref() {
                draw_num((350.0, 466.0), netcoder.max_rollback as i32);
            }
        }

        if let Some(x) = NEXT_DRAW_ENEMY_DELAY {
            draw_num((20.0, 466.0), x);
        }
        render_replay_progress_bar_and_numbers();

        if WARNING_FRAME_LOST_COUNTDOWN.load(Relaxed) != 0
            && WARNING_FRAME_LOST_COUNTDOWN.fetch_sub(1, Relaxed) != 0
            && WARNING_WHEN_LAGGING
            && *(0x8998b2 as *const bool) /* whether display fps */
            && *SOKU_FRAMECOUNT >= 120
        {
            let inner = D3DRECT {
                x1: 640 - get_num_length(60, false) as i32,
                x2: 640 + 2,
                y1: 466,
                y2: 480 - 2,
            };
            draw_block(*d3d9_devic3, &inner, red);
        }
    }

    let new =
        unsafe { ilhook::x86::Hooker::new(0x43e320, HookType::JmpBack(drawnumbers), 0).hook(7) };
    std::mem::forget(new);

    unsafe fn my_cselect_on_process(
        origin: unsafe extern "thiscall" fn(*mut c_void) -> usize,
        this_: *mut c_void,
    ) -> usize {
        static mut MAX_ROLLBACK_KEY_PRESSED: bool = false;
        let ret = origin(this_);
        if !matches!(ret, 8 | 9) {
            // switch to other scene.
            NEXT_DRAW_PING = None;
            *SELECT_SCENE_INPUT_SEND_TIME_DATA.lock().unwrap() = None;
            return ret;
        }
        if *((this_ as usize + 0x4f60) as *const i32) >= 1 {
            // if in stage select
            return ret;
        }
        if read_key_better(DECREASE_MAX_ROLLBACK_KEY) {
            if !MAX_ROLLBACK_KEY_PRESSED {
                MAX_ROLLBACK_PREFERENCE = MAX_ROLLBACK_PREFERENCE.saturating_sub(1).clamp(0, 15);
            }
            MAX_ROLLBACK_KEY_PRESSED = true;
        } else if read_key_better(INCREASE_MAX_ROLLBACK_KEY) {
            if !MAX_ROLLBACK_KEY_PRESSED {
                MAX_ROLLBACK_PREFERENCE = MAX_ROLLBACK_PREFERENCE.saturating_add(1).clamp(0, 15);
            }
            MAX_ROLLBACK_KEY_PRESSED = true;
        } else {
            MAX_ROLLBACK_KEY_PRESSED = false;
        }
        let mut time_data = SELECT_SCENE_INPUT_SEND_TIME_DATA.lock().unwrap();
        if time_data.is_none() {
            *time_data = Some(P2SendTimeData {
                has_received: false,
                last_max_latency: None,
                max_latency_to_be_shown: None,
                last_receive_time: Instant::now(),
                last_frame_id: 0,
                last_shown_frame: 0,
            });
        }
        update_toggle_stat_from_keys();
        return ret;
    }
    static mut ORI_CSELECT_CL_ON_PROCESS: Option<
        unsafe extern "thiscall" fn(*mut c_void) -> usize,
    > = None;
    static mut ORI_CSELECT_SV_ON_PROCESS: Option<
        unsafe extern "thiscall" fn(*mut c_void) -> usize,
    > = None;
    unsafe extern "thiscall" fn my_cselect_cl_on_process(this_: *mut c_void) -> usize {
        my_cselect_on_process(ORI_CSELECT_CL_ON_PROCESS.unwrap(), this_)
    }
    unsafe extern "thiscall" fn my_cselect_sv_on_process(this_: *mut c_void) -> usize {
        my_cselect_on_process(ORI_CSELECT_SV_ON_PROCESS.unwrap(), this_)
    }
    unsafe {
        ORI_CSELECT_SV_ON_PROCESS = Some(tamper_memory(
            0x8574e0 as *mut unsafe extern "thiscall" fn(*mut c_void) -> usize,
            my_cselect_sv_on_process,
        ));
        ORI_CSELECT_CL_ON_PROCESS = Some(tamper_memory(
            0x857538 as *mut unsafe extern "thiscall" fn(*mut c_void) -> usize,
            my_cselect_cl_on_process,
        ));
    }

    unsafe extern "cdecl" fn render_number_on_select(a: *mut ilhook::x86::Registers, _b: usize) {
        let gametype_main = *(0x898688 as *const usize);
        let is_netplay = *(0x8986a0 as *const usize) != 0;
        let in_stage_select = *(((*a).esi + 0x4f60) as *const i32) >= 1;
        if (gametype_main, is_netplay, in_stage_select, TOGGLE_STAT) == (1, true, false, true) {
            draw_num((300.0, 466.0 - 16.0), MAX_ROLLBACK_PREFERENCE as i32);
            if let Some(time_data) = SELECT_SCENE_INPUT_SEND_TIME_DATA.lock().unwrap().as_ref()
                && let Some(max_latency_to_show) = time_data.max_latency_to_be_shown
            {
                draw_num((300.0, 466.0), max_latency_to_show.as_millis() as i32);
            }
        }
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(0x42158f, HookType::JmpBack(render_number_on_select), 0).hook(5)
    };
    std::mem::forget(new);

    unsafe extern "cdecl" fn ongirlstalk(_a: *mut ilhook::x86::Registers, _b: usize) {
        GIRLSTALKED = true;
        BATTLE_STARTED = false;
    }

    let new =
        unsafe { ilhook::x86::Hooker::new(0x482960, HookType::JmpBack(ongirlstalk), 0).hook(5) };
    std::mem::forget(new);

    unsafe {
        tamper_memory(
            0x00857170 as *mut unsafe extern "stdcall" fn(_, _, _) -> _,
            heap_free_override,
        );
        tamper_memory(
            0x00857174 as *mut unsafe extern "stdcall" fn(_, _, _) -> _,
            heap_alloc_override,
        );
        ORI_HEAP_REALLOC = Some(tamper_memory(
            0x00857180 as *mut unsafe extern "stdcall" fn(_, _, _, _) -> _,
            heap_realloc_override,
        ));
    }

    // let s = 0x822499; //0x822465;
    // let new =
    //     unsafe { ilhook::x86::Hooker::new(s, HookType::JmpBack(heap_alloc_esi_result), 0).hook(6) };
    // std::mem::forget(new);

    /*
       for c in [0x82346f, 0x8233ee, 0x82f125] {
           let new = unsafe {
               ilhook::x86::Hooker::new(
                   c,
                   HookType::JmpBack(reallochook),
                   ilhook::x86::CallbackOption::None,
                   0,
               )
               .hook()
           }
           .unwrap();
           hook.push(new);
       }
    */

    //prevent A pause in replay mode

    let new = unsafe { ilhook::x86::Hooker::new(0x48267a, HookType::JmpBack(apause), 0).hook(8) };
    std::mem::forget(new);

    // 0x428358 calls function checking if there is a next frame in net object

    let new =
        unsafe { ilhook::x86::Hooker::new(0x41daea, HookType::JmpBack(readonlinedata), 0).hook(5) };
    std::mem::forget(new);

    /*
    407f43 is being set to 8 upon ESC. 407f43 likely stores desired screen 0x8a0040 and the comparison with DAT_008a0044 is where the state gets bugged.
    if it's possible to "flush" the state to "go back to character select", that would be ideal
    */

    /*

       unsafe extern "cdecl" fn set_eax_to_0(a: *mut ilhook::x86::Registers, _b: usize) {
           //let r = *(0x8a0040 as *const u32);
           //println!("esc: {}", r);

           //(*a).eax = *(0x8a0040 as *const u32);
       }
       let new =
           unsafe { ilhook::x86::Hooker::new(0x407f1b, HookType::JmpBack(set_eax_to_0), 0).hook(6) };
       std::mem::forget(new);
    */

    unsafe extern "cdecl" fn override_current_game_state(
        a: *mut ilhook::x86::Registers,
        _b: usize,
    ) {
        if ESC2.load(Relaxed) != 0 {
            ESC2.store(0, Relaxed);
            if !GIRLS_ARE_TALKING {
                //println!("esc from opponent detected");

                if is_p1() {
                    (*a).eax = 8;
                } else {
                    (*a).eax = 9;
                }
            }
        }
    }
    let new = unsafe {
        ilhook::x86::Hooker::new(0x407f48, HookType::JmpBack(override_current_game_state), 0)
            .hook(6)
    };
    std::mem::forget(new);

    unsafe extern "cdecl" fn handle_raw_input(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) {
        //        0046c902 8b ae 6c        MOV        EBP,dword ptr [ESI + 0x76c]

        (*a).ebp = *ptr_wrap!(((*a).esi + 0x76c) as *const u32);
        let input_manager = (*a).ecx;

        let real_input = match std::mem::replace(&mut REAL_INPUT, REAL_INPUT2.take()) {
            Some(x) => x,
            None => {
                IS_FIRST_READ_INPUTS = false;
                let f = std::mem::transmute::<usize, extern "fastcall" fn(u32)>(0x040a370);
                (f)(input_manager);
                return;
            }
        };
        if IS_FIRST_READ_INPUTS {
            let gametype_main = *(0x898688 as *const usize);
            let is_netplay = *(0x8986a0 as *const usize) != 0;
            if (gametype_main, is_netplay) == (2, false) {
                // replay mode
                let set_key = std::mem::transmute::<
                    usize,
                    unsafe extern "cdecl" fn(scan_code: u8, status: u8),
                >(0x0043de50);
                // F1~F7
                for k in 0x3b..=0x42 {
                    set_key(k, 0);
                }
            }
        }
        IS_FIRST_READ_INPUTS = false;

        {
            let td = &mut *ptr_wrap!((input_manager + 0x38) as *mut i32);
            let lr = &mut *ptr_wrap!((input_manager + 0x3c) as *mut i32);

            match (real_input[0], real_input[1]) {
                (false, true) => *lr = (*lr).max(0) + 1,
                (true, false) | (true, true) => *lr = (*lr).min(0) - 1,
                _ => *lr = 0,
            }

            match (real_input[2], real_input[3]) {
                (false, true) => *td = (*td).max(0) + 1,
                (true, false) | (true, true) => *td = (*td).min(0) - 1,
                _ => *td = 0,
            }
        }

        for a in 0..6 {
            let v = &mut *ptr_wrap!((input_manager + 0x40 + a * 4) as *mut u32);

            if real_input[a as usize + 4] {
                *v += 1;
            } else {
                *v = 0;
            }
        }

        let m = &mut *ptr_wrap!((input_manager + 0x62) as *mut u16);
        *m = 0;
        for a in 0..10 {
            if real_input[a] {
                *m += 1 << a;
            }
        }
    }

    // todo : rename

    //return: 0x42839a
    //online input loop,

    let new = unsafe {
        ilhook::x86::Hooker::new(0x428374, HookType::JmpToAddr(0x42837f /*- 5*/, 0, skip), 0)
            .hook(5)
    };
    std::mem::forget(new);

    unsafe extern "cdecl" fn skiponcehost(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) -> usize {
        if ESC > 120 {
            //old mechanism
            0x428393
        } else {
            //let skip = DISABLE_SEND.load(Relaxed) != 0;
            //DISABLE_SEND.store(1, Relaxed);

            let skip = true;

            if skip {
                0x428360
            } else {
                (*a).ecx = *ptr_wrap!(((*a).edi + 0x8) as *const u32);
                (*a).eax = *ptr_wrap!(((*a).ecx) as *const u32);
                0x428335
            }
        }
    }
    unsafe extern "cdecl" fn esc_host(_a: *mut ilhook::x86::Registers, _b: usize) {
        //println!("host has esced");
        send_packet_untagged(Box::new([0x6e, 0]))
    }

    let new = unsafe { ilhook::x86::Hooker::new(0x428394, HookType::JmpBack(esc_host), 0).hook(5) };
    std::mem::forget(new);

    //input 00428341
    /*
    00481980 hm
     */
    let new =
        unsafe { ilhook::x86::Hooker::new(0x428330, HookType::JmpToRet(skiponcehost), 0).hook(5) };
    std::mem::forget(new);

    unsafe extern "cdecl" fn skiponceclient(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) -> usize {
        if ESC > 120 {
            //old mechanism
            0x4286c3
        } else {
            //let skip = DISABLE_SEND.load(Relaxed) != 0;
            //DISABLE_SEND.store(1, Relaxed);

            let skip = true;

            if skip {
                0x428630
            } else {
                (*a).ecx = *ptr_wrap!(((*a).edi + 0x8) as *const u32);
                (*a).eax = *ptr_wrap!(((*a).ecx) as *const u32);
                0x428605
            }
        }
    }

    unsafe extern "cdecl" fn esc_client(_a: *mut ilhook::x86::Registers, _b: usize) {
        //println!("client has esced");
        send_packet_untagged(Box::new([0x6e, 0]))
    }

    let new =
        unsafe { ilhook::x86::Hooker::new(0x428664, HookType::JmpBack(esc_client), 0).hook(5) };
    std::mem::forget(new);

    let new =
        unsafe { ilhook::x86::Hooker::new(0x428681, HookType::JmpBack(esc_client), 0).hook(5) };
    std::mem::forget(new);

    //not sure why client has two "esc" spaces but I'm not going to question it

    let new = unsafe {
        ilhook::x86::Hooker::new(0x428600, HookType::JmpToRet(skiponceclient), 0).hook(5)
    };
    std::mem::forget(new);

    let new = unsafe {
        ilhook::x86::Hooker::new(0x428644, HookType::JmpToAddr(0x42864f, 0, skip), 0).hook(5)
    };
    std::mem::forget(new);

    unsafe extern "cdecl" fn timing_loop(a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {
        //#[cfg(feature = "f62")]
        //const TARGET_FRAMETIME: i32 = 1_000_000 / 62;
        //#[cfg(not(feature = "f62"))]
        //const TARGET_FRAMETIME: i32 = 1_000_000 / 60;

        let target_frametime = if F62_ENABLED {
            1_000_000 / 62
        } else {
            1_000_000 / 60
        };

        let waithandle = (*a).esi; //should I even use this? :/
        let (m, target) = match UPDATE {
            Some(x) => (x, TARGET.as_mut().unwrap()),
            None => {
                let m = SystemTime::now();
                UPDATE = Some(m);
                TARGET = Some(0);
                (m, TARGET.as_mut().unwrap())
            }
        };
        //let c = TARGET_OFFSET_COUNT.fetch_add(1, Relaxed);
        //if c % 10 == 0 {
        //    TARGET_OFFSET.store(0, Relaxed);
        //}

        let s = TARGET_OFFSET.swap(0, Relaxed).clamp(-1000, 10000);
        //TARGET_OFFSET.fetch_add(s / 2, Relaxed);
        *target += (target_frametime + s) as u128;

        let cur = m.elapsed().unwrap().as_micros();

        let diff = (*target as i128 + 1000) - cur as i128 - SPIN_TIME_MICROSECOND;
        //if diff > 1_000_000 {
        //    panic!("big diff {diff}");
        //}

        //info!("frame diff micro diff: {}", diff);
        let ddiff = (diff / 1000) as i32;
        if ddiff < 0 {
            println!("frameskip");
            #[cfg(feature = "logtofile")]
            info!("frameskip {diff}");
            if ddiff > 2 {
                *target = cur + (target_frametime) as u128;
            } else {
            }
            WARNING_FRAME_LOST_COUNTDOWN.store(120, Relaxed);
        } else {
            WaitForSingleObject(HANDLE(waithandle as isize), ddiff as u32);
            if SPIN_TIME_MICROSECOND != 0 {
                loop {
                    let r1 = m.elapsed().unwrap().as_micros();
                    if r1 >= *target {
                        break;
                    }
                }
            }
            if let Ok(event) = SOKU_LOOP_EVENT.lock() {
                // if the last signal hasn't been reset by the loop
                if let Some(event) = *event
                    && WaitForSingleObject(HANDLE(event), 0).0 == 0
                {
                    WARNING_FRAME_LOST_COUNTDOWN.store(120, Relaxed);
                }
            }
        };
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(0x4192f0, HookType::JmpToAddr(0x4193d7, 0, timing_loop), 0).hook(6)
    };
    std::mem::forget(new);
    //hook.push(new);

    static mut ORI_CLOSE_LOOP_EVENT: Option<unsafe extern "C" fn(isize) -> isize> = None;
    static mut ORI_CREATE_LOOP_EVENT: Option<unsafe extern "C" fn() -> isize> = None;

    unsafe extern "C" fn close_loop_event_override(handle: isize) -> isize {
        if let Ok(mut event) = SOKU_LOOP_EVENT.lock() {
            *event = None;
        }
        return ORI_CLOSE_LOOP_EVENT.unwrap()(handle);
    }
    unsafe extern "C" fn create_loop_event_override() -> isize {
        let handle = ORI_CREATE_LOOP_EVENT.unwrap()();
        if let Ok(mut event) = SOKU_LOOP_EVENT.lock() {
            *event = Some(handle);
        }
        return handle;
    }
    unsafe {
        ORI_CLOSE_LOOP_EVENT = Some(tamper_jmp_relative_opr(
            0x408092 as *mut c_void,
            close_loop_event_override as unsafe extern "C" fn(isize) -> isize,
        ));
        ORI_CREATE_LOOP_EVENT = Some(tamper_jmp_relative_opr(
            0x407dec as *mut c_void,
            create_loop_event_override as unsafe extern "C" fn() -> isize,
        ));
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x46c900,
            HookType::JmpToAddr(0x46c908, 0, handle_raw_input),
            0,
        )
        .hook(8)
    };
    std::mem::forget(new);

    unsafe extern "cdecl" fn sniff_sent(a: *mut ilhook::x86::Registers, _b: usize) {
        let ptr = ((*a).edi + 0x1c) as *const u8;
        let packet_size = *(((*a).edi + 0x18) as *const usize);
        let buf = std::slice::from_raw_parts(ptr, 400);

        update_input_time_data(buf, packet_size, true);

        if !FREEZE_MITIGATION {
            return;
        }
        // Ensure we don't assign the GAME_REQUEST packet for spectators to LAST_GAME_REQUEST.
        if buf[0] == if is_p1() { 13 } else { 14 } && buf[1] == 4 {
            if let Some(old_game_request) = LAST_GAME_REQUEST {
                println!("sending more game request!");
                assert_eq!(old_game_request[0..packet_size], buf[0..packet_size]);
            } else {
                let mut m = [0; 400];
                for i in 0..buf.len() {
                    m[i] = buf[i];
                }
                println!("get game request!");
                LAST_GAME_REQUEST = Some(m);
            }
        }

        if (buf[0] == 13 || buf[0] == 14) && buf[1] == 2 {
            let mut m = [0; 400];
            for i in 0..buf.len() {
                m[i] = buf[i];
            }

            LAST_LOAD_ACK = Some(m);
        }

        if (buf[0] == 13 || buf[0] == 14) && buf[1] == 5 {
            let mut m = [0; 400];
            for i in 0..buf.len() {
                m[i] = buf[i];
            }

            LAST_MATCH_ACK = Some(m);
        }

        if (buf[0] == 13 || buf[0] == 14) && buf[1] == 1 {
            let mut m = [0; 400];
            for i in 0..buf.len() {
                m[i] = buf[i];
            }

            LAST_MATCH_LOAD = Some(m);
        }
    }

    let new =
        unsafe { ilhook::x86::Hooker::new(0x4171b4, HookType::JmpBack(sniff_sent), 0).hook(5) };
    std::mem::forget(new);

    let new =
        unsafe { ilhook::x86::Hooker::new(0x4171c7, HookType::JmpBack(sniff_sent), 0).hook(5) };
    std::mem::forget(new);

    if freeze_mitigation {
        unsafe {
            tamper_jmp_relative_opr(
                0x0041dae5 as *mut c_void,
                recvfrom_with_fake_packet as unsafe extern "stdcall" fn(_, _, _, _, _, _) -> _,
            )
        };
    }

    //*HOOK.lock().unwrap() = Some(hook.into_boxed_slice());

    unsafe {
        std::thread::spawn(|| {
            //wait to avoid being overwritten by th123e
            std::thread::sleep(Duration::from_millis(3000));

            windows::Win32::UI::WindowsAndMessaging::SetWindowTextW(
                *(0x89ff90 as *const HWND),
                windows::core::PCWSTR::from_raw(ptr_wrap!(TITLE.as_ptr())),
            )
        })
    };

    let new = unsafe {
        ilhook::x86::Hooker::new(0x00482689, HookType::JmpToRet(is_replay_over), 0).hook(5)
    };
    std::mem::forget(new);

    Some(())
}

#[no_mangle]
pub extern "cdecl" fn cleanup() {
    if ISDEBUG {
        #[cfg(feature = "logtofile")]
        info!("cleaning up the hook")
    };

    //for a in std::mem::replace(&mut *FRAMES.lock().unwrap(), Vec::new()) {
    //    a.did_happen();
    //}

    HOOK.lock()
        .unwrap()
        .take()
        .unwrap()
        .into_vec()
        .into_iter()
        .for_each(|x| unsafe { x.unhook() });
}

unsafe fn set_input_buffer(input: [bool; 10], input2: [bool; 10]) {
    REAL_INPUT = Some(input);
    REAL_INPUT2 = Some(input2);
}
//might not be neccesseary
static REQUESTED_THREAD_ID: AtomicU32 = AtomicU32::new(0);

static mut NEXT_DRAW_PING: Option<i32> = None;
static mut NEXT_DRAW_ROLLBACK: Option<i32> = None;
static mut NEXT_DRAW_ENEMY_DELAY: Option<i32> = None;

static mut _NEXT_DRAW_PACKET_LOSS: Option<i32> = None;
static mut _NEXT_DRAW_PACKET_DESYNC: Option<i32> = None;

const SOKU_FRAMECOUNT: *mut usize = 0x8985d8 as *mut usize;
use windows::Win32::System::Threading::GetCurrentThreadId;

// static FREEMUTEX: Mutex<BTreeSet<usize>> = Mutex::new(BTreeSet::new());

#[cfg(feature = "fillfree")]
static mut HEAP_FREE_RNG: Option<rand::rngs::ThreadRng> = None;

#[macro_export]
macro_rules! soku_heap_free {
    ($ptr:expr) => {{
        use windows::Win32::{
            Foundation::HANDLE,
            System::Memory::{HeapFree, HEAP_FLAGS},
        };
        let a: usize = $ptr;
        #[cfg(feature = "fillfree")]
        {
            use crate::rollback::read_heap;
            let size = read_heap(a);
            let a = std::slice::from_raw_parts_mut(a as *mut u8, size);
            use crate::HEAP_FREE_RNG;
            use rand::{thread_rng, Rng};
            if HEAP_FREE_RNG.is_none() {
                HEAP_FREE_RNG = Some(thread_rng());
            }
            HEAP_FREE_RNG.as_mut().unwrap().fill(a);
        }
        HeapFree(
            HANDLE(*(0x89b404 as *const isize)),
            HEAP_FLAGS(0),
            Some(a as *const c_void),
        )
        .unwrap_or_else(|e| panic!("HeapFree failed for {:?}", e));
    }};
}

unsafe extern "stdcall" fn heap_free_override(heap: isize, flags: u32, s: *const c_void) -> i32 {
    // Soku2 unaligned (*_a).esp (can be triggered with j2a and db of Momizi):

    //if let Some(x) = MEMORYMUTEX.lock().unwrap().remove(&*s) {
    //    if x != *SOKU_FRAMECOUNT {
    //        println!("freeing memory allocated at frame: {}, current: {}", x, *SOKU_FRAMECOUNT)
    //    }
    //}

    //if GetCurrentThreadId() == REQUESTED_THREAD_ID.load(Relaxed) {
    if
    /* !matches!(*(0x8a0040 as *const u8), 0x5 | 0xe | 0xd) || */
    *(0x89b404 as *const isize) != heap
        || GetCurrentThreadId() != REQUESTED_THREAD_ID.load(Relaxed)
        || *SOKU_FRAMECOUNT == 0
    {
        return HeapFree(HANDLE(heap), HEAP_FLAGS(flags as u32), Some(s)).is_ok() as i32;
    }

    unsafe {
        MEMORY_SENDER_FREE
            .as_ref()
            .unwrap()
            .clone()
            .send(s as usize)
            .unwrap()
    };

    return 1;

    //} else {
    //    let heap = unsafe { *(0x89b404 as *const isize) };
    //    unsafe { HeapFree(heap, 0, *s as *const c_void) };
    //}
    //info!("{}", *s);
    //let mut f = FRAMES.lock().unwrap();
    //
    //match f.last_mut() {
    //    Some(x) => {
    //        x.frees.push(*s);
    //        //unsafe { *s = 0 };
    //    }
    //    None => (), //ignore
    //}
    //
    //A_COUNT.store(A_COUNT.load(Relaxed) + 1, Relaxed);
}

// static ALLOCMUTEX: Mutex<BTreeSet<usize>> = Mutex::new(BTreeSet::new());
//static MEMORYMUTEX: Mutex<BTreeMap<usize, usize>> = Mutex::new(BTreeMap::new());

fn store_alloc(u: usize) {
    unsafe {
        //ALLOCMUTEX.lock().unwrap().insert(u);
        //MEMORYMUTEX.lock().unwrap().insert(u, *SOKU_FRAMECOUNT);
        //return;

        MEMORY_SENDER_ALLOC
            .as_ref()
            .unwrap()
            .clone()
            .send(u)
            .unwrap();
    }
}

static mut LIKELY_DESYNCED: bool = false;

#[no_mangle]
pub extern "cdecl" fn is_likely_desynced() -> bool {
    unsafe { LIKELY_DESYNCED }
}

unsafe extern "stdcall" fn heap_alloc_override(heap: isize, flags: u32, s: usize) -> *mut c_void {
    let ret = HeapAlloc(HANDLE(heap), HEAP_FLAGS(flags), s);

    if *(0x89b404 as *const usize) != heap as usize
        /*|| !matches!(*(0x8a0040 as *const u8), 0x5 | 0xe | 0xd)*/
        || *SOKU_FRAMECOUNT == 0 ||
        GetCurrentThreadId() != REQUESTED_THREAD_ID.load(Relaxed)
    {
        //println!("wrong heap alloc");
    } else {
        assert_ne!(ret, null_mut(), "HeapAlloc failed for {:?}", GetLastError());
        store_alloc(ret as usize);
    }
    return ret;
}

// unsafe extern "cdecl" fn heap_alloc_esi_result(a: *mut ilhook::x86::Registers, _b: usize) {
//     if GetCurrentThreadId() == REQUESTED_THREAD_ID.load(Relaxed)
//         /* && matches!(*(0x8a0040 as *const u8), 0x5 | 0xe | 0xd) */
//         && *SOKU_FRAMECOUNT != 0
//     {
//         store_alloc((*a).esi as usize);
//     }
// }

static mut ORI_HEAP_REALLOC: Option<unsafe extern "stdcall" fn(isize, u32, usize, usize) -> usize> =
    None;

unsafe extern "stdcall" fn heap_realloc_override(
    heap: isize,
    flags: u32,
    p: usize,
    s: usize,
) -> usize {
    if *(0x89b404 as *const usize) != heap as usize
        /*|| !matches!(*(0x8a0040 as *const u8), 0x5 | 0xe | 0xd)*/
        || *SOKU_FRAMECOUNT == 0 ||
        GetCurrentThreadId() != REQUESTED_THREAD_ID.load(Relaxed)
    {
        //println!("wrong heap alloc");
        ORI_HEAP_REALLOC.unwrap()(heap, flags, p, s)
    } else {
        REQUESTED_THREAD_ID.store(0, Relaxed);
        panic!("HeapRealloc({},{},{},{})!!!", heap, flags, p, s);
    }
}

use core::sync::atomic::AtomicU8;

use crate::{
    netcode::send_packet_untagged,
    replay::{
        apause, clean_replay_statics, handle_replay, is_replay_over,
        render_replay_progress_bar_and_numbers,
    },
    rollback::CHARSIZEDATA,
};

static LAST_STATE: AtomicU8 = AtomicU8::new(0x6b);
static mut HAS_LOADED: bool = false;
static mut AFTER_GAME_REQUEST_FROM_P1: bool = false;

pub fn is_p1() -> bool {
    let is_p1 = unsafe {
        let netmanager = *(0x8986a0 as *const usize);
        *(netmanager as *const usize) == 0x858cac
    };
    is_p1
}

unsafe extern "stdcall" fn recvfrom_with_fake_packet(
    s: SOCKET,
    buf: *mut u8,
    len: i32,
    flags: i32,
    from: *mut SOCKADDR,
    fromlen: *mut i32,
) -> u32 {
    if let Some(ori_recvfrom) = ORI_RECVFROM {
        if AFTER_GAME_REQUEST_FROM_P1 {
            // AFTER_GAME_REQUEST_FROM_P1 = false;
            let netmanager = *(0x8986a0 as *const usize);
            let to;
            if *(netmanager as *const usize) == 0x858cac {
                let it = (netmanager + 0x4c8) as *const usize;
                if *it == 0 {
                    panic!();
                }
                to = *(it as *const *const SOCKADDR);
            } else {
                if *(netmanager as *const usize) != 0x858d14 {
                    panic!();
                }
                to = (netmanager + 0x47c) as *const SOCKADDR
            }

            *from = *to;
            *fromlen = 0x10;
            *buf = 0xd;
            *buf.offset(1) = 0x5;
            println!("Send a simulated LAST_MATCH_ACK packet to myself to get my GAME_REQUEST.");
            return 2;
        }
        return ori_recvfrom(s, buf, len, flags, from, fromlen);
    }
    panic!();
}

struct P2SendTimeData {
    has_received: bool,
    last_frame_id: usize,
    last_receive_time: Instant,
    last_max_latency: Option<Duration>,
    max_latency_to_be_shown: Option<Duration>,
    last_shown_frame: usize,
}

static SELECT_SCENE_INPUT_SEND_TIME_DATA: Mutex<Option<P2SendTimeData>> = Mutex::new(None);
fn update_input_time_data(buf: &[u8], packet_size: usize, is_sending: bool) {
    // https://github.com/delthas/touhou-protocol-docs/blob/master/protocol_123.md#game-packet-sub-type-0x03-game_input
    // | 0d or 0e  03  FRAME_ID (4 bytes)  SCENE_ID (1 bytes)  FRAME_COUNT (1 byte)  INPUTS (2*FRAME_COUNT bytes) |
    if packet_size >= 1 + 1 + 4 + 1 + 1 + 2
        && matches!(buf[0], 0xe | 0xd)
        && buf[1] == 0x3
        && buf[6] == 0x3
        && let Some(time_data) = SELECT_SCENE_INPUT_SEND_TIME_DATA.lock().unwrap().as_mut()
    {
        let input_count: u8 = buf[7];
        let input_pair_count = input_count.div_ceil(2);
        let frame_id_end: usize = u32::from_le_bytes(buf[2..2 + 4].try_into().unwrap()) as usize;
        let frame_id: usize = frame_id_end + 1 - input_pair_count as usize;
        // println!(
        //     "{} send/recv {} {} {}",
        //     buf[0], is_sending, frame_id, input_count
        // );
        if !is_sending && time_data.last_frame_id <= frame_id && !time_data.has_received {
            // println!("really get {}", frame_id);
            time_data.has_received = true;
            time_data.last_frame_id = frame_id_end;
            time_data.last_max_latency = Some(
                (Instant::now().saturating_duration_since(time_data.last_receive_time) / 2)
                    .max(time_data.last_max_latency.unwrap_or(Duration::ZERO)),
            );
            if frame_id > time_data.last_shown_frame + 60 {
                time_data.max_latency_to_be_shown = time_data.last_max_latency;
                time_data.last_shown_frame = frame_id;
                time_data.last_max_latency = None;
            }
        }

        if is_sending && time_data.has_received {
            let to_get_frame_id = match buf[0] {
                0xd => frame_id_end, // p1 is sending
                0xe => frame_id + 1, // p2 is sending
                _ => panic!("unreachable!"),
            };
            if time_data.last_frame_id < to_get_frame_id {
                // println!("to get {}", to_get_frame_id);
                time_data.has_received = false;
                time_data.last_receive_time = Instant::now();
                time_data.last_frame_id = to_get_frame_id;
            }
        }
    }
}

unsafe extern "cdecl" fn readonlinedata(a: *mut ilhook::x86::Registers, _b: usize) {
    const P1_PACKETS: [u8; 400] = [
        13, 3, 1, 0, 0, 0, 5, 2, 0, 0, 0, 0, 12, 0, 103, 0, 103, 0, 103, 0, 103, 0, 104, 0, 104, 0,
        104, 0, 104, 0, 106, 0, 106, 0, 200, 0, 203, 0, 208, 0, 208, 0, 208, 0, 208, 0, 1, 15, 0,
        0, 0, 189, 3, 21, 23, 251, 48, 70, 108, 0, 0, 0, 0, 0, 0, 221, 143, 113, 190, 134, 199,
        125, 39, 12, 12, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0,
    ];

    const P2_PACKETS: [u8; 400] = [
        14, 3, 0, 0, 0, 0, 5, 1, 0, 0, 20, 100, 0, 100, 0, 101, 0, 101, 0, 102, 0, 102, 0, 103, 0,
        103, 0, 200, 0, 200, 0, 200, 0, 200, 0, 201, 0, 201, 0, 201, 0, 201, 0, 203, 0, 203, 0,
        203, 0, 203, 0, 1, 15, 119, 144, 191, 37, 118, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

    //both packets here are the same, both are the 0th packet, this is maybe unneccesseary

    let esp = (*a).esp;

    let packet_pointer = esp + 0x70;
    let slic = std::slice::from_raw_parts_mut(packet_pointer as *mut u8, 400);
    let len = (*a).eax;
    let type1 = match len > 0 {
        true => slic[0],
        false => 0,
    };
    let type2 = slic[1];

    // let count = usize::from_le_bytes(slic[2..6].try_into().unwrap());
    let sceneid = slic[6];
    //   let somethingweird = slic[7];
    //   let input1 = slic[8];
    //   let input2 = slic[9];

    //println!("{} , {}", &slic[0], &slic[1]);

    update_input_time_data(slic, len as usize, false);

    if type1 == 0x6e {
        //opponent esc
        if BATTLE_STARTED {
            ESC2.store(1, Relaxed);

            if !is_p1() {
                slic.copy_from_slice(&P1_PACKETS)
            } else {
                slic.copy_from_slice(&P2_PACKETS)
            }
        }
    } else if type1 == 0x6c {
        let buf = [0x6d, 0x61];
        let sock = *ptr_wrap!(((*a).edi + 0x28) as *const u32);
        let to = (*a).esp + 0x44;

        windows::Win32::Networking::WinSock::sendto(
            std::mem::transmute::<u32, windows::Win32::Networking::WinSock::SOCKET>(sock),
            &buf,
            0,
            to as *const windows::Win32::Networking::WinSock::SOCKADDR,
            0x10,
        );

        (*a).eax = 0x400;
    } else if type1 > 0x6c && type1 <= 0x80 {
        (*a).eax = 0x400;
    }

    if type1 == 0x6b {
        let m = DISABLE_SEND.load(Relaxed);

        if BATTLE_STARTED {
            let z = NetworkPacket::decode(&slic[0..len as usize]);
            DATA_SENDER
                .as_ref()
                .unwrap()
                .send((z, Instant::now()))
                .unwrap();
        }

        if m < 150 {
            DISABLE_SEND.store(m + 1, Relaxed);

            let is_p1 = unsafe {
                let netmanager = *(0x8986a0 as *const usize);
                *(netmanager as *const usize) == 0x858cac
            };
            //the packet you receive first frame, every round. We are making it manually, to prevent data loss from freezing the game
            if !is_p1 {
                slic.copy_from_slice(&P1_PACKETS)
            } else {
                slic.copy_from_slice(&P2_PACKETS)
            }
        } else {
            (*a).eax = 0x400;
        }
    }

    if (type1 == 14 || type1 == 13) && type2 == 3 && sceneid == 0x5 && false {
        let is_p1 = unsafe {
            let netmanager = *(0x8986a0 as *const usize);
            *(netmanager as *const usize) == 0x858cac
        };

        println!(
            "received {} {}, data: {:?} as {}",
            type1, type2, slic, is_p1
        );
    }

    if type1 == 14 || type1 == 13 {
        if FREEZE_MITIGATION {
            if type2 == 4 {
                if HAS_LOADED {
                    println!("Receive redundance GAME_REQUEST. Ignore it.");
                    slic[0] = 0;
                } else if type1 == 13 {
                    println!("Receive GAME_REQUEST.");
                    if *(0x008A0044 as *const u32) == 8 || *(0x008A0044 as *const u32) == 9 {
                        if !is_p1() {
                            AFTER_GAME_REQUEST_FROM_P1 = true;
                            println!("It is from p1 to p2. A fake packet will be sent.");
                        } else {
                            println!("It is from p2 to p1. Nothing extra needs to be done.");
                        }
                    } else {
                        println!(
                            "WHILE the scene isn't SELECTSV or SELECTCL. It is {} instead.",
                            *(0x008A0044 as *const u32)
                        )
                    }
                }

                HAS_LOADED = true;
                //println!("has loaded !");
            }

            if type2 == 5
                && ([8, 9, 10, 11] as [usize; 4])
                    .contains((0x008A0044 as *const usize).as_ref().unwrap())
                && !is_p1()
            {
                if AFTER_GAME_REQUEST_FROM_P1 {
                    println!("p2 get its fake packet [{},{}]", type1, type2);
                    AFTER_GAME_REQUEST_FROM_P1 = false;
                } else {
                    if !is_p1() {
                        println!("p2 get [{},{}]. not send it to the game.", type1, type2);
                        slic[0] = 0;
                    }
                    if let Some(gr) = LAST_GAME_REQUEST {
                        println!("the opponent is requesting GAME_REQUEST packet. reply it with LAST_GAME_REQUEST.");
                        send_packet_untagged(Box::new(gr));
                        slic[0] = 0;
                    }
                }
            }
        }

        if type2 == 1 && false {
            if let Some(gr) = LAST_LOAD_ACK {
                send_packet_untagged(Box::new(gr));
                println!("successfully sent 2 :)");
            } else {
                if let Some(gr) = LAST_GAME_REQUEST {
                    send_packet_untagged(Box::new(gr));
                    println!("successfully sent 3 :)");
                }
                println!("possibly shouldn't be here 2?");
            }
        }
    }

    if (type1 == 14 || type1 == 13) && type2 == 1 {
        //opponent has esced (probably) exit, the 60 is to avoid stray packets causing exits

        //if sceneid == 3 {
        //    // starting battle, likely we can safely reset ESC
        //    ESC = 0;
        //}
        if BATTLE_STARTED {
            ESC += 1;

            if ESC == 10 {
                let is_p1 = unsafe {
                    let netmanager = *(0x8986a0 as *const usize);
                    *(netmanager as *const usize) == 0x858cac
                };
                //the packet you receive first frame, every round. We are making it manually, to prevent data loss from freezing the game
                if !is_p1 {
                    slic.copy_from_slice(&P1_PACKETS)
                } else {
                    slic.copy_from_slice(&P2_PACKETS)
                }
            }

            //if ESC == 20 {
            //    *(0x8a0040 as *mut u32) = 8
            //}

            if ESC > 250 {
                println!("here stuck state detected");
                slic[0] = 0xb;
                ESC = 0;
                send_packet_untagged(Box::new([0xb]));
                let netmanager = *(0x8986a0 as *const usize);
                let socket = netmanager + 0x3e4;

                closesocket(*(socket as *const windows::Win32::Networking::WinSock::SOCKET));
            }
        }
        //info!("received {} {} {}", type1, type2, sceneid);
    }

    if type1 == 5 {
        let is_spect = slic[25] == 0;

        //println!("slic26: {:?}", &);
        if is_spect {
            if F62_ENABLED {
                slic[1] = VERSION_BYTE_62;
            } else {
                slic[1] = VERSION_BYTE_60;
            }
        }
        //is_spect = slic[]
        //let gamever = slic[1..17];
    }
    //BATTLE_STARTED
}
// network round start, stores round number
//00858cb8 c0 5f 45 00     addr      FUN_00455fc0            [3]

static mut GIRLSTALKED: bool = false;
static DISABLE_SEND: AtomicU8 = AtomicU8::new(0);

//todo: improve rewind mechanism

fn input_to_accum(inp: &[bool; 10]) -> u16 {
    let mut inputaccum = 0u16;
    for a in 0..10 {
        if inp[a] {
            inputaccum += 0x1 << a;
        }
    }
    inputaccum
}

unsafe fn read_key_better(key: u8) -> bool {
    let raw_input_buffer = 0x8a01b8;

    *((raw_input_buffer + key as u32) as *const u8) != 0
}

unsafe fn read_current_input() -> [bool; 10] {
    let local_input_manager = 0x898938;
    let raw_input_buffer = 0x8a01b8;
    let mut input = [false; 10];

    let controller_id = *((local_input_manager + 0x4) as *const u8);
    //if 255, then keyboard, if 0, or maybe something else, then controller

    if controller_id == 255 {
        //no controllers, reading keyboard input
        for a in 0..10 {
            let key = (local_input_manager + 0x8 + a * 0x4) as *const u8;

            let key = *key as u32;

            let key = *((raw_input_buffer + key) as *const u8) != 0;
            input[a] = key;
        }
    } else {
        let get_controller =
            std::mem::transmute::<usize, extern "thiscall" fn(u32, u32) -> u32>(0x40dc60);
        let controler = get_controller(0x8a0198, controller_id as u32);

        if controler != 0 {
            let axis1 = *ptr_wrap!(controler as *const i32);
            let axis2 = *ptr_wrap!((controler + 4) as *const i32);

            input[2] = axis1 < -500;
            input[3] = axis1 > 500;

            input[0] = axis2 < -500;
            input[1] = axis2 > 500;

            for a in 0..6 {
                let key = *ptr_wrap!((local_input_manager + 0x18 + a * 0x4) as *const i32);

                if key > -1 {
                    input[a + 4] = *ptr_wrap!((key as u32 + 0x30 + controler) as *const u8) != 0;
                }
            }
        }
    }

    input
}

static mut ROLLBACKER: Option<Rollbacker> = None;
static mut NETCODER: Option<Netcoder> = None;

static mut DATA_SENDER: Option<std::sync::mpsc::Sender<(NetworkPacket, Instant)>> = None;
static mut DATA_RECEIVER: Option<std::sync::mpsc::Receiver<(NetworkPacket, Instant)>> = None;

static mut MEMORY_SENDER_FREE: Option<std::sync::mpsc::Sender<usize>> = None;
static mut MEMORY_RECEIVER_FREE: Option<std::sync::mpsc::Receiver<usize>> = None;

static mut MEMORY_SENDER_ALLOC: Option<std::sync::mpsc::Sender<usize>> = None;
static mut MEMORY_RECEIVER_ALLOC: Option<std::sync::mpsc::Receiver<usize>> = None;

// this value is offset by 1, because we start sending frames at frame 1, meaning that input for frame n + 1 is sent in packet n
static mut LAST_DELAY_VALUE: usize = 0;
static mut DEFAULT_DELAY_VALUE: usize = 0;

static mut AUTODELAY_ENABLED: bool = false;
static mut AUTODELAY_ROLLBACK: i8 = 0;

static mut LAST_DELAY_MANIP: u8 = 0; // 0 neither, 1 up, 2 down, 3 both

static mut BATTLE_STARTED: bool = false;

static mut ESC: u8 = 0;
static mut GIRLS_ARE_TALKING: bool = false;

static mut ESC2: AtomicU8 = AtomicU8::new(0);

static mut INCREASE_DELAY_KEY: u8 = 0;
static mut DECREASE_DELAY_KEY: u8 = 0;

static mut INCREASE_MAX_ROLLBACK_KEY: u8 = 0;
static mut DECREASE_MAX_ROLLBACK_KEY: u8 = 0;

static mut TOGGLE_STAT_KEY: u8 = 0;

static mut TAKEOVER_KEYS_SCHEME: [u8; 4] = [0, 0, 0, 0];

static mut TOGGLE_STAT: bool = false;
static mut LAST_TOGGLE: bool = false;

fn draw_num(pos: (f32, f32), num: i32) {
    let drawfn: extern "thiscall" fn(
        ptr: *const c_void,
        number: i32,
        x: f32,
        y: f32,
        a1: i32,
        a2: u8,
    ) = unsafe { std::mem::transmute::<usize, _>(0x414940) };

    drawfn(0x882940 as *const c_void, num, pos.0, pos.1, 0, 0);
}

fn get_num_length(num: i32, edge_spacing: bool) -> f32 {
    let mut len: usize = 0;
    let mut num_ = num;
    while num_ != 0 {
        num_ /= 10;
        len += 1;
    }
    if len == 0 {
        len = 1;
    }
    let width = unsafe { *((0x882940 + 0x4) as *const f32) };
    let spacing = unsafe { *((0x882940 + 0x8) as *const f32) };
    let scale = unsafe { *((0x882940 + 0xc) as *const f32) };
    return (width * (len as f32)
        + spacing * (if edge_spacing { len + 1 } else { len - 1 } as f32))
        * scale;
}

fn draw_num_x_center(pos: (f32, f32), num: i32) {
    let drawfn: extern "thiscall" fn(
        ptr: *const c_void,
        number: i32,
        x: f32,
        y: f32,
        a1: i32,
        a2: u8,
    ) = unsafe { std::mem::transmute::<usize, _>(0x414940) };
    drawfn(
        0x882940 as *const c_void,
        num,
        pos.0 + get_num_length(num, false) / 2.0,
        pos.1,
        0,
        0,
    );
}

fn pause(battle_state: &mut u32, state_sub_count: &mut u32) {
    // print!("pause {} ->", *battle_state);
    if *battle_state != 4 {
        LAST_STATE.store(*battle_state as u8, Relaxed);
        *state_sub_count = state_sub_count.wrapping_sub(1);
        *battle_state = 4;
    }
    // println!(" {}", *battle_state);
}
fn resume(battle_state: &mut u32) {
    // should be called every frame because fo the logic set in fn pause involving state_sub_count
    let last = LAST_STATE.load(Relaxed);
    // print!("resume (last: {}) {} ->", last, *battle_state);
    if last != 0x6b && *battle_state == 4 {
        //4 check to not accidentally override a state set by the game *maybe*
        *battle_state = last as u32;
        LAST_STATE.store(0x6b, Relaxed)
    }
    // println!(" {}", *battle_state);
}
//        info!("GAMETYPE TRUE {}", *(0x89868c as *const usize));

unsafe fn change_delay_from_keys(ori: usize) -> usize {
    let k_up = read_key_better(INCREASE_DELAY_KEY);
    let k_down = read_key_better(DECREASE_DELAY_KEY);

    let last_up = LAST_DELAY_MANIP & 1 == 1;
    let last_down = LAST_DELAY_MANIP & 2 == 2;
    LAST_DELAY_MANIP = k_up as u8 + k_down as u8 * 2;
    if !last_up && k_up {
        ori.saturating_add(1).clamp(0, 9)
    } else if !last_down && k_down {
        ori.saturating_sub(1)
    } else {
        ori
    }
}

unsafe fn update_toggle_stat_from_keys() {
    let stat_toggle = read_key_better(TOGGLE_STAT_KEY);
    if stat_toggle && !LAST_TOGGLE {
        TOGGLE_STAT = !TOGGLE_STAT;
    }
    LAST_TOGGLE = stat_toggle;
}

unsafe fn handle_online(
    framecount: usize,
    battle_state: &mut u32,
    cur_speed: &mut u32,
    cur_speed_iter: &mut u32,
    state_sub_count: &mut u32,
) {
    #[cfg(feature = "logrollback")]
    println!("handle {} ({})", framecount, cur_speed_iter);
    if framecount == 0 && !BATTLE_STARTED {
        let round = *ptr_wrap!((*(0x8986a0 as *const usize) + 0x6c0) as *const u8);

        BATTLE_STARTED = true;
        SOUND_MANAGER = Some(RollbackSoundManager::new());
        let m = DATA_RECEIVER.take().unwrap();

        let rollbacker = Rollbacker::new();

        ROLLBACKER = Some(rollbacker);
        let mut netcoder = Netcoder::new(m, MAX_ROLLBACK_PREFERENCE);
        if round == 1 {
            netcoder.autodelay_enabled = if AUTODELAY_ENABLED {
                Some(AUTODELAY_ROLLBACK)
            } else {
                None
            };
            netcoder.delay = DEFAULT_DELAY_VALUE;
        } else {
            netcoder.delay = LAST_DELAY_VALUE;
        }
        netcoder.max_rollback = 6;
        netcoder.display_stats = TOGGLE_STAT;
        NETCODER = Some(netcoder);

        //return;
    }

    if *battle_state == 6 {
        GIRLS_ARE_TALKING = true;
    }

    let rollbacker = ROLLBACKER.as_mut().unwrap();
    let netcoder = NETCODER.as_mut().unwrap();

    resume(battle_state);

    update_toggle_stat_from_keys();

    netcoder.display_stats = TOGGLE_STAT;

    if *cur_speed_iter == 0 {
        LAST_DELAY_VALUE = change_delay_from_keys(LAST_DELAY_VALUE);

        netcoder.delay = LAST_DELAY_VALUE;

        let input = read_current_input();
        let speed = netcoder.process_and_send(rollbacker, input);

        *cur_speed = speed;

        #[cfg(feature = "lowframetest")]
        {
            let target_frametime: i32 = if F62_ENABLED {
                1_000_000 / 62
            } else {
                1_000_000 / 60
            };
            use rand::{rngs::ThreadRng, Rng};
            static mut RNG: Option<ThreadRng> = None;
            if RNG.is_none() {
                RNG = Some(rand::thread_rng());
            }
            // let delayed_target = (*target + target_frametime as u128 + 1000)
            //     .saturating_sub(m.elapsed().unwrap().as_micros());
            std::thread::sleep(Duration::from_micros(
                RNG.as_mut()
                    .unwrap()
                    .gen_range(target_frametime * 3 / 4..=target_frametime) as u64,
            ));
        }

        if speed == 0 {
            pause(battle_state, state_sub_count);
            return;
        }
    }

    if let None = rollbacker.step(*cur_speed_iter as usize) {
        pause(battle_state, state_sub_count);
        return;
    }
}

unsafe extern "cdecl" fn main_hook(a: *mut ilhook::x86::Registers, _b: usize) {
    #[cfg(feature = "logtofile")]
    std::panic::set_hook(Box::new(|x| info!("panic! {:?}", x)));
    //REQUESTED_THREAD_ID.store(0, Relaxed);

    let framecount = *SOKU_FRAMECOUNT;

    let state_sub_count: &mut u32;
    let battle_state: &mut u32;
    let mut cur_speed: u32;
    let mut cur_speed_iter: u32;
    {
        let w = (*a).esi;
        cur_speed = (*a).ebx;
        cur_speed_iter = (*a).edi;

        let m = (w + 4 * 0x22) as *mut u32; //battle phase

        battle_state = &mut *m;
        state_sub_count = &mut *ptr_wrap!((w + 4) as *mut u32);
    }
    let gametype_main = *(0x898688 as *const usize);
    let is_netplay = *(0x8986a0 as *const usize) != 0;
    IS_FIRST_READ_INPUTS = true;

    match (gametype_main, is_netplay) {
        (2, false) => {
            if framecount > 0 {
                REQUESTED_THREAD_ID.store(GetCurrentThreadId(), Relaxed);
            }

            handle_replay(
                framecount,
                battle_state,
                &mut cur_speed,
                &mut cur_speed_iter,
                state_sub_count,
                &TAKEOVER_KEYS_SCHEME,
            )
        } //2 is replay
        (1, true) => {
            // 1 is netplay and v player
            // todo: detect v player
            if framecount > 0 {
                REQUESTED_THREAD_ID.store(GetCurrentThreadId(), Relaxed);
            } else {
                if let Some(fake_battle_manager) = FAKE_BATTLE_MANAGER_FOR_TSK.as_mut() {
                    fake_battle_manager.fake_left_win_count = 0;
                    fake_battle_manager.fake_right_win_count = 0;
                    fake_battle_manager.fake_battle_mode = *battle_state;
                }
            }

            if !GIRLSTALKED {
                handle_online(
                    framecount,
                    battle_state,
                    &mut cur_speed,
                    &mut cur_speed_iter,
                    state_sub_count,
                )
            }
        }
        _ => (),
    }

    let is_story_or_result_mode = matches!(*(0x00898690 as *const u32), 0 | 7);
    if !is_story_or_result_mode && matches!(*battle_state, 3 | 5) {
        // together with the no_ko_sound hook. Explanation in the no_ko_sound hook.
        //IS_KO = true;
        if *state_sub_count == 1 {
            std::mem::transmute::<usize, extern "stdcall" fn(u32)>(0x439490)(0x2c);
        }
    } else {
        //IS_KO = false;
    }

    let battle_manaer = (*a).esi as *const *const u8;
    if *battle_state == 5
        && NETCODER.is_some()
        && *state_sub_count as usize > 15  // ensure KO with confirmed battle result
        && let Some(fake_battle_manager) = FAKE_BATTLE_MANAGER_FOR_TSK.as_mut()
    {
        fake_battle_manager.fake_left_win_count = *(*battle_manaer.offset(3)).offset(0x573);
        fake_battle_manager.fake_right_win_count = *(*battle_manaer.offset(4)).offset(0x573);
        fake_battle_manager.fake_battle_mode = 5;
    }
    (*a).ebx = cur_speed;
    (*a).edi = cur_speed_iter;
}
