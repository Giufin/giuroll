#![feature(abi_thiscall)]
use core::panic;
use std::{
    collections::{BTreeMap, BTreeSet},
    ffi::c_void,
    os::windows::prelude::OsStringExt,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicI32, AtomicU32, Ordering::Relaxed},
        Mutex,
    },
    time::{Duration, Instant, SystemTime},
};
mod netcode;
mod rollback;

use ilhook::x86::{HookPoint, HookType};

//use libloadng::Library;
#[cfg(feature = "logtofile")]
use log::info;
use mininip::datas::{Identifier, Value};
use netcode::{Netcoder, NetworkPacket};
//use notify::{RecursiveMode, Watcher};
use rollback::{dump_frame, Frame, MemoryManip, Rollbacker};
use windows::{
    imp::{HeapAlloc, HeapFree, WaitForSingleObject},
    Win32::{
        Foundation::{HMODULE, HWND},
        System::{
            Console::AllocConsole,
            Memory::{VirtualProtect, PAGE_PROTECTION_FLAGS},
        },
    },
};

//mod netcode;
// +83E1
// +2836D
//004083dc actually
//00407a21 game thread created here

//#[cfg(debug_assertions)]
const ISDEBUG: bool = false;
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

//constexpr uintptr_t GAME_DRAW_NUMBER_FN = 0x00414940;
//reinterpret_cast<int(__thiscall *)(void* _this, int number, float x, float y, int a1, char a2)>(GAME_DRAW_NUMBER_FN)

//a 1 and a2 are 0 usually

/*#[no_mangle]
pub extern "cdecl"
fn init(_: usize) -> Option<()> {
    const BPATH: &'static str = r"C:\Users\Giuuu\Desktop\giuroll\giuroll\target\debug\fake";

    set_up_fern().ok()?;

    if ISDEBUG {
        info!("setting up logging")
    };

    std::fs::remove_dir_all(BPATH).ok()?;
    std::fs::create_dir(BPATH).ok()?;
    std::thread::spawn(|| {
        static PREVIOUS: Mutex<Option<(Library, String)>> = Mutex::new(None);

        static PID: AtomicI8 = AtomicI8::new(0);
        let mut watcher =
            notify::recommended_watcher(|x: Result<notify::Event, notify::Error>| unsafe {
                //pure chaos, due to the file locking "itself", will need to fix
                if let Ok(x) = x {
                    if !x.kind.is_modify() || x.kind.is_create() {
                        return;
                    }
                }

                if ISDEBUG {
                    info!("file change, reloading dll")
                };

                let nextidx = PID.load(Relaxed);
                PID.store(nextidx + 1, Relaxed);

                if let Some((lib, file)) = PREVIOUS.lock().unwrap().take() {
                    let func: libloading::Symbol<unsafe extern "C" fn() -> ()> =
                        lib.get(b"cleanup").unwrap();
                    func();
                    lib.close().unwrap();
                    let _ = std::fs::remove_dir(file);
                }

                let next = format!(
                    "{}{}{}.dll",
                    r"C:\Users\Giuuu\Desktop\giuroll\giuroll\target\debug\fake\giuroll",
                    nextidx,
                    std::process::id()
                );

                if let Err(_) = std::fs::copy(
                    r"C:\Users\Giuuu\Desktop\giuroll\giuroll\target\debug\true\giuroll.dll",
                    &next,
                ) {
                    info!("copy unsuccessfull");
                }

                let x = libloading::Library::new(&next);
                match x {
                    Ok(lib) => {
                        let func: libloading::Symbol<unsafe extern "C" fn() -> ()> =
                            lib.get(b"true_exec").unwrap();
                        func();
                        *PREVIOUS.lock().unwrap() = Some((lib, next));

                        if ISDEBUG {
                            info!("reload successfull")
                        };
                    }
                    Err(x) => {
                        if ISDEBUG {
                            info!("reload unsuccessfull{:?}", x);
                        };
                    }
                }
            })
            .unwrap();

        // Add a path to be watched. All files and directories at that path and
        // below will be monitored for changes.
        watcher
            .watch(
                Path::new(r"C:\Users\Giuuu\Desktop\giuroll\giuroll\target\debug\true"),
                RecursiveMode::NonRecursive,
            )
            .unwrap();

        std::mem::forget(watcher)
    });

    //let lib = libloading::Library::new(
    //    r"C:\Users\Giuuu\Desktop\giuroll\giuroll\target\debug\giuroll.dll",
    //)
    //.unwrap();
    //let func: libloading::Symbol<unsafe extern "C" fn() -> u32> = lib.get(b"my_func").unwrap();

    /* 0x407e30*/
    //std::mem::forget();
    Some(())
}
*/

use winapi::um::libloaderapi::GetModuleFileNameW;

static HOOK: Mutex<Option<Box<[HookPoint]>>> = Mutex::new(None);

#[no_mangle]
pub unsafe extern "cdecl" fn exeinit() {
    unsafe {
        //AllocConsole();
        //println!("here {:?}", std::env::current_dir());
    }
    truer_exec(Some(std::env::current_dir().unwrap()));
}

#[no_mangle]
pub extern "cdecl" fn Initialize(dllmodule: HMODULE) -> bool {
    let mut dat = [0u16; 1025];
    unsafe {
        GetModuleFileNameW(
            std::mem::transmute(dllmodule),
            &mut dat as *mut u16 as *mut u16,
            1024,
        )
    };

    let s = std::ffi::OsString::from_wide(&dat);

    //std::thread::sleep(Duration::from_millis(2000));
    //let m = init(0);
    let mut filepath = Path::new(&s).to_owned();
    filepath.pop();
    truer_exec(Some(filepath));
    true
}
//687040 true real input buffer manipulation
// 85b8ec some related varible, 487040
#[no_mangle]
pub extern "cdecl" fn CheckVersion(a: *const [u8; 16]) -> bool {
    const HASH110A: [u8; 16] = [
        0x26, 0x8a, 0xfd, 0x82, 0x76, 0x90, 0x3e, 0x16, 0x71, 0x6c, 0x14, 0x29, 0xc6, 0x95, 0x9c,
        0x9d,
    ];

    const HASH110: [u8; 16] = [
        0xdf, 0x35, 0xd1, 0xfb, 0xc7, 0xb5, 0x83, 0x31, 0x7a, 0xda, 0xbe, 0x8c, 0xd9, 0xf5, 0x3b,
        0x2e,
    ];
    unsafe { *a == HASH110 }
}

static mut REAL_INPUT: Option<[bool; 10]> = None;
static mut REAL_INPUT2: Option<[bool; 10]> = None;

static mut UPDATE: Option<SystemTime> = None;
static mut TARGET: Option<u128> = None;

static TARGET_OFFSET: AtomicI32 = AtomicI32::new(0);
//static TARGET_OFFSET_COUNT: AtomicI32 = AtomicI32::new(0);

static mut TITLE: &str = "Soku with giuroll 0.5.0 :YoumuSleep:\0";

unsafe extern "cdecl" fn skip(a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {}

static SOUND_MUTEX: Mutex<BTreeMap<usize, Vec<usize>>> = Mutex::new(BTreeMap::new());

// set this mutex at the start of each frame. after each rollback you can see which sounds are left in this mutex. these sounds can and should be pasued
static SOUND_DELET_MUTEX: Mutex<BTreeMap<usize, Vec<usize>>> = Mutex::new(BTreeMap::new());

static mut FORCE_SOUND_SKIP: bool = false;
//this is getting bad, fix the redundancy
static INPUTS_RAW: Mutex<BTreeMap<usize, [u16; 2]>> = Mutex::new(BTreeMap::new());

static mut SPECTATOR_LAST_FRAMECOUNT: u32 = 0;
static mut SPECTATOR_NEXT_INPUT: u16 = 0;

static mut SPIN_TIME_MICROSECOND: i128 = 0;

#[cfg(not(feature = "f62"))]
const VERSION_BYTE: u8 = 0x69;
#[cfg(feature = "f62")]
const VERSION_BYTE: u8 = 0x6a;

pub fn force_sound_skip(soundid: usize) {
    unsafe {
        let forcesound = std::mem::transmute::<usize, extern "stdcall" fn(u32)>(0x401d50);
        FORCE_SOUND_SKIP = true;

        forcesound(soundid as u32);

        FORCE_SOUND_SKIP = false;
    }
}

fn truer_exec(filename: Option<PathBuf>) {
    #[cfg(feature = "logtofile")]
    {
        set_up_fern().unwrap();
        info!("here");
    }

    unsafe {
        LAST_DELAY_VALUE = 1;
    }

    #[cfg(feature = "allocconsole")]
    unsafe {
        AllocConsole();
    }

    #[cfg(feature = "logtofile")]
    std::panic::set_hook(Box::new(|x| info!("panic! {:?}", x)));
    unsafe {
        let mut b = PAGE_PROTECTION_FLAGS(0);
        VirtualProtect(
            0x858b80 as *const c_void,
            1,
            PAGE_PROTECTION_FLAGS(0x40),
            &mut b,
        );

        *(0x858b80 as *mut u8) = VERSION_BYTE;
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

    unsafe {
        LAST_DELAY_VALUE = 3;
    }
    #[cfg(feature = "logtofile")]
    let _ = set_up_fern();
    if let Some(mut filepath) = filename {
        filepath.push("giuroll.ini");

        println!("{:?}", filepath);
        let conf = mininip::parse::parse_file(filepath).unwrap();
        //let keyboard_section = conf.section(Some("Keyboard")).unwrap();

        let inc = conf
            .get(&Identifier::new(
                Some("Keyboard".to_string()),
                "increase_delay_key".to_string(),
            ))
            .map(|x| match x {
                Value::Int(x) => *x,

                Value::Raw(x) | Value::Str(x) => {
                    i64::from_str_radix(x.strip_prefix("0x").unwrap(), 16).unwrap()
                }
                _ => todo!("non integer .ini entry"),
            })
            .unwrap_or(0);

        let dec = conf
            .get(&Identifier::new(
                Some("Keyboard".to_string()),
                "decrease_delay_key".to_string(),
            ))
            .map(|x| match x {
                Value::Int(x) => *x,
                Value::Raw(x) | Value::Str(x) => {
                    i64::from_str_radix(x.strip_prefix("0x").unwrap(), 16).unwrap()
                }
                _ => todo!("non integer .ini entry"),
            })
            .unwrap_or(0);

        let net = conf
            .get(&Identifier::new(
                Some("Keyboard".to_string()),
                "toggle_network_stats".to_string(),
            ))
            .map(|x| match x {
                Value::Int(x) => *x,
                Value::Raw(x) | Value::Str(x) => {
                    i64::from_str_radix(x.strip_prefix("0x").unwrap(), 16).unwrap()
                }
                _ => todo!("non integer .ini entry"),
            })
            .unwrap_or(0);

        let spin = conf
            .get(&Identifier::new(
                Some("FramerateFix".to_string()),
                "spin_amount".to_string(),
            ))
            .map(|x| match x {
                Value::Int(x) => *x,
                Value::Raw(x) | Value::Str(x) => {
                    i64::from_str_radix(x.strip_prefix("0x").unwrap(), 16).unwrap()
                }
                _ => todo!("non integer .ini entry"),
            })
            .unwrap_or(1500);

        let network_menu = conf
            .get(&Identifier::new(
                Some("Misc".to_string()),
                "enable_network_stats_by_default".to_string(),
            ))
            .map(|x| match x {
                Value::Bool(x) => *x,
                _ => todo!("non bool .ini entry"),
            })
            .unwrap_or(false);

        let mut default_delay = conf
            .get(&Identifier::new(
                Some("Misc".to_string()),
                "default_delay".to_string(),
            ))
            .map(|x| match x {
                Value::Int(x) => *x,
                Value::Raw(x) | Value::Str(x) => {
                    i64::from_str_radix(x.strip_prefix("0x").unwrap(), 16).unwrap()
                }
                _ => todo!("non integer .ini entry"),
            })
            .unwrap_or(2)
            .clamp(1, 9);

        let mut title = conf
            .get(&Identifier::new(
                Some("Misc".to_string()),
                "game_title".to_string(),
            ))
            .map(|x| match x {
                Value::Str(x) => x.clone(),
                _ => todo!("non string .ini entry"),
            })
            .unwrap_or("Soku with giuroll 0.5.0 :YoumuSleep:".to_string());

        title.push('\0');

        let tleak = Box::leak(Box::new(title));

        unsafe {
            TITLE = tleak.as_mut_str();
            SPIN_TIME_MICROSECOND = spin as i128;
            INCREASE_DELAY_KEY = inc as u8;
            DECREASE_DELAY_KEY = dec as u8;
            TOGGLE_STAT_KEY = net as u8;
            TOGGLE_STAT = network_menu;
            LAST_DELAY_VALUE = default_delay as usize;
        }
    } else {
        todo!()
    }

    //meiling d236 desync fix, original by PinkySmile, Slen, cc/delthas, Fear Nagae, PC_Volt
    unsafe {
        let mut previous = PAGE_PROTECTION_FLAGS(0);
        VirtualProtect(
            0x724316 as *const c_void,
            4,
            PAGE_PROTECTION_FLAGS(0x40),
            &mut previous,
        );
        *(0x724316 as *mut u8) = 0x66;
        *(0x724317 as *mut u8) = 0xB9;
        *(0x724318 as *mut u8) = 0x0F;
        *(0x724319 as *mut u8) = 0x00;
        VirtualProtect(0x724316 as *const c_void, 4, previous, &mut previous);
    }

    // 9 digit font fix, by ichirin
    unsafe {
        for a in [0x43DC7D, 0x882954] {
            let mut previous = PAGE_PROTECTION_FLAGS(0);
            VirtualProtect(
                a as *const c_void,
                1,
                PAGE_PROTECTION_FLAGS(0x40),
                &mut previous,
            );
            *(a as *mut u8) = 0x0A;

            VirtualProtect(a as *const c_void, 1, previous, &mut previous);
        }
    }

    let new = unsafe { ilhook::x86::Hooker::new(0x482701, HookType::JmpBack(goodhook), 0).hook(6) };
    std::mem::forget(new);

    let new =
        unsafe { ilhook::x86::Hooker::new(0x482745, HookType::JmpBack(frameexithook), 0).hook(6) };
    std::mem::forget(new);

    //0x899d60 maybe sound manager?
    unsafe extern "cdecl" fn handle_sound_real(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) -> usize {
        //let sw = REQUESTED_THREAD_ID.swap(0, Relaxed);
        (*a).ecx = 0x89f9f8;
        (*a).eax = *(((*a).esp + 4) as *const u32);
        let soundid = (*a).eax as usize;

        if !BATTLE_STARTED {
            return if soundid == 0 { 0x401db7 } else { 0x401d58 };
        }

        let mut w = SOUND_MUTEX.lock().unwrap();
        let w2 = &mut *w;
        let items = w2.entry(*SOKU_FRAMECOUNT).or_insert_with(|| Vec::new());

        let ret = if soundid == 0 || items.iter().find(|x| **x == soundid).is_some() {
            0x401db7
        } else {
            let old = SOUND_DELET_MUTEX.lock().unwrap();
            items.push(soundid);
            if let Some(vec) = old.get(&*SOKU_FRAMECOUNT) {
                if vec.iter().find(|x| **x == soundid).is_some() {
                    // sound already playing
                    0x401db7
                } else {
                    0x401d58
                }
            } else {
                0x401d58
            }
        };
        //REQUESTED_THREAD_ID.swap(sw, Relaxed);
        ret
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

            let eax = *(((*a).esi + 4) as *const u32);
            let ecx = *(eax as *const u32);
            let fun = *((ecx + 0x48) as *const u32);
            let true_fun = std::mem::transmute::<usize, extern "thiscall" fn(u32, u32 /* , u32*/)>(
                fun as usize,
            );

            true_fun(ecx, eax /*, *(((*a).esp + 0x8)  as *const u32)*/);

            0x401db6
        } else {
            //replicate the usual logic

            if ((*(((*a).esp + 8) as *const usize)) & 1) == 0 {
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

    unsafe extern "cdecl" fn onexit(a: *mut ilhook::x86::Registers, _b: usize) {
        REQUESTED_THREAD_ID.store(0, Relaxed);
        NEXT_DRAW_PING = None;

        *(0x8971C0 as *mut usize) = 0; // reset wether to prevent desyncs
        ESC = 0;
        BATTLE_STARTED = false;
        DISABLE_SEND.store(0, Relaxed);
        LAST_STATE.store(0, Relaxed);

        SOUND_MUTEX.lock().unwrap().clear();
        SOUND_DELET_MUTEX.lock().unwrap().clear();

        INPUTS_RAW.lock().unwrap().clear();
        let heap = unsafe { *(0x89b404 as *const isize) };

        // we should be removing allocations that happen during frames which were rolled back, but that somehow breaks it, possibly because of some null check initializations
        //let allocset = std::mem::replace(&mut *ALLOCMUTEX.lock().unwrap(), BTreeSet::new());
        //let freeset = std::mem::replace(&mut *FREEMUTEX.lock().unwrap(), BTreeSet::new());
        //
        //for a in allocset.difference(&freeset) {
        //    //    unsafe { HeapFree(heap, 0, *a as *const c_void) };
        //    println!("freed but not alloced: {}", a);
        //}

        if let Some(x) = NETCODER.take() {
            DATA_RECEIVER = Some(x.receiver);
        }

        // it cannot be used by any different thread now
        if let Some(x) = ROLLBACKER.take() {
            for a in x.guessed {
                a.prev_state.did_happen();
            }
        }

        for a in std::mem::replace(&mut *FRAMES.lock().unwrap(), vec![]) {
            a.did_happen();
        }

        GIRLSTALKED = false;
    }

    let new = unsafe { ilhook::x86::Hooker::new(0x481960, HookType::JmpBack(onexit), 0).hook(6) };
    std::mem::forget(new);

    unsafe extern "cdecl" fn onexitexit(a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {
        let f = std::mem::transmute::<usize, extern "fastcall" fn(u32)>((*a).edx as usize);

        f((*a).ecx);
        let allocset = std::mem::replace(&mut *ALLOCMUTEX.lock().unwrap(), BTreeSet::new());
        let freeset = std::mem::replace(&mut *FREEMUTEX.lock().unwrap(), BTreeSet::new());

        for a in allocset.difference(&freeset) {
            //    unsafe { HeapFree(heap, 0, *a as *const c_void) };
            println!("alloced but not freed: {}", a);
        }
        return;
        let allocset = std::mem::replace(&mut *ALLOCMUTEX.lock().unwrap(), BTreeSet::new());
        let freeset = std::mem::replace(&mut *FREEMUTEX.lock().unwrap(), BTreeSet::new());

        for a in &freeset {
            //    unsafe { HeapFree(heap, 0, *a as *const c_void) };
            println!("since then freed: {}", a);
        }
    }

    let return_insert_addr = 0x48196f + 5;
    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x48196f,
            HookType::JmpToAddr(return_insert_addr, 0, onexitexit),
            0,
        )
        .hook(5)
    };

    std::mem::forget(new);

    let funnyaddr = return_insert_addr;
    let mut b = PAGE_PROTECTION_FLAGS(0);
    unsafe {
        VirtualProtect(
            funnyaddr as *const c_void,
            1,
            PAGE_PROTECTION_FLAGS(0x40),
            &mut b as *mut PAGE_PROTECTION_FLAGS,
        );

        *(funnyaddr as *mut u8) = 0xc3;
    }

    unsafe extern "cdecl" fn spectator_skip(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) -> usize {
        let framecount_cur = *(((*a).esi + 0x4c) as *const u32);
        let edi = (*a).edi;
        SPECTATOR_LAST_FRAMECOUNT = edi;
        //println!("edi: {}, framecount: {}", edi, framecount_cur);
        let no_skip = edi + 16 < framecount_cur && BATTLE_STARTED;
        if no_skip {
            /*
            LAB_0042daa6                                    XREF[1]:     0042daa0(j)
            0042daa6 8b 5e 48        MOV        EBX,dword ptr [ESI + 0x48]
            0042daa9 8b 4e 4c        MOV        ECX,dword ptr [ESI + 0x4c]
            */

            (*a).ebx = *(((*a).esi + 0x48) as *const u32);
            (*a).ecx = framecount_cur;

            0x42daac
        } else {
            //println!("here 3");
            /*
            0042db1d 8b 5c 24 1c     MOV        EBX,dword ptr [ESP + local_10]
             */
            (*a).ebx = *(((*a).esp + 0x1c) as *const u32);
            0x42db21
        }
    }

    // changes the spectator logic to only send frame if there are at least 10 frames in the buffer. this prevent spectator from desyncing
    let new = unsafe {
        ilhook::x86::Hooker::new(0x42daa6, HookType::JmpToRet(spectator_skip), 0).hook(6)
    };
    std::mem::forget(new);

    unsafe extern "cdecl" fn drawnumbers(_a: *mut ilhook::x86::Registers, _b: usize) {
        if let Some(x) = NEXT_DRAW_PING {
            draw_num((320.0, 466.0), x);
        }
    }

    let new =
        unsafe { ilhook::x86::Hooker::new(0x43e320, HookType::JmpBack(drawnumbers), 0).hook(7) };
    std::mem::forget(new);

    unsafe extern "cdecl" fn ongirlstalk(_a: *mut ilhook::x86::Registers, _b: usize) {
        GIRLSTALKED = true;
        BATTLE_STARTED = false;
    }

    let new =
        unsafe { ilhook::x86::Hooker::new(0x482960, HookType::JmpBack(ongirlstalk), 0).hook(5) };
    std::mem::forget(new);

    for a in [0x821730, 0x821759, 0x82251e, 0x82f09e, 0x82f18c] {
        let new = unsafe {
            ilhook::x86::Hooker::new(a, HookType::JmpToAddr(a + 6, 0xc, heap_free_override), 0)
                .hook(6)
        };
        std::mem::forget(new);
    }

    for b in [0x821704, 0x823397, 0x82ed84, 0x82f15b, 0x8230e6] {
        let new = unsafe {
            ilhook::x86::Hooker::new(b, HookType::JmpToAddr(b + 6, 0xc, heap_alloc_override), 0)
                .hook(6)
        };
        std::mem::forget(new);
    }

    let s = 0x822499; //0x822465;
    let new =
        unsafe { ilhook::x86::Hooker::new(s, HookType::JmpBack(heap_alloc_esi_result), 0).hook(6) };
    std::mem::forget(new);

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

    let new = unsafe { ilhook::x86::Hooker::new(0x482696, HookType::JmpBack(apause), 0).hook(6) };
    std::mem::forget(new);

    // 0x428358 calls function checking if there is a next frame in net object

    let new =
        unsafe { ilhook::x86::Hooker::new(0x41daea, HookType::JmpBack(readonlinedata), 0).hook(5) };
    std::mem::forget(new);

    //prevents a check for whether there is a frame from the opponent to allow for smooth rollback
    unsafe extern "cdecl" fn set_eax_to_0(a: *mut ilhook::x86::Registers, _b: usize) {
        (*a).eax = *(0x8a0040 as *const u32);
    }

    let new =
        unsafe { ilhook::x86::Hooker::new(0x407f1b, HookType::JmpBack(set_eax_to_0), 0).hook(6) };
    std::mem::forget(new);

    unsafe extern "cdecl" fn handle_raw_input(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) {
        //        0046c902 8b ae 6c        MOV        EBP,dword ptr [ESI + 0x76c]

        (*a).ebp = *(((*a).esi + 0x76c) as *const u32);
        let input_manager = (*a).ecx;

        let real_input = match std::mem::replace(&mut REAL_INPUT, REAL_INPUT2.take()) {
            Some(x) => x,
            None => {
                let f = std::mem::transmute::<usize, extern "fastcall" fn(u32)>(0x040a370);
                (f)(input_manager);
                return;
            }
        };

        {
            let td = &mut *((input_manager + 0x38) as *mut i32);
            let lr = &mut *((input_manager + 0x3c) as *mut i32);

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
            let v = &mut *((input_manager + 0x40 + a * 4) as *mut u32);

            if real_input[a as usize + 4] {
                *v += 1;
            } else {
                *v = 0;
            }
        }

        let m = &mut *((input_manager + 0x62) as *mut u16);
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
        if ESC > 60 {
            ESC = 0;
            0x428393
        } else {
            //let skip = DISABLE_SEND.load(Relaxed) != 0;
            //DISABLE_SEND.store(1, Relaxed);

            let skip = true;

            if skip {
                0x428360
            } else {
                (*a).ecx = *(((*a).edi + 0x8) as *const u32);
                (*a).eax = *(((*a).ecx) as *const u32);
                0x428335
            }
        }
    }

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
        if ESC > 60 {
            ESC = 0;
            0x4286c3
        } else {
            //let skip = DISABLE_SEND.load(Relaxed) != 0;
            //DISABLE_SEND.store(1, Relaxed);

            let skip = true;

            if skip {
                0x428630
            } else {
                (*a).ecx = *(((*a).edi + 0x8) as *const u32);
                (*a).eax = *(((*a).ecx) as *const u32);
                0x428605
            }
        }
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(0x428600, HookType::JmpToRet(skiponceclient), 0).hook(5)
    };
    std::mem::forget(new);

    let new = unsafe {
        ilhook::x86::Hooker::new(0x428644, HookType::JmpToAddr(0x42864f, 0, skip), 0).hook(5)
    };
    std::mem::forget(new);

    unsafe extern "cdecl" fn timing_loop(a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {
        #[cfg(feature = "f62")]
        const TARGET_FRAMETIME: i32 = 1_000_000 / 62;
        #[cfg(not(feature = "f62"))]
        const TARGET_FRAMETIME: i32 = 1_000_000 / 60;

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

        let s = TARGET_OFFSET.swap(0, Relaxed);
        //TARGET_OFFSET.fetch_add(s / 2, Relaxed);
        *target += (TARGET_FRAMETIME + s).max(1005) as u128;

        let cur = m.elapsed().unwrap().as_micros();

        let diff = (*target as i128 + 1000) - cur as i128 - SPIN_TIME_MICROSECOND;
        //if diff > 1_000_000 {
        //    panic!("big diff {diff}");
        //}

        //info!("frame diff micro diff: {}", diff);
        let ddiff = (diff / 1000) as i32;
        if ddiff < 0 {
            #[cfg(feature = "logtofile")]
            info!("frameskip {diff}");
            *target = cur + (TARGET_FRAMETIME) as u128;
        } else {
            WaitForSingleObject(waithandle as isize, ddiff as u32);
            if SPIN_TIME_MICROSECOND != 0 {
                while m.elapsed().unwrap().as_micros() < *target {}
            }
        };
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(0x4192f0, HookType::JmpToAddr(0x4193d7, 0, timing_loop), 0).hook(6)
    };
    std::mem::forget(new);
    //hook.push(new);

    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x46c900,
            HookType::JmpToAddr(0x46c908, 0, handle_raw_input),
            0,
        )
        .hook(8)
    };
    std::mem::forget(new);
    //hook.push(new);

    //*HOOK.lock().unwrap() = Some(hook.into_boxed_slice());

    unsafe {
        std::thread::spawn(|| {
            //wait to avoid being overwritten by th123e
            std::thread::sleep(Duration::from_millis(3000));

            let mut whatever = PAGE_PROTECTION_FLAGS(0);
            VirtualProtect(
                0x89ffbe as *const c_void,
                1,
                PAGE_PROTECTION_FLAGS(0x40),
                &mut whatever,
            );

            windows::Win32::UI::WindowsAndMessaging::SetWindowTextA(
                *(0x89ff90 as *const HWND),
                windows::core::PCSTR::from_raw(TITLE.as_ptr()),
            )
        })
    };
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
static mut NEXT_DRAW_PACKET_LOSS: Option<i32> = None;
static mut NEXT_DRAW_PACKET_DESYNC: Option<i32> = None;

const SOKU_FRAMECOUNT: *mut usize = 0x8985d8 as *mut usize;
use windows::Win32::System::Threading::GetCurrentThreadId;

static FREEMUTEX: Mutex<BTreeSet<usize>> = Mutex::new(BTreeSet::new());

unsafe extern "cdecl" fn heap_free_override(_a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {
    (*_a).eax = 1;
    let s = ((*_a).esp as usize + 2 * 4) as *mut usize;
    let flags = ((*_a).esp as usize + 1 * 4) as *mut usize;
    let heap = ((*_a).esp as usize + 0 * 4) as *mut usize;

    //if let Some(x) = MEMORYMUTEX.lock().unwrap().remove(&*s) {
    //    if x != *SOKU_FRAMECOUNT {
    //        println!("freeing memory allocated at frame: {}, current: {}", x, *SOKU_FRAMECOUNT)
    //    }
    //}

    //if GetCurrentThreadId() == REQUESTED_THREAD_ID.load(Relaxed) {
    if
    /* !matches!(*(0x8a0040 as *const u8), 0x5 | 0xe | 0xd) || */
    *(0x89b404 as *const usize) != *heap
        || GetCurrentThreadId() != REQUESTED_THREAD_ID.load(Relaxed)
        || *SOKU_FRAMECOUNT == 0
    {
        HeapFree((*heap) as isize, *flags as u32, *s as *const c_void);
        return;
    }

    //if {
    //    println!("missmatched scene id")
    //} // never happens

    //FREEMUTEX.lock().unwrap().insert(*s);
    //return;

    unsafe {
        MEMORY_SENDER_FREE
            .as_ref()
            .unwrap()
            .clone()
            .send(*s)
            .unwrap()
    };

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

static ALLOCMUTEX: Mutex<BTreeSet<usize>> = Mutex::new(BTreeSet::new());
static MEMORYMUTEX: Mutex<BTreeMap<usize, usize>> = Mutex::new(BTreeMap::new());

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

unsafe extern "cdecl" fn heap_alloc_override(a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {
    let (s, flags, heap) = unsafe {
        (
            *(((*a).esp as usize + 2 * 4) as *mut usize),
            *(((*a).esp as usize + 1 * 4) as *mut u32),
            *(((*a).esp as usize + 0 * 4) as *mut isize),
        )
    };

    (*a).eax = HeapAlloc(heap, flags, s) as u32;

    if *(0x89b404 as *const usize) != heap as usize
        /*|| !matches!(*(0x8a0040 as *const u8), 0x5 | 0xe | 0xd)*/
        || *SOKU_FRAMECOUNT == 0 ||
        GetCurrentThreadId() != REQUESTED_THREAD_ID.load(Relaxed)
    {
        //println!("wrong heap alloc");
    } else {
        store_alloc((*a).eax as usize);
    }
}

unsafe extern "cdecl" fn heap_alloc_esi_result(a: *mut ilhook::x86::Registers, _b: usize) {
    if GetCurrentThreadId() == REQUESTED_THREAD_ID.load(Relaxed)
        /* && matches!(*(0x8a0040 as *const u8), 0x5 | 0xe | 0xd) */
        && *SOKU_FRAMECOUNT != 0
    {
        store_alloc((*a).esi as usize);
    }
}

#[allow(unused)]
unsafe extern "cdecl" fn reallochook(a: *mut ilhook::x86::Registers, _b: usize) {}

use core::sync::atomic::AtomicU8;

static LAST_STATE: AtomicU8 = AtomicU8::new(0x69);

fn pause(battle_state: &mut u32, state_sub_count: &mut u32) {
    if *battle_state != 4 {
        LAST_STATE.store(*battle_state as u8, Relaxed);
        *state_sub_count = state_sub_count.wrapping_sub(1);
        *battle_state = 4;
    }
}
fn resume(battle_state: &mut u32) {
    // should be called every frame because fo the logic set in fn pause involving state_sub_count
    let last = LAST_STATE.load(Relaxed);
    if last != 0x69 && *battle_state == 4 {
        //4 check to not accidentally override a state set by the game *maybe*
        *battle_state = last as u32;
        LAST_STATE.store(0x69, Relaxed)
    }
}
//        info!("GAMETYPE TRUE {}", *(0x89868c as *const usize));
static PAUSESTATE: AtomicU8 = AtomicU8::new(0);

unsafe extern "cdecl" fn apause(_a: *mut ilhook::x86::Registers, _b: usize) {
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

unsafe extern "cdecl" fn readonlinedata(a: *mut ilhook::x86::Registers, _b: usize) {
    let esp = (*a).esp;

    let packet_pointer = esp + 0x70;
    let slic = std::slice::from_raw_parts_mut(packet_pointer as *mut u8, 400);
    let type1 = slic[0];
    let type2 = slic[1];

    //   let frame_count = usize::from_le_bytes(slic[2..6].try_into().unwrap());
    let sceneid = slic[6];
    //   let somethingweird = slic[7];
    //   let input1 = slic[8];
    //   let input2 = slic[9];

    if type1 == 0x69 {
        let z = NetworkPacket::decode(slic);
        if DISABLE_SEND.load(Relaxed) == 0 {
            DISABLE_SEND.store(1, Relaxed);

            let is_p1 = unsafe {
                let netmanager = *(0x8986a0 as *const usize);
                *(netmanager as *const usize) == 0x858cac
            };
            //the packet you receive first frame, every round. We are making it manually, to prevent data loss from freezing the game
            if !is_p1 {
                slic.copy_from_slice(&[
                    13, 3, 1, 0, 0, 0, 5, 2, 0, 0, 0, 0, 12, 0, 103, 0, 103, 0, 103, 0, 103, 0,
                    104, 0, 104, 0, 104, 0, 104, 0, 106, 0, 106, 0, 200, 0, 203, 0, 208, 0, 208, 0,
                    208, 0, 208, 0, 1, 15, 0, 0, 0, 142, 15, 14, 252, 36, 143, 52, 108, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                ])
            } else {
                slic.copy_from_slice(&[
                    14, 3, 0, 0, 0, 0, 5, 1, 0, 0, 20, 100, 0, 100, 0, 101, 0, 101, 0, 102, 0, 102,
                    0, 103, 0, 103, 0, 200, 0, 200, 0, 200, 0, 200, 0, 201, 0, 201, 0, 201, 0, 201,
                    0, 203, 0, 203, 0, 203, 0, 203, 0, 1, 69, 119, 176, 189, 128, 118, 60, 0, 0,
                    18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0,
                ])
            }
        } else {
            (*a).eax = 0x400;
        }

        DATA_SENDER
            .as_ref()
            .unwrap()
            .send((z, Instant::now()))
            .unwrap();
    }
    if (type1 == 14 || type1 == 13) && type2 == 3 && sceneid == 0x5 {
        let is_p1 = unsafe {
            let netmanager = *(0x8986a0 as *const usize);
            *(netmanager as *const usize) == 0x858cac
        };

        println!(
            "received {} {}, data: {:?} as {}",
            type1, type2, slic, is_p1
        );
    }

    if (type1 == 14 || type1 == 13) && type2 == 1 && BATTLE_STARTED {
        //opponent has esced (probably) exit, the 60 is to avoid stray packets causing exits

        ESC += 1;
        if ESC > 60 {
            BATTLE_STARTED = false;
        }

        //info!("received {} {} {}", type1, type2, sceneid);
    }

    if type1 == 5 {
        let is_spect = slic[25] == 0;

        //println!("slic26: {:?}", &);
        if is_spect {
            slic[1] = VERSION_BYTE;
        }
        //is_spect = slic[]
        //let gamever = slic[1..17];
    }
    //BATTLE_STARTED
}

static mut GIRLSTALKED: bool = false;
static DISABLE_SEND: AtomicU8 = AtomicU8::new(0);

static FRAMES: Mutex<Vec<Frame>> = Mutex::new(Vec::new());

//todo: improve rewind mechanism
static IS_REWINDING: AtomicU8 = AtomicU8::new(0);

unsafe fn handle_replay(
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

    resume(battle_state);
    if PAUSESTATE.load(Relaxed) != 0 {
        pause(battle_state, weird_counter);
        return;
    }

    if *cur_speed_iter == 0 {
        //"true" frame

        IS_REWINDING.store(0, Relaxed);
        let qdown = read_key_better(0x10);

        if qdown {
            let target = (framecount as u32).saturating_sub(*cur_speed) - 1;
            #[cfg(feature = "logtofile")]
            if ISDEBUG {
                info!("qdown {}", target)
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
                        #[cfg(feature = "logtofile")]
                        if ISDEBUG {
                            info!("diff: {}", diff)
                        };

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
                    #[cfg(feature = "logtofile")]
                    if ISDEBUG {
                        info!("missing frame")
                    };
                    return;
                }
            }
            #[cfg(feature = "logtofile")]
            if ISDEBUG {
                info!(" restore complete success")
            };
        }
    }

    let framecount = *SOKU_FRAMECOUNT;

    if framecount % 16 == 1 || IS_REWINDING.load(Relaxed) == 1 {
        #[cfg(feature = "logtofile")]
        if ISDEBUG {
            info!("framecount: {}", framecount)
        };

        let frame = dump_frame();

        let mut mutex = FRAMES.lock().unwrap();

        mutex.push(frame);

        #[cfg(feature = "logtofile")]
        if ISDEBUG {
            info!("frame successfull")
        };
    }
}

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
    //return ([false; 10], 0);
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
            let axis1 = *(controler as *const i32);
            let axis2 = *((controler + 4) as *const i32);

            input[2] = axis1 < -500;
            input[3] = axis1 > 500;

            input[0] = axis2 < -500;
            input[1] = axis2 > 500;

            for a in 0..6 {
                let key = *((local_input_manager + 0x18 + a * 0x4) as *const i32);

                if key > -1 {
                    input[a + 4] = *((key as u32 + 0x30 + controler) as *const u8) != 0;
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

static mut LAST_DELAY_MANIP: u8 = 0; // 0 neither, 1 up, 2 down, 3 both

static mut BATTLE_STARTED: bool = false;
static mut ESC: u8 = 0; // maybe shouldn't be not atomic

static mut INCREASE_DELAY_KEY: u8 = 0;
static mut DECREASE_DELAY_KEY: u8 = 0;

static mut TOGGLE_STAT_KEY: u8 = 0;

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

unsafe fn handle_online(
    framecount: usize,
    battle_state: &mut u32,
    cur_speed: &mut u32,
    cur_speed_iter: &mut u32,
    state_sub_count: &mut u32,
) {
    if framecount == 0 {
        BATTLE_STARTED = true;
        let m = DATA_RECEIVER.take().unwrap();

        let rollbacker = Rollbacker::new();

        ROLLBACKER = Some(rollbacker);
        let mut netcoder = Netcoder::new(m);
        netcoder.delay = LAST_DELAY_VALUE;
        netcoder.max_rollback = 6;
        netcoder.display_stats = TOGGLE_STAT;
        NETCODER = Some(netcoder);

        return;
    }

    let rollbacker = ROLLBACKER.as_mut().unwrap();
    let netcoder = NETCODER.as_mut().unwrap();

    resume(battle_state);

    let stat_toggle = read_key_better(TOGGLE_STAT_KEY);
    if stat_toggle && !LAST_TOGGLE {
        TOGGLE_STAT = !TOGGLE_STAT;
        netcoder.display_stats = TOGGLE_STAT;
    }

    LAST_TOGGLE = stat_toggle;

    let k_up = read_key_better(INCREASE_DELAY_KEY);
    let k_down = read_key_better(DECREASE_DELAY_KEY);

    let last_up = LAST_DELAY_MANIP & 1 == 1;
    let last_down = LAST_DELAY_MANIP & 2 == 2;

    if !last_up && k_up {
        if netcoder.delay < 9 {
            // values skewed by 1 because of the my_frame-frame_id offset
            netcoder.delay += 1;
        }
    }

    if !last_down && k_down {
        if netcoder.delay > 1 {
            netcoder.delay -= 1;
        }
    }

    LAST_DELAY_VALUE = netcoder.delay;
    LAST_DELAY_MANIP = k_up as u8 + k_down as u8 * 2;

    if *cur_speed_iter == 0 {
        let input = read_current_input();
        let speed = netcoder.process_and_send(rollbacker, input);
        *cur_speed = speed;

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

unsafe extern "cdecl" fn frameexithook(a: *mut ilhook::x86::Registers, _b: usize) {
    //REQUESTED_THREAD_ID.store(0, Relaxed);
}

unsafe extern "cdecl" fn goodhook(a: *mut ilhook::x86::Registers, _b: usize) {
    //println!("GAMETYPE TRUE {}", *(0x8986a0 as *const usize));
    #[cfg(feature = "logtofile")]
    std::panic::set_hook(Box::new(|x| info!("panic! {:?}", x)));
    //REQUESTED_THREAD_ID.store(0, Relaxed);

    let framecount = *SOKU_FRAMECOUNT;

    read_current_input();

    let state_sub_count: &mut u32;
    let battle_state: &mut u32;
    let cur_speed: &mut u32;
    let cur_speed_iter: &mut u32;
    {
        let w = (*a).esi;
        cur_speed = &mut (*a).ebx;
        cur_speed_iter = &mut (*a).edi;

        let m = (w + 4 * 0x22) as *mut u32; //battle phase

        battle_state = &mut *m;
        state_sub_count = &mut *((w + 4) as *mut u32);
    }
    let gametype_main = *(0x898688 as *const usize);
    let is_netplay = *(0x8986a0 as *const usize) != 0;

    //println!("{:?}", (gametype_main, is_netplay));

    match (gametype_main, is_netplay) {
        (2, false) => {
            if framecount > 5 {
                REQUESTED_THREAD_ID.store(GetCurrentThreadId(), Relaxed);
            }

            handle_replay(
                framecount,
                battle_state,
                cur_speed,
                cur_speed_iter,
                state_sub_count,
            )
        } //2 is replay
        (1, true) => {
            // 1 is netplay and v player
            // todo: detect v player
            if framecount > 5 {
                REQUESTED_THREAD_ID.store(GetCurrentThreadId(), Relaxed);
            }

            if !GIRLSTALKED {
                handle_online(
                    framecount,
                    battle_state,
                    cur_speed,
                    cur_speed_iter,
                    state_sub_count,
                )
            }
        }
        _ => (),
    }
}
