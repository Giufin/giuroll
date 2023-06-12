#![feature(abi_thiscall)]
use core::panic;
use std::{
    collections::{BTreeMap, HashMap},
    ffi::c_void,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicI32, AtomicI8, AtomicU32, Ordering::Relaxed},
        Mutex,
    },
    time::{Duration, Instant, SystemTime},
};
mod netcode;
mod rollback;

use ilhook::x86::{HookFlags, HookPoint, HookType};

//use libloadng::Library;
#[cfg(any(NO))]
use log::info;
use mininip::datas::{Identifier, Value};
use netcode::{Netcoder, NetworkPacket};
//use notify::{RecursiveMode, Watcher};
use rollback::{dump_frame, Frame, MemoryManip, Rollbacker};
use windows::{
    imp::{HeapFree, WaitForSingleObject},
    Win32::{
        Foundation::{HMODULE, HWND},
        System::{
            Console::AllocConsole,
            Memory::{VirtualProtect, PAGE_PROTECTION_FLAGS},
        },
    },
};

//mod netcode;
//+83E1
// +2836D
//004083dc actually
//00407a21 game thread created here!

//#[cfg(debug_assertions)]
const ISDEBUG: bool = false;
//#[cfg(not(debug_assertions))]
//const ISDEBUG: bool = false;
#[cfg(any(NO))]
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

use winapi::um::libloaderapi::GetModuleFileNameA;

static HOOK: Mutex<Option<Box<[HookPoint]>>> = Mutex::new(None);
#[no_mangle]
pub extern "cdecl" fn Initialize(dllmodule: HMODULE) -> bool {
    let mut dat = [0u8; 1025];
    unsafe {
        GetModuleFileNameA(
            std::mem::transmute(dllmodule),
            &mut dat as *mut u8 as *mut i8,
            1024,
        )
    };

    let s = std::str::from_utf8(&dat).unwrap();

    //std::thread::sleep(Duration::from_millis(2000));
    //let m = init(0);
    truer_exec(Some(Path::new(s).to_owned()));
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

#[no_mangle]
pub extern "cdecl" fn true_exec() {
    truer_exec(None);
}

static mut REAL_INPUT: Option<[bool; 10]> = None;
static mut REAL_INPUT2: Option<[bool; 10]> = None;

static mut UPDATE: Option<SystemTime> = None;
static mut TARGET: Option<u128> = None;

static TARGET_OFFSET: AtomicI32 = AtomicI32::new(0);
//static TARGET_OFFSET_COUNT: AtomicI32 = AtomicI32::new(0);

static TITLE: &str = "Soku with giuroll whatever :YoumuSleep:\0";

unsafe extern "cdecl" fn skipbuffer(a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {}

static SOUND_MUTEX: Mutex<BTreeMap<usize, Vec<usize>>> = Mutex::new(BTreeMap::new());

// set this mutex at the start of each frame. after each rollback you can see which sounds are left in this mutex. these sounds can and should be pasued
static SOUND_DELET_MUTEX: Mutex<BTreeMap<usize, Vec<usize>>> = Mutex::new(BTreeMap::new());

static mut FORCE_SOUND_SKIP: bool = false;

pub fn force_sound_skip(soundid: usize) {
    unsafe {
        let forcesound = std::mem::transmute::<usize, extern "stdcall" fn(u32)>(0x401d50);
        FORCE_SOUND_SKIP = true;

        forcesound(soundid as u32);

        FORCE_SOUND_SKIP = false;
    }
}

fn truer_exec(filename: Option<PathBuf>) {
    unsafe {
        LAST_DELAY_VALUE = 1;
    }

    unsafe {
        AllocConsole();
    }

    println!("here");

    #[cfg(any(NO))]
    std::panic::set_hook(Box::new(|x| info!("panic! {:?}", x)));
    unsafe {
        let mut b = PAGE_PROTECTION_FLAGS(0);
        VirtualProtect(
            0x858b80 as *const c_void,
            1,
            PAGE_PROTECTION_FLAGS(0x40),
            &mut b,
        );

        *(0x858b80 as *mut u8) = 0x69;
    }

    unsafe {
        let (s, r) = std::sync::mpsc::channel();
        DATA_RECEIVER = Some(r);
        DATA_SENDER = Some(s);

        let (s, r) = std::sync::mpsc::channel();
        MEMORY_RECEIVER = Some(r);
        MEMORY_SENDER = Some(s);
    }

    unsafe {
        LAST_DELAY_VALUE = 3;
    }
    #[cfg(any(NO))]
    let _ = set_up_fern();
    if let Some(mut filepath) = filename {
        filepath.pop();
        filepath.push("giuroll.ini");

        let conf = mininip::parse::parse_file(filepath).unwrap();
        //let keyboard_section = conf.section(Some("Keyboard")).unwrap();

        let inc = conf
            .get(&Identifier::new(
                Some("Keyboard".to_string()),
                "increase_delay_key".to_string(),
            ))
            .map(|x| match x {
                Value::Int(x) => *x,
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
                _ => todo!("non integer .ini entry"),
            })
            .unwrap_or(0);

        
        unsafe {
            INCREASE_DELAY_KEY = inc as u8;
            DECREASE_DELAY_KEY = dec as u8;
        }
    }
    #[cfg(any(NO))]
    if ISDEBUG {
        info!("true_exec ran, or did it")
    };
    #[cfg(any(NO))]
    unsafe {
        if ISDEBUG {
            info!("moutain_vapor: {}", *(0x8971C0 as *const usize))
        }
    };

    unsafe {
        if false {
            //let funniest = 0x89ffbe;

            let mut b = PAGE_PROTECTION_FLAGS(0);
            VirtualProtect(
                0x454dad as *const c_void,
                0xc,
                PAGE_PROTECTION_FLAGS(0x40),
                &mut b,
            );
            VirtualProtect(
                0x454dcf as *const c_void,
                10,
                PAGE_PROTECTION_FLAGS(0x40),
                &mut b,
            );
            VirtualProtect(
                0x454d73 as *const c_void,
                6,
                PAGE_PROTECTION_FLAGS(0x40),
                &mut b,
            );

            *(0x454dad as *mut u32) = 0x90909090;
            *(0x454db1 as *mut u32) = 0x90909090;
            *(0x454db5 as *mut u32) = 0x90909090;
            *(0x454dcf as *mut u32) = 0x90909090;
            *(0x454dd3 as *mut u32) = 0x90909090;
            *(0x454dd7 as *mut u16) = 0x9090;
            *(0x454d73 as *mut u32) = 0x90909090;
            *(0x454d77 as *mut u16) = 0x9090;

            let funnyaddr = 0x858b80;

            VirtualProtect(
                funnyaddr as *const c_void,
                1,
                PAGE_PROTECTION_FLAGS(0x40),
                &mut b as *mut PAGE_PROTECTION_FLAGS,
            );
            *(funnyaddr as *mut u8) = 0x64;
            //8985e8 maybe effect manager please I beg
            let funnyaddr = 0x401d68;
            let mut b = PAGE_PROTECTION_FLAGS(0);

            VirtualProtect(
                funnyaddr as *const c_void,
                2, //sokuroll does 2 no idea why
                PAGE_PROTECTION_FLAGS(0x40),
                &mut b as *mut PAGE_PROTECTION_FLAGS,
            );
            *(funnyaddr as *mut u8) = 0xeb;
            let funnyaddr = 0x47e1d0;
            let mut b = PAGE_PROTECTION_FLAGS(0);

            VirtualProtect(
                funnyaddr as *const c_void,
                1,
                PAGE_PROTECTION_FLAGS(0x40),
                &mut b as *mut PAGE_PROTECTION_FLAGS,
            );
            *(funnyaddr as *mut u8) = 0xc3;

            let funnyaddr = 0x43b120;
            let mut b = PAGE_PROTECTION_FLAGS(0);

            VirtualProtect(
                funnyaddr as *const c_void,
                1,
                PAGE_PROTECTION_FLAGS(0x40),
                &mut b as *mut PAGE_PROTECTION_FLAGS,
            );

            *(funnyaddr as *mut u8) = 0xc3;
        }
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x482701, // 0x482820, //0x482532, sokuroll <-
            HookType::JmpBack(goodhook),
            ilhook::x86::CallbackOption::None,
            00,
            HookFlags::empty(),
        )
        .hook()
    }
    .unwrap();

    let mut hook = vec![new];

    //0x899d60 maybe sound manager?
    unsafe extern "cdecl" fn handle_sound(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) -> usize {
        let ptr = *(((*a).esi + 0x4) as *const usize);
        #[cfg(any(NO))]
        info!("sound: {}", ptr);

        let vtable = *(ptr as *const usize);
        #[cfg(any(NO))]
        info!("vtable: {}", vtable);

        //skip
        //0x401db6

        //start but not restart?
        //0x401da5

        //0x401d81 no idea
        0x401d92
    }

    unsafe extern "cdecl" fn report(a: *mut ilhook::x86::Registers, _b: usize, _c: usize) -> usize {
        (*a).ecx = 0x89f9f8;
        (*a).eax = *(((*a).esp + 4) as *const u32);
        let soundid = (*a).eax as usize;

        if !BATTLE_STARTED {
            return if soundid == 0 { 0x401db7 } else { 0x401d58 };
        }

        //if FORCE_SOUND_SKIP && false {

        //};

        let mut w = SOUND_MUTEX.lock().unwrap();
        let w2 = &mut *w;
        let items = w2.entry(*SOKU_FRAMECOUNT).or_insert_with(|| Vec::new());

        if soundid == 0 || items.iter().find(|x| **x == soundid).is_some() {
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
        }
    }
    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x401d50, // 0x482820, //0x482532, sokuroll <-
            HookType::JmpToRet(report),
            ilhook::x86::CallbackOption::None,
            0,
            HookFlags::empty(),
        )
        .hook()
    }
    .unwrap();
    hook.push(new);

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
            ilhook::x86::CallbackOption::None,
            0,
            HookFlags::empty(),
        )
        .hook()
    }
    .unwrap();
    hook.push(new);

    //sound
    if false {
        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x401d64, // 0x482820, //0x482532, sokuroll <-
                HookType::JmpToRet(handle_sound),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }
    if false {
        //if this returns to 1db6, it will not play the sound
        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x401d9f, // 0x482820, //0x482532, sokuroll <-
                HookType::JmpToAddr(0x401db6 - 6, 0, skipbuffer),
                ilhook::x86::CallbackOption::None,
                00,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }

    //0x82251e
    // on exit
    unsafe extern "cdecl" fn onexit(a: *mut ilhook::x86::Registers, _b: usize) {
        *(0x8971C0 as *mut usize) = 0; // reset wether to prevent desyncs
        ESC = 0;
        BATTLE_STARTED = false;
        DISABLE_SEND.store(0, Relaxed);
        LAST_STATE.store(0, Relaxed);

        SOUND_MUTEX.lock().unwrap().clear();
        SOUND_DELET_MUTEX.lock().unwrap().clear();

        // it cannot be used by any different thread now
        if let Some(x) = ROLLBACKER.take() {
            for a in x.guessed {
                a.prev_state.did_happen();
            }
        }

        if let Some(x) = NETCODER.take() {
            DATA_RECEIVER = Some(x.receiver);
        }

        for a in std::mem::replace(&mut *FRAMES.lock().unwrap(), vec![]) {
            a.did_happen();
        }

        GIRLSTALKED = false;
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x481960, // 0x482820, //0x482532, sokuroll <-
            HookType::JmpBack(onexit),
            ilhook::x86::CallbackOption::None,
            00,
            HookFlags::empty(),
        )
        .hook()
    }
    .unwrap();
    hook.push(new);

    //0x4545a7 jump to here if dword ptr [EDI + 0x6e0] less or equal  than ESI

    unsafe extern "cdecl" fn maybe_spectator(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) -> usize {
        //println!("here!");
        if *(((*a).edi + 0x6e0) as *const u32) <= (*a).esi + 10 {
            //println!("rock bottom");
            0x4545a7
        } else {
            0x45457f
        }
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x454577, // 0x482820, //0x482532, sokuroll <-
            HookType::JmpToRet(maybe_spectator),
            ilhook::x86::CallbackOption::None,
            00,
            HookFlags::empty(),
        )
        .hook()
    }
    .unwrap();
    hook.push(new);

    unsafe extern "cdecl" fn ongirlstalk(a: *mut ilhook::x86::Registers, _b: usize) {
        GIRLSTALKED = true;
        BATTLE_STARTED = false;
    }

    let new = unsafe {
        ilhook::x86::Hooker::new(
            0x482960,
            HookType::JmpBack(ongirlstalk),
            ilhook::x86::CallbackOption::None,
            00,
            HookFlags::empty(),
        )
        .hook()
    }
    .unwrap();
    hook.push(new);

    for a in [0x821730, 0x821759, 0x82251e, 0x82f09e, 0x82f18c] {
        let new = unsafe {
            ilhook::x86::Hooker::new(
                a,
                //HookType::JmpBack(freeoverride),
                HookType::JmpToAddr(a, 0xc, freeoverrideskip),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }

    //big source of problems 0x47d6f0

    for b in [0x821704, 0x8230e6, 0x823397, 0x82ed84, 0x82f15b] {
        let new = unsafe {
            ilhook::x86::Hooker::new(
                b + 6,
                HookType::JmpBack(allochook),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }
    let s = 0x82246d; //0x822465; /*this one is speccial, it's called a frame before so we don't know the heap, but it's constant and it's 0x89b404 */
    {
        let new = unsafe {
            ilhook::x86::Hooker::new(
                s,
                HookType::JmpBack(allochook2),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }

    for c in [0x82346f, 0x8233ee, 0x82f125] {
        let new = unsafe {
            ilhook::x86::Hooker::new(
                c,
                HookType::JmpBack(reallochook),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }

    //prevent A pause
    if true {
        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x482675,
                HookType::JmpBack(apause),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }

    // 0x428358 calls function checking if there is a next frame in net object

    {
        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x41daea, //0x454f1f maybethebuffer, //0x454eba,//0x454e44, //0x41df86,
                HookType::JmpBack(readonlinedata),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }

    {
        unsafe extern "cdecl" fn set_eax_to_0(a: *mut ilhook::x86::Registers, _b: usize) {
            (*a).eax = *(0x8a0040 as *const u32);
        }

        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x407f1b, //0x407f14,
                HookType::JmpBack(set_eax_to_0),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }

    unsafe extern "cdecl" fn handle_raw_input(
        a: *mut ilhook::x86::Registers,
        _b: usize,
        _c: usize,
    ) {
        //        0046c902 8b ae 6c        MOV        EBP,dword ptr [ESI + 0x76c]
        //        07 00 00

        (*a).ebp = *(((*a).esi + 0x76c) as *const u32);

        //        *(((*a).esi + 0x64) as *mut u8) = 0;

        //input manager;

        let m = (*a).ecx;

        //        info!("{m}");

        let somecheck = *((m + 0x4) as *const u8);

        let real_input = match std::mem::replace(&mut REAL_INPUT, REAL_INPUT2.take()) {
            Some(x) => x,
            None => {
                let f = std::mem::transmute::<usize, extern "fastcall" fn(u32)>(
                    0x040a370, /*0x40a1a0*/
                );
                (f)(m);
                return;
            }
        };

        //let next_check = *((m + 0x4) as *const i8);
        //info!("{next_check}");

        {
            let td = &mut *((m + 0x38) as *mut i32);
            let lr = &mut *((m + 0x3c) as *mut i32);

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
            let v = &mut *((m + 0x40 + a * 4) as *mut u32);

            if real_input[a as usize + 4] {
                *v += 1;
            } else {
                *v = 0;
            }
        }

        let m = &mut *((m + 0x62) as *mut u16);
        *m = 0;
        for a in 0..10 {
            if real_input[a] {
                *m += 1 << a;
            }
        }

        //additional check for some reason
        //if (*m & (4 + 8)) == 4 + 8 {
        //    *m = *m & ((0xFFFF - 8) - 4);
        //}
    }

    if true {
        // todo : rename

        //return: 0x42839a
        //online input loop,

        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x428374,
                HookType::JmpToAddr(0x42837f - 5, 0, skipbuffer),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);

        unsafe extern "cdecl" fn skiponcehost(
            a: *mut ilhook::x86::Registers,
            _b: usize,
            _c: usize,
        ) -> usize {
            if ESC > 60 {
                ESC = 0;
                0x428393
            } else {
                //info!("esi: {}", {(*a).esi});
                let skip = DISABLE_SEND.load(Relaxed) != 0;
                DISABLE_SEND.store(1, Relaxed);

                //let skip = true;

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
        let new = unsafe {
            ilhook::x86::Hooker::new(
                /*0x4282f2, */ 0x428330,
                //HookType::JmpToAddr(0x428360 - 5, 0, skipbuffer),
                HookType::JmpToRet(skiponcehost),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);

        unsafe extern "cdecl" fn skiponceclient(
            a: *mut ilhook::x86::Registers,
            _b: usize,
            _c: usize,
        ) -> usize {
            if ESC > 60 {
                ESC = 0;
                0x4286c3
            } else {
                let skip = DISABLE_SEND.load(Relaxed) != 0;
                DISABLE_SEND.store(1, Relaxed);
                //let skip = true;
                if skip {
                    0x428630
                } else {
                    (*a).ecx = *(((*a).edi + 0x8) as *const u32);
                    (*a).eax = *(((*a).ecx) as *const u32);
                    0x428605
                }
            }
        }

        //input otherGameLoop
        let new = unsafe {
            ilhook::x86::Hooker::new(
                /*0x4285c2,*/ 0x428600,
                HookType::JmpToRet(skiponceclient),
                //HookType::JmpToAddr(0x428630 - 5, 0, skipbuffer),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);

        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x428644,
                HookType::JmpToAddr(0x42864f - 5, 0, skipbuffer),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }

    unsafe extern "cdecl" fn timing_loop(a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {
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
        *target += ((1_000_000 / 60) + s).max(1005) as u128;

        let cur = m.elapsed().unwrap().as_micros();
        let diff = (*target as i128 - cur as i128); // - 1000; //spinning
        if diff > 1_000_000 {
            panic!("big diff {diff}");
        }

        //info!("frame diff micro diff: {}", diff);
        let ddiff = (diff / 1000) as i32;
        let ddiff = if ddiff < 0 {
            #[cfg(any(NO))]
            info!("frameskip {diff}");
            *target = cur + (1_000_000 / 60) as u128;
            3
        } else {
            ddiff
        };
        if ddiff > 0 {
            WaitForSingleObject(waithandle as isize, ddiff as u32);
            let w = target.saturating_sub(m.elapsed().unwrap().as_micros());
            if w > 0 && w < 1000 {
                while m.elapsed().unwrap().as_micros() < *target {}
            }
        }
    }
    if true {
        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x4192f0, //0x46c8f7,
                HookType::JmpToAddr(0x4193d7 - 6, 0, timing_loop),
                //HookType::JmpBack(write_input_to_esi),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        std::mem::forget(new);
        //hook.push(new);
    }

    if true {
        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x46c900, //0x46c8f7,
                HookType::JmpToAddr(0x46c908 - 8, 0, handle_raw_input),
                //HookType::JmpBack(write_input_to_esi),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);
    }
/*
    if false {
        static mut ST: Option<SystemTime> = None;
        unsafe { ST = Some(SystemTime::now()) };

        unsafe extern "cdecl" fn report_start(a: *mut ilhook::x86::Registers, _b: usize) {
            // *(0x89ffb9 as *mut u8) = 1;

            let netmanager = *(0x8986a0 as *const usize);
            if !(netmanager != 0 && *(netmanager as *const usize) != 0x858cac) {
                ST = Some(SystemTime::now());
                #[cfg(any(NO))]
                info!("started ",)
            }
        }

        unsafe extern "cdecl" fn reporttime(a: *mut ilhook::x86::Registers, _b: usize) {
            let netmanager = *(0x8986a0 as *const usize);
            if !(netmanager != 0 && *(netmanager as *const usize) != 0x858cac) {
                if let Some(st) = ST {
                    #[cfg(any(NO))]
                    info!(
                        "timeelapsed: {} {:0x}",
                        st.elapsed().unwrap().as_millis(),
                        _b
                    )
                } else {
                    #[cfg(any(NO))]
                    info!("BEFORE");
                }
            }
        }
        //00407cba insanely important

        let new = unsafe {
            ilhook::x86::Hooker::new(
                0x407e26,
                HookType::JmpBack(report_start),
                ilhook::x86::CallbackOption::None,
                0,
                HookFlags::empty(),
            )
            .hook()
        }
        .unwrap();
        hook.push(new);

        //0x4285c4 0x4285e0

        // 0x407f21 WAS REACHED
        // f89 WAS REACHED
        //0x407f3a
        //[0x407e33, 0x407e9e, 0x407f21, 0x407f2c, 0x407f89, 0x407f98, 0x407f9d, 0x407f3c, 0x407fc6, 0x407f2c, 0x407f3c, 0x407f66]
        /*
        [
            0x454760, 0x4547c0, 0x4549a0, 0x454800, 0x43fcc0, 0x43f950, 0x454640, 0x455340,
            0x455070, 0x454910, 0x455180, 0x454c90, 0x454d30, 0x454a40, 0x454860, 0x43fc20,
            0x43fc30, 0x454950, 0x43fc40, 0x4546a0, 0x4546c0,
            ]
            */
        for a in [
            0x407e33, 0x407ea4, 0x407f14, 0x408005, 0x408031, 0x419687, 0x41968d, 0x40803b,
            0x4192f0, 0x4193dc, 0x4193a2,
        ] {
            let new = unsafe {
                ilhook::x86::Hooker::new(
                    a,
                    HookType::JmpBack(reporttime),
                    ilhook::x86::CallbackOption::None,
                    a,
                    HookFlags::empty(),
                )
                .hook()
            }
            .unwrap();
            hook.push(new);
        }
    }


 */
    //if false {
    //    unsafe extern "cdecl" fn log_gametype(a: *mut ilhook::x86::Registers, _b: usize) {
    //        #[cfg(any(NO))]
    //        info!("GAMETYPE TRUE {}", *(0x89868c as *const usize));
    //    }

    //    let new = unsafe {
    //        ilhook::x86::Hooker::new(
    //            0x43e5fb,
    //            HookType::JmpBack(log_gametype),
    //            ilhook::x86::CallbackOption::None,
    //            0,
    //            HookFlags::empty(),
    //        )
    //        .hook()
    //    }
    //    .unwrap();
    //    hook.push(new);
    //}

    *HOOK.lock().unwrap() = Some(hook.into_boxed_slice());

    unsafe {
        std::thread::spawn(|| {
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
        #[cfg(any(NO))]
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
        .for_each(|x| unsafe { x.unhook() }.unwrap());
}

unsafe fn set_input_buffer(input: [bool; 10], input2: [bool; 10]) {
    REAL_INPUT = Some(input);
    REAL_INPUT2 = Some(input2);
}
//might not be neccesseary
static REQUESTED_THREAD_ID: AtomicU32 = AtomicU32::new(0);

const SOKU_FRAMECOUNT: *mut usize = 0x8985d8 as *mut usize;
use windows::Win32::System::Threading::GetCurrentThreadId;
unsafe extern "cdecl" fn freeoverrideskip(_a: *mut ilhook::x86::Registers, _b: usize, _c: usize) {
    (*_a).eax = 1;
    let s = ((*_a).esp as usize + 2 * 4) as *mut usize;
    return;
    if GetCurrentThreadId() == REQUESTED_THREAD_ID.load(Relaxed) {
        unsafe {
            MEMORY_SENDER
                .as_ref()
                .unwrap()
                .send(MemoryManip::Free(*s))
                .unwrap()
        };
    } else {
        let heap = unsafe { *(0x89b404 as *const isize) };
        unsafe { HeapFree(heap, 0, *s as *const c_void) };
    }
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

fn store_alloc(u: usize) {
    unsafe {
        MEMORY_SENDER
            .as_ref()
            .unwrap()
            .send(MemoryManip::Alloc(u))
            .unwrap()
    };
    //match FRAMES.lock().unwrap().last_mut() {
    //    Some(x) => x.allocs.push(u),
    //    None => (), //happened for sure
    //}
}

unsafe extern "cdecl" fn allochook(a: *mut ilhook::x86::Registers, _b: usize) {
    //let (s, heap) = unsafe {
    //    //(
    //    //    *(((*a).esp as usize + 2 * 4) as *mut usize),
    //    //    *(((*a).esp as usize + 1 * 4) as *mut usize),
    //    //)
    //
    //};

    if GetCurrentThreadId() == REQUESTED_THREAD_ID.load(Relaxed) {
        store_alloc((*a).eax as usize);
    }
}

unsafe extern "cdecl" fn allochook2(a: *mut ilhook::x86::Registers, _b: usize) {
    //let s = unsafe { *(((*a).esp as usize + 1 * 4) as *mut usize) };

    let ptr = unsafe { (*a).eax };
    store_alloc(ptr as usize);
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

static PAUSESTATE: AtomicU8 = AtomicU8::new(0);

unsafe extern "cdecl" fn apause(a: *mut ilhook::x86::Registers, _b: usize) {
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
    let slic = std::slice::from_raw_parts(packet_pointer as *const u8, 400);
    let type1 = slic[0];
    let type2 = slic[1];

    //   let frame_count = usize::from_le_bytes(slic[2..6].try_into().unwrap());
    let sceneid = slic[6];
    //   let somethingweird = slic[7];
    //   let input1 = slic[8];
    //   let input2 = slic[9];

    if type1 == 0x69 {
        (*a).eax = 0x400;
        let z = NetworkPacket::decode(slic);

        DATA_SENDER
            .as_ref()
            .unwrap()
            .send((z, Instant::now()))
            .unwrap();
    }
    if (type1 == 14 || type1 == 13) && type2 == 3 && sceneid == 0x5 {
        //        info!("received {} {}", type1, type2);
    }

    if (type1 == 14 || type1 == 13) && type2 == 1 && BATTLE_STARTED {
        //opponent has esced (probably) exit!
        // let's have an incrementing value to make sure

        ESC += 1;
        if ESC > 60 {
            BATTLE_STARTED = false;
        }

        //we should probably make sure this is not a stray packet getting to us late

        //info!("received {} {} {}", type1, type2, sceneid);
    }
    //BATTLE_STARTED
}

unsafe extern "cdecl" fn highloop(a: *mut ilhook::x86::Registers, _b: usize) {
    #[cfg(any(NO))]
    info!("here HL!");
}

static mut GIRLSTALKED: bool = false;
static DISABLE_SEND: AtomicU8 = AtomicU8::new(0);

//interesting adress 43E363

// relevant function to "skip buffer" : 0x4558a0

static LASTFR: AtomicI32 = AtomicI32::new(0);
static ENDED: AtomicI32 = AtomicI32::new(0);

static FRAMES: Mutex<Vec<Frame>> = Mutex::new(Vec::new());
static GOOF: AtomicU8 = AtomicU8::new(0);

unsafe fn handle_replay(
    fc2: usize,
    battle_state: &mut u32,
    cur_speed: &mut u32,
    cur_speed_iter: &mut u32,
    weird_counter: &mut u32,
) {
    {
        if let Some(x) = FRAMES.lock().unwrap().last_mut() {
            while let Ok(man) = MEMORY_RECEIVER.as_ref().unwrap().try_recv() {
                match man {
                    MemoryManip::Alloc(pos) => x.allocs.push(pos),
                    MemoryManip::Free(pos) => x.frees.push(pos),
                }
            }
        }
    }

    let fc2 = *SOKU_FRAMECOUNT;
    let p = LASTFR.load(Relaxed);

    LASTFR.store(fc2 as i32, Relaxed);

    if fc2 == 0 {
        #[cfg(any(NO))]
        if ISDEBUG {
            info!("frame 0")
        };
        unsafe {
            #[cfg(any(NO))]
            if ISDEBUG {
                info!("moutain_vapor_f0: {}", *(0x8971C0 as *const usize))
            }
        };
        let w = std::mem::replace(&mut *FRAMES.lock().unwrap(), Vec::new());
        for frame in w {
            frame.did_happen();
        }
    }

    //let mystcountpos: u32;

    //0x57

    resume(battle_state);
    if PAUSESTATE.load(Relaxed) != 0 {
        pause(battle_state, weird_counter);
        return;
    }

    if *cur_speed_iter == 0 {
        GOOF.store(0, Relaxed);
        //"true" frame
        let qdown = read_key_better(0x10);
        //let wdown = read_key_better(0x57);

        if qdown {
            let target = (fc2 as u32).saturating_sub(*cur_speed) - 1;
            #[cfg(any(NO))]
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
                        GOOF.store(1, Relaxed);
                        //good
                        let diff = target - framenum;
                        #[cfg(any(NO))]
                        if ISDEBUG {
                            info!("diff: {}", diff)
                        };

                        x.clone().did_happen();
                        x.restore();

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
                        //last.did_happen();

                        last.clone().did_happen();
                        last.restore();
                        //did it happen?
                    }
                    pause(battle_state, weird_counter);
                    *cur_speed_iter = *cur_speed;
                    #[cfg(any(NO))]
                    if ISDEBUG {
                        info!("missing frame")
                    };
                    return;
                }
            }
            #[cfg(any(NO))]
            if ISDEBUG {
                info!(" restore complete success")
            };
        }
    }

    let fc2 = *SOKU_FRAMECOUNT;

    if fc2 % 16 == 1 || GOOF.load(Relaxed) == 1 {
        let start = Instant::now();
        #[cfg(any(NO))]
        if ISDEBUG {
            info!("framecount: {}", fc2)
        };

        let frame = dump_frame();

        let mut mutex = FRAMES.lock().unwrap();

        mutex.push(frame);

        //if ISDEBUG { info!("frame size: {}", frame.size_data()) };
        //if ISDEBUG { info!("frame size: {}", frame.redundency_data()) };

        let duration = start.elapsed();

        #[cfg(any(NO))]
        if ISDEBUG {
            info!("time elapsed on saving: {}", duration.as_nanos())
        };
        #[cfg(any(NO))]
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

    let keyboard_d = *((local_input_manager + 0x4) as *const u8);
    //if 255, then keyboard, if 0, or maybe something else, then controller

    if keyboard_d == 255 {
        for a in 0..10 {
            let key = (local_input_manager + 0x8 + a * 0x4) as *const u8;
            //info!("here {}", key as usize);
            let key = *key as u32;
            //info!("here");
            let key = *((raw_input_buffer + key) as *const u8) != 0;
            input[a] = key;

            //info!("{a}: {}", key)
        }
    } else {
        let get_controller =
            std::mem::transmute::<usize, extern "thiscall" fn(u32, u32) -> u32>(0x40dc60);
        let controler = get_controller(0x8a0198, keyboard_d as u32);

        if controler != 0 {
            let axis1 = *(controler as *const i32);
            let axis2 = *((controler + 4) as *const i32);

            input[2] = axis1 < -500;
            input[3] = axis1 > 500;

            input[0] = axis2 < -500;
            input[1] = axis2 > 500;

            for a in 0..6 {
                #[cfg(any(NO))]
                info!("{a}");
                let key = *((local_input_manager + 0x18 + a * 0x4) as *const i32);
                #[cfg(any(NO))]
                info!("second {a}");
                if key > -1 {
                    input[a + 4] = *((key as u32 + 0x30 + controler) as *const u8) != 0;
                }
            }
            //info!("controller value1: {}", );
            //info!("controller value2: {}", ;
        }
    }

    input
}

static mut ROLLBACKER: Option<Rollbacker> = None;
static mut NETCODER: Option<Netcoder> = None;

static mut DATA_SENDER: Option<std::sync::mpsc::Sender<(NetworkPacket, Instant)>> = None;
static mut DATA_RECEIVER: Option<std::sync::mpsc::Receiver<(NetworkPacket, Instant)>> = None;

static mut MEMORY_SENDER: Option<std::sync::mpsc::Sender<MemoryManip>> = None;
static mut MEMORY_RECEIVER: Option<std::sync::mpsc::Receiver<MemoryManip>> = None;

// remember to increase by 1!
static mut LAST_DELAY_VALUE: usize = 0;

static mut LAST_DELAY_MANIP: u8 = 0; // 0 neither, 1 up, 2 down, 3 both

static mut BATTLE_STARTED: bool = false; //used for esc detection
static mut ESC: u8 = 0; // maybe shouldn't be not atomic

static mut INCREASE_DELAY_KEY: u8 = 0;
static mut DECREASE_DELAY_KEY: u8 = 0;

unsafe fn handle_online(
    fc2: usize,
    battle_state: &mut u32,
    cur_speed: &mut u32,
    cur_speed_iter: &mut u32,
    state_sub_count: &mut u32,
) {
    if fc2 == 0 {
        BATTLE_STARTED = true;
        let m = DATA_RECEIVER.take().unwrap();

        let rollbacker = Rollbacker::new();

        ROLLBACKER = Some(rollbacker);
        let mut netcoder = Netcoder::new(m);
        netcoder.delay = LAST_DELAY_VALUE;
        netcoder.max_rollback = 6;
        NETCODER = Some(netcoder);

        return;
    }

    let rollbacker = ROLLBACKER.as_mut().unwrap();
    let netcoder = NETCODER.as_mut().unwrap();

    resume(battle_state);

    let k_up = read_key_better(INCREASE_DELAY_KEY);
    let k_down = read_key_better(DECREASE_DELAY_KEY);

    let last_up = LAST_DELAY_MANIP & 1 == 1;
    let last_down = LAST_DELAY_MANIP & 2 == 2;

    if !last_up && k_up {
        if netcoder.delay < 9 {
            // values skewed by 1 because of the my frame-frame id offset
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

    //input buffer overrider first: 40a1a0, second 40A3AB

    //th123.exe+2833A
}

unsafe extern "cdecl" fn goodhook(a: *mut ilhook::x86::Registers, _b: usize) {
    #[cfg(any(NO))]
    std::panic::set_hook(Box::new(|x| info!("panic! {:?}", x)));

    read_current_input();
    let fc2 = *SOKU_FRAMECOUNT;

    let state_sub_count: &mut u32;
    let battle_state: &mut u32;
    let cur_speed: &mut u32;
    let cur_speed_iter: &mut u32;
    {
        let w = (*a).esi;
        cur_speed = &mut (*a).ebx;
        cur_speed_iter = &mut (*a).edi;

        let m = (w + 4 * 0x22) as *mut u32; //battle phase
                                            //mystcountpos = (w + 4 * 1) as u32; //battle phase

        battle_state = &mut *m;
        state_sub_count = &mut *((w + 4) as *mut u32);
    }
    //898680 potential manager one
    //498718 potential manager two

    //043e610 !!!!!!!!!!

    //info!("gametype {}", *(0x898688 as *const usize));
    REQUESTED_THREAD_ID.store(GetCurrentThreadId(), Relaxed);

    match *(0x898688 as *const usize) {
        2 => handle_replay(
            fc2,
            battle_state,
            cur_speed,
            cur_speed_iter,
            state_sub_count,
        ), //2 is replay
        1 => {
            if !GIRLSTALKED {
                // push unalocated memory onto the frames

                handle_online(
                    fc2,
                    battle_state,
                    cur_speed,
                    cur_speed_iter,
                    state_sub_count,
                )
            }
        }
        _ => (),
    }

    //handle uncollected references here

    //while let Ok(man) = MEMORY_RECEIVER.as_ref().unwrap().try_recv() {
    //    match man {
    //        MemoryManip::Alloc(pos) => (), /* let this go through I guess*/
    //        MemoryManip::Free(pos) => {
    //            /let heap = unsafe { *(0x89b404 as *const isize) };
    //            /unsafe { HeapFree(heap, 0, pos as *const c_void) };
    //        }
    //    }
    //}
}
