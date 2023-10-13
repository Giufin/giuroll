use std::{ffi::OsStr, path::PathBuf, time::Duration};

use dll_syringe::{process::OwnedProcess, Syringe};

fn main() {
    unsafe {
        /*
        let tl = tasklist::Tasklist::new();

        for i in tl {
            if i.get_pname() == "th123.exe" {
                let m = i.get_pid();
                let proc = djin::open_process(m).unwrap();
                let dll = "giuroll.dll";
                djin::inject_dll(proc, dll, "exeinit".as_bytes()).unwrap();
                std::thread::sleep(std::time::Duration::from_secs(5));
            }
        }
        */

        let target_process = match OwnedProcess::find_first_by_name("th123.exe") {
            Some(x) => x,
            None => {
                println!("th123.exe process not found, make sure soku is running");
                std::thread::sleep(Duration::from_secs(5));
                panic!()
            }
        };
        let syringe = Syringe::for_process(target_process);
        let injected_payload = syringe.inject("giuroll.dll").unwrap();

        match syringe
            .get_raw_procedure::<unsafe extern "C" fn() -> bool>(
                injected_payload,
                "better_exe_init",
            )
            .ok()
            .flatten()
            .map(|f1| {
                (
                    f1,
                    syringe
                        .get_raw_procedure::<unsafe extern "C" fn(u8)>(
                            injected_payload,
                            "better_exe_init_push_path",
                        )
                        .unwrap()
                        .unwrap(),
                )
            })
            .and_then(|function| {
                std::env::current_dir()
                    .ok()
                    .map(move |path| (function, path))
            }) {
            Some(((f1, f2), current_path)) => {
                let slice = current_path.as_os_str().as_encoded_bytes();
                for a in slice {
                    f2.call(*a).unwrap();
                }
                if f1.call().unwrap() {
                    println!("injection successfull")
                } else {
                    println!("injection failed, giuroll.ini not found")
                }
            }
            None => syringe
                .get_raw_procedure::<unsafe extern "C" fn()>(injected_payload, "exeinit")
                .unwrap()
                .unwrap()
                .call()
                .unwrap(),
        };
    }
    std::thread::sleep(Duration::from_secs(5));
}
