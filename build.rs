use std::env;

use winres::{VersionInfo, WindowsResource};

extern crate winres;

fn main() {
    let mut res = WindowsResource::new();
    if cfg!(unix) {
        // from https://github.com/mxre/winres/blob/1807bec3552cd2f0d0544420584d6d78be5e3636/example/build.rs#L10
        // ar tool for mingw in toolkit path
        res.set_ar_path("/usr/i686-w64-mingw32/bin/ar");
        // windres tool
        res.set_windres_path("/usr/bin/i686-w64-mingw32-windres");
    }

    if let Some(version_pre) = env::var("CARGO_PKG_VERSION_PRE").unwrap().splitn(2, "-").next() {
        let mut version = 0_u64;
        version |= env::var("CARGO_PKG_VERSION_MAJOR")
            .unwrap()
            .parse()
            .unwrap_or(0)
            << 48;
        version |= env::var("CARGO_PKG_VERSION_MINOR")
            .unwrap()
            .parse()
            .unwrap_or(0)
            << 32;
        version |= env::var("CARGO_PKG_VERSION_PATCH")
            .unwrap()
            .parse()
            .unwrap_or(0)
            << 16;
        version |= version_pre.parse().unwrap_or(0);
        res.set_version_info(VersionInfo::FILEVERSION, version);
        res.set_version_info(VersionInfo::PRODUCTVERSION, version);
    } else {
        panic!();
    }

    if let Err(e) = res.compile() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
