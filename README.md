# Giuroll  

Is a network rollback mod for 東方非想天則 / Touhou 12.3 Hisoutensoku, which aims to significantly improve the responsiveness of netplay, as well as introducing other rollback related improvements to replay mode.  

Currently this is an early version, and might slightly increase instability, but will still significantly improve the netplay experience for almost all connections.  

This repository also contains a stripped down version version of the crate [ilhook-rs](https://github.com/regomne/ilhook-rs), and a modified version of [mininip](https://github.com/SlooowAndFurious/mininip), all rights remain with their respective authors.

## Usage  

### For [SWRSToys](https://github.com/SokuDev/SokuMods/) users
1. Navigate to your Hisoutensoku folder. You should see a subfolder called `modules`, and a file called `SWRSToys.ini`.
2. Drop the giuroll folder from this zip into `modules`
3. Add the following line into your `SWRSToys.ini`
```
giuroll=modules/giuroll/giuroll.dll
```
4. find the following line
`
SWRSokuRoll=modules/SWRSokuRoll/SWRSokuRoll.dll
`
and add a `;` at the beginning of that line, making it
```
; SWRSokuRoll=modules/SWRSokuRoll/SWRSokuRoll.dll
```

### For users without SWRSToys
See the [Injector](/injector/).  

**More usage information is available in the `installation and usage.txt` file within the distributed zip file.**

## Replay rewind  

In replay mode pressing `q` will start rewinding, using keys that modify playback speed (A/S/D) will affect the rewind speed.  
You can also pause the replay by pressing `z`. When the replay is paused this way you can move frame by frame, backwards or forwards, by using `s` and `d`.

## Desync detection
Giuroll is able to detect desyncs intrinsically, but displaying them in-game requires a [secondary mod](https://github.com/kookie2332/Giuroll-Desync-Detector).

There is no guarantee that the desync detector will always correctly identify when a game is desynced or not. You can contact me if they:
- are common,
- persist after game restarts, or
- do not occur when using SokuRoll

## Building from source

Mod can be buit with `cargo` using the `nightly-i686-pc-windows-msvc` toolchain.  
When building from source please remember to add the `--release`/`-r` flag.

```
rustup toolchain install nightly-i686-pc-windows-msvc
cargo +nightly-i686-pc-windows-msvc build --release
```

## Common problems  

- Game doesn't load: check if the ini is valid according to the example ini provided in this repository, and is placed alongside the mod without any changes to it's name, and check for mod conflicts by disabling all other mods, and adding them back one by one.  
- Failed to connect: either player is using an incompatible version of giuroll, or is not using it at all.  

you can contact me about any issues through discord: `@giufin` in DMs.

### Special thanks to:

[DPhoenix](https://github.com/enebe-nb) and [PinkySmile](https://github.com/Gegel85) - for advice and support with hisoutensoku modding  

Ysaron - for majority of the testing 

TStar, Barcode, Rouen, ChèvreDeFeu, Klempfer, LunaTriv, Aquatic, BIG BREWED and Sigets - for additional testing

[Slavfox](https://github.com/slavfox) - for various help with reverse engineering and open source

Fireseal - for making the original rollback mod for hisoutensoku
