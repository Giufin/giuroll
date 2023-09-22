# Giuroll  
*tentatively named for the lack of a better name; will likely change in the future*


Is a network rollback mod for 東方非想天則 / Touhou 12.3 Hisoutensoku, which aims to significantly improve the responsiveness of netplay for connections without ideal conditions over Sokuroll, as well as allowing for rewinding replays in the future.  

Currently this is a very early version and contains a lot of issues, but will still significantly improve the netplay experience for many connections.  

This repository also contains a stripped down version version of the crate [ilhook-rs](https://github.com/regomne/ilhook-rs), and a modified version of [mininip](https://github.com/SlooowAndFurious/mininip), all rights remain with their respective authors.

## Current Issues  

- Game crashes upon attempting to load Vs Player mode
- In netplay mode, game slowly but constantly leaks memory, dependent especially on the count of frames being rolled back
- In replay mode, game leaks memory very rapidly
- Replay rewind can cause desyncs when stopping rollback at moments with a lot of inputs
- Game will very rarely freeze on round start when the initial original network packet is lost due to packet loss

## Usage  

Mod is currently only usable with the [SWRSToys](https://github.com/SokuDev/SokuMods/) loader, place the mod alongside the `giuroll.ini` config file in a subdirectory of the `modules` directory, and point to it in the `SWRSToys.ini` file, similar to other SWRSToys mods. You will need to disable any version of Sokuroll you might have already enabled.

## Replay rewind  

In replay mode pressing `q` will start rewinding, using keys that modify playback speed (A/S/D) will affect the rewind speed.  
Currently this functionality is incomplete, leaking a lot of memory, and desyncing on input heavy sections (because the buffer that stores information about charge of both players is stored differently in different modes, and I have not located the replay buffers yet)

## Building from source

Mod can be buit with `cargo` using the `nightly-i686-pc-windows-msvc` toolchain.  
When building from source please remember to add the `--release`/`-r` flag.

## Common problems  

- Game doesn't load: check if the ini is valid according to the example ini provided in this repository, and is placed alongside the mod without any changes to it's name, and check for mod conflicts by disabling all other mods, and adding them back one by one.  
- Failed to connect: either player is using an incompatible version of giuroll, or is not using it at all.  
- Game froze/crashed: Game will periodically crash due to the aforementioned memory leak, other than that crashes and freezes are still possible for unrelated reasons, this will be worked out with time.  
- Game desynced: I'm planning on adding a desync detector to make debugging desyncs easier, but since desyncs also occur with SokuRoll there is no guarantee they are caused solely by the rollback. If the desyncs are common, persists between game restarts, and are not appearing with Sokuroll, you can contact me about it.


you can contact me about any issues through discord: `@giufin` in DMs or in the hisouten sever.

### Special thanks to:

[DPhoenix](https://github.com/enebe-nb) and [PinkySmile](https://github.com/Gegel85) - for advice and support with hisoutensoku modding  

Ysaron - for majority of the testing 

ChèvreDeFeu, TStar, Barcode, Rouen, Klempfer, LunaTriv, Aquatic, BIG BREWED and Sigets - for additional testing

[Slavfox](https://github.com/slavfox) - for various help with reverse engineering and open source

Fireseal - for making the original rollback mod for hisoutensoku
