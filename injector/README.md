# Injector for the giuroll DLL
Applies Giuroll to Hisoutensoku without requiring `SWRSToys`. Works akin to the legacy `SokurollLoader.exe` used in older copies of the game.

## Compilation
1. Navigate to this directory in the command prompt
```
cd <...>/giuroll/injector
```

2. If you are using a 64-bit PC, compile with
```
cargo +nightly-x86_64-pc-windows-msvc build --release
``` 
If you are using a 32-bit PC, compile with
```
cargo +nightly-i686-pc-windows-msvc build --release
```


## Usage
1. Ensure that `th123.exe`, `injector.exe`, `giuroll.dll` and `giuroll.ini` are all in the same directory,
2. Start `th123.exe`,
3. Run `injector.exe`

If successful, the title of the game window will include `Giuroll <version_number>` at the end.

If the injector closes abruptly, contact me about it.