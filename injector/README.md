# Injector for the giuroll DLL
Applies Giuroll to Hisoutensoku without requiring `SWRSToys`. Works akin to the legacy `SokurollLoader.exe` used in older copies of the game.

## Compilation
1. Navigate to this directory in the command prompt
```
cd <...>/giuroll/injector
```

2. Compile with
```
cargo +nightly-i686-pc-windows-msvc build --release
```

## Usage
1. Ensure that `injector.exe`, `giuroll.dll` and `giuroll.ini` are all in the same directory,
2. Start `th123.exe`,
3. Run `injector.exe`

If successful, the console window will display the words `Injection successful` for a few seconds before closing, and the title of the game window will include `Giuroll <version_number>` at the end.

If the injector closes abruptly, contact me about it.