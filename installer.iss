; Script for creating Quackle Windows installer
; For Inno Setup 5.1

[Setup]
AppName=Quackle
AppVerName=Quackle 1.0.4
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
DefaultDirName={commonpf}\Quackle
DefaultGroupName=Quackle
ChangesAssociations=yes
UninstallDisplayIcon={app}\Quackle.exe

[Icons]
Name: "{group}\Quackle 1.0.4"; Filename: "{app}\Quackle.exe"; WorkingDir: "{app}"

[Registry]
Root: HKCU; Subkey: "Software\Quackle.org"
Root: HKCR; Subkey: ".gcg"; ValueType: string; ValueName: ""; ValueData: "QuackleGameFile"; Flags: uninsdeletevalue 
Root: HKCR; Subkey: "QuackleGameFile"; ValueType: string; ValueName: ""; ValueData: "Quackle Game File"; Flags: uninsdeletekey 
Root: HKCR; Subkey: "QuackleGameFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Quackle.exe,0" 
Root: HKCR; Subkey: "QuackleGameFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Quackle.exe"" ""%1"""


[Files]
Source: "quacker\build\RelWithDebInfo\Quackle.exe"; DestDir: "{app}"
Source: "LICENSE"; DestDir: "{app}"

; Assuming either a cmake process or a human copied the DLL files
; in the same dir.  Right now, I'm building from vcpkg, and the list of DLLs
; is Qt5Core, Qt5Gui, Qt5Widgets, zlib1, bz2, freetype, harfbuzz, libpng16, pcre2-16.
; Also the MSVC VC runtime redist DLLs.
; But, depending upon your build chain, this will vary. -jfultz
Source: "quacker\build\RelWithDebInfo\*.dll"; DestDir: "{app}"

; Ditto for various Qt plugins
; Right now, I'm installing platforms\qwindows.dll, styles\qwindowsvistastyle.dll,
; and imageformats\*
Source: "quacker\build\RelWithDebInfo\imageformats\*"; DestDir: "{app}\imageformats"
Source: "quacker\build\RelWithDebInfo\platforms\*"; DestDir: "{app}\platforms"
Source: "quacker\build\RelWithDebInfo\styles\*"; DestDir: "{app}\styles"


Source: "data\themes\*"; DestDir: "{app}\data\themes"
Source: "data\alphabets\*"; DestDir: "{app}\data\alphabets"
Source: "data\lexica\*"; DestDir: "{app}\data\lexica"
Source: "data\strategy\*"; DestDir: "{app}\data\strategy"; Flags: recursesubdirs
