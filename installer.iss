; Script for creating Quackle Windows installer
; For Inno Setup 6.7
;
; Build with, e.g.:
;   ISCC.exe installer.iss
; Overridable defines:
;   /DBuildDir=<dir containing Quackle.exe and the windeployqt output>

; Keep in sync with project(Quackle VERSION ...) in quacker/CMakeLists.txt.
#define AppVersion "1.1.0"

#ifndef BuildDir
  #define BuildDir "build.vs22\RelWithDebInfo"
#endif

[Setup]
; Versioned so that releases install side by side, each with its own uninstaller.
AppId=Quackle_{#AppVersion}
AppName=Quackle
AppVersion={#AppVersion}
AppPublisher=Quackle.org
AppPublisherURL=https://quackle.org
AppSupportURL=https://github.com/quackle/quackle/issues
VersionInfoVersion={#AppVersion}
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
; Qt 6 needs Windows 10 1809 or later.
MinVersion=10.0.17763
PrivilegesRequired=admin
DefaultDirName={autopf}\Quackle\{#AppVersion}
DisableProgramGroupPage=yes
ChangesAssociations=yes
SetupIconFile=quacker\quacker.ico
UninstallDisplayIcon={app}\Quackle.exe
OutputDir=.
OutputBaseFilename=QuackleInstaller-{#AppVersion}

[Icons]
Name: "{autoprograms}\Quackle {#AppVersion}"; Filename: "{app}\Quackle.exe"; WorkingDir: "{app}"

[Registry]
Root: HKA; Subkey: "Software\Classes\.gcg"; ValueType: string; ValueName: ""; ValueData: "QuackleGameFile"; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\QuackleGameFile"; ValueType: string; ValueName: ""; ValueData: "Quackle Game File"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\QuackleGameFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Quackle.exe,0"
Root: HKA; Subkey: "Software\Classes\QuackleGameFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Quackle.exe"" ""%1"""

[Files]
Source: "{#BuildDir}\Quackle.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion

; Qt DLLs and plugins are put here by the windeployqt POST_BUILD step in quacker/CMakeLists.txt.
Source: "{#BuildDir}\*.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#BuildDir}\imageformats\*"; DestDir: "{app}\imageformats"; Flags: ignoreversion
Source: "{#BuildDir}\platforms\*"; DestDir: "{app}\platforms"; Flags: ignoreversion
Source: "{#BuildDir}\styles\*"; DestDir: "{app}\styles"; Flags: ignoreversion

Source: "data\themes\*"; DestDir: "{app}\data\themes"; Excludes: "CMakeLists.txt"; Flags: ignoreversion
Source: "data\alphabets\*"; DestDir: "{app}\data\alphabets"; Excludes: "CMakeLists.txt"; Flags: ignoreversion
Source: "data\lexica\*"; DestDir: "{app}\data\lexica"; Excludes: "CMakeLists.txt"; Flags: ignoreversion
Source: "data\strategy\*"; DestDir: "{app}\data\strategy"; Excludes: "CMakeLists.txt"; Flags: ignoreversion recursesubdirs

; Assuming either a cmake process or a human copied the DLL files
; in the same dir.  Right now, I'm building from vcpkg, and the list of DLLs
; is Qt5Core, Qt5Gui, Qt5Widgets, zlib1, bz2, freetype, harfbuzz, libpng16, pcre2-16.
; Also the MSVC VC runtime redist DLLs.
; But, depending upon your build chain, this will vary. -jfultz
Source: "build.vs22\RelWithDebInfo\*.dll"; DestDir: "{app}"

; Ditto for various Qt plugins
; Right now, I'm installing platforms\qwindows.dll, styles\qwindowsvistastyle.dll,
; and imageformats\*
Source: "build.vs22\RelWithDebInfo\imageformats\*"; DestDir: "{app}\imageformats"
Source: "build.vs22\RelWithDebInfo\platforms\*"; DestDir: "{app}\platforms"
Source: "build.vs22\RelWithDebInfo\styles\*"; DestDir: "{app}\styles"


Source: "data\themes\*"; DestDir: "{app}\data\themes"
Source: "data\alphabets\*"; DestDir: "{app}\data\alphabets"
Source: "data\lexica\*"; DestDir: "{app}\data\lexica"
Source: "data\strategy\*"; DestDir: "{app}\data\strategy"; Flags: recursesubdirs
