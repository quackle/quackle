; Script for creating Quackle Windows installer
; For Inno Setup 6.7
;
; Build with, e.g.:
;   ISCC.exe installer.iss
; Overridable defines:
;   /DBuildDir=<dir containing Quackle.exe and the windeployqt output>
;   /DVCRedistDir=<dir containing vc_redist.x64.exe>
;     (defaults to %VCToolsRedistDir%, which a VS developer prompt sets)

; Keep in sync with project(Quackle VERSION ...) in quacker/CMakeLists.txt.
#define AppVersion "1.1.0"

#ifndef BuildDir
  #define BuildDir "build.vs22\RelWithDebInfo"
#endif

#ifndef VCRedistDir
  #define VCRedistDir GetEnv("VCToolsRedistDir")
#endif
#if VCRedistDir == ""
  #error VCRedistDir is empty. Run from a Visual Studio developer prompt, or pass /DVCRedistDir=<path>
#endif
#define VCRedist AddBackslash(VCRedistDir) + "vc_redist.x64.exe"
#if !FileExists(VCRedist)
  #error vc_redist.x64.exe not found in VCRedistDir
#endif
#define VCRedistVersion GetVersionNumbersString(VCRedist)

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
WizardStyle=modern dynamic
Compression=lzma2/max
SolidCompression=yes
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

; Last, in its own solid block, so a skipped redist costs nothing to decompress past.
Source: "{#VCRedist}"; DestDir: "{tmp}"; Flags: deleteafterinstall solidbreak; Check: VCRedistNeeded

[Run]
Filename: "{tmp}\vc_redist.x64.exe"; Parameters: "/install /quiet /norestart"; StatusMsg: "Installing Microsoft Visual C++ Runtime..."; Check: VCRedistNeeded
Filename: "{app}\Quackle.exe"; Description: "{cm:LaunchProgram,Quackle}"; Flags: nowait postinstall skipifsilent

[Code]
const
  VCRuntimeKey = 'SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64';

function VCRedistNeeded: Boolean;
var
  Installed, Major, Minor, Bld, Rbld: Cardinal;
  Required: Int64;
begin
  if not StrToVersion('{#VCRedistVersion}', Required) then
    RaiseException('Bad VCRedistVersion: {#VCRedistVersion}');
  Result := not (RegQueryDWordValue(HKLM, VCRuntimeKey, 'Installed', Installed) and (Installed = 1) and
    RegQueryDWordValue(HKLM, VCRuntimeKey, 'Major', Major) and
    RegQueryDWordValue(HKLM, VCRuntimeKey, 'Minor', Minor) and
    RegQueryDWordValue(HKLM, VCRuntimeKey, 'Bld', Bld) and
    RegQueryDWordValue(HKLM, VCRuntimeKey, 'Rbld', Rbld) and
    (ComparePackedVersion(PackVersionComponents(Major, Minor, Bld, Rbld), Required) >= 0));
end;
