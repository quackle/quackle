; Script for creating Quackle Windows installer
; For Inno Setup 5.1

[Setup]
AppName=Quackle
AppVerName=Quackle 0.98
DefaultDirName={pf}\Quackle
DefaultGroupName=Quackle
ChangesAssociations=yes

[Icons]
Name: "{group}\Quackle 0.98"; Filename: "{app}\Quackle.exe"; WorkingDir: "{app}"

[Registry]
Root: HKCU; Subkey: "Software\Quackle.org"
Root: HKCR; Subkey: ".gcg"; ValueType: string; ValueName: ""; ValueData: "QuackleGameFile"; Flags: uninsdeletevalue 
Root: HKCR; Subkey: "QuackleGameFile"; ValueType: string; ValueName: ""; ValueData: "Quackle Game File"; Flags: uninsdeletekey 
Root: HKCR; Subkey: "QuackleGameFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Quackle.exe,0" 
Root: HKCR; Subkey: "QuackleGameFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Quackle.exe"" ""%1"""


[Files]
Source: "quacker\release\Quackle.exe"; DestDir: "{app}"
Source: "LICENSE"; DestDir: "{app}"
Source: "mingwm10.dll"; DestDir: "{app}"
Source: "libgcc_s_dw2-1.dll"; DestDir: "{app}"
Source: "libstdc++-6.dll"; DestDir: "{app}"
Source: "QtCore4.dll"; DestDir: "{app}"
Source: "QtGui4.dll"; DestDir: "{app}"

Source: "data\themes\*"; DestDir: "{app}\data\themes"
Source: "data\alphabets\*"; DestDir: "{app}\data\alphabets"
Source: "data\lexica\*"; DestDir: "{app}\data\lexica"

Source: "data\strategy\ods5\*"; DestDir: "{app}\data\strategy\ods5"
Source: "data\strategy\twl06\*"; DestDir: "{app}\data\strategy\twl06"
Source: "data\strategy\twl98\*"; DestDir: "{app}\data\strategy\twl98"
