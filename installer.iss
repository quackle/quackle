; Script for creating Quackle Windows installer
; For Inno Setup 5.1

[Setup]
AppName=Quackle
AppVerName=Quackle 0.97 [Beta]
DefaultDirName={pf}\Quackle
DefaultGroupName=Quackle
ChangesAssociations=yes

[Icons]
Name: "{group}\Quackle 0.97"; Filename: "{app}\Quackle.exe"; WorkingDir: "{app}"

[Registry]
Root: HKCU; Subkey: "Software\Quackle.org"
Root: HKCR; Subkey: ".gcg"; ValueType: string; ValueName: ""; ValueData: "QuackleGameFile"; Flags: uninsdeletevalue 
Root: HKCR; Subkey: "QuackleGameFile"; ValueType: string; ValueName: ""; ValueData: "Quackle Game File"; Flags: uninsdeletekey 
Root: HKCR; Subkey: "QuackleGameFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Quackle.exe,0" 
Root: HKCR; Subkey: "QuackleGameFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Quackle.exe"" ""%1"""


[Files]
Source: "quacker\release\quacker.exe"; DestDir: "{app}"; DestName: "Quackle.exe"
Source: "LICENSE"; DestDir: "{app}"
Source: "mingwm10.dll"; DestDir: "{app}"
Source: "libgcc_s_dw2-1.dll"; DestDir: "{app}"
Source: "QtCore4.dll"; DestDir: "{app}"
Source: "QtGui4.dll"; DestDir: "{app}"

Source: "data\alphabets\english.quackle_alphabet"; DestDir: "{app}\data\alphabets"
Source: "data\alphabets\english_super.quackle_alphabet"; DestDir: "{app}\data\alphabets"
Source: "data\alphabets\french.quackle_alphabet"; DestDir: "{app}\data\alphabets"
Source: "data\alphabets\greek.quackle_alphabet"; DestDir: "{app}\data\alphabets"
Source: "data\alphabets\korean.quackle_alphabet"; DestDir: "{app}\data\alphabets"
;Source: "data\alphabets\russian.quackle_alphabet"; DestDir: "{app}\data\alphabets"
;Source: "data\alphabets\tuvan.quackle_alphabet"; DestDir: "{app}\data\alphabets"

Source: "data\lexica\csw12.dawg"; DestDir: "{app}\data\lexica"
Source: "data\lexica\csw12.gaddag"; DestDir: "{app}\data\lexica"
Source: "data\lexica\cswapr07.dawg"; DestDir: "{app}\data\lexica"
Source: "data\lexica\cswapr07.gaddag"; DestDir: "{app}\data\lexica"
Source: "data\lexica\greek.dawg"; DestDir: "{app}\data\lexica"
Source: "data\lexica\korean.dawg"; DestDir: "{app}\data\lexica"
Source: "data\lexica\ods5.dawg"; DestDir: "{app}\data\lexica"
;Source: "data\lexica\russian.dawg"; DestDir: "{app}\data\lexica"
Source: "data\lexica\sowpods.dawg"; DestDir: "{app}\data\lexica"
;Source: "data\lexica\tuvan.dawg"; DestDir: "{app}\data\lexica"
Source: "data\lexica\twl06.dawg"; DestDir: "{app}\data\lexica"
Source: "data\lexica\twl06.gaddag"; DestDir: "{app}\data\lexica"
;Source: "data\lexica\twl06_wild.dawg"; DestDir: "{app}\data\lexica"
Source: "data\lexica\twl98.dawg"; DestDir: "{app}\data\lexica"

Source: "data\strategy\cswapr07\superleaves"; DestDir: "{app}\data\strategy\cswapr07"
Source: "data\strategy\greek\superleaves"; DestDir: "{app}\data\strategy\greek"
Source: "data\strategy\greek\syn2"; DestDir: "{app}\data\strategy\greek"
Source: "data\strategy\greek\worths"; DestDir: "{app}\data\strategy\greek"
Source: "data\strategy\korean\superleaves"; DestDir: "{app}\data\strategy\korean"
Source: "data\strategy\korean\syn2"; DestDir: "{app}\data\strategy\korean"
Source: "data\strategy\korean\worths"; DestDir: "{app}\data\strategy\korean"
Source: "data\strategy\ods5\superleaves"; DestDir: "{app}\data\strategy\ods5"
Source: "data\strategy\ods5\worths"; DestDir: "{app}\data\strategy\ods5"
;Source: "data\strategy\russian\superleaves"; DestDir: "{app}\data\strategy\russian"
;Source: "data\strategy\russian\syn2"; DestDir: "{app}\data\strategy\russian"
;Source: "data\strategy\russian\worths"; DestDir: "{app}\data\strategy\russian"
;Source: "data\strategy\tuvan\superleaves"; DestDir: "{app}\data\strategy\tuvan"
;Source: "data\strategy\tuvan\syn2"; DestDir: "{app}\data\strategy\tuvan"
;Source: "data\strategy\tuvan\worths"; DestDir: "{app}\data\strategy\tuvan"
Source: "data\strategy\twl06\bogowin"; DestDir: "{app}\data\strategy\twl06"
Source: "data\strategy\twl06\superleaves"; DestDir: "{app}\data\strategy\twl06"
Source: "data\strategy\twl06\syn2"; DestDir: "{app}\data\strategy\twl06"
Source: "data\strategy\twl06\vcplace"; DestDir: "{app}\data\strategy\twl06"
Source: "data\strategy\twl06\worths"; DestDir: "{app}\data\strategy\twl06"
Source: "data\strategy\twl98\worths"; DestDir: "{app}\data\strategy\twl98"
