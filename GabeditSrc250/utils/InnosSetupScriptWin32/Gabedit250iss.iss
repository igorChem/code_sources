; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
AppName=Gabedit
AppVerName=Gabedit 2.5.0
AppPublisher=Abdul-Rahman Allouche
AppPublisherURL=http://gabedit.sf.net
AppSupportURL=http://gabedit.sf.net
AppUpdatesURL=http://gabedit.sf.net
DefaultDirName=C:\Gabedit250
DefaultGroupName=Gabedit
LicenseFile=C:\Users\allou\tmp\Gabedit250\License
SetupIconFile=C:\Users\allou\tmp\Gabedit250\Gabedit48.ico
OutputDir=C:\Users\allou\tmp
OutputBaseFilename=setupGabedit250
Compression=lzma
SolidCompression=yes
PrivilegesRequired=none

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "C:\Users\allou\tmp\Gabedit250\gabedit.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\Users\allou\tmp\Gabedit250\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\Gabedit"; Filename: "{app}\gabedit.exe"
Name: "{group}\Gabedit"; Filename: "{app}\gabedit.exe"; IconFilename: "{app}\Gabedit48.ico"
Name: "{userdesktop}\Gabedit"; Filename: "{app}\gabedit.exe"; Tasks: desktopicon


[Run]
Filename: "{app}\gabedit.exe"; Description: "{cm:LaunchProgram,Gabedit}"; Flags: nowait postinstall skipifsilent

