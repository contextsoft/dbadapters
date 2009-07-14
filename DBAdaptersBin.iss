; Installs Additional Database Extensions Adapters

[Setup]
AppName=Additional Database Extensions Adapters
AppVerName=Additional Database Extensions Adapters v.3.02
AppCopyright=Copyright © 2003-2009, Context Software LLC.
DefaultDirName={pf}\Context Software\DBAdapters3\bin
DefaultGroupName=Additional Database Extensions Adapters
Compression=lzma/max
SolidCompression=true
InternalCompressLevel=ultra
SourceDir=.\
OutputDir=-releases
OutputBaseFilename=dbadapters-bin-$$$

[Types]
Name: Default; Description: "Full installation"
Name: Custom; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "ABS"; Description: "Absolute Database Adapter"; Types: Default
Name: "DBISAM3"; Description: "DBISAM 3 Adapter"; Types: Default
Name: "DBISAM4"; Description: "DBISAM 4 Adapter"; Types: Default
Name: "EDB"; Description: "Elevate DB Adapter"; Types: Default
Name: "FBPlus"; Description: "FBPlus Adapter"; Types: Default
Name: "MySQL"; Description: "MySQL Adapter"; Types: Default
Name: "NEXUS3"; Description: "Nexus 3 Adapter"; Types: Default

[Tasks]
Name: RegAdapters; Description: "Register Adapters in the Context Database Designer";

[Files]
Source: "ABS\adapter\*.dll"; DestDir: "{app}"; Components: ABS
Source: "DBISAM3\adapter\*.dll"; DestDir: "{app}"; Components: DBISAM3
Source: "DBISAM4\adapter\*.dll"; DestDir: "{app}"; Components: DBISAM4
Source: "ElevateDB\adapter\*.dll"; DestDir: "{app}"; Components: EDB
Source: "FBPlus\adapter\*.dll"; DestDir: "{app}"; Components: FBPlus
Source: "MySQLCR\adapter\*.dll"; DestDir: "{app}"; Components: MySQL
Source: "NEXUS3\adapter\*.dll"; DestDir: "{app}"; Components: NEXUS3

[Registry]
Root: HKCU; Subkey: "Software\Context Software\DBDesigner3\Adapters"; ValueType: string; ValueName: "ABS"; ValueData: "{app}\CtxABS.dll"; Flags: uninsdeletevalue; Components: ABS; Check: IsRegAdapter('ABS')
Root: HKCU; Subkey: "Software\Context Software\DBDesigner3\Adapters"; ValueType: string; ValueName: "DBISAM3"; ValueData: "{app}\CtxDBISAM3.dll"; Flags: uninsdeletevalue; Components: DBISAM3; Check: IsRegAdapter('DBISAM3')
Root: HKCU; Subkey: "Software\Context Software\DBDesigner3\Adapters"; ValueType: string; ValueName: "DBISAM4"; ValueData: "{app}\CtxDBISAM4.dll"; Flags: uninsdeletevalue; Components: DBISAM4; Check: IsRegAdapter('DBISAM4')
Root: HKCU; Subkey: "Software\Context Software\DBDesigner3\Adapters"; ValueType: string; ValueName: "EDB"; ValueData: "{app}\CtxEDB.dll"; Flags: uninsdeletevalue; Components: EDB; Check: IsRegAdapter('EDB')
Root: HKCU; Subkey: "Software\Context Software\DBDesigner3\Adapters"; ValueType: string; ValueName: "FBPlus"; ValueData: "{app}\CtxIBFBYA.dll"; Flags: uninsdeletevalue; Components: FBPlus; Check: IsRegAdapter('FBPlus')
Root: HKCU; Subkey: "Software\Context Software\DBDesigner3\Adapters"; ValueType: string; ValueName: "MySQL"; ValueData: "{app}\CtxMySQL.dll"; Flags: uninsdeletevalue; Components: MySQL; Check: IsRegAdapter('MySQL')
Root: HKCU; Subkey: "Software\Context Software\DBDesigner3\Adapters"; ValueType: string; ValueName: "NEXUS3"; ValueData: "{app}\CtxNX3.dll"; Flags: uninsdeletevalue; Components: NEXUS3; Check: IsRegAdapter('NEXUS3')

[Code]
function IsRegAdapter(const CompName: String): Boolean;
begin
  Result := IsTaskSelected('RegAdapters') and IsComponentSelected(CompName);
end;

