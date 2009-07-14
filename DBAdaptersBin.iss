; Installs Additional Database Extensions Adapters

[Setup]
AppName=Additional Database Extensions Adapters
AppVerName=Additional Database Extensions Adapters v.3.02
AppCopyright=Copyright © 2003-2009, Michael Baytalsky
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

[Files]
Source: "ABS\adapter\*.dll"; DestDir: "{app}"; Components: ABS
Source: "DBISAM3\adapter\*.dll"; DestDir: "{app}"; Components: DBISAM3
Source: "DBISAM4\adapter\*.dll"; DestDir: "{app}"; Components: DBISAM4
Source: "ElevateDB\adapter\*.dll"; DestDir: "{app}"; Components: EDB
Source: "FBPlus\adapter\*.dll"; DestDir: "{app}"; Components: FBPlus
Source: "MySQLCR\adapter\*.dll"; DestDir: "{app}"; Components: MySQL
Source: "NEXUS3\adapter\*.dll"; DestDir: "{app}"; Components: NEXUS3

