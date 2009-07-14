; Installs Source Code for Additional Database Extensions Adapters

[Setup]
AppName=Source Code for Additional Database Extensions Adapters
AppVerName=Source Code for Additional Database Extensions Adapters v 3.02
AppCopyright=Copyright © 2003-2009, Context Software LLC.
DefaultDirName={pf}\Context Software\DBAdapters3
DefaultGroupName=Source Code for Additional Database Extensions Adapters
Compression=lzma/max
SolidCompression=true
InternalCompressLevel=ultra
SourceDir=.\
OutputDir=-releases
OutputBaseFilename=dbadapters-src-$$$

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
Source: "ABS\*.*"; Excludes: "*.dll,*.dcu"; DestDir: "{app}\ABS"; Flags: recursesubdirs; Components: ABS
Source: "DBISAM3\*.*"; Excludes: "*.dll,*.dcu"; DestDir: "{app}\DBISAM3"; Flags: recursesubdirs; Components: DBISAM3
Source: "DBISAM4\*.*"; Excludes: "*.dll,*.dcu"; DestDir: "{app}\DBISAM4"; Flags: recursesubdirs; Components: DBISAM4
Source: "ElevateDB\*.*"; Excludes: "*.dll,*.dcu"; DestDir: "{app}\ElevateDB"; Flags: recursesubdirs; Components: EDB
Source: "FBPlus\*.*"; Excludes: "*.dll,*.dcu"; DestDir: "{app}\FIBPlus"; Flags: recursesubdirs; Components: FBPlus
Source: "MySQLCR\*.*"; Excludes: "*.dll,*.dcu"; DestDir: "{app}\MySQLCR"; Flags: recursesubdirs; Components: MySQL
Source: "NEXUS3\*.*"; Excludes: "*.dll,*.dcu"; DestDir: "{app}\NEXUS3"; Flags: recursesubdirs; Components: NEXUS3

