; Installs Source Code for Additional Database Extensions Adapters

[Setup]
AppName=Source Code for Additional Database Extensions Adapters
AppVerName=Source Code for Additional Database Extensions Adapters v 3.26
AppCopyright=Copyright © 2003-2010, Context Software LLC.
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
Name: "NEXUS1"; Description: "Nexus 1 Adapter"; Types: Default
Name: "NEXUS2"; Description: "Nexus 2 Adapter"; Types: Default
Name: "NEXUS3"; Description: "Nexus 3 Adapter"; Types: Default
Name: "DBX"; Description: "DBX Adapter"; Types: Default
Name: "AnyDAC"; Description: "AnyDAC Adapter"; Types: Default
Name: "AnyDAC2"; Description: "AnyDAC2 Adapter"; Types: Default
Name: "SQLite"; Description: "SQLite Adapter"; Types: Default

[Files]
Source: "ABS\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\ABS"; Flags: recursesubdirs; Components: ABS
Source: "DBISAM3\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\DBISAM3"; Flags: recursesubdirs; Components: DBISAM3
Source: "DBISAM4\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\DBISAM4"; Flags: recursesubdirs; Components: DBISAM4
Source: "ElevateDB\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\ElevateDB"; Flags: recursesubdirs; Components: EDB
Source: "FBPlus\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\FIBPlus"; Flags: recursesubdirs; Components: FBPlus
Source: "MySQLCR\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\MySQLCR"; Flags: recursesubdirs; Components: MySQL
Source: "NEXUS1\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\NEXUS1"; Flags: recursesubdirs; Components: NEXUS1
Source: "NEXUS2\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\NEXUS2"; Flags: recursesubdirs; Components: NEXUS2
Source: "NEXUS3\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\NEXUS3"; Flags: recursesubdirs; Components: NEXUS3
Source: "DBX\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\DBX"; Flags: recursesubdirs; Components: DBX
Source: "AnyDAC\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\AnyDAC"; Flags: recursesubdirs; Components: AnyDAC
Source: "AnyDAC2\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\AnyDAC2"; Flags: recursesubdirs; Components: AnyDAC2
Source: "SQLite\*.*"; Excludes: "*.bat,*.dll,*.dcu"; DestDir: "{app}\SQLite"; Flags: recursesubdirs; Components: SQLite

