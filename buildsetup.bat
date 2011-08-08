@set innosetup="c:\Program Files\Inno Setup 5\ISCC.exe"

@set package=DBAdaptersBin
@echo Building %package%.iss...
@%innosetup% %package%.iss > %package%.log
@echo Done.

@set package=DBAdaptersSrc
@echo Building %package%.iss...
@%innosetup% %package%.iss > %package%.log
@echo Done.

release 326