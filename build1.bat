@echo off

set innosetup="e:\Programs\Inno Setup 5\ISCC.exe"

cd %1
echo Building dbext_%1.iss...
%innosetup% dbext_%1.iss > dbext_%1.log
cd ..
