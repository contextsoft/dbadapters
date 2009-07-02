@echo off

set ver=%1

if "%1" == "" goto _exit

call pack1.bat abs %ver%
call pack1.bat dbisam3 %ver%
call pack1.bat dbisam4 %ver%
call pack1.bat nexus1 %ver%
call pack1.bat nexus2 %ver%
call pack1.bat nexus3 %ver%
call pack1.bat fbplus %ver%
call pack1.bat mysqlcr %ver%
call pack1.bat elevatedb %ver%


:_exit