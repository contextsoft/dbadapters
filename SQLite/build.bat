@echo off

set buildto=..\
set dcu=..\dcu
set dpath=e:\Programs\Borland\Delphi7\Bin\dcc32.exe -Q
set usepkg=-LUrtl;vcl;dbrtl
set stdlib=e:\Programs\Borland\Delphi7\Lib;e:\projects\Context\DBExt\source;..\..\vcl\source

cd adapter
mkdir dcu
cd source

rem ==========================================================================
echo Building SQLite adapter
rem ==========================================================================

set include=%stdlib%;e:\projects\sdk\db\ASQLite

%dpath% %usepkg% -N%dcu% -E%buildto% -I%include% -U%include% -B CtxSQLite.dpr

echo Done.

cd ..\..
