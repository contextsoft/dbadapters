@echo off

set buildto=..\
set dcu=..\dcu
set dpath=e:\Programs\Borland\Delphi7\Bin\dcc32.exe -Q
set usepkg=-LUrtl;vcl;dbrtl
set stdlib=e:\Programs\Borland\Delphi7\Lib;E:\projects\Context\DBExt\source;"..\..\vcl\source"

cd adapter
mkdir dcu
cd source

rem ==========================================================================
echo Building IB\FB adapter
rem ==========================================================================

set include=%stdlib%

%dpath% %usepkg% -N%dcu% -E%buildto% -I%include% -U%include% -B CtxIB.dpr

echo Done.

cd ..\..
