@echo off

set buildto=..\
set dcu=..\dcu
set dpath=e:\programs\Borland\Delphi7\Bin\dcc32.exe -Q
set usepkg=-LUrtl;vcl;dbrtl
set stdlib=e:\programs\Borland\Delphi7\Lib;E:\projects\Context\DBExt\source;..\..\vcl\source

cd adapter
mkdir dcu
cd source

rem ==========================================================================
echo Building Absolute Database adapter
rem ==========================================================================

set include=%stdlib%;"e:\Programs\DBEngines\ABS\Lib\Delphi 7"

%dpath% %usepkg% -N%dcu% -E%buildto% -I%include% -U%include% -B CtxABS.dpr

echo Done.

cd ..\..
