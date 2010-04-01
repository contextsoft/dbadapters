@echo off

set buildto=..\
set dcu=..\dcu
set dpath="e:\programs\Borland\Delphi7\Bin\dcc32.exe" -Q
set usepkg=-LUrtl;vcl;dbrtl
set stdlib=e:\programs\Borland\Delphi7\Lib;E:\projects\Context\DBExt\source;..\..\vcl\source

cd adapter
mkdir dcu
cd source

rem ==========================================================================
echo Building EDB adapter
rem ==========================================================================

set include="%stdlib%;e:\projects\sdk\db\ElevateDB2\Delphi7\code"

%dpath% %usepkg% -N%dcu% -E%buildto% -I%include% -U%include% -B CtxEDB.dpr 

echo Done.

cd ..\..
