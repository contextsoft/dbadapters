@echo off

set buildto=..\
set dcu=..\dcu
set dpath=e:\Programs\Borland\Delphi7\Bin\dcc32.exe -Q
set usepkg=-LUrtl;vcl;dbrtl
set stdlib=e:\Programs\Borland\Delphi7\Lib;E:\projects\Context\DBExt\source;..\..\vcl\source

cd adapter
mkdir dcu
cd source

rem ==========================================================================
echo Building dbisam 3 adapter
rem ==========================================================================

set include=%stdlib%;"e:\projects\sdk\db\dbisam3\d7\code"

%dpath% %usepkg% -N%dcu% -E%buildto% -I%include% -U%include% -B CtxDBISAM3.dpr

echo Done.

cd ..\..
