@echo off

set buildto=..\
set dcu=..\dcu
set dpath=e:\Programs\Borland\Delphi7\Bin\dcc32.exe -Q
set usepkg=-LUrtl;vcl;dbrtl
set stdlib=e:\Programs\Borland\Delphi7\Lib;E:\projects\Context\DBExt\source\;..\..\vcl\source

cd adapter
mkdir dcu
cd source

rem ==========================================================================
echo Building AnyDAC adapter
rem ==========================================================================

set include=%stdlib%;e:\projects\sdk\db\AnyDAC2\Dcu\D7

%dpath% %usepkg% -DCTXDBEXT30 -N%dcu% -E%buildto% -I%include% -U%include% -B CtxAnyDAC2.dpr

echo Done.

cd ..\..
