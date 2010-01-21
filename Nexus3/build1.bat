@echo off

set buildto=..\
set dcu=..\dcu
set dpath=F:\D7\Bin\dcc32.exe -Q
set usepkg=-LUrtl;vcl;dbrtl
set stdlib=F:\D7\Lib;F:\D7\SDK\Context\DBExt\source;..\..\vcl\source

cd adapter
mkdir dcu
cd source

rem ==========================================================================
echo Building Nexus 3 adapter
rem ==========================================================================

set include=%stdlib%;F:\D7\SDK\Context\DBEngines\NexusDB3\Delphi7

%dpath% %usepkg% -DNX_REMOTESERVER -N%dcu% -E%buildto% -I%include% -U%include% -B CtxNX3.dpr 

echo Done.

cd ..\..
