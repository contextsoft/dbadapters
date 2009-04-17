(******************************************************************************)
(*
(*  Context Database Extensions Suite (DBISAM)
(*
(*  Notification Expert.
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit DBISAMNotifyExpert;

interface

uses
  Windows, Classes, SysUtils, ToolsApi, Dialogs, DBISAMTb;

type
  TdbisamNotifyExpert = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  protected
    FAbortCompilation: Boolean;
    { Protected declarations }
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
    function OnDbisamSession(Session: TDBISAMSession): Boolean;
  end;

procedure Register;

implementation

{$I dbisamvr.inc}

var
  NotifierIndex: Integer;

resourcestring
  SConfirmDisconnect = 'A session is connected. Disconnect before compiling?'#13#10'Session: ';

procedure Register;
begin
  NotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(TdbisamNotifyExpert.Create);
end;

function TdbisamNotifyExpert.OnDbisamSession(Session: TDBISAMSession): Boolean; 
begin
  Result := True;
  if Session.Active then
  begin
    case MessageBox(0, PChar(SConfirmDisconnect + Session.SessionName),
      PChar('Warning'), MB_YESNOCANCEL or MB_ICONEXCLAMATION)
    of
      IDYES: Session.Active := False;
      IDNO:;
      IDCANCEL: begin
        FAbortCompilation := True;
        Result := False;
      end;
    end;
  end;
end;

{ TdbisamNotifyExpert }

procedure TdbisamNotifyExpert.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
var
  I: Integer;
begin
  // Check all instances of databases and see if any are connected.
  // Ask to a) disconnect before compiling, b) abort compilation c) ignore
  // dbSchema, dbisamExt
  FAbortCompilation := False;
  {$IFDEF DBISAM_V4}
  for I := 0 to Engine.SessionCount - 1 do
    if not OnDbisamSession(Engine.Sessions[I]) then break;
  {$ELSE}
  for I := 0 to Sessions.Count - 1 do
    if not OnDbisamSession(Sessions[I]) then break;
  {$ENDIF}
  Cancel := FAbortCompilation;
end;

procedure TdbisamNotifyExpert.AfterCompile(Succeeded: Boolean);
begin
  // Nothing here
end;

procedure TdbisamNotifyExpert.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
  // Nothing here
end;

initialization
  NotifierIndex := -1;
finalization
  if NotifierIndex <> -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(NotifierIndex);
end.


