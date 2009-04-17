(******************************************************************************)
(*
(*  Context Database Extensions Suite (DBISAM)
(*
(*  TDBISAMEvents component. Extends fucntionality of TCustomApplicationEvents.
(*  Allows to handle DBISAM specific errors and substitute user friendly
(*  error messages.
(*
(*  Copyright (c) 2004-2006 Michael Baytalsky
(*
(******************************************************************************)
unit DBISAMEv;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  DB, DBISAMTb, DBISAMCn, AppEvnts, Dialogs;

type

  {:$ TDBISAMEvents intercepts DBISAM-specific as well as other application-level events. }
  {:: How to use: <br> }
  {:: 1. Place it on a form or a Data Module. You may use one component }
  {:: per application (and then place it on any form or data module, that }
  {:: is accessible throughout the application) or you may use one }
  {:: component per form (and then place it on that form - this is }
  {:: useful when you want to define specific behaviour for this }
  {:: particular form).<br> }
  {:: 2. Setup published properties (see description above).<br> }
  {:: 3. If you choose to use one component per application, then for each }
  {:: form, where you want to handle DBISAM events and errors, place }
  {:: a call to TDBISAMEvents.HookUpToDataSets(Self); }
  {:: into FormCreate procedure. }
  TDBISAMEvents = class(TCustomApplicationEvents)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FMessages: TStrings;
    FAskRetryMessage: String;  
    FOnException: TExceptionEvent; // Saved previos exception handler
    FAskRetryOnLockError: Boolean;
    FIgnoreRecordChangedError: Boolean;
    FAutoHookUpToDBISAMDataSets: Boolean;
    FCancelIfDispatched: Boolean;

    procedure SetMessages(const Value: TStrings);
    procedure DoOnException(Sender: TObject; E: Exception);
    procedure Loaded; override;
  public
    { Public declarations }
    {:$ Creates an instance of TDBISAMEvents component. }
    constructor Create(AOwner: TComponent); override;
    {:$ Destroys the instance of TDBISAMEvents component. }
    destructor Destroy; override;
    {:$ Returns the message text for the specified ErrorCode. }
    function GetMessage(E: EDBISAMEngineError; ErrorCode: Integer = 0): String;
    {:$ Returns the message text for the specified ErrorCode. }
    function GetMessageStr(ErrorCode: Integer): String;
    {:$ Hooks up EventHandler to all the childs (inherited from TDBISAMDataSet) }
    {:$ of the Component (Component is usually a form). }
    {:: This method allows to hook up EventHandler component, }
    {:: located on the central data module, to dataset components on the }
    {:: current form. Usually, you'd place a code like this into }
    {:: the FormCreate method of any form, that requires DBISAM Event }
    {:: Handling. <br> }
    {:: Example: <br> }
    {:: procedure TForm1.FormCreate(Sender: TObject); <br>}
    {:: begin <br>}
    {::   // DataModule1 is a main data module <br>}
    {::   DataModule1.DBISAMEvents.HookUpToDataSets(Self); <br>}
    {:: end; <br>}
    procedure HookUpToDataSets(Component: TComponent); overload;
    {:$ Hooks up EventHandler to the TDBISAMDataSet components passed in the }
    {:$ DataSets array. }
    {:: This method allows to hook up EventHandler component, }
    {:: located on the central data module, to dataset components on the }
    {:: current form. Usually, you'd place a code like this into }
    {:: the FormCreate method of any form, that requires DBISAM Event }
    {:: Handling. <br> }
    {:: Example: <br> }
    {:: procedure TForm1.FormCreate(Sender: TObject); <br>}
    {:: begin <br>}
    {::   // DataModule1 is a main data module <br>}
    {::   DataModule1.DBISAMEvents.HookUpToDataSets([Table1, Table2, Query1]); <br>}
    {:: end; <br>}
    procedure HookUpToDataSets(const DataSets: array of TDBISAMDataSet); overload;
    {:$ Handles both lock and record changed errors for the DataSet according to }
    {:$ the properties of TDBISAMEvents. }
    procedure HandleErrors(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
    {:$ Handles lock errors for the DataSet according to }
    {:$ the properties of TDBISAMEvents. }
    procedure HandleLockError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
    {:$ Handles record changed errors for the DataSet according to }
    {:$ the properties of TDBISAMEvents. }
    procedure HandleRecordChangedError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
    {:$ Handles DBISAMEngineError exception and displays the appropriate message, as defined in Messages property.  }
    procedure HandleDBISAMEngineError(Sender: TObject; E: Exception);
  published
    { Published declarations }
    {:$ Allows to overwrite default messages for different error codes. }
    {:: The format is: <br> }
    {::   ERROR_CODE=MESSAGE <br> }
    {::    or <br> }
    {::   ERROR_CODE=. // Ignore this error and don't show any mesasges <br> }
    {:: Example: <br> }
    {::   10258=The record you are trying to access is locked by another user <br> }
    {::   10241=The table you are trying to access (%table%) is locked by another user <br> }
    {::   8708=The record you are trying to access has been deleted or changed by another user <br> }
    {::   9729=A record with these key values already exists in the table %table% <br> }
    {::   11280=. <br> }
    {::   11276=. <br> }
    {:: <br> }
    {:: DBISAM Errors usually contains additional information, like table }
    {:: name, user name, database name, etc. All this parameters are accessible }
    {:: through the use of %parameter% format (see above for 10241 and 9729). <br> }
    {:: The following parameters are available: <br> }
    {::    %errorcode%, %database%, %field%, %index%, %message%, <br> }
    {::    %remote%, %table%, %user%, %application% }
    property Messages: TStrings read FMessages write SetMessages;
    {:: The message that will be added to the error message to ask user }
    {:: if he/she wants to retry to lock the record, that is currently }
    {:: alocked by another user. }
    property AskRetryMessage: String read FAskRetryMessage write FAskRetryMessage;
    {:: If True EventHandler will ask the user about whether to retry }
    {:: to lock record, that is currently locked by another user. }
    property AskRetryOnLockError: Boolean read FAskRetryOnLockError write FAskRetryOnLockError default True;
    {:: If True then DBISAM_KEYORRECDELETED error will be ignored and }
    {:: the warning will not show. }
    property IgnoreRecordChangedError: Boolean read FIgnoreRecordChangedError write FIgnoreRecordChangedError default True;
    {:: If True the component will automatically hook up itself to all }
    {:: DBISAM Data Sets located on the same form or data module with the }
    {:: component. }
    property AutoHookUpToDBISAMDataSets: Boolean read FAutoHookUpToDBISAMDataSets write FAutoHookUpToDBISAMDataSets default False;
    {:: If True the component will cancel any dispatched event, otherwise }
    {:: it will pass it to other DBISAMEvents and ApplicationEvents components, }
    {:: registered in the application. }
    property CancelIfDispatched: Boolean read FCancelIfDispatched write FCancelIfDispatched default True;
    {:$ Occurs when an action’s Execute method is called and its action list has not already handled it. }
    property OnActionExecute;
    {:$ Occurs when an action’s Update method is called and its action list has not already handled it. }
    property OnActionUpdate;
    {:$ Occurs when an application becomes active. }
    property OnActivate;
    {:$ Occurs when an application becomes inactive. }
    property OnDeactivate;
    {:$ Occurs when an unhandled exception occurs in the application. }
    {:: Use OnException to change the default behavior that occurs when an exception
    {:: is not handled by application code. The OnException event handler is called
    {:: automatically in the application’s HandleException method. }
    property OnException read FOnException write FOnException;
    {:$ Occurs when an application becomes idle.}
    property OnIdle;
    {:$ Occurs when the application receives a request for help. }
    property OnHelp;
    {:$ Occurs when the mouse pointer moves over a control or menu item that can display a Help Hint. }
    property OnHint;
    {:$ Occurs when the application receives a Windows message. }
    property OnMessage;
    {:$ Occurs when an application is minimized. }
    property OnMinimize;
    {:$ Occurs when the previously minimized application is restored to its normal size. }
    property OnRestore;
    {:$ Occurs when the application is about to display the hint window for a Help Hint. }
    property OnShowHint;
    {:$ Occurs when the user presses a key (before the OnKeyDown event). }
    property OnShortCut;
  end;

implementation

const
  strDefaultAskRetry = 'Would you like to try again?';
  strIgnore = '.';
  strDefault = 'default';

(****************************************************************************)
(* Helper routings
(****************************************************************************)
function PosFrom(const SubStr, Str: String; FromPos: Integer): Integer;
var P: PChar;
begin
  P := StrPos(@PChar(Str)[FromPos - 1], PChar(SubStr));
  if P <> nil then
    Result := Longint(P) - Longint(PChar(Str)) + 1
  else Result := 0;
end;

(****************************************************************************)
function ReplaceFields(const Str: String): String;
const
  cFields: array [0..8] of String = ('%errorcode%', '%database%', '%field%',
    '%index%', '%message%','%remote%', '%table%', '%user%','%application%');
var
  I, OpenPos, ClosePos: Integer;
  Fld, Res: String;
begin
  Result := '';
  Res := Str;
  ClosePos := 1;
  repeat
    // Look for next open bracket from last Close pos
    OpenPos := PosFrom('%', Res, ClosePos);
    if OpenPos = 0 then break;
    Result := Result + copy(Res, ClosePos, OpenPos - ClosePos);
    // Open bracket found. Look for Close
    ClosePos := PosFrom('%', Res, OpenPos + 1);
    if ClosePos = 0 then break;
    Inc(ClosePos, 1);
    // Close Bracket Found. Extract/Replace field
    Fld := LowerCase(copy(Res, OpenPos, ClosePos - OpenPos));
    if Length(Fld) > 3 then
      for I := 0 to 7 do
        if Fld = cFields[I] then begin
          Fld := '%'+IntToStr(I)+':s';
          Break;
        end;
    Result := Result + Fld;
  until false;
  Result := Result + copy(Res, ClosePos, Length(Res));
end;

(****************************************************************************)
(* TDBISAMErrorHandler Implementation
(******************************************************************************)
constructor TDBISAMEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited OnException := DoOnException;
  FMessages := TStringList.Create;
  FAskRetryMessage := strDefaultAskRetry;
  FAskRetryOnLockError := True;
  FIgnoreRecordChangedError := True;
  AutoHookUpToDBISAMDataSets := False;
  CancelIfDispatched := True;
end;

(******************************************************************************)
destructor TDBISAMEvents.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

(******************************************************************************)
procedure TDBISAMEvents.DoOnException(Sender: TObject;
  E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Sender, E)
  else if E is EDBISAMEngineError then
    HandleDBISAMEngineError(Sender, E)
  else begin
    Application.ShowException(E);
    CancelDispatch;
  end;
end;

(******************************************************************************)
procedure TDBISAMEvents.HandleDBISAMEngineError(Sender: TObject;
  E: Exception);
var Msg: String;
begin
  if E is EDBISAMEngineError then
  begin
    Msg := GetMessage(EDBISAMEngineError(E));
    if (Msg <> strIgnore) and (Msg <> '') then
    begin
      E.Message := Msg;
      Application.ShowException(E);
    end;
    // All messages here considered to be dispatched
    // Ignored messages are still 'dispatched'
    // If creating a chain of event handlers is desired, default
    // entry should be set to ignore (i.e. default= or default=.)
    // and CancelIfDispatched should be set to 'False'
    // This will allow message to propagate to the next event handler
    if CancelIfDispatched then
      CancelDispatch;
  end;
end;

(******************************************************************************)
function TDBISAMEvents.GetMessage(E: EDBISAMEngineError; ErrorCode: Integer = 0): String;
var
  I: Integer;
  Temp: String;
begin
  if ErrorCode = 0 then
    ErrorCode := E.ErrorCode;

  Temp := IntToStr(ErrorCode);
  I := Messages.IndexOfName(Temp);
  // if error code not found, then locate default message
  if I < 0 then
    I := Messages.IndexOfName(strDefault);
  // if default message not found, then display original message
  if I < 0 then
    Result := E.Message
  else begin
    // otherwise get message text from Messages 
    Result := Copy(Messages[I], Length(Temp) + 2, MaxInt);
    if (Result = strIgnore) or (Result = '') then
      exit;
    // format message
    with E do
      Result := Format(ReplaceFields(Result), [
        IntToStr(ErrorCode),   { %errorcode%    %0:s }
        ErrorDatabaseName,     { %database%     %1:s }
        ErrorFieldName,        { %field%        %2:s }
        ErrorIndexName,        { %index%        %3:s }
        ErrorMessage,          { %message%      %4:s }
        ErrorRemoteName,       { %remote%       %5:s }
        ErrorTableName,        { %table%        %6:s }
        ErrorUserName,         { %user%         %7:s }
        Application.Title]);   { %application%  %8:s }
  end;
  // Append '.' if necessary
  if (Result <> '') and (AnsiLastChar(Result) > '.') then
    Result := Result + '.';
end;

(******************************************************************************)
function TDBISAMEvents.GetMessageStr(ErrorCode: Integer): String;
begin
  Result := Messages.Values[IntToStr(ErrorCode)];
end;

(******************************************************************************)
procedure TDBISAMEvents.HandleErrors(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  if (E is EDBISAMEngineError) then
  begin
    if FAskRetryOnLockError then
      HandleLockError(DataSet, E, Action);
    if FIgnoreRecordChangedError then
      HandleRecordChangedError(DataSet, E, Action);
  end;
end;

(******************************************************************************)
procedure TDBISAMEvents.HandleLockError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  if (E is EDBISAMEngineError) then
  begin
    case EDBISAMEngineError(E).ErrorCode of
      DBISAM_LOCKED, DBISAM_RECLOCKFAILED:
        if MessageDlg(GetMessage(EDBISAMEngineError(E)) + #13#10 + AskRetryMessage,
          mtWarning, [mbYes, mbNo], 0) = mrYes
        then Action := daRetry
        else Action := daAbort;
    end;
  end;
end;

(******************************************************************************)
procedure TDBISAMEvents.HandleRecordChangedError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
var
  ID1, ID2: TBookmark;
begin
  if (E is EDBISAMEngineError) then
    if EDBISAMEngineError(E).ErrorCode = DBISAM_KEYORRECDELETED then
      with DataSet do
      begin
        ID1 := GetBookmark;
        Refresh;
        ID2 := GetBookmark;
        try
          if CompareBookmarks(ID1, ID2) = 0 then
            Action := daRetry
          else Action := daFail
        finally
          FreeBookmark(ID1);
          FreeBookmark(ID2);
        end;
      end;
end;

(******************************************************************************)
procedure TDBISAMEvents.SetMessages(const Value: TStrings);
begin
  if FMessages <> Value then
    FMessages.Assign(Value);
end;

(******************************************************************************)
procedure TDBISAMEvents.Loaded;
begin
  inherited;
  if FAutoHookUpToDBISAMDataSets and Assigned(Owner) then
    HookUpToDataSets(Owner);
end;

(******************************************************************************)
procedure TDBISAMEvents.HookUpToDataSets(Component: TComponent);
var
  I: Integer;
begin
  for I := 0 to Component.ComponentCount - 1 do
  begin
    if Component.Components[I].InheritsFrom(TDBISAMDataSet) then
      with TDBISAMDataSet(Component.Components[I]) do
      begin
        if not Assigned(OnEditError) then
          OnEditError := HandleErrors;
        if not Assigned(OnDeleteError) then
          OnDeleteError := HandleErrors;
        if not Assigned(OnPostError) then
          OnPostError := HandleErrors;
      end;
  end;
end;

(******************************************************************************)
procedure TDBISAMEvents.HookUpToDataSets(const DataSets: array of TDBISAMDataSet);
var
  I: Integer;
begin
  for I := Low(DataSets) to High(DataSets) do
    with DataSets[I] do
    begin
      if not Assigned(OnEditError) then
        OnEditError := HandleErrors;
      if not Assigned(OnDeleteError) then
        OnDeleteError := HandleErrors;
      if not Assigned(OnPostError) then
        OnPostError := HandleErrors;
    end;
end;

end.
