unit IApplication;

interface
uses
  SysUtils, Classes, PCLogWriter, PCMemoryLog, PCLog, Dialogs,
  DB;

type
  TInternalLogger = class;  
  IMainDataModule = Interface(IUnknown)
    ['{0D73EA0E-4FD9-4240-82ED-569FF6E4C097}']
    procedure SetMsgID(const Value: Cardinal); safecall;
    function GetInternalLogger: TInternalLogger; safecall;
    
    function GetOpenDialog: TOpenDialog; safecall;
    function GetSaveDialog: TSaveDialog; safecall;   

    {Shortcut untuk mengirim message pada MainForm melalui DataModule}
    property AppMsgID: Cardinal write SetMsgID;
    property Logger: TInternalLogger read GetInternalLogger;
  end;
  
  TInternalLogger = class(TComponent)
  private
    FLogger: TPCLogWriter;
    FErrorExist: boolean;
    FMainDataModule: IMainDataModule;
    procedure SetMainDataModule(const Value: IMainDataModule);
  protected
    procedure SetLogger(ALogger: TPCLogWriter);
  public
    procedure StartLog;
    procedure Note(const Text: String); overload;
    procedure Log(const Text: String); overload;
    procedure Warning(const Text: String); overload;
    procedure Debug(const Text: String); overload;
    procedure Error(const Text: String; const AStopOnError: boolean = False); overload;
    procedure Error(AExcept: Exception; const AStopOnError: boolean = False); overload;

    procedure Note(const Text: String; const Args: array of const); overload;
    procedure Log(const Text: String; const Args: array of const); overload;
    procedure Warning(const Text: String; const Args: array of const); overload;
    procedure Debug(const Text: String; const Args: array of const); overload;
    procedure Error(const Text: String; const Args: array of const; 
      const AStopOnError: boolean = False); overload;

    procedure NoteWhen(const ACondition: boolean; const Text: String;
      const Args: array of const);
    procedure LogWhen(const ACondition: boolean; const Text: String;
      const Args: array of const);
    procedure WarningWhen(const ACondition: boolean; const Text: String;
      const Args: array of const);
    procedure DebugWhen(const ACondition: boolean; const Text: String;
      const Args: array of const);
    procedure ErrorWhen(const ACondition: boolean; const Text: String;
      const Args: array of const; const AStopOnError: boolean = False);

    procedure StopOnError;
    property MainDataModule: IMainDataModule read FMainDataModule write SetMainDataModule;
  end;

procedure SetMainDataModule(MainDM: IMainDataModule);
function GetMainDataModule: IMainDataModule;

implementation

uses CSPConsts;

var
  FMainDataModuleInterface: IMainDataModule;

procedure SetMainDataModule(MainDM: IMainDataModule);
begin
  FMainDataModuleInterface := MainDM;
end;

function GetMainDataModule: IMainDataModule;
begin
  Result := FMainDataModuleInterface;
end;

{ TInternalLogger }

procedure TInternalLogger.Debug(const Text: String;
  const Args: array of const);
begin
  FLogger.Log(llDebug, Text, Args);
end;

procedure TInternalLogger.Debug(const Text: String);
begin
  FLogger.Log(llDebug, Text);
end;

procedure TInternalLogger.DebugWhen(const ACondition: boolean;
  const Text: String; const Args: array of const);
begin
  if ACondition then Debug(Text, Args);
end;

procedure TInternalLogger.Error(const Text: String;
  const Args: array of const; const AStopOnError: boolean);
begin
  FErrorExist := True;
  FLogger.Log(llError, Text, Args);
  if AStopOnError then
    StopOnError;
end;

procedure TInternalLogger.Error(const Text: String;
  const AStopOnError: boolean);
begin
  FErrorExist := True;
  FLogger.Log(llError, Text);
  if AStopOnError then
    StopOnError;
end;

procedure TInternalLogger.Error(AExcept: Exception;
  const AStopOnError: boolean);
var
  AStr: string;
begin
  if AExcept is EDatabaseError then
  begin
    AStr := AExcept.Message;
    Error(AStr, AStopOnError)
  end else
    Error(AExcept.Message, AStopOnError);  
end;

procedure TInternalLogger.ErrorWhen(const ACondition: boolean;
  const Text: String; const Args: array of const;
  const AStopOnError: boolean);
begin
  if ACondition then Error(Text, Args, AStopOnError);
end;

procedure TInternalLogger.Log(const Text: String;
  const Args: array of const);
begin
  FLogger.Log(llNormal, Text, Args);
end;

procedure TInternalLogger.Log(const Text: String);
begin
  FLogger.Log(llNormal, Text);
end;

procedure TInternalLogger.LogWhen(const ACondition: boolean;
  const Text: String; const Args: array of const);
begin
  if ACondition then Log(Text, Args);
end;

procedure TInternalLogger.Note(const Text: String;
  const Args: array of const);
begin
  FLogger.Log(llNote, Text, Args);
end;

procedure TInternalLogger.Note(const Text: String);
begin
  FLogger.Log(llNote, Text);
end;

procedure TInternalLogger.NoteWhen(const ACondition: boolean;
  const Text: String; const Args: array of const);
begin
  if ACondition then Note(Text, Args);
end;

procedure TInternalLogger.SetLogger(ALogger: TPCLogWriter);
begin
  FLogger := ALogger;
end;

procedure TInternalLogger.SetMainDataModule(const Value: IMainDataModule);
begin
  FMainDataModule := Value;
end;

procedure TInternalLogger.StartLog;
begin
  FLogger.Target.Active := True;
  FMainDataModule.AppMsgID := CMD_USER + 1001;
  FErrorExist := False;  
end;

procedure TInternalLogger.StopOnError;
begin
  if FErrorExist then
  begin
    FMainDataModule.AppMsgID := CMD_USER + 1000;
    raise Exception.Create('One or more error has been raised during '+
      'processing current task, please check the log');
  end;
end;

procedure TInternalLogger.Warning(const Text: String;
  const Args: array of const);
begin
  FLogger.Log(llWarning, Text, Args);
end;

procedure TInternalLogger.Warning(const Text: String);
begin
  FLogger.Log(llWarning, Text);
end;

procedure TInternalLogger.WarningWhen(const ACondition: boolean;
  const Text: String; const Args: array of const);
begin
  if ACondition then Warning(Text, Args);
end;


end.
