unit frxACRComponents;

interface

{$I frx.inc}

uses
  Windows, Classes, SysUtils, frxClass, frxCustomDB, DB, ACRMain
{$IFDEF Delphi6}
, Variants
{$ENDIF}
{$IFDEF QBUILDER}
, fqbClass
{$ENDIF};


type
  TfrxACRComponents = class(TfrxDBComponents)
  private
    FDefaultDatabase: String;
    FDefaultSession: String;
    FOldComponents: TfrxACRComponents;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDescription: String; override;
  published
    property DefaultDatabase: String read FDefaultDatabase write FDefaultDatabase;
    property DefaultSession: String read FDefaultSession write FDefaultSession;
  end;

  TfrxACRDatabase = class(TfrxCustomDatabase)
  private
    FDatabase: TACRDatabase;
  protected
    function GetConnected: Boolean; override;
    function GetDatabaseName: String; override;
    procedure SetConnected(Value: Boolean); override;
    procedure SetDatabaseName(const Value: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: String; override;
    property Database: TACRDatabase read FDatabase;
  published
    property DatabaseName;
    property Connected;
  end;

  TfrxACRTable = class(TfrxCustomTable)
  private
    FTable: TACRTable;
    procedure SetDatabaseName(const Value: String);
    function GetDatabaseName: String;
    procedure SetSessionName(const Value: String);
    function GetSessionName: String;
  protected
    procedure SetMaster(const Value: TDataSource); override;
    procedure SetMasterFields(const Value: String); override;
    procedure SetIndexName(const Value: String); override;
    procedure SetIndexFieldNames(const Value: String); override;
    procedure SetTableName(const Value: String); override;
    function GetIndexName: String; override;
    function GetIndexFieldNames: String; override;
    function GetTableName: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: String; override;
    property Table: TACRTable read FTable;
  published
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property SessionName: String read GetSessionName write SetSessionName;
  end;

  TfrxACRQuery = class(TfrxCustomQuery)
  private
    FQuery: TACRQuery;
    procedure SetDatabaseName(const Value: String);
    function GetDatabaseName: String;
    procedure SetSessionName(const Value: String);
    function GetSessionName: String;
  protected
    procedure SetMaster(const Value: TDataSource); override;
    procedure SetSQL(Value: TStrings); override;
    function GetSQL: TStrings; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: String; override;
    procedure UpdateParams; override;
{$IFDEF QBUILDER}
    function QBEngine: TfqbEngine; override;
{$ENDIF}
    property Query: TACRQuery read FQuery;
  published
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property SessionName: String read GetSessionName write SetSessionName;
  end;

{$IFDEF QBUILDER}
  TfrxEngineACR = class(TfqbEngine)
  private
    FQuery: TACRQuery;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadTableList(ATableList: TStrings); override;
    procedure ReadFieldList(const ATableName: string; var AFieldList: TfqbFieldList); override;
    function ResultDataSet: TDataSet; override;
    procedure SetSQL(const Value: string); override;
  end;
{$ENDIF}

var
  ACRComponents: TfrxACRComponents;


implementation

uses
  frxACRRTTI,
{$IFNDEF NO_EDITORS}
  frxACREditor,
{$ENDIF}
  frxDsgnIntf, frxRes;


{ TfrxDBComponents }

constructor TfrxACRComponents.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultSession := 'Default';
  FOldComponents := ACRComponents;
  ACRComponents := Self;
end;

destructor TfrxACRComponents.Destroy;
begin
  if ACRComponents = Self then
    ACRComponents := FOldComponents;
  inherited;
end;

function TfrxACRComponents.GetDescription: String;
begin
  Result := 'Connection';
end;


{ TfrxACRDatabase }

constructor TfrxACRDatabase.Create(AOwner: TComponent);
begin
  inherited;
  FDatabase := TACRDatabase.Create(nil);
  Component := FDatabase;
end;

class function TfrxACRDatabase.GetDescription: String;
begin
  Result := frxResources.Get('obBDEDB');
end;

function TfrxACRDatabase.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TfrxACRDatabase.GetDatabaseName: String;
begin
  Result := FDatabase.DatabaseName;
end;

procedure TfrxACRDatabase.SetConnected(Value: Boolean);
begin
  BeforeConnect(Value);
  FDatabase.Connected := Value;
end;

procedure TfrxACRDatabase.SetDatabaseName(const Value: String);
begin
  FDatabase.DatabaseName := Value;
end;

{ TfrxACRTable }

constructor TfrxACRTable.Create(AOwner: TComponent);
begin
  FTable := TACRTable.Create(nil);
  DataSet := FTable;
  if ACRComponents <> nil then
  begin
    DatabaseName := ACRComponents.DefaultDatabase;
    SessionName := ACRComponents.DefaultSession;
  end;
  inherited;
end;

class function TfrxACRTable.GetDescription: String;
begin
  Result := frxResources.Get('obBDETb');
end;

function TfrxACRTable.GetDatabaseName: String;
begin
  Result := FTable.DatabaseName;
end;

function TfrxACRTable.GetSessionName: String;
begin
  Result := FTable.SessionName;
end;

procedure TfrxACRTable.SetDatabaseName(const Value: String);
begin
  FTable.DatabaseName := Value;
  DBConnected := True;
end;

procedure TfrxACRTable.SetSessionName(const Value: String);
begin
  FTable.SessionName := Value;
end;

function TfrxACRTable.GetIndexName: String;
begin
  Result := FTable.IndexName;
end;

function TfrxACRTable.GetIndexFieldNames: String;
begin
  Result := FTable.IndexFieldNames;
end;

function TfrxACRTable.GetTableName: String;
begin
  Result := FTable.TableName;
end;

procedure TfrxACRTable.SetIndexName(const Value: String);
begin
  FTable.IndexName := Value;
end;

procedure TfrxACRTable.SetIndexFieldNames(const Value: String);
begin
  FTable.IndexFieldNames := Value;
end;

procedure TfrxACRTable.SetTableName(const Value: String);
begin
  FTable.TableName := Value;
end;

procedure TfrxACRTable.SetMaster(const Value: TDataSource);
begin
  FTable.MasterSource := Value;
end;

procedure TfrxACRTable.SetMasterFields(const Value: String);
begin
  FTable.MasterFields := Value;
end;


{ TfrxACRQuery }

constructor TfrxACRQuery.Create(AOwner: TComponent);
begin
  FQuery := TACRQuery.Create(nil);
  Dataset := FQuery;
  if ACRComponents <> nil then
  begin
    DatabaseName := ACRComponents.DefaultDatabase;
    SessionName := ACRComponents.DefaultSession;
  end;
  inherited;
end;

class function TfrxACRQuery.GetDescription: String;
begin
  Result := frxResources.Get('obBDEQ');
end;

function TfrxACRQuery.GetDatabaseName: String;
begin
  Result := FQuery.DatabaseName;
end;

function TfrxACRQuery.GetSessionName: String;
begin
  Result := FQuery.SessionName;
end;

function TfrxACRQuery.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

procedure TfrxACRQuery.SetDatabaseName(const Value: String);
begin
  FQuery.DatabaseName := Value;
  DBConnected := True;
end;

procedure TfrxACRQuery.SetMaster(const Value: TDataSource);
begin
  FQuery.DataSource := Value;
end;

procedure TfrxACRQuery.SetSessionName(const Value: String);
begin
  FQuery.SessionName := Value;
end;

procedure TfrxACRQuery.SetSQL(Value: TStrings);
begin
  FQuery.SQL := Value;
end;

procedure TfrxACRQuery.UpdateParams;
begin
  frxParamsToTParams(Self, FQuery.Params);
end;

{$IFDEF QBUILDER}
function TfrxACRQuery.QBEngine: TfqbEngine;
begin
  Result := TfrxEngineACR.Create(nil);
  TfrxEngineACR(Result).FQuery.SessionName  := FQuery.SessionName;
  TfrxEngineACR(Result).FQuery.DatabaseName := FQuery.DatabaseName;
end;
{$ENDIF}


{$IFDEF QBUILDER}
constructor TfrxEngineACR.Create(AOwner: TComponent);
begin
  inherited;
  FQuery := TACRQuery.Create(Self);
end;

destructor TfrxEngineACR.Destroy;
begin
  FQuery.Free;
  inherited
end;

procedure TfrxEngineACR.ReadFieldList(const ATableName: string;
  var AFieldList: TfqbFieldList);
var
  TempTable: TACRTable;
  Fields: TFieldDefs;
  i: Integer;
  tmpField: TfqbField;
begin
  AFieldList.Clear;
  TempTable := TACRTable.Create(Self);
  TempTable.SessionName := FQuery.SessionName;
  TempTable.DatabaseName := FQuery.DatabaseName;
  TempTable.TableName := ATableName;
  Fields := TempTable.FieldDefs;
  try
    try
      TempTable.Active := True;
      tmpField:= TfqbField(AFieldList.Add);
      tmpField.FieldName := '*';
      for i := 0 to Fields.Count - 1 do
      begin
        tmpField := TfqbField(AFieldList.Add);
        tmpField.FieldName := Fields.Items[i].Name;
        tmpField.FieldType := Ord(Fields.Items[i].DataType)
      end;
    except
    end;
  finally
    TempTable.Free;
  end;
end;

procedure TfrxEngineACR.ReadTableList(ATableList: TStrings);
var
  ASession: TACRSession;
begin
  ATableList.BeginUpdate;
  ATableList.Clear;
  try
    ASession := Sessions.FindSession(FQuery.SessionName);
    if Assigned(ASession) and (FQuery.DatabaseName <> '') then
      ASession.GetTableNames(FQuery.DatabaseName, ATableList);
  finally
    ATableList.EndUpdate;
  end;
end;

function TfrxEngineACR.ResultDataSet: TDataSet;
begin
  Result := FQuery;
end;

procedure TfrxEngineACR.SetSQL(const Value: string);
begin
  FQuery.SQL.Text := Value;
end;
{$ENDIF}

initialization
  // frxObjects.RegisterObject1(TfrxACRDataBase, nil, '', '', 0, 54);
  frxObjects.RegisterObject1(TfrxACRTable, nil, '', '', 0, 55);
  frxObjects.RegisterObject1(TfrxACRQuery, nil, '', '', 0, 56);

finalization
  // frxObjects.UnRegister(TfrxACRDataBase);
  frxObjects.UnRegister(TfrxACRTable);
  frxObjects.UnRegister(TfrxACRQuery);


end.
