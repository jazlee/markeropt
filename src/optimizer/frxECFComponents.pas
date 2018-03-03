unit frxECFComponents;

{$I frx.inc}

interface

uses
  Windows, Classes, SysUtils, frxClass, frxCustomDB, DB, ecfconnection,
  ecfdataset, ecfapiproc
{$IFDEF Delphi6}
, Variants
{$ENDIF}
{$IFDEF QBUILDER}
, fqbClass
{$ENDIF};

type
  TfrxECFComponent = class(TfrxDBComponents)
  private
    FOldComponents: TfrxECFComponent;
    FECFConnection: TECFConnection;
    procedure SetECFConnection(const Value: TECFConnection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDescription: String; override;
  published
    property ECFConnection: TECFConnection read FECFConnection write SetECFConnection;
  end;

  TfrxECFAPIObject = class(TfrxCustomDataset)
  private
    FBProc: TECFAPIObject;
    FSaveOnBeforeOpen: TDataSetNotifyEvent;
    FSaveOnMethodChange: TNotifyEvent;
    FSaveOnObjectChange: TNotifyEvent;
    FParams: TfrxParams;

    procedure SetupConnection;
    procedure SetParams(const Value: TfrxParams);
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    function GetAPIMethodName: string;
    function GetAPIObjectName: string;
    procedure SetAPIMethodName(const Value: string);
    procedure SetAPIObjectName(const Value: string);
  protected
    procedure SetMaster(const Value: TDataSource); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure OnBeforeOpen(DataSet: TDataSet); virtual;
    procedure OnAPIObjectNameChange(Sender: TObject); virtual;
    procedure OnAPIMethodNameChange(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    class function GetDescription: String; override;
    procedure BeforeStartReport; override;
    procedure UpdateDataTree;
    procedure GetFieldList(List: TStrings); override;
    procedure Prepare;

    procedure ExecInsert;
    procedure ExecUpdate;
    procedure ExecDelete;

    function ParamByName(const Value: String): TfrxParamItem;
    procedure UpdateParams; virtual;
    
    property APIObject: TECFAPIObject read FBProc;
  published
    property APIObjectName: string read GetAPIObjectName write SetAPIObjectName;
    property APIMethodName: string read GetAPIMethodName write SetAPIMethodName;
    property Params: TfrxParams read FParams write SetParams;
  end;   

var
  ECFComponent: TfrxECFComponent;
    
implementation

uses
  frxUtils,
  frxECFRTTI,
{$IFNDEF NO_EDITORS}
  frxECFEditor,
{$ENDIF}
  frxDsgnIntf, frxRes, frxDBSet;

procedure frxProcParamsToTParams(Query: TfrxECFAPIObject; Params: TParams);
var
  i: Integer;
  Item: TfrxParamItem;
begin
  for i := 0 to Params.Count - 1 do
    if Query.Params.IndexOf(Params[i].Name) <> -1 then
    begin
      Item := Query.Params[Query.Params.IndexOf(Params[i].Name)];
      Params[i].Clear;
      { Bound should be True in design mode }
      if not (Query.IsLoading or Query.IsDesigning) then
        Params[i].Bound := False
      else
        Params[i].Bound := True;
      Params[i].DataType := Item.DataType;
      if Trim(Item.Expression) <> '' then
        if not (Query.IsLoading or Query.IsDesigning) then
          if Query.Report <> nil then
          begin
            Query.Report.CurObject := Query.Name;
            Item.Value := Query.Report.Calc(Item.Expression);
          end;
      if not VarIsEmpty(Item.Value) then
      begin
        Params[i].Bound := True;
        if Params[i].DataType in [ftString, ftWideString] then
          Params[i].Text := VarToStr(Item.Value)
        else
          Params[i].Value := Item.Value;
      end;
    end;
end;

procedure frxProcTParamsToParams(Query: TfrxECFAPIObject; AParams: TParams);
var
  i: Integer;
  Item: TfrxParamItem;
begin
  with Query do
  begin
    for i := 0 to AParams.Count - 1 do
      if Query.Params.IndexOf(AParams[i].Name) <> -1 then
      begin
        Item := Query.Params[Query.Params.IndexOf(AParams[i].Name)];
        Item.DataType := AParams[i].DataType;
        if not VarIsEmpty(AParams[i].Value) then
        begin
          Item.DataType := AParams[i].DataType;
          Item.Value := AParams[i].Value
        end;
      end else
      begin
        Item := Query.Params.Add;
        Item.DataType := AParams[i].DataType;
        Item.Name     := AParams[i].Name;
        Item.Value    := AParams[i].Value;
      end;
  end;
end;

{ TfrxECFComponent }

constructor TfrxECFComponent.Create(AOwner: TComponent);
begin
  inherited;
  FECFConnection := nil;
  FOldComponents := ECFComponent;
  ECFComponent  := Self;
end;

destructor TfrxECFComponent.Destroy;
begin
  if ECFComponent = Self then
    ECFComponent := FOldComponents;
  inherited Destroy;
end;

function TfrxECFComponent.GetDescription: String;
begin
  Result := 'ECF';
end;

procedure TfrxECFComponent.SetECFConnection(const Value: TECFConnection);
begin
  if (Value <> FECFConnection) then
    FECFConnection := Value;
end;

{ TfrxECFAPIObject }

procedure TfrxECFAPIObject.BeforeStartReport;
begin
  SetupConnection;
end;

constructor TfrxECFAPIObject.Create(AOwner: TComponent);
begin
  FBProc := TECFAPIObject.Create(nil);
  FParams:= TfrxParams.Create;
  Dataset := FBProc;
  FSaveOnBeforeOpen := DataSet.BeforeOpen;
  DataSet.BeforeOpen := OnBeforeOpen;
  FSaveOnObjectChange := FBProc.OnAPIObjectChange;
  FSaveOnMethodChange := FBProc.OnAPIMethodChange;
  FBProc.OnAPIObjectChange := OnAPIObjectNameChange;
  FBProc.OnAPIMethodChange := OnAPIMethodNameChange;
  SetupConnection;
  inherited Create(AOwner);
end;

procedure TfrxECFAPIObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Parameters', ReadData, WriteData, True);
end;

destructor TfrxECFAPIObject.Destroy;
begin
  inherited Destroy;
end;

procedure TfrxECFAPIObject.ExecDelete;
begin
  UpdateParams;
  ExecDelete;
end;

procedure TfrxECFAPIObject.ExecInsert;
begin
  UpdateParams;
  ExecInsert;
end;

procedure TfrxECFAPIObject.ExecUpdate;
begin
  UpdateParams;
  ExecUpdate;
end;

function TfrxECFAPIObject.GetAPIMethodName: string;
begin
  Result := FBProc.APIMethodName;
end;

function TfrxECFAPIObject.GetAPIObjectName: string;
begin
  Result := FBProc.APIObjectName;
end;

class function TfrxECFAPIObject.GetDescription: String;
begin
  Result := 'ECF-API Object';
end;

procedure TfrxECFAPIObject.GetFieldList(List: TStrings);
var
  i: Integer;
begin
  List.Clear;
  if FieldAliases.Count = 0 then
  begin
    try
      if (FBProc.APIObjectName <> '') and (DataSet <> nil) then
        DataSet.GetFieldNames(List);
    except; end;
  end else
  begin
    for i := 0 to FieldAliases.Count - 1 do
      if Pos('-', FieldAliases.Names[i]) <> 1 then
        List.Add(FieldAliases.Values[FieldAliases.Names[i]]);
  end;
end;

procedure TfrxECFAPIObject.OnAPIMethodNameChange(Sender: TObject);
begin
  if (FBProc.APIObjectName <> EmptyStr) and (FBProc.APIMethodName <> EmptyStr) then
    Prepare;
  if Assigned(FSaveOnMethodChange) then
    FSaveOnMethodChange(Sender);
end;

procedure TfrxECFAPIObject.OnBeforeOpen(DataSet: TDataSet);
begin
  UpdateParams;
  if Assigned(FSaveOnBeforeOpen) then
    FSaveOnBeforeOpen(DataSet);
end;

procedure TfrxECFAPIObject.OnAPIObjectNameChange(Sender: TObject);
begin
  if (FBProc.APIObjectName <> EmptyStr) and (FBProc.APIMethodName <> EmptyStr) then
    Prepare;
  if Assigned(FSaveOnObjectChange) then
    FSaveOnObjectChange(Sender);  
end;

function TfrxECFAPIObject.ParamByName(const Value: String): TfrxParamItem;
begin
  Result := FParams.Find(Value);
  if Result = nil then
    raise Exception.Create('Parameter "' + Value + '" not found');
end;

procedure TfrxECFAPIObject.Prepare;
begin
  FParams.Clear;
  FBProc.Prepare;
  frxProcTParamsToParams(Self, FBProc.Params);
end;

procedure TfrxECFAPIObject.ReadData(Reader: TReader);
begin
  frxReadCollection(FParams, Reader, Self);
  UpdateParams;
end;

procedure TfrxECFAPIObject.SetAPIMethodName(const Value: string);
begin
  FBProc.APIMethodName := Value;
end;

procedure TfrxECFAPIObject.SetAPIObjectName(const Value: string);
begin
  FBProc.APIObjectName := Value;
end;

procedure TfrxECFAPIObject.SetMaster(const Value: TDataSource);
begin
  FBProc.MasterSource := Value;
end;

procedure TfrxECFAPIObject.SetParams(const Value: TfrxParams);
begin
  FParams.Assign(Value);
end;

procedure TfrxECFAPIObject.SetupConnection;
begin
  if ECFComponent <> nil then
    FBProc.ConnectionPool := ECFComponent.ECFConnection
  else
    FBProc.ConnectionPool := nil;
end;

procedure TfrxECFAPIObject.UpdateDataTree;
begin
  if (Report <> nil) and (Report.Designer <> nil) then
    Report.Designer.UpdateDataTree;
end;

procedure TfrxECFAPIObject.UpdateParams;
begin
  frxProcParamsToTParams(Self, FBProc.Params);
end;

procedure TfrxECFAPIObject.WriteData(Writer: TWriter);
begin
  frxWriteCollection(FParams, Writer, Self);
end;

initialization
    frxObjects.RegisterObject1(TfrxECFAPIObject, nil, '', '', 0, 55);
  
finalization
   frxObjects.Unregister(TfrxECFAPIObject);

end.
