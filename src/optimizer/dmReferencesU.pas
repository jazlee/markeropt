unit dmReferencesU;

interface

uses
  SysUtils, Classes, DB, ACRMain, ACRTypes;

type
  TdmReferences = class(TDataModule)
  private
    FRefSession: TACRSession;
    FRefDB: TACRDatabase;
    function GetDatabaseFile: string;
    procedure SetDatabaseFile(const Value: string);
    function GetOpened: boolean;
    procedure CreateOrUpdateTable(ATable: TACRTable);
    
  protected
    procedure EnsureOpened;
    procedure EnsureClosed;

    procedure CreateTables;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnsureRefExists;

    procedure CreateReferences(const AFileName: string = '');
    procedure OpenReferences(const AFileName: string = '');

    {Gunakan fungsi ini untuk mengecek keberadaan suatu record dalam tabel}
    function RecordExist(const ATableName, AKeyFields: string;
                         const AValues: variant): boolean;

    {Fungsi ini digunakan untuk mengambil SATU nilai dari SATU field }
    function SelectValue(ASQL: string): Variant; overload;
    function SelectValue(ASQL: string; AValues: Variant): Variant; overload;
    function SelectValue(const ATable, AFilter, ARetFields: string; AValues: Variant): Variant; overload;

    {ExecSQL digunakan untuk mengeksekusi secara shortcut sebuah query}
    function ExecSQL(ASQL: string): boolean; overload;
    function ExecSQL(ASQL: string; AValues: Variant): boolean; overload;
    
    {LastID digunakan untuk mengambil nilai maksimum suatu field yang terdapat
     dalam tabel.
    }
    function LastID(ATableName, AFieldName: string): Variant;

    function GetACRTable(const ATableName: string; const AIndexName: string = ''): TACRTable;
    function GetDatasource(ADataset: TDataset): TDataSource;

    property RefConnection: TACRDatabase read FRefDB;
    property RefSession: TACRSession read FRefSession;

    property DatabaseFile: string read GetDatabaseFile write SetDatabaseFile;
    property Opened: boolean read GetOpened;    
  end;

var
  dmReferences: TdmReferences;

implementation
uses
  StrUtils, ECFUtils, CSPAppUtil, CSPConsts, Variants;

{$R *.dfm}

function ConstructFilter(const AFilterFields: string; AValues: Variant): string;
var
  AParams: TECFParams;

  function ConstructFilterStr: string;
  var
    i: integer;
    AStr: string;
  begin
    Result := '';
    for i := 0 to AParams.Count - 1 do
    begin
      AStr := Format('(%s = %s)',[AParams[I].Name, QuotedStr(AParams[I].AsString)]);
      Result := IfThen(Result = '', AStr, Result + ' and '+ AStr);
    end;
  end;
    
begin
  AParams := TECFParams.Create(nil);
  try
    AParams.ParseParams(AFilterFields);
    AParams.ParamValues[AFilterFields] := AValues;
    Result := ConstructFilterStr;
  finally
    AParams.Free;
  end;  
end;

function GetLookupValues(const AFields: string; ADataset: TDataSet): Variant;
var
  ACount, APos, I: integer;
  AField: string;
begin
  if Pos(';', AFields) > 0 then
  begin
    ACount := FieldCount(AFields);
    APos := 1;
    I := 0;
    Result := VarArrayCreate([0, ACount], varVariant);
    while APos <= Length(AFields) do
    begin
      AField := ExtractFieldName(AFields, APos);
      Result[I] := ADataSet.FieldValues[AField];
      Inc(I);
    end;
  end else
    Result := ADataSet.FieldValues[AFields];
end;

{ TdmReferences }

constructor TdmReferences.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRefSession := TACRSession.Create(Self);
  FRefDB := TACRDatabase.Create(Self);
  FRefSession.SessionName := 'REFSESSION';
  FRefDB.DatabaseName:= 'REFDATABASE';
  FRefDB.SessionName := FRefSession.SessionName;  
end;

procedure TdmReferences.CreateOrUpdateTable(ATable: TACRTable);
var
  BTable, CTable: TACRTable;
  AFieldDef: TACRAdvFieldDef;
  AIndex: TIndexDef;
  I: integer;
  AAlter: boolean;
begin
  if not ATable.Exists then
    ATable.CreateTable
  else begin
    BTable := TACRTable.Create(Self);
    CTable := TACRTable.Create(Self);
    try
      BTable.DatabaseName := ATable.DatabaseName;
      BTable.SessionName  := ATable.SessionName;
      BTable.TableName    := ATable.TableName;
      CTable.DatabaseName := ATable.DatabaseName;
      CTable.SessionName  := ATable.SessionName;
      CTable.TableName    := ATable.TableName;
      CTable.RestructureFieldDefs.Clear;
      CTable.RestructureIndexDefs.Clear;
      CTable.RestructureForeignKeyDefs.Clear;
      BTable.Open;
      AAlter := False;
      for I := 0 to ATable.AdvFieldDefs.Count - 1 do
      begin
        AFieldDef := BTable.AdvFieldDefs.Find(ATable.AdvFieldDefs[I].Name);
        if (AFieldDef = nil) or
           (AFieldDef.DataType <> ATable.AdvFieldDefs[I].DataType) or
           (AFieldDef.Size <> ATable.AdvFieldDefs[I].Size) then
        begin
          AAlter := True;
          break;
        end;
      end;
      if AAlter then
      begin
        for I := 0 to ATable.AdvFieldDefs.Count - 1 do
        begin
          AFieldDef := ATable.AdvFieldDefs[I];
          CTable.RestructureFieldDefs.Add(AFieldDef.Name,
            AFieldDef.DataType,
            AFieldDef.Size,
            AFieldDef.Required);
        end;
        for I := 0 to ATable.IndexDefs.Count - 1 do
        begin
          AIndex := ATable.IndexDefs[I];
          CTable.RestructureIndexDefs.Add(AIndex.Name, AIndex.Fields, AIndex.Options);
        end;
        CTable.RestructureForeignKeyDefs.Assign(ATable.ForeignKeyDefs);      
      end;
      BTable.Close;
      if CTable.RestructureFieldDefs.Count > 0 then
      begin
        CTable.Exclusive := True;
        CTable.RestructureTable;
      end;
    finally
      CTable.Exclusive := False;
      CTable.Free;
      BTable.Free;
    end;
  end;
end;

procedure TdmReferences.CreateReferences(const AFileName: string);
begin
  EnsureClosed;
  if AFileName <> EmptyStr then
    FRefDB.DatabaseFileName := AFileName;
  FRefDB.CreateDatabase;
  CreateTables;
end;

procedure TdmReferences.CreateTables;
var
  ATable: TACRTable;
begin
  EnsureClosed;
  try
    ATable := TACRTable.Create(Self);
    try
      ATable.DatabaseName := FRefDB.DatabaseName;
      ATable.SessionName  := FRefDB.SessionName;
      
      FRefDB.Exclusive := True;
      EnsureOpened;
        
      ATable.TableName := 'umcodes';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('umcode', aftWideString, 4, True);
      ATable.AdvFieldDefs.Add('umname', aftWideString, 16, True);
      ATable.AdvFieldDefs.Add('umdesc', aftWideString, 48);
      ATable.AdvFieldDefs.Add('umoptx', aftWideString, 8);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_umcodes','umcode', [ixPrimary]);
      CreateOrUpdateTable(ATable);

      ATable.TableName := 'umconv';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('basumcode', aftWideString, 4, True);
      ATable.AdvFieldDefs.Add('altumcode', aftWideString, 4, True);
      ATable.AdvFieldDefs.Add('umcvtfact', aftDouble);
      ATable.AdvFieldDefs.Add('umcvtmeth', aftSmallint);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_umconv','basumcode,altumcode', [ixPrimary]);
      CreateOrUpdateTable(ATable);

      ATable.TableName := 'features';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('ftname', aftWideString, 8, True);
      ATable.AdvFieldDefs.Add('ftgrp', aftSmallint);
      ATable.AdvFieldDefs.Add('ftdesc', aftWideString, 48);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_features','ftname', [ixPrimary]);
      CreateOrUpdateTable(ATable);

      ATable.TableName := 'itmfeatures';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('ftname', aftWideString, 8, True);
      ATable.AdvFieldDefs.Add('fitname', aftWideString, 16, True);
      ATable.AdvFieldDefs.Add('fitprio', aftSmallint);
      ATable.AdvFieldDefs.Add('fitprio2', aftSmallint);
      ATable.AdvFieldDefs.Add('fitdesc', aftWideString, 48);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_itmfeatures','ftname,fitname', [ixPrimary]);
      CreateOrUpdateTable(ATable);

      ATable.TableName := 'layrule';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('lrname', aftWideString, 12, True);
      ATable.AdvFieldDefs.Add('lrdesc', aftWideString, 48);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_layrule','lrname', [ixPrimary]);
      CreateOrUpdateTable(ATable);

      ATable.TableName := 'itmlayrule';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('lrname', aftWideString, 8, True);
      ATable.AdvFieldDefs.Add('minlyr', aftInteger);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_itmlayrule','lrname,minlyr', [ixPrimary, ixDescending]);
      CreateOrUpdateTable(ATable);

      ATable.TableName := 'materialcat';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('catcd', aftWideString, 16, True);
      ATable.AdvFieldDefs.Add('catname', aftWideString, 32, True);
      ATable.AdvFieldDefs.Add('catdesc', aftWideString, 48);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_materialcat','catcd', [ixPrimary]);
      CreateOrUpdateTable(ATable);

      ATable.TableName := 'materials';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('mtcd', aftWideString, 10, True);
      ATable.AdvFieldDefs.Add('mtname', aftWideString, 32, True);
      ATable.AdvFieldDefs.Add('mtdesc', aftWideString, 48);
      ATable.AdvFieldDefs.Add('avgthick', aftDouble);
      ATable.AdvFieldDefs.Add('mtcatcd', aftWideString, 16);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_materials','mtcd', [ixPrimary]);
      CreateOrUpdateTable(ATable);

      ATable.TableName := 'cardfile';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('crdid', aftWideString, 10, True);
      ATable.AdvFieldDefs.Add('crdname', aftWideString, 32, True);
      ATable.AdvFieldDefs.Add('crddesc', aftWideString, 48);
      ATable.AdvFieldDefs.Add('crdadr1', aftWideString, 48);
      ATable.AdvFieldDefs.Add('crdadr2', aftWideString, 48);
      ATable.AdvFieldDefs.Add('crdadr3', aftWideString, 48);
      ATable.AdvFieldDefs.Add('crdphn1', aftWideString, 32);
      ATable.AdvFieldDefs.Add('crdphn2', aftWideString, 32);
      ATable.AdvFieldDefs.Add('crdstat', aftBoolean);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_cardfile','crdid', [ixPrimary]);
      ATable.IndexDefs.Add('idx_cardfile_1','crdstat', []);
      CreateOrUpdateTable(ATable);

      ATable.TableName := 'styletypes';
      ATable.AdvFieldDefs.Clear;
      ATable.AdvFieldDefs.Add('sttname', aftWideString, 8, True);
      ATable.AdvFieldDefs.Add('sttdesc', aftWideString, 48, True);
      ATable.IndexDefs.Clear;
      ATable.IndexDefs.Add('pk_styletypes','sttname',[ixPrimary]);
      CreateOrUpdateTable(ATable);
      
    finally
      ATable.Free;
      EnsureClosed;
      FRefDB.Exclusive := False;
    end;
  except; end;
end;

destructor TdmReferences.Destroy;
begin
  EnsureClosed;
  FRefSession.Free;
  FRefDB.Free;
  inherited Destroy;
end;

procedure TdmReferences.EnsureClosed;
begin
  if not FRefDB.Connected then
    exit;
  if FRefDB.InTransaction then
    FRefDB.Commit;
  if FRefSession.Active then
    FRefSession.Close;
  FRefDB.Close;
end;

procedure TdmReferences.EnsureOpened;
begin
  if FRefDB.Connected then
    exit;
  FRefDB.Open;
end;

procedure TdmReferences.EnsureRefExists;
begin
  try
    DatabaseFile := GetAppIDClass.UserDataPath+'\'+SCSPRefDB;
    if FileExists(DatabaseFile) then
      OpenReferences(DatabaseFile)
    else
      CreateReferences;
  except; end;
end;

function TdmReferences.ExecSQL(ASQL: string; AValues: Variant): boolean;
var
  AQuery: TACRQuery;
  I: integer;  
begin
  Result := False;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FRefDB.DatabaseName;
  AQuery.SessionName := FRefDB.SessionName;
  try
    AQuery.SQL.Text := ASQL;
    try
      if not AQuery.Prepared then
      AQuery.Prepare;
      if VarIsArray(AValues) then
      begin
        for i := 0 to VarArrayHighBound(AValues,1) do
        begin
          AQuery.Params[i].Value := AValues[i];          
        end;
      end else
        if AValues <> null then
          AQuery.Params[0].Value := AValues; 
      if not FRefDB.InTransaction then
        FRefDB.StartTransaction;

      AQuery.ExecSQL;

      if FRefDB.InTransaction then
        FRefDB.Commit;
      Result := True;
    except
      if FRefDB.InTransaction then
        FRefDB.Rollback;
      raise;
    end;
  finally
    FreeAndNil(AQuery);
  end;
end;

function TdmReferences.ExecSQL(ASQL: string): boolean;
var
  AQuery: TACRQuery;
begin
  Result := False;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FRefDB.DatabaseName;
  AQuery.SessionName := FRefDB.SessionName;
  try
    AQuery.SQL.Text := ASQL;
    try
      if not FRefDB.InTransaction then
        FRefDB.StartTransaction;

      AQuery.ExecSQL;

      if FRefDB.InTransaction then
        FRefDB.Commit;
      Result := True;
    except
      if FRefDB.InTransaction then
        FRefDB.Rollback;
      raise;
    end;
  finally
    FreeAndNil(AQuery);
  end;
end;

function TdmReferences.GetACRTable(const ATableName: string;
  const AIndexName: string): TACRTable;
begin
  Result := TACRTable.Create(Self);
  Result.DatabaseName := FRefDB.DatabaseName;
  Result.SessionName := FRefDB.SessionName;
  Result.TableName := ATableName;
  if AIndexName <> '' then
    Result.IndexName := AIndexName;
end;

function TdmReferences.GetDatabaseFile: string;
begin
  Result := FRefDB.DatabaseFileName;
end;

function TdmReferences.GetDatasource(ADataset: TDataset): TDataSource;
begin
  Result := TDataSource.Create(Self);
  Result.DataSet := ADataset;
end;

function TdmReferences.GetOpened: boolean;
begin
  Result := FRefDB.Connected;
end;

function TdmReferences.LastID(ATableName, AFieldName: string): Variant;
var
  AQuery: TACRQuery;
begin
  Result := False;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FRefDB.DatabaseName;
  AQuery.SessionName := FRefDB.SessionName;
  AQuery.SQL.Text := Format('SELECT MAX(%s) FROM %s',[AFieldName, ATableName]);
  try
    AQuery.Open;
    Result := AQuery.Fields[0].Value;
  finally
    if AQuery.Active then
      AQuery.Close;
    FreeAndNil(AQuery);
  end;
end;

procedure TdmReferences.OpenReferences(const AFileName: string);
begin
  EnsureClosed;
  if AFileName <> EmptyStr then
    FRefDB.DatabaseFileName := AFileName;
  CreateTables;
  EnsureOpened;
end;

function TdmReferences.RecordExist(const ATableName, AKeyFields: string;
  const AValues: variant): boolean;
var
  AQuery: TACRQuery;
  Pos: integer;
  sFieldName: string;
  lHasField: boolean;
  nFieldCount: integer;
begin
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FRefDB.DatabaseName;
  AQuery.SessionName := FRefDB.SessionName;
  AQuery.SQL.Text := 'SELECT * FROM '+
                     ATableName+ ' WHERE ';
  lHasField := False;
  Pos:= 1;
  nFieldCount := 0;
  while Pos <= Length(AKeyFields) do
  begin
    sFieldName := ExtractFieldName(AKeyFields, Pos);
    if lHasField then
      AQuery.SQL.Add('AND');
    AQuery.SQL.Add(Format('(%s = :%s)',[sFieldName, sFieldName]));
    lHasField := True;
    inc(nFieldCount);
  end;
  try
    if not AQuery.Prepared then
      AQuery.Prepare;
    for Pos := 0 to nFieldCount-1 do
    begin
      if VarIsArray(AValues) then
        AQuery.Params[Pos].Value := AValues[Pos]
      else
        AQuery.Params[Pos].Value := AValues;
    end;
    AQuery.Open;
    Result := not AQuery.Eof;
  finally
    if AQuery.Active then
      AQuery.Close;
    FreeAndNil(AQuery);
  end;
end;

function TdmReferences.SelectValue(ASQL: string): Variant;
var
  AQuery: TACRQuery;
begin
  Result := Null;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FRefDB.DatabaseName;
  AQuery.SessionName := FRefDB.SessionName;
  try
    AQuery.SQL.Text := ASQL;
      AQuery.Open;
      if not AQuery.Eof then
        Result := AQuery.Fields[0].Value;
  finally
    FreeAndNil(AQuery);
  end;
end;

function TdmReferences.SelectValue(ASQL: string; AValues: Variant): Variant;
var
  AQuery: TACRQuery;
  I : integer;
begin
  Result := NULL;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FRefDB.DatabaseName;
  AQuery.SessionName := FRefDB.SessionName;  
  try
    AQuery.SQL.Text := ASQL;
    if not AQuery.Prepared then
      AQuery.Prepare;
    if  VarIsArray(AValues) then
    begin
      for i := 0 to VarArrayHighBound(AValues,1) do
        AQuery.Params[i].Value := AValues[i];
    end else
      if AValues <> null then
        AQuery.Params[0].Value := AValues;      
    AQuery.Open;
    if not AQuery.Eof then
      Result := AQuery.Fields[0].Value;
  finally
    FreeAndNil(AQuery);
  end;
end;

function TdmReferences.SelectValue(const ATable, AFilter, ARetFields: string;
  AValues: Variant): Variant;
var
  ADataset: TDataSet;
begin
  ADataset := GetACRTable(ATable, format('pk_%s', [ATable]));
  try
    ADataset.Filter := ConstructFilter(AFilter, AValues);
    ADataset.Filtered := True;
    ADataset.Open;
    if ADataset.IsEmpty then
      Result := null
    else
      Result := GetLookupValues(ARetFields, ADataset);
  finally
    ADataset.Free;
  end;
end;

procedure TdmReferences.SetDatabaseFile(const Value: string);
begin
  EnsureClosed;
  FRefDB.DatabaseFileName := Value;
end;

end.
