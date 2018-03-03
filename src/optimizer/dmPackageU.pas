unit dmPackageU;

interface

uses
  SysUtils, Classes, DB, ACRMain, ACRTypes, CSPConsts, NativeXML, NativeRegXML,
  CSPCustomEngine, Forms;

type
  TPackageInfoRow = class(TCollectionItem)
  private
    FName: string;
    FValue: string;
  public
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  TPackageInfoCollection = class(TCollection)
  private
    function GetValue(AName: string): string;
    procedure SetValue(AName: string; const Value: string);

    function FindOrCreate(const AName: string): TPackageInfoRow;
  public
    constructor Create;

    procedure ReadXML(AXMLReg: TRegXML; const ARoot: string);
    procedure WriteXML(AXMLReg: TRegXML; const ARoot: string);

    property Values[AName: string]: string read GetValue write SetValue;
  end;

  TItemRow = class(TCollectionItem)
  private
    FValues: TStringList;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Values: TStringList read FValues;
  end;

  TItemRowCollection = class(TCollection)
  private
    FFields: TStringList;
    FStyle: string;
    FStyleFormat: string;
    FDataset: TDataSet;

    function GetRow(const AColor: string): TItemRow;
    function GetValues(Index, AField: string): string;
    procedure SetValues(Index, AField: string; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure AssignFields(AFields: TStringList);
    procedure SetColors(AValues: TStringList); overload;
    procedure SetColors(AValues: TStringList; const ADefault: integer); overload;

    property Values[Index, AField: string]: string read GetValues write SetValues;
    property Style: string read FStyle write FStyle;
    property StyleFormat: string read FStyleFormat write FStyleFormat;
    property Dataset: TDataSet read FDataset write FDataset;
  end;

  TUpdateStatusEvent = procedure (Sender: TObject; const AText: string) of object;
    
  TdmPackage = class(TDataModule)
  private
    FOnVerify: boolean;
    FPkgSession: TACRSession;
    FPkgDB: TACRDatabase;
    FNotifyMainForm: TNotifyEvent;
    FPackageInfo: TPackageInfoCollection;
    function GetDatabaseFile: string;
    procedure SetDatabaseFile(const Value: string);
    function GetPackageStatus: Boolean;
    procedure SetPackageStatus(const Value: Boolean);
    procedure SetNotifyMainForm(const Value: TNotifyEvent);

    procedure OnPackageNotify(Sender: TObject);
    procedure OnDBProgress(Sender: TComponent; Progress: Double;
                      Operation: TACRDatabaseOperation; var Abort: Boolean);
    procedure OnTableProgress(Sender: TComponent;Progress: Double;
                      Operation:  TACRTableOperation; var Abort: Boolean);

  private
    procedure CreateOrUpdateTable(ATable: TACRTable);
    procedure ReadPackageInfo;
    procedure WritePackageInfo;

    function SaveDatasetState: Variant;
    procedure RestoreDatasetState(AState: Variant);

  protected
    procedure EnsureOpened;
    procedure EnsureClosed;

    procedure CreateTables;

    procedure VerifyDatabase;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MaintainOrderStyle(const AOrdNo: string);

    procedure CreatePackage(const AFileName: string = '');
    procedure OpenPackage(const AFileName: string = '');
    procedure ClosePackage;

    procedure RepairPackage;
    procedure CompactPackage;
    procedure RepairExternPackage(const APkgName: string);

    procedure RefreshPackageInfo;

    procedure BuildXMLDatasource(AXMLData: TNativeXML; ADataset: TDataSet);
    procedure ExtractXMLSizes(AXMLData: TNativeXml);
    procedure ExtractXMLColors(AXMLData: TNativeXml);
    procedure ExtractXMLMaterials(AXMLData: TNativeXml);

    procedure RegisterXMLEngines(AXMLData: TNativeXml);

    procedure CreateItems(AItems: TItemRowCollection; AOnUpdateStatus: TUpdateStatusEvent);
    procedure CreateOrderItems(AItems: TItemRowCollection; AOnUpdateStatus: TUpdateStatusEvent);
    function TableExists(const ATableName: string): boolean;

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
        
    property PkgConnection: TACRDatabase read FPkgDB;
    property PkgSession: TACRSession read FPkgSession;

    property DatabaseFile: string read GetDatabaseFile write SetDatabaseFile;
    property Opened: Boolean read GetPackageStatus write SetPackageStatus;
    property NotifyMainForm: TNotifyEvent read FNotifyMainForm write SetNotifyMainForm;

    property PackageInfo: TPackageInfoCollection read FPackageInfo;        
  end;

var
  dmPackage: TdmPackage;

implementation

uses
  ECFUtils, StrUtils, Math, Variants, gnugettext, frmInputPasswdU, 
  frmProgressU, frmMainU;

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

function IsLeadChar(C: AnsiChar): Boolean;
begin
  Result := C in LeadBytes;
end;

procedure FormatItemCode(var Result: string; const Format: string;
  const AStyle, AColor, ASize: string; AIncludeExtra: boolean = True);
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..255] of Char;
  AFormat: string;

  procedure AppendChars(P: PChar; Count: Integer);
  var
    N: Integer;
  begin
    N := Length(Buffer) - BufPos;
    if N > Count then N := Count;
    if N <> 0 then Move(P[0], Buffer[BufPos], N * SizeOf(Char));
    Inc(BufPos, N);
  end;

  procedure AppendString(const S: string);
  begin
    AppendChars(Pointer(S), Length(S));
  end;

  procedure AppendNumber(Number, Digits: Integer);
  const
    Format: array[0..3] of Char = '%.*d';
  var
    NumBuf: array[0..15] of Char;
  begin
    AppendChars(NumBuf, FormatBuf(NumBuf, Length(NumBuf), Format,
      Length(Format), [Digits, Number]));
  end;

  procedure AppendFormat(Format: PChar);
  var
    Starter, Token, LastToken: Char;
    BetweenQuotes: Boolean;
    P: PChar;
    Count: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec, H: Word;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

  begin
    if (Format <> nil) and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      while Format^ <> #0 do
      begin
        Starter := Format^;
        if IsLeadChar(Starter) then
        begin
          Format := StrNextChar(Format);
          LastToken := ' ';
          Continue;
        end;
        Format := StrNextChar(Format);
        Token := Starter;
        if Token in ['a'..'z'] then Dec(Token, 32);
        case Token of
          'S':
            begin
              GetCount;
              if Count = 3 then
                AppendString(UpperCase(AStyle));
            end;
          'X':
            begin
              GetCount;
              if Count = 3 then
                AppendString(UpperCase(ASize));
            end;
          'Y':
            begin
              GetCount;
              if Count = 3 then
                AppendString(UpperCase(AColor));
            end;
          '''', '"':
            begin
              P := Format;
              while (Format^ <> #0) and (Format^ <> Starter) do
              begin
                if IsLeadChar(Format^) then
                  Format := StrNextChar(Format)
                else
                  Inc(Format);
              end;
              if AIncludeExtra then
                AppendChars(P, Format - P);
              if Format^ <> #0 then Inc(Format);
            end;
        else
          AppendChars(@Starter, 1);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  BufPos := 0;
  AppendLevel := 0;
  AFormat := SCSPDefaultItemFormat;
  if Format <> '' then
    AppendFormat(Pointer(Format))
  else
    AppendFormat(Pointer(AFormat));
  SetString(Result, Buffer, BufPos);
end;

{ TItemRow }

constructor TItemRow.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FValues := TStringList.Create;
end;

destructor TItemRow.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

{ TItemRowCollection }

procedure TItemRowCollection.AssignFields(AFields: TStringList);
var
  i: integer;
begin
  FFields.Clear;
  FFields.Add('Color');
  for i := 0 to AFields.Count - 1 do
    FFields.Add(AFields[i]);    
end;

constructor TItemRowCollection.Create;
begin
  inherited Create(TItemRow);
  FFields := TStringList.Create;
end;

destructor TItemRowCollection.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TItemRowCollection.GetRow(const AColor: string): TItemRow;
var
  I: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if TItemRow(Items[i]).Values[0] = AColor then
    begin
      Result := TItemRow(Items[i]);
      break;          
    end;
  end;
end;

function TItemRowCollection.GetValues(Index, AField: string): string;
var
  AIndex: Integer;
  AItem: TItemRow;
begin
  Result := '';
  AIndex := FFields.IndexOf(AField);
  if AIndex >= 0 then
  begin
    AItem := GetRow(Index);
    if AItem <> nil then
      Result := AItem.Values[AIndex];
  end;
end;

procedure TItemRowCollection.Reset;
begin
  Clear;
  FFields.Clear;
  FDataset := nil;
end;

procedure TItemRowCollection.SetColors(AValues: TStringList);
var
  i,j: integer;
  AItem: TItemRow;
begin
  Clear;
  for i := 0 to AValues.Count - 1 do
  begin
    AItem := TItemRow(Add);
    AItem.Values.Add(AValues[i]);
    if FFields.Count > 1 then    
      for j := 1 to FFields.Count - 1 do
        AItem.Values.Add(BoolToStr(True, True));      
  end;
end;

procedure TItemRowCollection.SetColors(AValues: TStringList;
  const ADefault: integer);
var
  i,j: integer;
  AItem: TItemRow;
begin
  Clear;
  for i := 0 to AValues.Count - 1 do
  begin
    AItem := TItemRow(Add);
    AItem.Values.Add(AValues[i]);
    if FFields.Count > 1 then    
      for j := 1 to FFields.Count - 1 do
        AItem.Values.Add(IntToStr(ADefault));      
  end;
end;

procedure TItemRowCollection.SetValues(Index, AField: string;
  const Value: string);
var
  AIndex: Integer;
  AItem: TItemRow;
begin
  AIndex := FFields.IndexOf(AField);
  if AIndex >= 0 then
  begin
    AItem := GetRow(Index);
    if AItem <> nil then
      AItem.Values[AIndex] := Value;
  end;
end;

{ TPackageInfoCollection }

constructor TPackageInfoCollection.Create;
begin
  inherited Create(TPackageInfoRow);
end;

function TPackageInfoCollection.FindOrCreate(
  const AName: string): TPackageInfoRow;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if SameText(TPackageInfoRow(Items[i]).Name, AName) then
    begin
      Result := TPackageInfoRow(Items[i]);
      break;
    end;
   if Result = nil then
   begin
    Result := TPackageInfoRow(Add);
    Result.Name := AName;
   end;
end;

function TPackageInfoCollection.GetValue(AName: string): string;
begin
  Result := FindOrCreate(AName).Value;
end;

procedure TPackageInfoCollection.ReadXML(AXMLReg: TRegXML; const ARoot: string);
var
  AList: TStringList;
  I: integer;
begin
  if AXMLReg.openKey(ARoot, False) then
  begin
    AList := TStringList.Create;
    try
      Clear;
      AXMLReg.getValueNames(AList);
      for I := 0 to AList.Count - 1 do
        Values[AList[i]] := AXMLReg.readString(AList[i]);        
    finally
      AList.Free;
    end;  
  end;
end;

procedure TPackageInfoCollection.SetValue(AName: string; const Value: string);
begin
  FindOrCreate(AName).Value := Value;
end;

procedure TPackageInfoCollection.WriteXML(AXMLReg: TRegXML;
  const ARoot: string);
var
  I: integer;
  AItem: TPackageInfoRow;
begin
  if AXMLReg.openKey(ARoot, True) then
  begin
    for I := 0 to Count - 1 do
    begin
      AItem := TPackageInfoRow(Items[i]);
      AXMLReg.writeString(AItem.Name, AItem.Value);
    end;
  end;
end;

{ TdmPackage }

procedure TdmPackage.BuildXMLDatasource(AXMLData: TNativeXML;
  ADataset: TDataSet);

  procedure RetrieveStyles;
  var
    AStyles: TACRTable;
    ARoot, ANode, ASubNode, ASubSubNode: TXmlNode;
    AStrStream: TStringStream;    
  begin
    AStyles := GetACRTable('orstyles', 'pk_orstyles');
    ARoot := AXMLData.Root.NodeNew('styles');
    try
      AStyles.Filter := ConstructFilter('ordno', ADataset.FieldByName('ordno').AsWideString);
      AStyles.Filtered := True;
      AStyles.Open;
      while not AStyles.Eof do
      begin
        ANode := ARoot.NodeNew('style');
        ANode.WriteAttributeString('ordno', AStyles.FieldByName('ordno').AsWideString);
        ANode.WriteAttributeString('stname', AStyles.FieldByName('stname').AsWideString);
        ANode.WriteAttributeString('sttname', AStyles.FieldByName('sttname').AsWideString);
        ANode.WriteAttributeString('stdesc', AStyles.FieldByName('stdesc').AsWideString);
        ANode.WriteAttributeFloat('avgyy', AStyles.FieldByName('avgyy').AsFloat);
        ANode.WriteAttributeFloat('avgthick', AStyles.FieldByName('avgthick').AsFloat);
        ANode.WriteAttributeString('umcode', AStyles.FieldByName('umcode').AsWideString);

        ANode.WriteAttributeString('dsnfile', AStyles.FieldByName('dsnfile').AsWideString);
        AStrStream := TStringStream.Create(EmptyStr);
        try
          TBlobField(AStyles.FieldByName('dsndata')).SaveToStream(AStrStream);
          if AStrStream.Size > 0 then
          begin
            ASubNode := ANode.NodeNew('attachments');
            ASubSubNode := ASubNode.NodeNew('dsndata');
            ASubSubNode.WriteAttributeString('name', AStyles.FieldByName('dsnfile').AsWideString);
            ASubSubNode.BinaryString := AStrStream.DataString;
          end;
        finally
          AStrStream.Free;
        end;

        if AStyles.FieldByName('maxlen').AsFloat <> 0 then
          ANode.WriteAttributeFloat('maxlen', AStyles.FieldByName('maxlen').AsFloat)
        else if ADataset.FieldByName('maxlen').AsFloat <> 0 then
          ANode.WriteAttributeFloat('maxlen', ADataset.FieldByName('maxlen').AsFloat);

        if AStyles.FieldByName('maxthick').AsFloat <> 0 then
          ANode.WriteAttributeFloat('maxthick', AStyles.FieldByName('maxthick').AsFloat)
        else if ADataset.FieldByName('maxthick').AsFloat <> 0 then
          ANode.WriteAttributeFloat('maxthick', ADataset.FieldByName('maxthick').AsFloat);

        if AStyles.FieldByName('mrklen').AsFloat <> 0 then
          ANode.WriteAttributeFloat('mrklen', AStyles.FieldByName('mrklen').AsFloat)
        else if ADataset.FieldByName('mrklen').AsFloat <> 0 then
          ANode.WriteAttributeFloat('mrklen', ADataset.FieldByName('mrklen').AsFloat);

        if AStyles.FieldByName('mrkwidth').AsFloat <> 0 then
          ANode.WriteAttributeFloat('mrkwidth', AStyles.FieldByName('mrkwidth').AsFloat)
        else if ADataset.FieldByName('mrkwidth').AsFloat <> 0 then
          ANode.WriteAttributeFloat('mrkwidth', ADataset.FieldByName('mrkwidth').AsFloat);

        if AStyles.FieldByName('algorithm').AsString <> EmptyStr then
          ANode.WriteAttributeString('algorithm', AStyles.FieldByName('algorithm').AsWideString)
        else if ADataset.FieldByName('algorithm').AsString <> EmptyStr then
          ANode.WriteAttributeString('algorithm', ADataset.FieldByName('algorithm').AsWideString);

        ANode.WriteAttributeInteger('lrmethod', AStyles.FieldByName('lrmethod').AsInteger);
        ANode.WriteAttributeInteger('lrmethodcnt', AStyles.FieldByName('lrmethodcnt').AsInteger);
        AStyles.Next;
      end;
    finally
      AStyles.Free;
    end;
  end;

  procedure RetrieveItems;
  var
    AItems: TACRTable;
    ARoot, ANode, ASubNode, ASubSubNode: TXmlNode;
    AStrStream: TStringStream;
  begin
    AItems := GetACRTable('oritems', 'idx_oritems_1');
    ARoot := AXMLData.Root.NodeNew('items');
    try
      AItems.Filter := ConstructFilter('ordno', ADataset.FieldByName('ordno').AsWideString);
      AItems.Filtered := True;
      AItems.Open;
      while not AItems.Eof do
      begin
        ANode := ARoot.NodeNew('item');
        ANode.WriteAttributeString('ordno', AItems.FieldByName('ordno').AsWideString);
        ANode.WriteAttributeString('itmname', AItems.FieldByName('itmname').AsWideString);
        ANode.WriteAttributeString('itmdesc', AItems.FieldByName('itmdesc').AsWideString);
        ANode.WriteAttributeString('stname', AItems.FieldByName('stname').AsWideString);
        ANode.WriteAttributeString('xftname', AItems.FieldByName('xftname').AsWideString);
        ANode.WriteAttributeString('xfitname', AItems.FieldByName('xfitname').AsWideString);
        ANode.WriteAttributeInteger('xfitprio', AItems.FieldByName('xfitprio').AsInteger);
        ANode.WriteAttributeInteger('xfitprio2', AItems.FieldByName('xfitprio2').AsInteger);
        ANode.WriteAttributeString('yftname', AItems.FieldByName('yftname').AsWideString);
        ANode.WriteAttributeString('yfitname', AItems.FieldByName('yfitname').AsWideString);
        ANode.WriteAttributeFloat('orqty', AItems.FieldByName('orqty').AsFloat);
        ANode.WriteAttributeString('umcode', AItems.FieldByName('umcode').AsWideString);
        ANode.WriteAttributeString('dsnfile', AItems.FieldByName('dsnfile').AsWideString);

        if AItems.FieldByName('maxlen').AsFloat <> 0 then
          ANode.WriteAttributeFloat('maxlen', AItems.FieldByName('maxlen').AsFloat)
        else if ADataset.FieldByName('maxlen').AsFloat <> 0 then
          ANode.WriteAttributeFloat('maxlen', ADataset.FieldByName('maxlen').AsFloat);

        if AItems.FieldByName('mrklen').AsFloat <> 0 then
          ANode.WriteAttributeFloat('mrklen', AItems.FieldByName('mrklen').AsFloat)
        else if ADataset.FieldByName('mrklen').AsFloat <> 0 then
          ANode.WriteAttributeFloat('mrklen', ADataset.FieldByName('mrklen').AsFloat);

        if AItems.FieldByName('mrkwidth').AsFloat <> 0 then
          ANode.WriteAttributeFloat('mrkwidth', AItems.FieldByName('mrkwidth').AsFloat)
        else if ADataset.FieldByName('mrkwidth').AsFloat <> 0 then
          ANode.WriteAttributeFloat('mrkwidth', ADataset.FieldByName('mrkwidth').AsFloat);

        if AItems.FieldByName('algorithm').AsString <> EmptyStr then
          ANode.WriteAttributeString('algorithm', AItems.FieldByName('algorithm').AsWideString)
        else if ADataset.FieldByName('algorithm').AsString <> EmptyStr then
          ANode.WriteAttributeString('algorithm', ADataset.FieldByName('algorithm').AsWideString);
                       
        ANode.WriteAttributeInteger('lrmethod', AItems.FieldByName('lrmethod').AsInteger);
        ANode.WriteAttributeInteger('lrmethodcnt', AItems.FieldByName('lrmethodcnt').AsInteger);
        AStrStream := TStringStream.Create(EmptyStr);
        try
          TBlobField(AItems.FieldByName('dsndata')).SaveToStream(AStrStream);
          if AStrStream.Size > 0 then
          begin
            ASubNode := ANode.NodeNew('attachments');
            ASubSubNode := ASubNode.NodeNew('dsndata');
            ASubSubNode.WriteAttributeString('name', AItems.FieldByName('dsnfile').AsWideString);
            ASubSubNode.BinaryString := AStrStream.DataString;
          end;
        finally
          AStrStream.Free;
        end;
        AItems.Next;
      end;
    finally
      AItems.Free;
    end;
  end;

  function FindItem(const ordno, itmname: string): TXmlNode;
  var
    ARoot, ANode: TXmlNode;
    I: integer;
  begin
    Result := nil;
    ARoot := AXMLData.Root.FindNode('items');
    if ARoot = nil then
      exit;
    for I := 0 to ARoot.NodeCount - 1 do
    begin
      ANode := ARoot.Nodes[I];
      if (ANode.ReadAttributeString('ordno') = ordno) and
         (ANode.ReadAttributeString('itmname') = itmname) then
      begin
        Result := ANode;
        break;
      end;      
    end;
  end;

  function FindStyle(const ordno, stname: string): TXmlNode;
  var
    ARoot, ANode: TXmlNode;
    I: integer;
  begin
    Result := nil;
    ARoot := AXMLData.Root.FindNode('styles');
    if ARoot = nil then
      exit;
    for I := 0 to ARoot.NodeCount - 1 do
    begin
      ANode := ARoot.Nodes[I];
      if (ANode.ReadAttributeString('ordno') = ordno) and
         (ANode.ReadAttributeString('stname') = stname) then
      begin
        Result := ANode;
        break;
      end;      
    end;
  end;

  procedure RetrieveMaterials;
  var
    AItems: TACRTable;
    ARoot, ANode, ASubNode, ASubSubNode, ANodeStyle, ANodeitem: TXmlNode;
    AStrStream: TStringStream;
  begin
    AItems := GetACRTable('ormaterials', 'pk_ormaterials');
    ARoot := AXMLData.Root.NodeNew('materials');
    try
      AItems.Filter := ConstructFilter('ordno', ADataset.FieldByName('ordno').AsWideString);
      AItems.Filtered := True;
      AItems.Open;
      while not AItems.Eof do
      begin
        ANodeStyle := nil;
        ANode := ARoot.NodeNew('material');
        ANodeitem := FindItem(AItems.FieldByName('ordno').AsString, AItems.FieldByName('itmname').AsString);
        if ANodeitem <> nil then        
          ANodeStyle := FindStyle(AItems.FieldByName('ordno').AsString, ANodeitem.AttributeByName['stname']);
        ANode.WriteAttributeString('ordno', AItems.FieldByName('ordno').AsWideString);
        ANode.WriteAttributeString('itmname', AItems.FieldByName('itmname').AsWideString);
        ANode.WriteAttributeString('mtcd', AItems.FieldByName('mtcd').AsWideString);
        ANode.WriteAttributeString('mtname', AItems.FieldByName('mtname').AsWideString);
        ANode.WriteAttributeString('mtdesc', AItems.FieldByName('mtdesc').AsWideString);
        ANode.WriteAttributeString('mtcatcd', AItems.FieldByName('mtcatcd').AsWideString);
        ANode.WriteAttributeString('lrname', AItems.FieldByName('lrname').AsWideString);
        ANode.WriteAttributeString('lrdesc', AItems.FieldByName('lrdesc').AsWideString);
        ANode.WriteAttributeFloat('avgyy', AItems.FieldByName('avgyy').AsFloat);
        if (AItems.FieldByName('umcode').AsString <> EmptyStr) then
          ANode.WriteAttributeString('umcode', AItems.FieldByName('umcode').AsString)
        else if (ANodeitem <> nil) and (ANodeitem.ReadAttributeString('umcode') <> EmptyStr) then
            ANode.WriteAttributeString('umcode', ANodeitem.ReadAttributeString('umcode'));
        if (ANode.AttributeByName['umcode'] = EmptyStr) and (ANodeStyle <> nil) then
          ANode.WriteAttributeString('umcode', ANodeStyle.ReadAttributeString('umcode'));

        ANode.WriteAttributeFloat('fballowance', AItems.FieldByName('fballowance').AsFloat);
        ANode.WriteAttributeInteger('fbaltype', AItems.FieldByName('fbaltype').AsInteger);
        ANode.WriteAttributeString('mtlayout', AItems.FieldByName('mtlayout').AsWideString);
        ANode.WriteAttributeString('dsnfile', AItems.FieldByName('dsnfile').AsWideString);
        ANode.WriteAttributeString('xftname', ANodeitem.ReadAttributeString('xftname'));
        ANode.WriteAttributeString('xfitname', ANodeitem.ReadAttributeString('xfitname'));
        ANode.WriteAttributeInteger('xfitprio', ANodeitem.ReadAttributeInteger('xfitprio'));
        ANode.WriteAttributeInteger('xfitprio2', ANodeitem.ReadAttributeInteger('xfitprio2'));
        ANode.WriteAttributeString('yftname', ANodeitem.ReadAttributeString('yftname'));
        ANode.WriteAttributeString('yfitname', ANodeitem.ReadAttributeString('yfitname'));
        if (AItems.FieldByName('maxlen').AsFloat <> 0) then
          ANode.WriteAttributeFloat('maxlen', AItems.FieldByName('maxlen').AsFloat)
        else if (ANodeitem <> nil) and (ANodeitem.ReadAttributeFloat('maxlen') <> 0) then
          ANode.WriteAttributeFloat('maxlen', ANodeitem.ReadAttributeFloat('maxlen'))
        else if (ANodeStyle <> nil) and (ANodeStyle.ReadAttributeFloat('maxlen') <> 0) then
          ANode.WriteAttributeFloat('maxlen', ANodeStyle.ReadAttributeFloat('maxlen'))
        else if (ADataset.FieldByName('maxlen').AsFloat <> 0) then
          ANode.WriteAttributeFloat('maxlen', ADataset.FieldByName('maxlen').AsFloat);
        if (AItems.FieldByName('mrklen').AsFloat <> 0) then
          ANode.WriteAttributeFloat('mrklen', AItems.FieldByName('mrklen').AsFloat)
        else if (ANodeitem <> nil) and (ANodeitem.ReadAttributeFloat('mrklen') <> 0) then
          ANode.WriteAttributeFloat('mrklen', ANodeitem.ReadAttributeFloat('mrklen'))
        else if (ANodeStyle <> nil) and (ANodeStyle.ReadAttributeFloat('mrklen') <> 0) then
          ANode.WriteAttributeFloat('mrklen', ANodeStyle.ReadAttributeFloat('mrklen'))
        else if (ADataset.FieldByName('mrklen').AsFloat <> 0) then
          ANode.WriteAttributeFloat('mrklen', ADataset.FieldByName('mrklen').AsFloat);
        if (AItems.FieldByName('mrkwidth').AsFloat <> 0) then
          ANode.WriteAttributeFloat('mrkwidth', AItems.FieldByName('mrkwidth').AsFloat)
        else if (ANodeitem <> nil) and (ANodeitem.ReadAttributeFloat('mrkwidth') <> 0) then
          ANode.WriteAttributeFloat('mrkwidth', ANodeitem.ReadAttributeFloat('mrkwidth'))
        else if (ANodeStyle <> nil) and (ANodeStyle.ReadAttributeFloat('mrkwidth') <> 0) then
          ANode.WriteAttributeFloat('mrkwidth', ANodeStyle.ReadAttributeFloat('mrkwidth'))
        else if (ADataset.FieldByName('mrkwidth').AsFloat <> 0) then
          ANode.WriteAttributeFloat('mrkwidth', ADataset.FieldByName('mrkwidth').AsFloat);
        if (AItems.FieldByName('algorithm').AsString <> EmptyStr) then
          ANode.WriteAttributeString('algorithm', AItems.FieldByName('algorithm').AsString)
        else if (ANodeitem <> nil) and (ANodeitem.ReadAttributeString('algorithm') <> EmptyStr) then
          ANode.WriteAttributeString('algorithm', ANodeitem.ReadAttributeString('algorithm'))
        else if (ANodeStyle <> nil) and (ANodeStyle.ReadAttributeString('algorithm') <> EmptyStr) then
          ANode.WriteAttributeString('algorithm', ANodeStyle.ReadAttributeString('algorithm'))
        else if (ADataset.FieldByName('algorithm').AsString <> EmptyStr) then
          ANode.WriteAttributeString('algorithm', ADataset.FieldByName('algorithm').AsString);
        if (AItems.FieldByName('lrmethod').AsInteger <> 0) then
          ANode.WriteAttributeInteger('lrmethod', AItems.FieldByName('lrmethod').AsInteger)
        else if (ANodeitem <> nil) and (ANodeitem.ReadAttributeInteger('lrmethod') <> 0) then
          ANode.WriteAttributeInteger('lrmethod', ANodeitem.ReadAttributeInteger('lrmethod'))
        else if (ANodeStyle <> nil) and (ANodeStyle.ReadAttributeInteger('lrmethod') <> 0) then
          ANode.WriteAttributeInteger('lrmethod', ANodeStyle.ReadAttributeInteger('lrmethod'))
        else if (ADataset.FieldByName('lrmethod').AsInteger <> 0) then
          ANode.WriteAttributeInteger('lrmethod', ADataset.FieldByName('lrmethod').AsInteger);
        if (AItems.FieldByName('lrmethodcnt').AsInteger <> 0) then
          ANode.WriteAttributeInteger('lrmethodcnt', AItems.FieldByName('lrmethodcnt').AsInteger)
        else if (ANodeitem <> nil) and (ANodeitem.ReadAttributeInteger('lrmethodcnt') <> 0) then
          ANode.WriteAttributeInteger('lrmethodcnt', ANodeitem.ReadAttributeInteger('lrmethodcnt'))
        else if (ANodeStyle <> nil) and (ANodeStyle.ReadAttributeInteger('lrmethodcnt') <> 0) then
          ANode.WriteAttributeInteger('lrmethodcnt', ANodeStyle.ReadAttributeInteger('lrmethodcnt'))
        else if (ADataset.FieldByName('lrmethodcnt').AsInteger <> 0) then
          ANode.WriteAttributeInteger('lrmethodcnt', ADataset.FieldByName('lrmethodcnt').AsInteger);
        AStrStream := TStringStream.Create(EmptyStr);
        try
          TBlobField(AItems.FieldByName('dsndata')).SaveToStream(AStrStream);
          if AStrStream.Size > 0 then
          begin
            ASubNode := ANode.NodeNew('attachments');
            ASubSubNode := ASubNode.NodeNew('dsndata');
            ASubSubNode.WriteAttributeString('name', AItems.FieldByName('dsnfile').AsWideString);
            ASubSubNode.BinaryString := AStrStream.DataString;
          end;
        finally
          AStrStream.Free;
        end;
        AItems.Next;
      end;
    finally
      AItems.Free;
    end;
  end;

  procedure RetrieveRules;
  var
    AItems: TACRTable;
    ARoot, ANode: TXmlNode;
  begin
    AItems := GetACRTable('orrules', 'pk_orrules');
    ARoot := AXMLData.Root.NodeNew('rules');
    try
      AItems.Filter := ConstructFilter('ordno', ADataset.FieldByName('ordno').AsWideString);
      AItems.Filtered := True;
      AItems.Open;
      while not AItems.Eof do
      begin
        ANode := ARoot.NodeNew('rule');
        ANode.WriteAttributeString('ordno', AItems.FieldByName('ordno').AsWideString);
        ANode.WriteAttributeString('itmname', AItems.FieldByName('itmname').AsWideString);
        ANode.WriteAttributeString('mtcd', AItems.FieldByName('mtcd').AsWideString);
        ANode.WriteAttributeInteger('minlyr', AItems.FieldByName('minlyr').AsInteger);
        AItems.Next;
      end;
    finally
      AItems.Free;
    end;
  end;

var
  ANode, ASubNode, ASubSubNode: TXmlNode;
  AStrStream: TStringStream;
begin
  AXMLData.Root.Clear;
  AXMLData.Root.Name := 'rootorder';
  if (not ADataset.Active) or ADataset.IsEmpty then
    exit;
  AStrStream := TStringStream.Create(EmptyStr);
  try
    ANode := AXMLData.Root.NodeNew('orders')
                .NodeNew('order');
    ANode.WriteAttributeString('ordno', ADataset.FieldByName('ordno').AsWideString);
    ANode.WriteAttributeDateTime('orddate', ADataset.FieldByName('orddate').AsDateTime);
    ANode.WriteAttributeString('orddesc', ADataset.FieldByName('orddesc').AsWideString);
    ANode.WriteAttributeString('cstcode', ADataset.FieldByName('cstcode').AsWideString);
    ANode.WriteAttributeString('cstname', ADataset.FieldByName('cstname').AsWideString);
    ANode.WriteAttributeString('dsnfile', ADataset.FieldByName('dsnfile').AsWideString);
    ANode.WriteAttributeFloat('maxlen', ADataset.FieldByName('maxlen').AsFloat);
    ANode.WriteAttributeFloat('mrklen', ADataset.FieldByName('mrklen').AsFloat);
    ANode.WriteAttributeFloat('mrkwidth', ADataset.FieldByName('mrkwidth').AsFloat);
    ANode.WriteAttributeString('algorithm', ADataset.FieldByName('algorithm').AsWideString);
    ANode.WriteAttributeInteger('lrmethod', ADataset.FieldByName('lrmethod').AsInteger);
    ANode.WriteAttributeInteger('lrmethodcnt', ADataset.FieldByName('lrmethodcnt').AsInteger);
    TBlobField(ADataset.FieldByName('dsndata')).SaveToStream(AStrStream);
    if AStrStream.Size > 0 then
    begin
      ASubNode := ANode.NodeNew('attachments');
      ASubSubNode := ASubNode.NodeNew('dsndata');
      ASubSubNode.WriteAttributeString('name', ADataset.FieldByName('dsnfile').AsWideString);
      ASubSubNode.BinaryString := AStrStream.DataString;
    end;
    RetrieveStyles;
    RetrieveItems;
    RetrieveMaterials;
    RetrieveRules;
  finally
    AStrStream.Free;
  end;
end;

procedure TdmPackage.ClosePackage;
begin
  EnsureClosed;
end;

procedure TdmPackage.CompactPackage;
var
  ADatasetState: Variant;
begin
  if not FPkgDB.Connected then
    raise Exception.Create(_('Package has not been loaded'));
  ADatasetState := SaveDatasetState;
  EnsureClosed;
  FOnVerify := True;
  FPkgDB.OnProgress := OnDBProgress;
  FPkgDB.OnTableProgress := OnTableProgress;
  try
    frmProgress.Caption := _('Compact package');
    frmProgress.lbCaption.Caption := _('Compacting package...');
    frmProgress.DBProgress.Position := 0;
    frmProgress.TBProgress.Position := 0;
    frmProgress.DBProgress.Properties.Max := 100;
    frmProgress.DBProgress.Properties.Min := 0;
    frmProgress.TBProgress.Properties.Max := 100;
    frmProgress.TBProgress.Properties.Min := 0;
    frmProgress.Show; 
    FPkgDB.Exclusive := True;
    FPkgDB.CompactDatabase;
  finally
    frmProgress.Hide;
    FPkgDB.OnProgress := nil;
    FPkgDB.OnTableProgress := nil;
    EnsureClosed;
    FOnVerify := False;
    EnsureOpened;
    RestoreDatasetState(ADatasetState);
  end;
end;

constructor TdmPackage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPkgSession := TACRSession.Create(Self);
  FPkgDB := TACRDatabase.Create(Self);
  FPkgDB.HandleShared := True;
  FPkgSession.SessionName := 'PKGSESSION';
  FPkgDB.DatabaseName:= 'PKGDATABASE';
  FPkgDB.SessionName := FPkgSession.SessionName;
  FPackageInfo := TPackageInfoCollection.Create;
  FPkgDB.AfterConnect := OnPackageNotify;
  FPkgDB.AfterDisconnect := OnPackageNotify;
  FPkgDB.AfterConnectionLost := OnPackageNotify;
  FOnVerify := False;
end;

procedure TdmPackage.CreateItems(AItems: TItemRowCollection;
  AOnUpdateStatus: TUpdateStatusEvent);
var
  AItemDataset,
  AMaterialDataset,
  ARuleDataset: TDataSet;
  ADefAvgYY: Variant;

  procedure ApplyFilter(ADataset: TDataset; const AFilter: string; AValues: Variant);
  begin
    ADataset.Filtered := False;
    ADataset.Filter := ConstructFilter(AFilter, AValues);
    ADataset.Filtered := True;
    if not ADataset.Active then
      ADataset.Open;
  end;

  procedure DoCreateItem(const AName, AStyle, ACol, ASiz: string);
  var
    ADSStFeature, ADSStFtItem,
    ADSStMaterial, ADSStRules: TDataSet;
    AStyleName, AItemDesc: string;
    AUMCode: variant;
  begin
    ADSStFeature := GetACRTable('stfeature', 'pk_stfeature');
    ADSStFtItem := GetACRTable('stitmfeatures', 'pk_stitmfeatures');
    ADSStMaterial := GetACRTable('stmaterials', 'pk_stmaterials');
    ADSStRules := GetACRTable('strules', 'pk_strules');
    try
      AItemDataset.Append;
      AItemDataset.FieldValues['itmname'] := AName;
      AItemDataset.FieldValues['stname'] := AItems.Dataset.FieldValues['stname'];
      AItemDataset.FieldValues['umcode'] := AItems.Dataset.FieldValues['umcode'];
      ADefAvgYY := AItems.Dataset.FieldValues['avgyy'];
      AStyleName := AItems.Dataset.FieldByName('stdesc').AsString;
      AUMCode := AItems.Dataset.FieldValues['umcode'];
      
      // Get Y feature
      ApplyFilter(ADSStFeature, 'stname;ftgrp', VarArrayOf([AStyle,1]));
      ApplyFilter(ADSStFtItem, 'stname;ftname;fitname',
        VarArrayOf([AStyle,ADSStFeature.FieldValues['ftname'],ACol]));
      if ADSStFeature.IsEmpty then
        raise Exception.Create('Style has no Y-feature specified');
      if ADSStFtItem.IsEmpty then
        raise Exception.Create('Style has no Y-feature item specified');
        
      AItemDataset.FieldValues['yftname'] := ADSStFtItem.FieldValues['ftname'];
      AItemDataset.FieldValues['yfitname'] := ADSStFtItem.FieldValues['fitname'];
      
      ApplyFilter(ADSStMaterial, 'stname;ftname;fitname', VarArrayOf(
          [
            AStyle,
            ADSStFtItem.FieldByName('ftname').AsString,
            ADSStFtItem.FieldByName('fitname').AsString
          ]
        )
      );
      while not ADSStMaterial.Eof do
      begin
        if not RecordExist('itmmaterial','itmname;mtcd',
                  VarArrayOf(
                    [
                      AName,
                      ADSStMaterial.FieldByName('mtcd').AsString
                    ]
                  )
        ) then
        begin
          AMaterialDataset.Append;
          if (ADSStMaterial.FieldValues['umcode'] <> null) then
            AUMCode := ADSStMaterial.FieldValues['umcode'];

          AMaterialDataset.FieldValues['itmname'] := AName;
          AMaterialDataset.FieldValues['mtcd'] := ADSStMaterial.FieldValues['mtcd'];
          AMaterialDataset.FieldValues['mtname'] := ADSStMaterial.FieldValues['mtname'];
          AMaterialDataset.FieldValues['mtdesc'] := ADSStMaterial.FieldValues['mtdesc'];
          AMaterialDataset.FieldValues['mtcatcd'] := ADSStMaterial.FieldValues['mtcatcd'];
          AMaterialDataset.FieldValues['lrname'] := ADSStMaterial.FieldValues['lrname'];
          AMaterialDataset.FieldValues['lrdesc'] := ADSStMaterial.FieldValues['lrdesc'];
          AMaterialDataset.FieldValues['fballowance'] := ADSStMaterial.FieldValues['fballowance'];
          AMaterialDataset.FieldValues['fbaltype'] := ADSStMaterial.FieldValues['fbaltype'];
          AMaterialDataset.FieldValues['mtlayout'] := ADSStMaterial.FieldValues['mtlayout'];
          AMaterialDataset.FieldValues['mtwidth'] := ADSStMaterial.FieldValues['mtwidth'];
          AMaterialDataset.FieldValues['umcode'] := AUMCode;
          if (ADSStFtItem.FieldValues['avgyy'] <> null) then          
            AMaterialDataset.FieldValues['avgyy'] := ADSStFtItem.FieldValues['avgyy']
          else
            AMaterialDataset.FieldValues['avgyy'] := ADefAvgYY;            
          AMaterialDataset.Post;
        end;
        ADSStMaterial.Next;
      end;
      
      ApplyFilter(ADSStRules, 'stname;ftname;fitname', VarArrayOf(
          [
            AStyle,
            ADSStFtItem.FieldByName('ftname').AsString,
            ADSStFtItem.FieldByName('fitname').AsString
          ]
        )
      );
      while not ADSStRules.Eof do
      begin
        if not RecordExist('itmrules','itmname;mtcd;minlyr',
                  VarArrayOf(
                    [
                      AName,
                      ADSStRules.FieldByName('mtcd').AsString,
                      ADSStRules.FieldByName('minlyr').AsInteger
                    ]
                  )
        ) then
        begin      
          ARuleDataset.Append;
          ARuleDataset.FieldValues['itmname'] := AName;
          ARuleDataset.FieldValues['mtcd'] := ADSStRules.FieldValues['mtcd'];
          ARuleDataset.FieldValues['minlyr'] := ADSStRules.FieldValues['minlyr'];
          ARuleDataset.Post;
        end;
        ADSStRules.Next;
      end;

      // Get X feature
      ApplyFilter(ADSStFeature, 'stname;ftgrp', VarArrayOf([AStyle,0]));
      ApplyFilter(ADSStFtItem, 'stname;ftname;fitname',
        VarArrayOf([AStyle,ADSStFeature.FieldValues['ftname'],ASiz]));
      if ADSStFeature.IsEmpty then
        raise Exception.Create('Style has no X-feature specified');
      if ADSStFtItem.IsEmpty then
        raise Exception.Create('Style has no X-feature item specified');
        
      AItemDataset.FieldValues['xftname'] := ADSStFtItem.FieldValues['ftname'];
      AItemDataset.FieldValues['xfitname'] := ADSStFtItem.FieldValues['fitname'];
      AItemDataset.FieldValues['xfitprio'] := ADSStFtItem.FieldValues['fitprio'];
      AItemDataset.FieldValues['xfitprio2'] := ADSStFtItem.FieldValues['fitprio2'];

      FormatItemCode(AItemDesc, AItems.StyleFormat,
        AStyleName+' ', ACol+' ', ASiz+' ', False);
      AItemDataset.FieldValues['itmdesc'] := Trim(AItemDesc);
      AItemDataset.Post;
    finally
      ADSStFeature.Free;
      ADSStFtItem.Free;
      ADSStMaterial.Free;
      ADSStRules.Free;
    end;
  end;

var
  AItem: TItemRow;
  I, J, ACount: integer;
  AName, ACol, ASiz: string;
begin
  ACount := 0;
  AItemDataset := GetACRTable('items', 'pk_items');
  AMaterialDataset := GetACRTable('itmmaterial', 'pk_itmmaterial');
  ARuleDataset := GetACRTable('itmrules', 'pk_itmrules');
  try
    AItemDataset.Active := True;
    AMaterialDataset.Active := True;
    ARuleDataset.Active := True;
    for I := 0 to AItems.Count - 1 do
    begin
      AItem := TItemRow(AItems.Items[I]);
      ACol := AItem.Values[0];
      for J := 1 to AItems.FFields.Count - 1 do
      begin
        ASiz := AItems.FFields[J];
        if StrToBool(AItems.Values[ACol, ASiz]) then
        begin
          FormatItemCode(AName, AItems.StyleFormat, AItems.Style, ACol, ASiz);
          if not RecordExist('items', 'itmname', AName) then
          begin
            Inc(ACount);
            AOnUpdateStatus(Self, Format('Creating item: %s',[AName]));
            DoCreateItem(AName, AItems.Style, ACol, ASiz);
          end;
        end;
      end;
    end;
    if ACount > 0 then
      AOnUpdateStatus(Self, Format('Finished: %d item(s) created',[ACount]))
    else
      AOnUpdateStatus(Self, Format('Finished: all items has been created previously',[ACount]));

  finally
    AItemDataset.Free;
    AMaterialDataset.Free;
    ARuleDataset.Free;
  end;
end;

procedure TdmPackage.CreateOrderItems(AItems: TItemRowCollection;
  AOnUpdateStatus: TUpdateStatusEvent);
var
  AItemDataset,
  AMaterialDataset,
  ARuleDataset: TDataSet;

  procedure CopyOrderData(ADstDataset: TDataSet);
  var
    I: integer;
    AField: TField;
  begin
    for I := 0 to AItems.Dataset.FieldCount - 1 do
    begin
      AField := ADstDataset.FindField(AItems.Dataset.Fields[I].FieldName);
      if (AField <> nil) and
         (not SameText('ordno', AItems.Dataset.Fields[I].FieldName)) and
         (not SameText('dsnfile', AItems.Dataset.Fields[I].FieldName)) and
         (not SameText('dsndata', AItems.Dataset.Fields[I].FieldName)) then
        AField.Value := AItems.Dataset.Fields[I].Value;
    end;
  end;

  procedure ApplyFilter(ADataset: TDataset; const AFilter: string; AValues: Variant);
  begin
    ADataset.Filtered := False;
    ADataset.Filter := ConstructFilter(AFilter, AValues);
    ADataset.Filtered := True;
    if not ADataset.Active then
      ADataset.Open;
  end;

  procedure DoCreateItem(const AName: WideString; const APcs: Integer);
  var
    AOrdNo: WideString;
    AOrderItem, AOrderMaterial, AOrderRule: TDataSet;
    I: integer;
    AField: TField;
  begin
    AOrderItem := GetACRTable('oritems', 'pk_oritems');
    AOrderMaterial := GetACRTable('ormaterials', 'pk_ormaterials');
    AOrderRule := GetACRTable('orrules', 'pk_orrules');
    try
      AOrdNo := AItems.Dataset.FieldByName('ordno').AsWideString;
      ApplyFilter(AItemDataset,'itmname', AName);
      if not AItemDataset.IsEmpty then
      begin
        ApplyFilter(AOrderItem, 'ordno;itmname', VarArrayOf([AOrdNo, AName]));
        if AOrderItem.IsEmpty then
        begin
          AOrderItem.Append;
          AOrderItem.FieldValues['ordno;itmname'] := VarArrayOf([AOrdNo, AName]);           
        end else
          AOrderItem.Edit;
        CopyOrderData(AOrderItem);
        for I := 0 to AItemDataset.FieldCount - 1 do
        begin
          AField := AOrderItem.FindField(AItemDataset.Fields[I].FieldName);
          if (AField <> nil) and
             (not SameText('itmname', AItemDataset.Fields[I].FieldName)) then
            AField.Value := AItemDataset.Fields[I].Value;
        end;
        AOrderItem.FieldByName('orqty').Value := APcs;
        AOrderItem.Post;
        ApplyFilter(AMaterialDataset, 'itmname', AName);
        while not AMaterialDataset.Eof do
        begin
          ApplyFilter(AOrderMaterial, 'ordno;itmname;mtcd',
            VarArrayOf([
                AOrdNo,
                AName,
                AMaterialDataset.FieldByName('mtcd').AsWideString
              ]));
          if AOrderMaterial.IsEmpty then
          begin
            AOrderMaterial.Append;
            AOrderMaterial.FieldValues['ordno;itmname;mtcd'] :=
              VarArrayOf([
                  AOrdNo,
                  AName,
                  AMaterialDataset.FieldByName('mtcd').AsWideString
                ]);
          end else
            AOrderMaterial.Edit;
          CopyOrderData(AOrderMaterial);
          for I := 0 to AMaterialDataset.FieldCount - 1 do
          begin
            AField := AOrderMaterial.FindField(AMaterialDataset.Fields[I].FieldName);
            if (AField <> nil) and
              (
               (not SameText('itmname', AMaterialDataset.Fields[I].FieldName)) and
               (not SameText('mtcd', AMaterialDataset.Fields[I].FieldName))
              ) then
              AField.Value := AMaterialDataset.Fields[I].Value;
          end;
          if AMaterialDataset.FieldByName('mtwidth').AsFloat <> 0 then
            AOrderMaterial.FieldValues['mrkwidth'] :=
              AMaterialDataset.FieldValues['mtwidth'];
          AOrderMaterial.Post;
          ApplyFilter(ARuleDataset, 'itmname;mtcd',
            VarArrayOf([
                AName,
                AMaterialDataset.FieldByName('mtcd').AsWideString
              ]));
          ApplyFilter(AOrderRule, 'ordno;itmname;mtcd',
            VarArrayOf([
                AOrdNo,
                AName,
                AMaterialDataset.FieldByName('mtcd').AsWideString
              ]));
          while not AOrderRule.IsEmpty do
            AOrderRule.Delete;
          while not ARuleDataset.Eof do
          begin
            AOrderRule.Append;
            AOrderRule.FieldValues['ordno;itmname;mtcd;minlyr'] :=
              VarArrayOf([
                  AOrdNo,
                  AName,
                  AMaterialDataset.FieldByName('mtcd').AsWideString,
                  ARuleDataset.FieldValues['minlyr']
                ]);
            AOrderRule.Post;
            ARuleDataset.Next;
          end;
          AMaterialDataset.Next;
        end;
      end;
    finally
      AOrderItem.Free;
      AOrderMaterial.Free;
      AOrderRule.Free;
    end;
  end;

var
  AItem: TItemRow;
  I, J, ACount, APcs: integer;
  AName, ACol, ASiz: String;
begin
  ACount := 0;
  AItemDataset := GetACRTable('items', 'pk_items');
  AMaterialDataset := GetACRTable('itmmaterial', 'pk_itmmaterial');
  ARuleDataset := GetACRTable('itmrules', 'pk_itmrules');
  try
    AItemDataset.Active := True;
    AMaterialDataset.Active := True;
    ARuleDataset.Active := True;
    for I := 0 to AItems.Count - 1 do
    begin
      AItem := TItemRow(AItems.Items[I]);
      ACol := AItem.Values[0];
      for J := 1 to AItems.FFields.Count - 1 do
      begin
        ASiz := AItems.FFields[J];
        try
          APcs := StrToInt(AItems.Values[ACol, ASiz]);
        except
          APcs := 0;
        end;
        if APcs > 0 then
        begin
          FormatItemCode(AName, AItems.StyleFormat, AItems.Style, ACol, ASiz);
          if RecordExist('items', 'itmname', AName) then
          begin
            Inc(ACount);
            AOnUpdateStatus(Self, Format('Creating item: %s',[AName]));
            DoCreateItem(AName, APcs);
          end;
        end;
      end;
    end;
    if ACount > 0 then
      AOnUpdateStatus(Self, Format('Finished: %d item(s) created',[ACount]))
    else
      AOnUpdateStatus(Self, Format('Finished: all items has been created previously',[ACount]));
  finally
    AItemDataset.Free;
    AMaterialDataset.Free;
    ARuleDataset.Free;
  end;
end;

procedure TdmPackage.CreateOrUpdateTable(ATable: TACRTable);
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

procedure TdmPackage.CreatePackage(const AFileName: string);
begin
  EnsureClosed;
  if AFileName <> EmptyStr then
    FPkgDB.DatabaseFileName := AFileName;
  if FileExists(AFileName) then
    DeleteFile(AFileName);
  FPkgDB.CreateDatabase;
  CreateTables;
  EnsureOpened;
end;

procedure TdmPackage.CreateTables;
  procedure InitializePackage;
  begin
    PackageInfo.Clear;
    PackageInfo.Values['data_0'] := SCSPNewPackage;
    PackageInfo.Values['data_4'] := frmMain.LicenseManager.RegisteredCompany;     
    WritePackageInfo;
  end;
var
  ATable: TACRTable;
  ANeedInitialization: boolean;
begin
  EnsureClosed;
  FPkgDB.Exclusive := True;
  EnsureOpened;
  ATable := TACRTable.Create(Self);
  try
    ATable.DatabaseName := FPkgDB.DatabaseName;
    ATable.SessionName  := FPkgDB.SessionName;

    ATable.TableName := 'settings';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('keyname', aftWideString, 16, True);
    ATable.AdvFieldDefs.Add('refid_1', aftWideString, 8, True);
    ATable.AdvFieldDefs.Add('refid_2', aftWideString, 8, True);
    ATable.AdvFieldDefs.Add('refid_3', aftWideString, 8, True);
    ATable.AdvFieldDefs.Add('data', aftBlob);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_settings','keyname,refid_1,refid_2,refid_3',
      [ixPrimary]);
    ANeedInitialization := not ATable.Exists;    
    CreateOrUpdateTable(ATable);
    if ANeedInitialization then    
      InitializePackage;

    ATable.TableName := 'styles';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('stname', aftWideString, 12, True);
    ATable.AdvFieldDefs.Add('sttname', aftWideString, 8, True);
    ATable.AdvFieldDefs.Add('stdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('avgyy', aftDouble);
    ATable.AdvFieldDefs.Add('umcode', aftWideString, 4);
    ATable.AdvFieldDefs.Add('itmfmt', aftWideString, 32, True);
    ATable.AdvFieldDefs.Add('stitmcr', aftBoolean);
    ATable.AdvFieldDefs.Add('itmgen', aftBlob);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_styles','stname',
      [ixPrimary]);
    CreateOrUpdateTable(ATable);

    ATable.TableName := 'stfeature';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('stname', aftWideString, 12, True);
    ATable.AdvFieldDefs.Add('ftname', aftWideString, 8, True);
    ATable.AdvFieldDefs.Add('ftgrp', aftSmallint);
    ATable.AdvFieldDefs.Add('ftdesc', aftWideString, 48);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_stfeature','stname,ftgrp', [ixPrimary]);
    CreateOrUpdateTable(ATable);

    ATable.TableName := 'stitmfeatures';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('stname', aftWideString, 12, True);
    ATable.AdvFieldDefs.Add('ftname', aftWideString, 8, True);
    ATable.AdvFieldDefs.Add('fitname', aftWideString, 16, True);
    ATable.AdvFieldDefs.Add('fitprio', aftSmallint);
    ATable.AdvFieldDefs.Add('fitprio2', aftSmallint);
    ATable.AdvFieldDefs.Add('fitdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('avgyy', aftDouble);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_stitmfeatures','stname,ftname,fitname',
      [ixPrimary]);
    ATable.IndexDefs.Add('idx_stitmfeatures_1','stname,ftname,fitprio',
      []);
    ATable.IndexDefs.Add('idx_stitmfeatures_2','stname,ftname,fitprio2',
      []);
    CreateOrUpdateTable(ATable);      

    ATable.TableName := 'stmaterials';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('stname', aftWideString, 12, True);
    ATable.AdvFieldDefs.Add('ftname', aftWideString, 8, True);
    ATable.AdvFieldDefs.Add('fitname', aftWideString, 16, True);
    ATable.AdvFieldDefs.Add('mtcd', aftWideString, 10, True);
    ATable.AdvFieldDefs.Add('mtname', aftWideString, 32);
    ATable.AdvFieldDefs.Add('mtdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('avgthick', aftDouble);
    ATable.AdvFieldDefs.Add('mtcatcd', aftWideString, 16);
    ATable.AdvFieldDefs.Add('lrname', aftWideString, 12);
    ATable.AdvFieldDefs.Add('lrdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('umcode', aftWideString, 4);
    ATable.AdvFieldDefs.Add('fballowance', aftDouble);
    ATable.AdvFieldDefs.Add('fbaltype', aftSmallInt);
    ATable.AdvFieldDefs.Add('mtlayout', aftWideString, 32);
    ATable.AdvFieldDefs.Add('mtwidth',  aftDouble);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_stmaterials','stname,ftname,fitname,mtcd',
      [ixPrimary]);
    CreateOrUpdateTable(ATable);

    ATable.TableName := 'strules';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('stname', aftWideString, 12, True);
    ATable.AdvFieldDefs.Add('ftname', aftWideString, 8, True);
    ATable.AdvFieldDefs.Add('fitname', aftWideString, 16, True);
    ATable.AdvFieldDefs.Add('mtcd', aftWideString, 10, True);
    ATable.AdvFieldDefs.Add('minlyr', aftInteger);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_strules',
      'stname,ftname,fitname,mtcd,minlyr',
      [ixPrimary, ixDescending]);
    CreateOrUpdateTable(ATable);

    ATable.TableName := 'items';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('itmname', aftWideString, 48, True);
    ATable.AdvFieldDefs.Add('itmdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('stname', aftWideString, 12, True);
    ATable.AdvFieldDefs.Add('xftname', aftWideString, 8);
    ATable.AdvFieldDefs.Add('xfitname', aftWideString, 16);
    ATable.AdvFieldDefs.Add('xfitprio', aftSmallint);
    ATable.AdvFieldDefs.Add('xfitprio2', aftSmallint);
    ATable.AdvFieldDefs.Add('yftname', aftWideString, 8);
    ATable.AdvFieldDefs.Add('yfitname', aftWideString, 16);
    ATable.AdvFieldDefs.Add('umcode', aftWideString, 4);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_items',
      'itmname', [ixPrimary]);
    CreateOrUpdateTable(ATable);

    ATable.TableName := 'itmmaterial';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('itmname', aftWideString, 48, True);
    ATable.AdvFieldDefs.Add('mtcd', aftWideString, 10, True);
    ATable.AdvFieldDefs.Add('mtname', aftWideString, 32);
    ATable.AdvFieldDefs.Add('mtdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('avgthick', aftDouble);
    ATable.AdvFieldDefs.Add('mtcatcd', aftWideString, 16);
    ATable.AdvFieldDefs.Add('lrname', aftWideString, 12);
    ATable.AdvFieldDefs.Add('lrdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('avgyy', aftDouble);
    ATable.AdvFieldDefs.Add('umcode', aftWideString, 4);
    ATable.AdvFieldDefs.Add('fballowance', aftDouble);
    ATable.AdvFieldDefs.Add('fbaltype', aftSmallInt);
    ATable.AdvFieldDefs.Add('mtlayout', aftWideString, 32);
    ATable.AdvFieldDefs.Add('mtwidth',  aftDouble);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_itmmaterial','itmname,mtcd',
      [ixPrimary]);
    CreateOrUpdateTable(ATable);

    ATable.TableName := 'itmrules';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('itmname', aftWideString, 48, True);
    ATable.AdvFieldDefs.Add('mtcd', aftWideString, 10, True);
    ATable.AdvFieldDefs.Add('minlyr', aftInteger);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_itmrules',
      'itmname,mtcd,minlyr',
      [ixPrimary, ixDescending]);
    CreateOrUpdateTable(ATable);

{
    /*
      Actual Orders Definition
    */
}
    ATable.TableName := 'orders';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('ordno', aftWideString, 16, True);
    ATable.AdvFieldDefs.Add('orddate', aftDate);
    ATable.AdvFieldDefs.Add('orddesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('cstcode', aftWideString, 8);
    ATable.AdvFieldDefs.Add('cstname', aftWideString, 48);
    ATable.AdvFieldDefs.Add('dsnfile', aftWideString, 255);
    ATable.AdvFieldDefs.Add('dsndata', aftBlob);
    ATable.AdvFieldDefs.Add('maxlen',  aftDouble);
    ATable.AdvFieldDefs.Add('maxthick', aftDouble);
    ATable.AdvFieldDefs.Add('mrklen',  aftDouble);
    ATable.AdvFieldDefs.Add('mrkwidth',  aftDouble);
    ATable.AdvFieldDefs.Add('algorithm',  aftWideString, 16);
    ATable.AdvFieldDefs.Add('lrmethod',  aftInteger);
    ATable.AdvFieldDefs.Add('lrmethodcnt',  aftInteger);
    ATable.AdvFieldDefs.Add('optresult', aftBlob);

    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_orders',
      'ordno',
      [ixPrimary, ixDescending]);
    ATable.IndexDefs.Add('idx_orders_1',
      'orddate', [ixDescending]);
    CreateOrUpdateTable(ATable);

{
    /*
      Actual style per order definition
    */
}
    ATable.TableName := 'orstyles';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('ordno', aftWideString, 16, True);
    ATable.AdvFieldDefs.Add('stname', aftWideString, 12, True);
    ATable.AdvFieldDefs.Add('sttname', aftWideString, 8, True);
    ATable.AdvFieldDefs.Add('stdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('avgyy', aftDouble);
    ATable.AdvFieldDefs.Add('avgthick', aftDouble);    
    ATable.AdvFieldDefs.Add('umcode', aftWideString, 4);
    ATable.AdvFieldDefs.Add('dsnfile', aftWideString, 255);
    ATable.AdvFieldDefs.Add('dsndata', aftBlob);
    ATable.AdvFieldDefs.Add('maxlen',  aftDouble);
    ATable.AdvFieldDefs.Add('maxthick', aftDouble);
    ATable.AdvFieldDefs.Add('mrklen',  aftDouble);
    ATable.AdvFieldDefs.Add('mrkwidth',  aftDouble);
    ATable.AdvFieldDefs.Add('algorithm',  aftWideString, 16);
    ATable.AdvFieldDefs.Add('lrmethod',  aftInteger);
    ATable.AdvFieldDefs.Add('lrmethodcnt',  aftInteger);
    
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_orstyles',
      'ordno,stname',
      [ixPrimary]);
    CreateOrUpdateTable(ATable);
    
{
    /*
      Actual item per order definition
    */
}    
    ATable.TableName := 'oritems';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('ordno', aftWideString, 16, True);
    ATable.AdvFieldDefs.Add('itmname', aftWideString, 48, True);
    ATable.AdvFieldDefs.Add('itmdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('stname', aftWideString, 12, True);
    ATable.AdvFieldDefs.Add('xftname', aftWideString, 8);
    ATable.AdvFieldDefs.Add('xfitname', aftWideString, 16);
    ATable.AdvFieldDefs.Add('xfitprio', aftSmallint);
    ATable.AdvFieldDefs.Add('xfitprio2', aftSmallint);
    ATable.AdvFieldDefs.Add('yftname', aftWideString, 8);
    ATable.AdvFieldDefs.Add('yfitname', aftWideString, 16);    
    ATable.AdvFieldDefs.Add('orqty',  aftdouble);
    ATable.AdvFieldDefs.Add('umcode', aftWideString, 4);
    ATable.AdvFieldDefs.Add('dsnfile', aftWideString, 255);
    ATable.AdvFieldDefs.Add('dsndata', aftBlob);
    ATable.AdvFieldDefs.Add('maxlen',  aftDouble);
    ATable.AdvFieldDefs.Add('maxthick', aftDouble);
    ATable.AdvFieldDefs.Add('mrklen',  aftDouble);
    ATable.AdvFieldDefs.Add('mrkwidth',  aftDouble);
    ATable.AdvFieldDefs.Add('algorithm',  aftWideString, 16);
    ATable.AdvFieldDefs.Add('lrmethod',  aftInteger);
    ATable.AdvFieldDefs.Add('lrmethodcnt',  aftInteger);
      
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_oritems',
      'ordno,itmname',
      [ixPrimary]);
    ATable.IndexDefs.Add('idx_oritems_1',
      'ordno,xftname,xfitprio', []);
    ATable.IndexDefs.Add('idx_oritems_2',
      'ordno,xftname,xfitprio2', []);
    CreateOrUpdateTable(ATable);
{
    /*
      Actual material used for item per order definition
    */
}
    ATable.TableName := 'ormaterials';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('ordno', aftWideString, 16, True);
    ATable.AdvFieldDefs.Add('itmname', aftWideString, 48, True);
    ATable.AdvFieldDefs.Add('mtcd', aftWideString, 10, True);
    ATable.AdvFieldDefs.Add('mtname', aftWideString, 32);
    ATable.AdvFieldDefs.Add('mtdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('lrname', aftWideString, 12);
    ATable.AdvFieldDefs.Add('lrdesc', aftWideString, 48);
    ATable.AdvFieldDefs.Add('avgyy', aftDouble);
    ATable.AdvFieldDefs.Add('avgthick', aftDouble);
    ATable.AdvFieldDefs.Add('mtcatcd', aftWideString, 16);
    ATable.AdvFieldDefs.Add('umcode', aftWideString, 4);
    ATable.AdvFieldDefs.Add('fballowance', aftDouble);
    ATable.AdvFieldDefs.Add('fbaltype', aftSmallInt);
    ATable.AdvFieldDefs.Add('mtlayout', aftWideString, 32);
    ATable.AdvFieldDefs.Add('dsnfile', aftWideString, 255);
    ATable.AdvFieldDefs.Add('dsndata', aftBlob);
    ATable.AdvFieldDefs.Add('maxlen',  aftDouble);
    ATable.AdvFieldDefs.Add('mrklen',  aftDouble);
    ATable.AdvFieldDefs.Add('maxthick', aftDouble);
    ATable.AdvFieldDefs.Add('mrkwidth',  aftDouble);
    ATable.AdvFieldDefs.Add('algorithm',  aftWideString, 16);
    ATable.AdvFieldDefs.Add('lrmethod',  aftInteger);
    ATable.AdvFieldDefs.Add('lrmethodcnt',  aftInteger);

    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_ormaterials',
      'ordno,itmname,mtcd',
      [ixPrimary]);
    CreateOrUpdateTable(ATable);
{
    /*
      Actual material used for item per order definition
    */
}    
    ATable.TableName := 'orrules';
    ATable.AdvFieldDefs.Clear;
    ATable.AdvFieldDefs.Add('ordno', aftWideString, 16, True);
    ATable.AdvFieldDefs.Add('itmname', aftWideString, 48, True);
    ATable.AdvFieldDefs.Add('mtcd', aftWideString, 10, True);
    ATable.AdvFieldDefs.Add('minlyr', aftInteger);
    ATable.IndexDefs.Clear;
    ATable.IndexDefs.Add('pk_orrules',
      'ordno,itmname,mtcd,minlyr',
      [ixPrimary, ixDescending]);
    CreateOrUpdateTable(ATable);        
  finally
    ATable.Free;
    EnsureClosed;
    FPkgDB.Exclusive := False;
  end;
end;

destructor TdmPackage.Destroy;
begin
  EnsureClosed;
  FPkgSession.Free;
  FPkgDB.Free;
  inherited Destroy;
end;

procedure TdmPackage.EnsureClosed;
begin
  if not FPkgDB.Connected then
    exit;
  if FPkgDB.InTransaction then
    FPkgDB.Commit;
  if FPkgSession.Active then
    FPkgSession.Close;
  FPkgDB.Close;
end;

procedure TdmPackage.EnsureOpened;
begin
  if FPkgDB.Connected then
    exit;
  FPkgDB.Open;
end;

function TdmPackage.ExecSQL(ASQL: string): boolean;
var
  AQuery: TACRQuery;
begin
  Result := False;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FPkgDB.DatabaseName;
  AQuery.SessionName := FPkgDB.SessionName;
  try
    AQuery.SQL.Text := ASQL;
    try
      if not FPkgDB.InTransaction then
        FPkgDB.StartTransaction;

      AQuery.ExecSQL;

      if FPkgDB.InTransaction then
        FPkgDB.Commit;
      Result := True;
    except
      if FPkgDB.InTransaction then
        FPkgDB.Rollback;
      raise;
    end;
  finally
    FreeAndNil(AQuery);
  end;
end;

function TdmPackage.ExecSQL(ASQL: string; AValues: Variant): boolean;
var
  AQuery: TACRQuery;
  I: integer;
begin
  Result := False;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FPkgDB.DatabaseName;
  AQuery.SessionName := FPkgDB.SessionName;
  try
    AQuery.SQL.Text := ASQL;
    try
      if not AQuery.Prepared then
      AQuery.Prepare;
      if  VarIsArray(AValues) then
      begin
        for i := 0 to VarArrayHighBound(AValues,1) do
        begin
          AQuery.Params[i].Value := AValues[i];
        end;
      end else
        if AValues <> null then
          AQuery.Params[0].Value := AValues;
      if not FPkgDB.InTransaction then
        FPkgDB.StartTransaction;

      AQuery.ExecSQL;

      if FPkgDB.InTransaction then
        FPkgDB.Commit;
      Result := True;
    except
      if FPkgDB.InTransaction then
        FPkgDB.Rollback;
      raise;
    end;
  finally
    FreeAndNil(AQuery);
  end;
end;

procedure TdmPackage.ExtractXMLColors(AXMLData: TNativeXml);
  function FindOrCreateColor(ARoot: TXmlNode;
                            const AFtName, AFitName: WideString): TXmlNode;
  var
    I: integer;
    AItem: TXmlNode;
  begin
    Result := nil;
    for I := 0 to ARoot.NodeCount - 1 do
    begin
      AItem := ARoot.Nodes[i];
      if (AItem.Name = 'color') and
         (AItem.AttributeByNameWide['ftname'] = AFtName) and
         (AItem.AttributeByNameWide['fitname'] = AFitName) then
      begin
        Result := AItem;
        Break;
      end;
    end;
  end;

var
  I: integer;
  ARootNode, ANode, AItemNode, AItem: TXmlNode;
begin
  ARootNode := AXMLData.Root.FindNode('allcolors');
  if ARootNode = nil then
    ARootNode := AXMLData.Root.NodeNew('allcolors');
  AItemNode := AXMLData.Root.FindNode('items');
  for I := 0 to AItemNode.NodeCount - 1 do
  begin
    AItem := AItemNode.Nodes[I];
    ANode := FindOrCreateColor(ARootNode,
                AItem.AttributeByNameWide['yftname'],
                AItem.AttributeByNameWide['yfitname']);
    if ANode = nil then
    begin
      ANode := ARootNode.NodeNew('color');
      ANode.AttributeByNameWide['ftname'] := AItem.AttributeByNameWide['yftname'];
      ANode.AttributeByNameWide['fitname']:= AItem.AttributeByNameWide['yfitname'];
    end;
  end;
end;

procedure TdmPackage.ExtractXMLMaterials(AXMLData: TNativeXml);

  procedure ExpandStyle(ANode: TXmlNode; const AStName: WideString);
  var
    ARoot, AItem: TXmlNode;
    I: integer;
  begin
    ANode.AttributeByNameWide['stname'] := AStName;
    ARoot := AXMLData.Root.FindNode('styles');
    if ARoot <> nil then
    begin
      for I := 0 to ARoot.NodeCount - 1 do
      begin
        AItem := ARoot.Nodes[I];
        if AItem.AttributeByNameWide['stname'] = AStName then
        begin
          ANode.Attributes.Assign(AItem.Attributes);
          Break;
        end;
      end;
    end;
  end;

  function FindOrCreateStyle(ARoot: TXmlNode;
                            const AStName: WideString): TXmlNode;
  var
    I: integer;
    AItem: TXmlNode;
  begin
    Result := nil;
    for I := 0 to ARoot.NodeCount - 1 do
    begin
      AItem := ARoot.Nodes[i];
      if (AItem.Name = 'style') and
         (AItem.AttributeByNameWide['stname'] = AStName) then
      begin
        Result := AItem;
        Break;
      end;
    end;
    if Result = nil then
    begin
      Result := ARoot.NodeNew('style');
      ExpandStyle(Result, AStName);
    end;
  end;

  function FindMaterial(AStyleRoot: TXmlNode;
      const AFtName, AFitName, MtCD: WideString): TXmlNode;
  var
    I: integer;
    ANode: TXmlNode;
  begin
    Result := nil;
    for I := 0 to AStyleRoot.NodeCount - 1 do
    begin
      ANode := AStyleRoot.Nodes[I];
      if (ANode.Name = 'material') and
         (ANode.AttributeByNameWide['ftname'] = AFtName) and
         (ANode.AttributeByNameWide['fitname'] = AFitName) and 
         (ANode.AttributeByNameWide['mtcd'] = MtCD) then
      begin
        Result := ANode;
        Break;
      end;
    end;
    if Result = nil then
    begin
      Result := AStyleRoot.NodeNew('material');
      Result.AttributeByNameWide['mtcd'] := MtCD;
      Result.AttributeByNameWide['ftname'] := AFtName;
      Result.AttributeByNameWide['fitname'] := AFitName;
    end;         
  end;

  procedure JoinItemSize(AMaterial, AItem: TXmlNode);
  var
    I: integer;
    ARoot, ANode: TXmlNode;
    AFound: Boolean;
  begin
    AFound := False;
    ARoot := AMaterial.FindNode('sizes');
    if ARoot = nil then
      ARoot := AMaterial.NodeNew('sizes');
    for I := 0 to ARoot.NodeCount - 1 do
    begin
      ANode := ARoot.Nodes[I];
      if (ANode.Name = 'size')
         and (ANode.AttributeByNameWide['ftname'] = AItem.AttributeByNameWide['xftname'])
         and (ANode.AttributeByNameWide['fitname'] = AItem.AttributeByNameWide['xfitname'])
         then
      begin
        AFound := True;
        break;
      end;
    end;
    if not AFound then
    begin
      ANode := ARoot.NodeNew('size');
      ANode.AttributeByNameWide['ftname'] := AItem.AttributeByNameWide['xftname'];
      ANode.AttributeByNameWide['fitname'] := AItem.AttributeByNameWide['xfitname'];
      ANode.AttributeByNameWide['umcode'] := AItem.AttributeByNameWide['umcode'];
      ANode.AttributeByNameWide['dsnfile'] := AItem.AttributeByNameWide['dsnfile'];
      ANode.AttributeByNameWide['mrklen'] := AItem.AttributeByNameWide['mrklen'];
      ANode.AttributeByNameWide['mrkwidth'] := AItem.AttributeByNameWide['mrkwidth'];
      ANode.AttributeByNameWide['algorithm'] := AItem.AttributeByNameWide['algorithm'];
      ANode.AttributeByNameWide['lrmethod'] := AItem.AttributeByNameWide['lrmethod'];
      ANode.AttributeByNameWide['lrmethodcnt'] := AItem.AttributeByNameWide['lrmethodcnt'];
    end;
    ANode.WriteAttributeFloat('orqty', ANode.ReadAttributeFloat('orqty') +
      AItem.ReadAttributeFloat('orqty'));
  end;

  procedure JoinRule(ARuleRoot, AMaterial, AItem: TXmlNode);
    function FindRule(ARoot: TXmlNode; const AMinLyr: Integer): TXmlNode;
    var
      I: integer;
      ANode: TXmlNode;
    begin
      Result := nil;
      for I := 0 to ARoot.NodeCount - 1 do
      begin
        ANode := ARoot.Nodes[I];
        if    (ANode.Name = 'rule')
          and (ANode.ReadAttributeInteger('minlyr') = AMinLyr)
        then begin
            Result := ANode;
            Break;
        end;
      end;
    end;
  var
    I: integer;
    ARoot, ANode, ARule: TXmlNode;
  begin
    ARoot := AMaterial.FindNode('rules');
    if ARoot = nil then
      ARoot := AMaterial.NodeNew('rules');
    for I := 0 to ARuleRoot.NodeCount - 1 do
    begin
      ANode := ARuleRoot.Nodes[I];
      if    (ANode.Name = 'rule')
        and (ANode.ReadAttributeString('ordno') = AItem.ReadAttributeString('ordno'))
        and (ANode.ReadAttributeString('itmname') = AItem.ReadAttributeString('itmname'))
        and (ANode.ReadAttributeString('mtcd') = AMaterial.ReadAttributeString('mtcd'))
      then begin
        ARule := FindRule(ARoot, ANode.ReadAttributeInteger('minlyr'));
        if ARule = nil then
        begin
          ARule := ARoot.NodeNew('rule');
          ARule.WriteAttributeInteger('minlyr', ANode.ReadAttributeInteger('minlyr'));
        end;
      end;     
    end;      
  end;

  procedure ExpandMaterials(AMaterialRoot, ARuleRoot, AStyleRoot, AItem: TXmlNode);
  var
    I: integer;
    AMaterial, ANode: TXmlNode;
  begin
    for I := 0 to AMaterialRoot.NodeCount - 1 do
    begin
      ANode := AMaterialRoot.Nodes[I];
      if (ANode.Name = 'material') and
         (ANode.AttributeByNameWide['itmname'] = AItem.AttributeByNameWide['itmname']) then
      begin
        AMaterial := FindMaterial(AStyleRoot,
                        ANode.AttributeByNameWide['yftname'],
                        ANode.AttributeByNameWide['yfitname'],
                        ANode.AttributeByNameWide['mtcd']);
        AMaterial.AttributeByNameWide['mtname'] := ANode.AttributeByNameWide['mtname'];
        AMaterial.AttributeByNameWide['mtdesc'] := ANode.AttributeByNameWide['mtdesc'];
        AMaterial.AttributeByNameWide['mtcatcd'] := ANode.AttributeByNameWide['mtcatcd'];
        AMaterial.AttributeByNameWide['lrname'] := ANode.AttributeByNameWide['lrname'];
        AMaterial.AttributeByNameWide['lrdesc'] := ANode.AttributeByNameWide['lrdesc'];
        AMaterial.AttributeByNameWide['avgyy'] := ANode.AttributeByNameWide['avgyy'];
        AMaterial.AttributeByNameWide['avgthick'] := ANode.AttributeByNameWide['avgthick'];        
        AMaterial.AttributeByNameWide['umcode'] := ANode.AttributeByNameWide['umcode'];
        AMaterial.AttributeByNameWide['fballowance'] := ANode.AttributeByNameWide['fballowance'];
        AMaterial.AttributeByNameWide['fbaltype'] := ANode.AttributeByNameWide['fbaltype'];
        AMaterial.AttributeByNameWide['mtlayout'] := ANode.AttributeByNameWide['mtlayout'];
        AMaterial.AttributeByNameWide['mtwidth'] := ANode.AttributeByNameWide['mtwidth'];
        AMaterial.AttributeByNameWide['dsnfile'] := ANode.AttributeByNameWide['dsnfile'];
        AMaterial.AttributeByNameWide['maxthick'] := ANode.AttributeByNameWide['maxthick'];
        AMaterial.AttributeByNameWide['mrklen'] := ANode.AttributeByNameWide['mrklen'];
        AMaterial.AttributeByNameWide['mrkwidth'] := ANode.AttributeByNameWide['mrkwidth'];
        AMaterial.AttributeByNameWide['algorithm'] := ANode.AttributeByNameWide['algorithm'];
        AMaterial.AttributeByNameWide['lrmethod'] := ANode.AttributeByNameWide['lrmethod'];
        AMaterial.AttributeByNameWide['lrmethodcnt'] := ANode.AttributeByNameWide['lrmethodcnt'];
        if (AMaterial.ReadAttributeFloat('avgyy') = 0) and (AStyleRoot.HasAttribute('avgyy')) then
          AMaterial.AttributeByNameWide['avgyy'] := AStyleRoot.AttributeByNameWide['avgyy'];
        if (AMaterial.ReadAttributeFloat('avgthick') = 0) and (AStyleRoot.HasAttribute('avgthick')) then
          AMaterial.AttributeByNameWide['avgthick'] := AStyleRoot.AttributeByNameWide['avgthick'];
        if (AMaterial.AttributeByName['umcode'] = EmptyStr) and (AStyleRoot.HasAttribute('umcode')) then
          AMaterial.AttributeByNameWide['umcode'] := AStyleRoot.AttributeByNameWide['umcode'];
        if (AMaterial.ReadAttributeFloat('maxthick') = 0) and (AStyleRoot.HasAttribute('maxthick')) then
          AMaterial.AttributeByNameWide['maxthick'] := AStyleRoot.AttributeByNameWide['maxthick'];
        if (AMaterial.ReadAttributeFloat('mrklen') = 0) and (AStyleRoot.HasAttribute('mrklen')) then
          AMaterial.AttributeByNameWide['mrklen'] := AStyleRoot.AttributeByNameWide['mrklen'];
        if (AMaterial.ReadAttributeFloat('mrkwidth') = 0) and (AStyleRoot.HasAttribute('mrkwidth')) then
          AMaterial.AttributeByNameWide['mrkwidth'] := AStyleRoot.AttributeByNameWide['mrkwidth'];
        JoinItemSize(AMaterial, AItem);
        JoinRule(ARuleRoot, AMaterial, AItem);
      end;
    end;
  end;

var
  I: integer;
  ARootNode, AItemRoot, AMaterialRoot, ARuleRoot: TXmlNode;
  AStyleRoot,
  ANode, AItem: TXmlNode;
begin
  ARootNode := AXMLData.Root.FindNode('breakdown');
  if ARootNode = nil then
    ARootNode := AXMLData.Root.NodeNew('breakdown');
  AItemRoot := AXMLData.Root.FindNode('items');
  AMaterialRoot := AXMLData.Root.FindNode('materials');
  ARuleRoot := AXMLData.Root.FindNode('rules');
  for I := 0 to AItemRoot.NodeCount - 1 do
  begin
    AItem := AItemRoot.Nodes[I];
    AStyleRoot := FindOrCreateStyle(ARootNode,
                                    AItem.AttributeByNameWide['stname']);
    ExpandMaterials(AMaterialRoot, ARuleRoot, AStyleRoot, AItem);
  end;
end;

procedure TdmPackage.ExtractXMLSizes(AXMLData: TNativeXml);
  function FindOrCreateSize(ARoot: TXmlNode;
                            const AFtName, AFitName: WideString): TXmlNode;
  var
    I: integer;
    AItem: TXmlNode;
  begin
    Result := nil;
    for I := 0 to ARoot.NodeCount - 1 do
    begin
      AItem := ARoot.Nodes[i];
      if (AItem.Name = 'size') and
         (AItem.AttributeByNameWide['ftname'] = AFtName) and
         (AItem.AttributeByNameWide['fitname'] = AFitName) then
      begin
        Result := AItem;
        Break;
      end;
    end;
  end;

var
  I: integer;
  ARootNode, ANode, AItemNode, AItem: TXmlNode;
begin
  ARootNode := AXMLData.Root.FindNode('allsizes');
  if ARootNode = nil then
    ARootNode := AXMLData.Root.NodeNew('allsizes');
  AItemNode := AXMLData.Root.FindNode('items');
  for I := 0 to AItemNode.NodeCount - 1 do
  begin
    AItem := AItemNode.Nodes[I];
    ANode := FindOrCreateSize(ARootNode,
                AItem.AttributeByNameWide['xftname'],
                AItem.AttributeByNameWide['xfitname']);
    if ANode = nil then
    begin
      ANode := ARootNode.NodeNew('size');
      ANode.AttributeByNameWide['ftname'] := AItem.AttributeByNameWide['xftname'];
      ANode.AttributeByNameWide['fitname']:= AItem.AttributeByNameWide['xfitname'];
      ANode.WriteAttributeInteger('fitprio', AItem.ReadAttributeInteger('xfitprio'));
      ANode.WriteAttributeInteger('fitprio2', AItem.ReadAttributeInteger('xfitprio2'));         
    end;
  end;
end;

function TdmPackage.GetACRTable(const ATableName,
  AIndexName: string): TACRTable;
begin
  Result := TACRTable.Create(Self);
  Result.DatabaseName := FPkgDB.DatabaseName;
  Result.SessionName := FPkgDB.SessionName;
  Result.TableName := ATableName;
  if AIndexName <> '' then
    Result.IndexName := AIndexName;
end;

function TdmPackage.GetDatabaseFile: string;
begin
  Result := FPkgDB.DatabaseFileName;
end;

function TdmPackage.SaveDatasetState: Variant;
var
  I, ACount: integer;
begin
  ACount := FPkgDB.DataSetCount;
  Result := VarArrayCreate([0, ACount], varVariant);
  for I := 0 to ACount - 1 do
    Result[I] := FPkgDB.DataSets[I].Active;
end;

function TdmPackage.GetDatasource(ADataset: TDataset): TDataSource;
begin
  Result := TDataSource.Create(Self);
  Result.DataSet := ADataset;
end;

function TdmPackage.GetPackageStatus: Boolean;
begin
  Result := FPkgDB.Connected;
end;

function TdmPackage.LastID(ATableName, AFieldName: string): Variant;
var
  AQuery: TACRQuery;
begin
  Result := False;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FPkgDB.DatabaseName;
  AQuery.SessionName := FPkgDB.SessionName;
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

procedure TdmPackage.MaintainOrderStyle(const AOrdNo: string);
var
  AOrdTB, AOrdItemTB, AOrdStyleTB, AStyleTB: TACRTable;

  procedure CopyData(ASrcDataset, ADstDataset: TDataSet; const AExceptField: string);
  var
    I: integer;
    AField: TField;
    AParams: TECFParams;
  begin
    AParams := TECFParams.Create(nil);
    try
      AParams.ParseParams(AExceptField);
      for I := 0 to ASrcDataset.FieldCount - 1 do
      begin
        if AParams.FindParam(ASrcDataset.Fields[I].FieldName) = nil then
        begin
          AField := ADstDataset.FindField(ASrcDataset.Fields[I].FieldName);
          if AField <> nil then
            AField.Value := ASrcDataset.Fields[I].Value;
        end;
      end;
    finally
      AParams.Free;
    end;
  end;  

  procedure DoAutoInsertStyle;
  var
    AStName: string;
  begin
    AOrdItemTB.Filter := ConstructFilter('ordno', AOrdNo);
    AOrdTB.Active := True;
    AOrdItemTB.Active := True;
    AOrdStyleTB.Active := True;
    try
      AOrdItemTB.First;
      while not AOrdItemTB.Eof do
      begin
        AStName := AOrdItemTB.FieldValues['stname'];
        if AStName <> EmptyStr then
        begin
          if not AOrdStyleTB.FindKey([AOrdNo, AStName]) then
          begin
            AStyleTB.Active := False;
            AStyleTB.Filter := ConstructFilter('stname', AStName);
            AStyleTB.Filtered := True;
            AStyleTB.Active := True;
            if (not AStyleTB.IsEmpty) and (not AOrdTB.IsEmpty) then
            begin
              AOrdStyleTB.Append;
              CopyData(AStyleTB, AOrdStyleTB, '');
              CopyData(AOrdTB, AOrdStyleTB, 'dsnfile;dsndata');
              AOrdStyleTB.Post;
            end;
          end;
        end;
        AOrdItemTB.Next;
      end;
    finally
      AOrdItemTB.Active := False;
      AOrdTB.Active := False;
      AStyleTB.Active := False;
      AOrdStyleTB.Active := False;
    end;
  end;

  procedure DoAutoSyncStyle;
  var
    I: integer;
    AStName: string;
  begin
    AOrdTB.Active := True;
    AOrdItemTB.Active := True;
    AOrdStyleTB.Active := True;
    try
      AOrdStyleTB.First;
      while not AOrdStyleTB.Eof do
      begin
        AStName := AOrdStyleTB.FieldValues['stname'];
        if AStName <> EmptyStr then
        begin
          AOrdItemTB.Active := False;
          AOrdItemTB.Filter := ConstructFilter('ordno;stname',
            VarArrayOf([AOrdNo, AStName]));
          AOrdItemTB.Filtered := True;
          AOrdItemTB.Active := True;
          if not AOrdItemTB.IsEmpty then
            AOrdStyleTB.Next
          else
            AOrdStyleTB.Delete;
        end else
          AOrdStyleTB.Delete;
      end;
    finally
      AOrdItemTB.Active := False;
      AOrdTB.Active := False;
      AStyleTB.Active := False;
    end;
  end;
  
begin
  AOrdTB := GetACRTable('orders', 'pk_orders');
  AOrdItemTB := GetACRTable('oritems', 'pk_oritems');
  AOrdStyleTB := GetACRTable('orstyles', 'pk_orstyles');
  AStyleTB := GetACRTable('styles', 'pk_styles'); 
  try
    AOrdTB.Filter := ConstructFilter('ordno', AOrdNo);
    AOrdStyleTB.Filter := ConstructFilter('ordno', AOrdNo);
    DoAutoInsertStyle;
    DoAutoSyncStyle;
  finally
    AOrdStyleTB.Free;
    AOrdItemTB.Free;
    AOrdTB.Free;
    AStyleTB.Free;
  end;  
end;

procedure TdmPackage.OnDBProgress(Sender: TComponent; Progress: Double;
  Operation: TACRDatabaseOperation; var Abort: Boolean);
begin
 frmProgress.DBProgress.Position := round(Progress);
 Application.ProcessMessages;
end;

procedure TdmPackage.OnPackageNotify(Sender: TObject);
begin
  if FOnVerify then exit;
  if FPkgDB.Connected then
    ReadPackageInfo;
  if Assigned(FNotifyMainForm) then
    FNotifyMainForm(Sender);  
end;

procedure TdmPackage.OnTableProgress(Sender: TComponent; Progress: Double;
  Operation: TACRTableOperation; var Abort: Boolean);
begin
 frmProgress.TBProgress.Position := round(Progress);
 Application.ProcessMessages;
end;

procedure TdmPackage.OpenPackage(const AFileName: string);
begin
  EnsureClosed;
  if AFileName <> EmptyStr then
    FPkgDB.DatabaseFileName := AFileName;
  VerifyDatabase;
  EnsureOpened;
end;

procedure TdmPackage.ReadPackageInfo;
var
  AMemory: TMemoryStream;
  ARegXML: TRegXML;
  ADataset: TDataSet;
begin
  AMemory := TMemoryStream.Create;
  ARegXML := TRegXML.Create(self);
  ADataset :=   GetACRTable('settings', 'pk_settings');
  try
    ADataset.Filter := ConstructFilter('keyname;refid_1;refid_2;refid_3',
      VarArrayOf(['RDB$PROPS', 0, 0, 0]));
    if TACRTable(ADataset).Exists then
    begin
      ADataset.Open;
      if not ADataset.IsEmpty then
      begin
        try
          TBlobField(ADataset.FieldByName('data')).SaveToStream(AMemory);
          AMemory.Position := 0;
          if (AMemory.Size > 0) then          
            ARegXML.LoadFromStream(AMemory);
          FPackageInfo.ReadXML(ARegXML, 'properties');
        except; end;
      end;
    end;    
  finally
    ADataset.Free;
    ARegXML.Free;
    AMemory.Free;
  end;
end;

function TdmPackage.RecordExist(const ATableName, AKeyFields: string;
  const AValues: variant): boolean;
var
  AQuery: TACRQuery;
  Pos: integer;
  sFieldName: string;
  lHasField: boolean;
  nFieldCount: integer;
begin
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FPkgDB.DatabaseName;
  AQuery.SessionName := FPkgDB.SessionName;
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

procedure TdmPackage.RefreshPackageInfo;
begin
  ReadPackageInfo;
end;

procedure TdmPackage.RegisterXMLEngines(AXMLData: TNativeXml);
var
  AList: TStringList;
  I: integer;
  ARoot, ANode: TXmlNode;
  AClass: TCSPBaseOptimizerClass;
begin
  ARoot := AXMLData.Root.FindNode('engines');
  if ARoot = nil then
    ARoot := AXMLData.Root.NodeNew('engines');
  AList := TStringList.Create;
  try
    GetEngineList(AList);
    for I := 0 to AList.Count - 1 do
    begin
      AClass := GetEngineClass(AList[I]);
      if AClass <> nil then
      begin
        ANode := ARoot.FindNode(AClass.EngineUniqueID);
        if ANode = nil then
        begin
          ANode := ARoot.NodeNew(AClass.EngineUniqueID);
          ANode.WriteAttributeString('name', AClass.EngineName);
          ANode.WriteAttributeString('desc', AClass.EngineDescription);
          ANode.WriteAttributeString('class', AClass.ClassName);
        end;
      end;
    end;
  finally
    AList.Free;
  end;
end;

procedure TdmPackage.RepairExternPackage(const APkgName: string);
var
  APkgDB: TACRDatabase;
  APkgSession: TACRSession;
begin
  APkgDB := TACRDatabase.Create(Self);
  APkgSession := TACRSession.Create(Self);
  try
    APkgDB.DatabaseName := 'EXTERNDB';
    APkgSession.SessionName := 'EXTERNSESSION';
    APkgDB.SessionName := APkgSession.SessionName;
    APkgDB.DatabaseFileNameAnsi := APkgName;
    APkgDB.Exclusive := True;
    APkgDB.OnProgress := OnDBProgress;
    APkgDB.OnTableProgress := OnTableProgress;
    frmProgress.Caption := _('Repair external package');
    frmProgress.lbCaption.Caption := _('Repairing external package...');
    frmProgress.DBProgress.Position := 0;
    frmProgress.TBProgress.Position := 0;
    frmProgress.DBProgress.Properties.Max := 100;
    frmProgress.DBProgress.Properties.Min := 0;
    frmProgress.TBProgress.Properties.Max := 100;
    frmProgress.TBProgress.Properties.Min := 0;
    frmProgress.Show;
    APkgDB.RepairDatabase(True);
    APkgDB.CompactDatabase;
  finally
    frmProgress.Hide;
    APkgDB.Free;
    APkgSession.Free;
  end;
end;

procedure TdmPackage.RepairPackage;
var
  ADatasetState: Variant;
begin
  if not FPkgDB.Connected then
    raise Exception.Create(_('Package has not been loaded'));
  ADatasetState := SaveDatasetState;
  EnsureClosed;
  FOnVerify := True;
  FPkgDB.OnProgress := OnDBProgress;
  FPkgDB.OnTableProgress := OnTableProgress;
  try
    frmProgress.Caption := _('Repair package');
    frmProgress.lbCaption.Caption := _('Repairing package...');
    frmProgress.DBProgress.Position := 0;
    frmProgress.TBProgress.Position := 0;
    frmProgress.DBProgress.Properties.Max := 100;
    frmProgress.DBProgress.Properties.Min := 0;
    frmProgress.TBProgress.Properties.Max := 100;
    frmProgress.TBProgress.Properties.Min := 0;
    frmProgress.Show; 
    FPkgDB.Exclusive := True;
    FPkgDB.RepairDatabase(True);
  finally
    frmProgress.Hide;
    FPkgDB.OnProgress := nil;
    FPkgDB.OnTableProgress := nil;
    EnsureClosed;
    FOnVerify := False;
    EnsureOpened;
    RestoreDatasetState(ADatasetState);
  end;
end;

procedure TdmPackage.RestoreDatasetState(AState: Variant);
var
  I: integer;
begin
  if not FPkgDB.Connected then
    exit;  
  for I := 0 to FPkgDB.DataSetCount - 1 do
    FPkgDB.DataSets[I].Active := AState[I];
end;

function TdmPackage.SelectValue(ASQL: string): Variant;
var
  AQuery: TACRQuery;
begin
  Result := Null;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FPkgDB.DatabaseName;
  AQuery.SessionName := FPkgDB.SessionName;
  try
    AQuery.SQL.Text := ASQL;
      AQuery.Open;
      if not AQuery.Eof then
        Result := AQuery.Fields[0].Value;
  finally
    FreeAndNil(AQuery);
  end;
end;

function TdmPackage.SelectValue(ASQL: string; AValues: Variant): Variant;
var
  AQuery: TACRQuery;
  I : integer;
begin
  Result := NULL;
  AQuery := TACRQuery.Create(nil);
  AQuery.DatabaseName := FPkgDB.DatabaseName;
  AQuery.SessionName := FPkgDB.SessionName;  
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

function TdmPackage.SelectValue(const ATable, AFilter, ARetFields: string;
  AValues: Variant): Variant;
var
  ADataset: TDataSet;
begin
  ADataset := GetACRTable(ATable, format('pk_%s', [ATable]));
  try
    ADataset.Filter := ConstructFilter(AFilter, AValues);
    ADataset.Filtered := True;
    ADataset.Open;
    Result := GetLookupValues(ARetFields, ADataset);
  finally
    ADataset.Free;
  end;
end;

procedure TdmPackage.SetDatabaseFile(const Value: string);
begin
  EnsureClosed;
  FPkgDB.DatabaseFileName := Value;
end;

procedure TdmPackage.SetNotifyMainForm(const Value: TNotifyEvent);
begin
  FNotifyMainForm := Value;
end;

procedure TdmPackage.SetPackageStatus(const Value: Boolean);
begin
  if Value then
    EnsureOpened
  else
    EnsureClosed;
end;

function TdmPackage.TableExists(const ATableName: string): boolean;
var
  ATable: TACRTable;
begin
  ATable := GetACRTable(ATableName);
  try
    Result := ATable.Exists;
  finally
    ATable.Free;
  end;  
end;

procedure TdmPackage.VerifyDatabase;
begin
  FOnVerify := true;
  try
    if not FPkgDB.IsAccuracerDatabaseFile then
      raise Exception.Create(_(SCSPInvalidDBFile));
    if not TfrmInputPassword.VerifyPassword(FPkgDB) then
      raise EAbort.Create('');
    CreateTables;
  finally
    FOnVerify := False;
  end;
end;

procedure TdmPackage.WritePackageInfo;
var
  AMemory: TMemoryStream;
  ARegXML: TRegXML;
  ADataset: TDataSet;
begin
  AMemory := TMemoryStream.Create;
  ARegXML := TRegXML.Create(self);
  ADataset :=   GetACRTable('settings', 'pk_settings');
  try
    ADataset.Filter := ConstructFilter('keyname;refid_1;refid_2;refid_3',
      VarArrayOf(['RDB$PROPS', 0, 0, 0]));
    if TACRTable(ADataset).Exists then
    begin
      ADataset.Open;
      if ADataset.IsEmpty then
        ADataset.AppendRecord(['RDB$PROPS', 0, 0, 0,'']);
      FPackageInfo.WriteXML(ARegXML, 'properties');
      ARegXML.SaveToStream(AMemory);
      AMemory.Position := 0;
      ADataset.Edit;
      TBlobField(ADataset.FieldByName('data')).LoadFromStream(AMemory);
      ADataset.Post;
    end;    
  finally
    ADataset.Free;
    ARegXML.Free;
    AMemory.Free;
  end;
end;

end.
