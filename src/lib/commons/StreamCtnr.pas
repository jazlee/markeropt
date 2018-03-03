unit StreamCtnr;

interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: StreamCtnr.pas
//       Author: Jaimy Azle (jaimy@usg.co.id)
//    Date Code: dd.mm.yyyy
// ========================================================================
// Source Owner: PT. Ungaran Sari Garments, 2003 IT Department
//    Copyright: Seluruh isi dari file ini dilindungi oleh undang-undang
//               hak cipta dan kesepakatan internasional. Segala bentuk
//               reproduksi, reverse-engineering, dan distribusi atas
//               seluruh atau bagian apapun dari kode yang ada dalam file
//               ini tanpa ijin tertulis merupakan pelanggaran hukum dan
//               akan dituntut ke pengadilan dengan sanksi maksimum yang
//               ada.
//
//  Restriction: SEGALA BENTUK KODE SUMBER (SOURCE CODE) YANG TERDAPAT DALAM
//               DISTRIBUSI YANG MENGHASILKAN KODE INTERMEDIATE (DCU, OBJ,
//               SO, DAN LAIN-LAIN) MERUPAKAN ASSET PENTING DAN RAHASIA
//               PT. UNGARAN SARI GARMENTS DAN HANYA UNTUK DIGUNAKAN DALAM
//               LINGKUP INTERNAL PT. UNGARAN SARI GARMENTS. TIDAK DIIJINKAN
//               KEPADA PIHAK LUAR UNTUK MEMBAWA FILE SOURCE CODE INI,
//               ATAUPUN BAGIAN-BAGIAN DARI FILE SOURCE, MAUPUN FILE
//               INTERMEDIATE YANG DIHASILKAN OLEH KOMPILER DALAM MEDIA APAPUN
//               KELUAR DARI LINGKUNGAN PT. UNGARAN SARI GARMENTS.
//
// Code Version: (3rd Generation Code)
// ========================================================================
//  Description:
//    Mohon tidak melakukan update apapun terhadap file-file library support
//    yang digunakan untuk modul ini karena library tersebut sudah tidak lagi
//    sesuai seperti aslinya, melainkan telah diubahsuaikan untuk keperluan
//    dalam modul ini.
// ========================================================================
///////////////////////////////////////////////////////////////////////////////
{$I ecfdefs.inc}
uses
  Classes, Variants, DB;

type
  TNativeFieldInfo = class(TObject)
  private
    FFieldSegLen: integer;
    FFieldScale: integer;
    FFieldCLen: integer;
    FFieldPrecision: integer;
    FFieldPos: integer;
    FFieldSubType: integer;
    FFieldCSID: integer;
    FFieldType: integer;
    FFieldLength: integer;
    FFieldTypeStr: string;
  public
    property FieldType: integer read FFieldType write FFieldType;
    property FieldSubType: integer read FFieldSubType write FFieldSubType;
    property FieldPos: integer read FFieldPos write FFieldPos;
    property FieldScale: integer read FFieldScale write FFieldScale;
    property FieldPrecision: integer read FFieldPrecision write FFieldPrecision;
    property FieldLength: integer read FFieldLength write FFieldLength;
    property FieldSegmentLength: integer read FFieldSegLen write FFieldSegLen;
    property FieldCharsetID: integer read FFieldCSID write FFieldCSID;
    property FieldCharLength: integer read FFieldCLen write FFieldCLen;
    property FieldTypeStr: string read FFieldTypeStr write FFieldTypeStr; 
  end;
  
  TJzFieldDef = class(TNamedItem)
  private
    FPrecision: Integer;
    FAttributes: TFieldAttributes;
    FDataType: TFieldType;
    FSize: Integer;
    FValue: Variant;
    FDatasetIndex: integer;
    FNativeInfo: TNativeFieldInfo;

    function GetSize: Integer;
    procedure SetAttributes(const Value: TFieldAttributes);
    procedure SetDataType(const Value: TFieldType);
    procedure SetPrecision(const Value: Integer);
    procedure SetSize(const Value: Integer);
    function GetRequired: Boolean;
    procedure SetRequired(const Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    property Required: Boolean read GetRequired write SetRequired;
    property NativeInfo: TNativeFieldInfo read FNativeInfo;
  published
    property Attributes: TFieldAttributes read FAttributes write SetAttributes default [];
    property DataType: TFieldType read FDataType write SetDataType default ftUnknown;
    property Precision: Integer read FPrecision write SetPrecision default 0;
    property Size: Integer read GetSize write SetSize default 0;
    property FieldValue: Variant read FValue write FValue;
    property DatasetIndex: integer read FDatasetIndex;
  end;

  TJzFieldDefs = class(TOwnedCollection)
  private
    FHiddenFields: Boolean;
    function GetFieldDef(Index: Integer): TJzFieldDef;
    procedure SetFieldDef(Index: Integer; Value: TJzFieldDef);
    procedure SetHiddenFields(Value: Boolean);
  protected
    procedure SetItemName(AItem: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);

    function AddFieldDef: TJzFieldDef;
    function Find(const AName: string): TJzFieldDef;

    function IndexOf(const AName: string): Integer;
    procedure Add(const Name: string; DataType: TFieldType; Size: Integer = 0;
      Required: Boolean = False);

    procedure ClearValues;
    procedure AssignTo(Dest: TPersistent); override;

    property HiddenFields: Boolean read FHiddenFields write SetHiddenFields;
    property Items[Index: Integer]: TJzFieldDef read GetFieldDef write SetFieldDef; default;
  end;

  TJzOperationType = (otSave, otLoad);
  TJzOnProgress = procedure(DataSet:TDataSet; Percentage:integer;
    const OpType: TJzOperationType) of object;
  TJzOnPrepare = procedure(DataSet:TDataSet; FieldDefs: TJzFieldDefs) of object;

  TJzDatasetContainer = class(TComponent)
  private
    FWriter: TWriter;
    FReader: TReader;
    FStream: TStream;
    FDataset: TDataSet;
    FOnProgress: TJzOnProgress;
    FBufferSize: Longint;
    FProgressCnt:integer;
    FCount: Longint;
    FFieldDefs: TJzFieldDefs;
    FOnPrepare: TJzOnPrepare;

    procedure Progress(Pct: integer;const OpType: TJzOperationType);
    procedure SetBuffSize(Value: LongInt);
    function GetFieldDefs: TJzFieldDefs;
  protected
    procedure DoBeforeSave;
    procedure SaveDefs;
    procedure SaveData;
    procedure DoAfterSave;
    procedure InternalSaveDatasetToStream(ADataset: TDataset; Stream: TStream);

    procedure DoBeforeLoad;
    procedure LoadDefs;
    procedure LoadData(const ReprepareDataset: Boolean);
    procedure DoAfterLoad;
    procedure InternalLoadDatasetFromStream(Stream: TStream; ADataset: TDataset;
      const CopyFieldDefs: boolean; const ReprepareDataset: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveDatasetToStream(ADataset: TDataset; Stream: TStream; const
      Count: Longint);
    procedure LoadDatasetFromStream(Stream: TStream; ADataset: TDataset;
      const CopyFieldDefs: boolean = False; const ReprepareDataset: Boolean = False);

    property OnProgress: TJzOnProgress read FOnProgress write FOnProgress;
    property OnPrepare: TJzOnPrepare read FOnPrepare write FOnPrepare;
    property BufferSize: LongInt read FBufferSize write SetBuffSize;
    property FieldDefs: TJzFieldDefs read GetFieldDefs;
  end;


implementation
uses
  SysUtils;

const
  DefaultBuffSize = 32768;
  FieldKindNames: array[0..4] of string = (
    'Data', 'Calculated', 'Lookup', 'InternalCalc', 'Aggregate');

{ TJzFieldDef }

procedure TJzFieldDef.Assign(Source: TPersistent);
var
  AField: TField;
  S: TFieldDef;
  Sz: TJzFieldDef;
begin
  if Source is TFieldDef then
  begin
    if Collection <> nil then Collection.BeginUpdate;
    try
      S := TFieldDef(Source);
      {FieldNo is defaulted}
      Name := S.Name;
      DataType := S.DataType;
      Size := S.Size;
      Precision := S.Precision;
      Attributes := S.Attributes;
      if (TFieldDefs(S.Collection).DataSet <> nil) then
      begin
        AField := TFieldDefs(S.Collection).DataSet.FindField(S.Name);
        if AField <> nil then
          FDatasetIndex := TFieldDefs(S.Collection).DataSet.Fields.IndexOf(AField);
      end;
    finally
      if Collection <> nil then Collection.EndUpdate;
    end;
  end else
  if Source is TJzFieldDef then
  begin
    if Collection <> nil then Collection.BeginUpdate;
    try
      Sz := TJzFieldDef(Source);
      {FieldNo is defaulted}
      Name := Sz.Name;
      DataType := Sz.DataType;
      Size := Sz.Size;
      Precision := Sz.Precision;
      Attributes := Sz.Attributes;
    finally
      if Collection <> nil then Collection.EndUpdate;
    end;
  end else inherited;
end;

procedure TJzFieldDef.AssignTo(Dest: TPersistent);
var
  S: TFieldDef;
  Sz: TJzFieldDef;
begin
  if Dest is TFieldDef then
  begin
    S := TFieldDef(Dest);
    {FieldNo is defaulted}
    S.Name := Name;
    S.DataType := DataType;
    S.Size := Size;
    S.Precision := Precision;
    S.Attributes := Attributes;
  end else
  if Dest is TJzFieldDef then
  begin
    Sz := TJzFieldDef(Dest);
    {FieldNo is defaulted}
    Name := Sz.Name;
    DataType := Sz.DataType;
    Size := Sz.Size;
    Precision := Sz.Precision;
    Attributes := Sz.Attributes;
  end else inherited;
end;

constructor TJzFieldDef.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDatasetIndex := -1;
  FNativeInfo := TNativeFieldInfo.Create;
end;

destructor TJzFieldDef.Destroy;
begin
  FNativeInfo.Free;
  inherited Destroy;
end;

function TJzFieldDef.GetRequired: Boolean;
begin
  Result := faRequired in Attributes;
end;

function TJzFieldDef.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TJzFieldDef.SetAttributes(const Value: TFieldAttributes);
begin
  FAttributes := Value;
  Changed(False);
end;

procedure TJzFieldDef.SetDataType(const Value: TFieldType);
const
  TypeSizes: packed array[TFieldType] of Byte =
    (0 {ftUnknown}, 20 {ftString}, 0 {ftSmallint}, 0 {ftInteger}, 0 {ftWord},
     0 {ftBoolean}, 0 {ftFloat}, 0 {ftCurrency}, 4 {ftBCD}, 0 {ftDate},
     0 {ftTime}, 0 {ftDateTime}, 16 {ftBytes}, 16 {ftVarBytes}, 0 {ftAutoInc},
     0 {ftBlob}, 0 {ftMemo}, 0 {ftGraphic}, 0 {ftFmtMemo}, 0 {ftParadoxOle},
     0 {ftDBaseOle}, 0 {ftTypedBinary}, 0 {ftCursor}, 20 { ftFixedChar },
     0 {ftWideString}, 0 {ftLargeInt} , 0 {ftADT}, 10 {ftArray}, 0 {ftReference},
     0 {ftDataSet}, 0 {ftOraBlob}, 0 {ftOraClob}, 0 {ftVariant}, 0 {ftInterface},
     0 {ftIDispatch}, 0 {ftGuid}, 0 {ftTimeStamp}, 0 {ftFMTBcd}
     {$IFDEF DELPHI10},
     0 {ftFixedWideChar}, 0 {ftWideMemo}, 0 {ftOraTimeStamp}, 0 {ftOraInterval}
     {$ENDIF}
     );
begin
  FDataType := Value;
  FPrecision := 0;
  FSize := TypeSizes[Value];
  Changed(False);
end;

procedure TJzFieldDef.SetPrecision(const Value: Integer);
begin
  FPrecision := Value;
  Changed(False);
end;

procedure TJzFieldDef.SetRequired(const Value: Boolean);
begin
  if Value then
    Attributes := Attributes + [faRequired] else
    Attributes := Attributes - [faRequired];
end;

procedure TJzFieldDef.SetSize(const Value: Integer);
begin
  FSize := Value;
  Changed(False);
end;

{ TJzFieldDefs }

procedure TJzFieldDefs.Add(const Name: string; DataType: TFieldType;
  Size: Integer; Required: Boolean);
var
  FieldDef: TJzFieldDef;
begin
  BeginUpdate;
  try
    FieldDef := AddFieldDef;
    try
      {FieldNo is defaulted}
      FieldDef.Name := Name;
      FieldDef.DataType := DataType;
      FieldDef.Size := Size;
      { Precision is defaulted }
      FieldDef.Required := Required;
    except
      FieldDef.Free;
      raise;
    end;
  finally
    EndUpdate;
  end;
end;

function TJzFieldDefs.AddFieldDef: TJzFieldDef;
begin
  Result := TJzFieldDef(inherited Add);
end;

procedure TJzFieldDefs.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TCollection then
  begin
    TCollection(Dest).BeginUpdate;
    try
      TCollection(Dest).Clear;
      for I := 0 to Count - 1 do
        TCollection(Dest).Add.Assign(Items[I]);
    finally
      TCollection(Dest).EndUpdate;
    end;
    Exit;
  end else
    inherited;
end;

procedure TJzFieldDefs.ClearValues;
var
  I: integer;
begin
  for i := 0 to Count-1 do
    Items[i].FieldValue := null;
end;

constructor TJzFieldDefs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJzFieldDef);
end;

function TJzFieldDefs.Find(const AName: string): TJzFieldDef;
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I < 0 then Result := nil else Result := TJzFieldDef(Items[I]);
end;

function TJzFieldDefs.GetFieldDef(Index: Integer): TJzFieldDef;
begin
  Result := TJzFieldDef(inherited Items[Index]);
end;

function TJzFieldDefs.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(TNamedItem(Items[Result]).Name, AName) = 0 then Exit;
  Result := -1;
end;

procedure TJzFieldDefs.SetFieldDef(Index: Integer; Value: TJzFieldDef);
begin
  inherited Items[Index] := Value;
end;

procedure TJzFieldDefs.SetHiddenFields(Value: Boolean);
begin
  FHiddenFields := Value;
end;

procedure TJzFieldDefs.SetItemName(AItem: TCollectionItem);
begin
  with TNamedItem(AItem) do
    if Name = '' then
      Name := TJzFieldDef(Self.GetOwner).Name + Copy(ClassName, 2, 5) + IntToStr(ID+1);
end;

{ TJzDatasetContainer }

constructor TJzDatasetContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBufferSize := DefaultBuffSize;
  FFieldDefs := TJzFieldDefs.Create(Self);
end;

destructor TJzDatasetContainer.Destroy;
begin
  FFieldDefs.Free;
  inherited;
end;

procedure TJzDatasetContainer.DoAfterLoad;
begin
  FReader.Free;
end;

procedure TJzDatasetContainer.DoAfterSave;
begin
  FWriter.FlushBuffer;
  FWriter.Free;
  FWriter:=nil;
end;

procedure TJzDatasetContainer.DoBeforeLoad;
begin
  FProgressCnt := 0;
  FStream.Position := 0;
  FReader := TReader.Create(FStream, FBufferSize);
  FReader.ReadSignature;
end;

procedure TJzDatasetContainer.DoBeforeSave;
begin
  FStream.Size := 0;
  FStream.Position := 0;
  FWriter := TWriter.Create(FStream, FBufferSize);
  FWriter.WriteSignature;
end;

function TJzDatasetContainer.GetFieldDefs: TJzFieldDefs;
begin
  Result := FFieldDefs;
end;

procedure TJzDatasetContainer.InternalLoadDatasetFromStream(
  Stream: TStream; ADataset: TDataset;
  const CopyFieldDefs: boolean;
  const ReprepareDataset: Boolean);
begin
  FDataset := ADataset;
  FStream  := Stream;
  Progress(0, otLoad);
  DoBeforeLoad;
  try
    LoadDefs;
    if CopyFieldDefs then
    begin
      FDataset.Active := False;
      FDataset.FieldDefs.Clear;
      FFieldDefs.AssignTo(FDataset.FieldDefs);
      FDataset.Open;
    end;
    LoadData(ReprepareDataset);
  finally
    DoAfterLoad;
  end;
end;

procedure TJzDatasetContainer.InternalSaveDatasetToStream(
  ADataset: TDataset; Stream: TStream);
begin
  FDataset := ADataset;
  FStream  := Stream;
  Progress(0, otSave);
  DoBeforeSave;
  try
    SaveDefs;
    SaveData;
  finally
    DoAfterSave;
  end;
end;

procedure TJzDatasetContainer.LoadData(const ReprepareDataset: Boolean);
var
  DIndex, j, FieldIndex, StreamSize: Longint;
  AField: TField;
  bNull: boolean;
begin
  StreamSize := FStream.Size;
  if (StreamSize = 0) then exit;
  DIndex := 0;
  Progress(Trunc((FStream.Position / StreamSize) * 100), otLoad);
  FReader.ReadListBegin;
  try
    while not(FReader.EndofList) do
    begin
      Inc(DIndex, 1);
      if (DIndex mod 100) = 0 then
        Progress(Trunc((FStream.Position / StreamSize) * 100), otLoad);
      FReader.ReadListBegin;
      FFieldDefs.ClearValues;
      for j := 0 to FFieldDefs.Count-1 do
      begin
        bNull := FReader.ReadBoolean;
        if not bNull then
        begin
          case  FFieldDefs[j].DataType of
            ftBoolean : FFieldDefs[j].FieldValue := FReader.ReadBoolean;
            ftLargeInt: FFieldDefs[j].FieldValue := FReader.ReadFloat;
            ftSmallInt,
            ftInteger,
            ftAutoInc,
            ftWord : FFieldDefs[j].FieldValue := FReader.ReadInteger;
            ftFloat : FFieldDefs[j].FieldValue := FReader.ReadFloat;
            ftBCD,
            ftCurrency : FFieldDefs[j].FieldValue := FReader.ReadCurrency;
            ftDate,
            ftTime,
            ftDateTime : FFieldDefs[j].FieldValue := FReader.ReadDate;
            else
              FFieldDefs[j].FieldValue := FReader.ReadString;
          end;
        end;
      end;
      FReader.ReadListEnd;
      if ReprepareDataset and Assigned(FOnPrepare) then
      begin
        FDataset.Active := False;
        FOnPrepare(FDataset, FFieldDefs);
      end;
      if not FDataset.Active then
        FDataset.Open;
      if ReprepareDataset then
      begin
        // if FDataset.IsEmpty then FDataset.Insert else FDataset.Edit;
        if FDataset.IsEmpty then FDataset.Append else FDataset.Edit;
      end else
        // FDataset.Insert;
        FDataset.Append;
      for j := 0 to FFieldDefs.Count-1 do
      begin
        if (FFieldDefs[j].FDatasetIndex = -1) then
        begin
          AField := FDataset.FindField(FieldDefs[j].Name);
          if (AField <> nil) then
            FieldDefs[j].FDatasetIndex := FDataset.Fields.IndexOf(AField);
        end;
        FieldIndex := FieldDefs[j].FDatasetIndex;
        if (FieldIndex <> -1) and (FDataset.Fields[FieldIndex].ReadOnly <> True) then
          FDataset.Fields[FieldIndex].Value := FFieldDefs.Items[j].FieldValue;
      end;
      if FDataset.State in [dsEdit, dsInsert] then
        FDataset.Post;
    end;
  finally
    FReader.ReadListEnd;
  end;
end;

procedure TJzDatasetContainer.LoadDatasetFromStream(Stream: TStream;
  ADataset: TDataset; const CopyFieldDefs, ReprepareDataset: Boolean);
begin
  InternalLoadDatasetFromStream(Stream, ADataset, CopyFieldDefs, ReprepareDataset);
end;

procedure TJzDatasetContainer.LoadDefs;
var
  FName,TName,DName:string;
  FSize:integer;
  REQ:boolean;
  FT:TFieldType;
  i: integer;
begin
  if FStream.Size = 0 then
    exit;
  FFieldDefs.Clear;
  FReader.ReadListBegin;
  try
    while not(FReader.EndofList) do
    begin
      FName := FReader.ReadString;
      TName := FReader.ReadString;
      FSize := FReader.ReadInteger;
      DName := FReader.ReadString;
      REQ := FReader.ReadBoolean;
      for i:=0 to ord(High(FieldTypeNames)) do
        if FieldTypeNames[TFieldType(i)]=TName then break;
      FT:=TFieldType(i);
      FFieldDefs.Add(FName,FT,FSize,REQ);
    end;
  finally
    FReader.ReadListEnd;
  end;
end;

procedure TJzDatasetContainer.Progress(Pct: integer;
  const OpType: TJzOperationType);
begin
  if Assigned(FOnProgress) then FOnProgress(FDataset, pct, OpType);
end;

procedure TJzDatasetContainer.SaveData;
var
  DIndex, j, FieldIndex: Longint;
  AField: TField;
begin
  DIndex := 0;
  FWriter.WriteListBegin;
  try
    while not FDataset.Eof do
    begin
      FWriter.WriteListBegin;
      try
        Inc(DIndex, 1);
        if (DIndex mod 100) = 0 then
          Progress(Trunc((DIndex / FCount) * 100), otSave);
        for j := 0 to FieldDefs.Count-1 do
        begin
          if (FieldDefs[j].FDatasetIndex = -1) then
          begin
            AField := FDataset.FindField(FieldDefs[j].Name);
            if (AField <> nil) then
              FieldDefs[j].FDatasetIndex := FDataset.Fields.IndexOf(AField);
          end;
          FieldIndex := FieldDefs[j].FDatasetIndex;
          if FieldIndex <> -1 then
          begin
            FWriter.WriteBoolean(FDataset.Fields[FieldIndex].IsNull);
            if not FDataset.Fields[FieldIndex].IsNull then
            begin
              case FDataset.Fields[FieldIndex].DataType of
                ftBoolean : FWriter.WriteBoolean(FDataset.Fields[FieldIndex].AsBoolean);
                ftLargeInt: FWriter.WriteFloat(FDataset.Fields[FieldIndex].AsFloat);
                ftSmallInt,
                ftInteger,
                ftWord,
                ftAutoInc : FWriter.WriteInteger(FDataset.Fields[FieldIndex].AsInteger);
                ftFloat : FWriter.WriteFloat(FDataset.Fields[FieldIndex].AsFloat);
                ftBCD,
                ftCurrency : FWriter.WriteCurrency(FDataset.Fields[FieldIndex].AsCurrency);
                ftDate,
                ftTime,ftDateTime: FWriter.WriteDate(FDataset.Fields[FieldIndex].AsDateTime);
              else
                 FWriter.WriteString(FDataset.Fields[FieldIndex].AsString);
              end;
            end;
          end;
        end;
      finally
        FWriter.WriteListEnd;
      end;
      FDataset.Next;
    end;
  finally
    FWriter.WriteListEnd;
  end;
end;

procedure TJzDatasetContainer.SaveDatasetToStream(ADataset: TDataset;
  Stream: TStream; const Count: Integer);
begin
  FCount := Count;
  InternalSaveDatasetToStream(ADataset, Stream);
end;

procedure TJzDatasetContainer.SaveDefs;
var
  i: integer;
begin
  if FieldDefs.Count <= 0 then
    FFieldDefs.Assign(FDataset.FieldDefs);
  if FieldDefs.Count > 0 then
  begin
    FWriter.WriteListBegin;
    try
      for i := 0 to FieldDefs.Count-1 do
      begin
        FWriter.WriteString(FFieldDefs.Items[i].Name);
        FWriter.WriteString(FieldTypeNames[FFieldDefs.Items[i].DataType]);
        FWriter.WriteInteger(FFieldDefs.Items[i].Size);
        FWriter.WriteString(FFieldDefs.Items[i].DisplayName);
        FWriter.WriteBoolean(FFieldDefs.Items[i].Required);
      end;
    finally
      FWriter.WriteListEnd;
    end;
  end;
end;

procedure TJzDatasetContainer.SetBuffSize(Value: LongInt);
begin
  if Value < DefaultBuffSize then
    Value := DefaultBuffSize;
  FBufferSize := Value;
end;

end.
