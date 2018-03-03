unit frxDBExporter;

interface
{$I frx.inc}
///////////////////////////////////////////////////////////////////////////////
//         Unit: frxDBFExporter.pas
//       Author: Jaimy Azle (jazle@usg.co.id)
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
// ========================================================================
///////////////////////////////////////////////////////////////////////////////
uses
  Windows, Classes, SyncObjs, frxClass, frxCustomDB, DB,  frxDBSet, Graphics,
  dbf_common, dbf, xlcClasses, xlEngine, xlReport, xlcOPack, xlProOPack,
  Dialogs, fs_xml
{$IFDEF Delphi6}
, Variants
{$ENDIF}
;

type
  TfrxXLExporter = class;

  TfrxXLItem = class(TCollectionItem)
  private
    FXLExporter: TfrxXLExporter;
    FSheetPref: string;
  published
    property XLExporter: TfrxXLExporter
      read FXLExporter
      write FXLExporter
      stored true;
    property SheetPrefix: string
      read FSheetPref
      write FSheetPref
      stored true;
  end;

  TfrxXLItems = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TfrxXLItem;
    procedure SetItem(Index: integer; const Value: TfrxXLItem);

    procedure ReadItems(Reader: TReader);
    procedure WriteItems(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent);

    function Add: TfrxXLItem;
    function AddItem: integer;
        
    property Items[Index: integer]: TfrxXLItem read GetItem
      write SetItem; default;
  end;  


  TfrxXLParam = class(TCollectionItem)
  private
    FName: string;
    FValue: Variant;

  published
    property Name: string read FName write FName stored true;
    property Value: Variant read FValue write FValue stored true;
  end;

  TfrxXLParams = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TfrxXLParam;
    procedure SetItem(Index: integer; const Value: TfrxXLParam);

    procedure ReadParams(Reader: TReader);
    procedure WriteParams(Writer: TWriter);
  protected
    function GetParamByName(Name: string): TfrxXLParam;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent);

    function Add: TfrxXLParam;
    function AddItem: integer;
        
    property Items[Index: integer]: TfrxXLParam read GetItem
      write SetItem; default;
  end;

  TfrxXLDatasource = class(TCollectionItem)
  private
    FAlias: string;
    FDataset: TfrxDBDataset;
    FDetails: TList;
    FMasterSource: TfrxXLDatasource;
    FRange: string;

    procedure AddDetail(DS: TfrxXLDatasource);
    procedure DeleteDetail(DS: TfrxXLDatasource);
    function GetMasterSourceName: string;
    procedure SetMasterSource(const Value: TfrxXLDatasource);
    function GetMasterSource: TfrxXLDatasource;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property Details: TList read FDetails;
  published
    property Dataset: TfrxDBDataset read FDataset write FDataset stored True;
    property Alias: string read FAlias write FAlias stored True;
    property Range: string read FRange write FRange stored True;

    property MasterSource: TfrxXLDatasource read GetMasterSource write SetMasterSource stored True;
    property MasterSourceName: string read GetMasterSourceName;
  end;

  TfrxXLDatasources = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TfrxXLDatasource;
    procedure SetItem(Index: integer; const Value: TfrxXLDatasource);

    procedure ReadDatasources(Reader: TReader);
    procedure WriteDatasources(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;    
  public
    constructor Create(AOwner: TComponent);

    function Add: TfrxXLDatasource;
    function AddItem: integer;
    property Items[Index: integer]: TfrxXLDatasource read GetItem
      write SetItem; default;
  end;  
  
  TfrxGlobalExporterList = class(TList)
    FCriticalSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;  

  TfrxCustomExporter = class(TfrxDataComponent)
  private
    FDataSources: TfrxXLDatasources;
    FAutoGenerate: boolean;
    procedure SetDataSources(const Value: TfrxXLDatasources);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DataSources: TfrxXLDatasources read FDataSources
      write SetDataSources stored True;
    property AutoGenerate: boolean
      read FAutoGenerate
      write FAutoGenerate
      stored True;      
  end;

  TfrxDBFExporter = class(TfrxCustomExporter)
  private
    FDBF: TDbf;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeforeStartReport; override;

    procedure ExportData;
  published
    property DataSources;
    property AutoGenerate;
  end;

  TfrxXLExporter = class(TfrxCustomExporter)
  private
    FXLReport: TxlReport;
    FParams: TfrxXLParams;
    FItems: TfrxXLItems;
    FCompositeReport: boolean;
    function GetTemplateFileName: string;
    procedure SetTemplateFileName(const Value: string);
    procedure SetParams(const Value: TfrxXLParams);

    procedure BindParams;
    procedure SetItems(const Value: TfrxXLItems);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure BeforeStartReport; override;
    
    procedure ShowReport;
    procedure MergeReports(Reports: array of TfrxXLExporter; SheetPrefixes: array of string); overload;
    procedure MergeReports; overload;

  published
    property AutoGenerate;
    property DataSources;
    property Params: TfrxXLParams read FParams write SetParams stored True;
    property XLSTemplate: string read GetTemplateFileName
      write SetTemplateFileName stored True;
    property XLSItems: TfrxXLItems read FItems write SetItems stored True;
    property CompositeReport: boolean
      read FCompositeReport
      write FCompositeReport
      stored True;
  end;

function frxFindExporter(Exporter: TfrxCustomExporter; const Name: String;
  Owner: TComponent): TfrxCustomExporter;
function frxGetExporter: TfrxGlobalExporterList;
procedure frxGetExporterList(List: TStrings);

implementation

{$R *.res}

uses
  SysUtils, frxDsgnIntf, fs_itools, frxVariables, frxUtils;

var
  ExporterList: TfrxGlobalExporterList;

function frxGetExporter: TfrxGlobalExporterList;
begin
  Result := ExporterList;
end;  


function frxFindExporter(Exporter: TfrxCustomExporter; const Name: String;
  Owner: TComponent): TfrxCustomExporter;
var
  i: Integer;
  ds: TfrxCustomExporter;
begin
  Result := Exporter;
  if Name = '' then
  begin
    Result := nil;
    Exit;
  end;
  if Owner = nil then Exit;
  ExporterList.Lock;
  for i := 0 to ExporterList.Count - 1 do
  begin
    ds := ExporterList[i];
    if AnsiCompareText(ds.Name, Name) = 0 then
      if not ((Owner is TfrxReport) and (ds.Owner is TfrxReport) and
        (ds.Owner <> Owner)) then
      begin
        Result := ExporterList[i];
        break;
      end;
  end;
  ExporterList.Unlock;
end;

procedure frxGetExporterList(List: TStrings);
var
  i: Integer;
  ds: TfrxDataSet;
begin
  ExporterList.Lock;
  List.Clear;
  for i := 0 to ExporterList.Count - 1 do
  begin
    ds := ExporterList[i];
    if (ds <> nil) and (ds.Name <> '') and ds.Enabled then
      List.AddObject(ds.Name, ds);
  end;
  ExporterList.Unlock;
end;

{ TfrxGlobalExporterList }

constructor TfrxGlobalExporterList.Create;
begin
  FCriticalSection := TCriticalSection.Create;
  inherited;
end;

destructor TfrxGlobalExporterList.Destroy;
begin
  FCriticalSection.Free;
  FCriticalSection := nil;
  inherited;
end;

procedure TfrxGlobalExporterList.Lock;
begin
  if FCriticalSection <> nil then
    FCriticalSection.Enter;
end;

procedure TfrxGlobalExporterList.Unlock;
begin
  if FCriticalSection <> nil then
    FCriticalSection.Leave;
end;

{ TfrxCustomExporter }

constructor TfrxCustomExporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ExporterList.Lock;
  ExporterList.Add(Self);
  ExporterList.Unlock;
  FDataSources := TfrxXLDatasources.Create(Self);
  FAutoGenerate := True;
end;

destructor TfrxCustomExporter.Destroy;
begin
  FDataSources.Free;
  ExporterList.Lock;
  ExporterList.Remove(Self);
  inherited Destroy;
  ExporterList.Unlock;
end;

procedure TfrxCustomExporter.SetDataSources(
  const Value: TfrxXLDatasources);
begin
  FDataSources.Assign(Value);
end;

{ TfrxDBFExporter }

procedure TfrxDBFExporter.BeforeStartReport;
begin
  if AutoGenerate then
    ExportData;
end;

constructor TfrxDBFExporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDBF := TDbf.Create(Self);
end;

destructor TfrxDBFExporter.Destroy;
begin
  inherited Destroy;
end;

procedure TfrxDBFExporter.ExportData;
var
  i,j: integer;
  FFieldDef: TFieldDef;
  ADataset: TDataSet;
begin
  if DataSources.Count > 0 then
    for i := 0 to DataSources.Count-1 do
    begin
      with TSaveDialog.Create(Self) do
      try
        Filter := 'DBF File (*.dbf)|*.dbf';
        DefaultExt := '.dbf';
        if Execute then
        begin
          if FileExists(FileName) then
            DeleteFile(FileName);
          FDBF.Active := False;
          FDBF.TableName := FileName;
          FDBF.FieldDefs.Clear;
          ADataset := DataSources.Items[i].DataSet.GetDataSet;
          ADataset.Active := True; 
          if ADataset.FieldDefs.Count > 0 then
          begin
            for j := 0 to ADataset.FieldDefs.Count-1 do
            begin
              if not (faHiddenCol in ADataset.FieldDefs.Items[j].Attributes) then
              begin
                FFieldDef := FDBF.FieldDefs.AddFieldDef;
                FFieldDef.Assign(ADataset.FieldDefs.Items[j]);
                case ADataset.Fields[j].DataType of
                  ftBCD : FFieldDef.DataType := ftFloat;
                  ftWideString: FFieldDef.DataType := ftString;
                end;
              end;
            end;
            FDBF.CreateTable;
            FDBF.Active := True;
            DataSources.Items[i].DataSet.First;
            while not DataSources.Items[i].DataSet.Eof do
            begin
              FDBF.Append;
              for j := 0 to ADataset.FieldCount-1 do
                if FDBF.FindField(ADataset.Fields[j].FieldName) <> nil then
                  FDBF.FieldValues[ADataset.Fields[j].FieldName] :=
                    ADataset.FieldValues[ADataset.Fields[j].FieldName];
              FDBF.Post;
              DataSources.Items[i].DataSet.Next;
            end;
            FDBF.Active := False;
          end;
        end;
      finally
        Free;
      end;
    end;
end;

{ TfrxXLExporter }

procedure TfrxXLExporter.BindParams;
var
  i: integer;
begin
  FXLReport.DataSources.Clear;
  for i := 0 to FDatasources.Count-1 do
    with FXLReport.DataSources.Add do
    begin
      DataSet := FDatasources.Items[i].Dataset.GetDataSet;
      Alias   := FDatasources.Items[i].Alias;
      Range   := FDataSources.Items[i].Range;
      MasterSourceName := FDatasources.Items[i].MasterSourceName;
      if not DataSet.Active then
        FDatasources.Items[i].Dataset.Open;
    end;
  FXLReport.Params.Clear;
  for i := 0 to FParams.Count-1 do
    with FXLReport.Params.Add do
    begin
      Name := FParams.Items[i].Name;
      Value := FParams.Items[i].Value;
    end;
  if Self.Report.Variables.Count > 0 then
    for i := 0 to Self.Report.Variables.Count-1 do
    with FXLReport.Params.Add do
    begin
      Name := Self.Report.Variables.Items[i].Name;
      Value:= Self.Report.Variables.Items[i].Value; 
    end;
  if XLSItems.Count > 0 then
  for i := 0 to XLSItems.Count-1 do 
    XLSItems[i].XLExporter.BindParams;
end;

constructor TfrxXLExporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXLReport := TxlReport.Create(Self);
  FParams := TfrxXLParams.Create(Self);
  FItems  := TfrxXLItems.Create(Self);
  FCompositeReport := False;
end;


destructor TfrxXLExporter.Destroy;
begin
  FItems.Free;
  FParams.Free;
  FXLReport.Free;
  inherited Destroy;
end;

function TfrxXLExporter.GetTemplateFileName: string;
begin
  Result := FXLReport.XLSTemplate;
end;

procedure TfrxXLExporter.MergeReports(Reports: array of TfrxXLExporter;
  SheetPrefixes: array of string);
var
  AReports: array of TxlExcelReport;
  i: integer;
begin
  BindParams;
  SetLength(AReports, High(Reports)+1);
  for i := Low(Reports) to High(Reports) do
    AReports[i] := TxlExcelReport(Reports[i].FXLReport);
  FXLReport.MergeReports(AReports, SheetPrefixes);
end;

procedure TfrxXLExporter.ShowReport;
begin
  BindParams;
  FXLReport.Report;
end;

procedure TfrxXLExporter.SetParams(const Value: TfrxXLParams);
begin
  FParams.Assign(Value);
end;

procedure TfrxXLExporter.SetTemplateFileName(const Value: string);
begin
  FXLReport.XLSTemplate := Value;
end;

procedure TfrxXLExporter.SetItems(const Value: TfrxXLItems);
begin
  FItems.Assign(Value);
end;

procedure TfrxXLExporter.MergeReports;
var
  AReports: array of TxlExcelReport;
  ASheetPrefixes: array of string;
  i: integer;
begin
  if FItems.Count <= 0 then
    exit;
  BindParams;
  SetLength(AReports, FItems.Count);
  SetLength(ASheetPrefixes, FItems.Count);
  for i := 0 to FItems.Count-1 do
  begin
    AReports[i] := TxlExcelReport(XLSItems[i].XLExporter.FXLReport);
    ASheetPrefixes[i] := XLSItems[i].SheetPrefix; 
  end;
  FXLReport.MergeReports(AReports, ASheetPrefixes);
end;


procedure TfrxXLExporter.BeforeStartReport;
begin
  if AutoGenerate then
  begin
    if CompositeReport then
      MergeReports
    else
      ShowReport;
  end;
end;

{ TfrxXLParams }

function TfrxXLParams.Add: TfrxXLParam;
begin
  Result := TfrxXLParam(inherited Add);
end;

function TfrxXLParams.AddItem: integer;
begin
  Result := Add.Index;
end;

constructor TfrxXLParams.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TfrxXLParam);  
end;

procedure TfrxXLParams.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Params', ReadParams, WriteParams, True);
end;

function TfrxXLParams.GetItem(Index: integer): TfrxXLParam;
begin
  Result := TfrxXLParam(inherited GetItem(Index));
end;

function TfrxXLParams.GetParamByName(Name: string): TfrxXLParam;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, Name) then
      Result := Items[i];
end;

procedure TfrxXLParams.ReadParams(Reader: TReader);
begin
  frxReadCollection(Self, Reader, TfrxComponent(GetOwner));
end;

procedure TfrxXLParams.SetItem(Index: integer; const Value: TfrxXLParam);
begin
  inherited SetItem(Index, Value);
end;

procedure TfrxXLParams.WriteParams(Writer: TWriter);
begin
  frxWriteCollection(Self, Writer, TfrxComponent(GetOwner));
end;

{ TfrxXLDatasources }

function TfrxXLDatasources.Add: TfrxXLDatasource;
begin
  Result := TfrxXLDatasource(inherited Add);
end;

function TfrxXLDatasources.AddItem: integer;
begin
  Result := Add.Index;
end;

constructor TfrxXLDatasources.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TfrxXLDatasource);
end;

procedure TfrxXLDatasources.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Datasources', ReadDatasources, WriteDatasources, True);
end;

function TfrxXLDatasources.GetItem(Index: integer): TfrxXLDatasource;
begin
  Result := TfrxXLDatasource(inherited GetItem(Index));
end;

procedure TfrxXLDatasources.ReadDatasources(Reader: TReader);
begin
  frxReadCollection(Self, Reader, TfrxComponent(GetOwner));
end;

procedure TfrxXLDatasources.SetItem(Index: integer;
  const Value: TfrxXLDatasource);
begin
  inherited SetItem(Index, Value);
end;

procedure TfrxXLDatasources.WriteDatasources(Writer: TWriter);
begin
  frxWriteCollection(Self, Writer, TfrxComponent(GetOwner));
end;

{ TfrxXLDatasource }

procedure TfrxXLDatasource.AddDetail(DS: TfrxXLDatasource);
begin
  if Details.IndexOf(DS) = -1 then
    Details.Add(DS);
end;

constructor TfrxXLDatasource.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FDetails := TList.Create;
  FMasterSource := nil;
end;

procedure TfrxXLDatasource.DeleteDetail(DS: TfrxXLDatasource);
var
  Indx: integer;
begin
  Indx := Details.IndexOf(DS);
  if Indx <> -1 then
    Details.Delete(Indx);
end;

destructor TfrxXLDatasource.Destroy;
begin
  FDetails.Free;
  inherited;
end;

function TfrxXLDatasource.GetMasterSource: TfrxXLDatasource;
begin
  if Assigned(FMasterSource) then
    Result := FMasterSource
  else
    Result := nil;
end;

function TfrxXLDatasource.GetMasterSourceName: string;
begin
  Result := '';
  if Assigned(FMasterSource) then
    Result := MasterSource.Alias;
end;

procedure TfrxXLDatasource.SetMasterSource(const Value: TfrxXLDatasource);
begin
  if Value = Self then
    raise Exception.Create('Circular link is not allowed');
  if Assigned(MasterSource) then
    MasterSource.DeleteDetail(Self);
  FMasterSource := Value;
  if Assigned(Value) then
    Value.AddDetail(Self);
end;

{ TfrxXLItems }

function TfrxXLItems.Add: TfrxXLItem;
begin
  Result := TfrxXLItem(inherited Add);
end;

function TfrxXLItems.AddItem: integer;
begin
  Result := Add.Index;
end;

constructor TfrxXLItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TfrxXLItem);
end;

procedure TfrxXLItems.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Items', ReadItems, WriteItems, True);
end;

function TfrxXLItems.GetItem(Index: integer): TfrxXLItem;
begin
  Result := TfrxXLItem(inherited GetItem(Index));
end;

procedure TfrxXLItems.ReadItems(Reader: TReader);
begin
  frxReadCollection(Self, Reader, TfrxComponent(GetOwner));
end;

procedure TfrxXLItems.SetItem(Index: integer; const Value: TfrxXLItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TfrxXLItems.WriteItems(Writer: TWriter);
begin
  frxWriteCollection(Self, Writer, TfrxComponent(GetOwner));
end;

var
  XLBmp, DBFBmp,
  CatBmp: TBitmap;
  
initialization
  ExporterList := TfrxGlobalExporterList.Create;
  CatBmp := Graphics.TBitmap.Create;
  XLBmp := Graphics.TBitmap.Create;
  DBFBmp := Graphics.TBitmap.Create;
  XLBmp.LoadFromResourceName(hInstance, 'FRXDBEXPXL');
  DBFBmp.LoadFromResourceName(hInstance, 'FRXDBEXPDBF');
  CatBmp.LoadFromResourceName(hInstance, 'FRXDBEXP');
  // frxObjects.RegisterCategory('Exporter', CatBmp, 'DB Exporter');
  // frxObjects.RegisterObject1(TfrxDBFExporter, DBFBmp, '', 'Exporter', 0);
  // frxObjects.RegisterObject1(TfrxXLExporter, XLBmp, '', 'Exporter', 0);
  frxObjects.RegisterObject1(TfrxDBFExporter, DBFBmp, '', '', 0);
  frxObjects.RegisterObject1(TfrxXLExporter, XLBmp, '', '', 0);

finalization
  ExporterList.Free;
  CatBmp.Free;
  XLBmp.Free;
  DBFBmp.Free;
  frxObjects.UnRegister(TfrxXLExporter);
  frxObjects.UnRegister(TfrxDBFExporter);
end.
