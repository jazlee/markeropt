unit fs_dbexprtti;

interface
{$i fs.inc}
///////////////////////////////////////////////////////////////////////////////
//         Unit: fs_dbexprtti.pas
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
  SysUtils, Classes, fs_iinterpreter, fs_itools, fs_iclassesrtti, fs_ievents,
  xlcClasses, xlEngine, xlReport, frxDBExporter;

type
  TfsDBExporterRTTI = class(TComponent); // fake component

implementation

type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
    procedure SetProp(Instance: TObject; ClassType: TClass;
      const PropName: String; Value: Variant);
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TFunctions }

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if ClassType = TfrxDBFExporter then
  begin
    if MethodName = 'EXPORTDATA' then
      TfrxDBFExporter(Instance).ExportData    
  end else
  if ClassType = TfrxXLExporter then
  begin
    if MethodName = 'SHOWREPORT' then
      TfrxXLExporter(Instance).ShowReport
    else
    if MethodName = 'MERGEREPORT' then
      TfrxXLExporter(Instance).MergeReports;
  end else
  if ClassType = TfrxXLDatasources then
  begin
    if MethodName = 'CLEAR' then
      TfrxXLDatasources(Instance).Clear
    else if MethodName = 'ITEMS.GET' then
      Result := Integer(TfrxXLDatasources(Instance).Items[Caller.Params[0]])
    else if MethodName = 'ITEMS.SET' then
      TfrxXLDatasources(Instance).Items[Caller.Params[0]] := Pointer(Integer(Caller.Params[1]))
    else if MethodName = 'ADDITEM' then
      Result := TfrxXLDatasources(Instance).AddItem
    else if MethodName = 'ADD' then
      Result := Integer(TfrxXLDatasources(Instance).Add);
  end else
  if ClassType = TfrxXLParams then
  begin
    if MethodName = 'CLEAR' then
      TfrxXLParams(Instance).Clear
    else if MethodName = 'ITEMS.GET' then
      Result := Integer(TfrxXLParams(Instance).Items[Caller.Params[0]])
    else if MethodName = 'ITEMS.SET' then
      TfrxXLParams(Instance).Items[Caller.Params[0]] := Pointer(Integer(Caller.Params[1]))      
    else if MethodName = 'ADDITEM' then
      Result := TfrxXLParams(Instance).AddItem
    else if MethodName = 'ADD' then
      Result := Integer(TfrxXLParams(Instance).Add);      
  end else
  if ClassType = TfrxXLItems then
  begin
    if MethodName = 'CLEAR' then
      TfrxXLItems(Instance).Clear
    else if MethodName = 'ITEMS.GET' then
      Result := Integer(TfrxXLItems(Instance).Items[Caller.Params[0]])
    else if MethodName = 'ITEMS.SET' then
      TfrxXLItems(Instance).Items[Caller.Params[0]] := Pointer(Integer(Caller.Params[1]))      
    else if MethodName = 'ADDITEM' then
      Result := TfrxXLItems(Instance).AddItem
    else if MethodName = 'ADD' then
      Result := Integer(TfrxXLItems(Instance).Add);      
  end  
end;

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    with AddClass(TfrxDBFExporter, 'TComponent') do
    begin
      AddProperty('Datasources', 'TfrxXLDatasources', GetProp, SetProp);
      AddProperty('AutoGenerate', 'Boolean', GetProp, SetProp);
      AddMethod('procedure ExportData', CallMethod);
    end;
    with AddClass(TfrxXLExporter, 'TComponent') do
    begin
      AddProperty('Datasources', 'TfrxXLDatasources', GetProp, SetProp);
      AddProperty('AutoGenerate', 'Boolean', GetProp, SetProp);
      AddProperty('CompositeReport', 'Boolean', GetProp, SetProp);
      AddProperty('Params', 'TfrxXLParams', GetProp, SetProp);
      AddProperty('XLSTemplate', 'String', GetProp, SetProp);
      AddMethod('procedure ShowReport', CallMethod);
      AddMethod('procedure MergeReport', CallMethod);
    end;    
    with AddClass(TfrxXLDatasource, 'TCollectionItem') do
    begin
      AddProperty('Dataset','TfrxDBDataset', GetProp, SetProp);
      AddProperty('Alias','String', GetProp, SetProp);
      AddProperty('Range','String', GetProp, SetProp);
      AddProperty('MasterSource', 'TfrxXLDatasource', GetProp, SetProp);
      AddProperty('MasterSourceName','String', GetProp);
    end;
    with AddClass(TfrxXLParam, 'TCollectionItem') do
    begin
      AddProperty('Name','String', GetProp, SetProp);
      AddProperty('Value','Variant', GetProp, SetProp);
    end;
    with AddClass(TfrxXLItem, 'TCollectionItem') do
    begin
      AddProperty('XLExporter','TfrxXLExporter', GetProp, SetProp);
      AddProperty('SheetPrefix','string', GetProp, SetProp);
    end;
    with AddClass(TfrxXLItems, 'TCollection') do
    begin
      AddMethod('procedure Clear', CallMethod);
      AddMethod('function AddItem: integer', CallMethod);
      AddMethod('function Add: TfrxXLItem', CallMethod);
      AddProperty('Count', 'Integer', GetProp, nil);
      AddDefaultProperty('Items', 'Integer', 'TfrxXLItem', CallMethod, True);    
    end;
    with AddClass(TfrxXLDatasources, 'TCollection') do
    begin
      AddMethod('procedure Clear', CallMethod);
      AddMethod('function AddItem: integer', CallMethod);
      AddMethod('function Add: TfrxXLDatasource', CallMethod);
      AddProperty('Count', 'Integer', GetProp, nil);
      AddDefaultProperty('Items', 'Integer', 'TfrxXLDatasource', CallMethod, True);
    end;
    with AddClass(TfrxXLParams, 'TCollection') do
    begin
      AddMethod('procedure Clear', CallMethod);
      AddMethod('function AddItem: integer', CallMethod);
      AddMethod('function Add: TfrxXLParam', CallMethod);
      AddProperty('Count', 'Integer', GetProp, nil);
      AddDefaultProperty('Items', 'Integer', 'TfrxXLParam', CallMethod, True);
    end;        
  end;
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;

  if ClassType = TfrxDBFExporter then
  begin
    if PropName = 'DATASOURCES' then
      Result := Integer(TfrxDBFExporter(Instance).DataSources)
    else if PropName = 'AUTOGENERATE' then
      Result := TfrxDBFExporter(Instance).AutoGenerate;
  end else
  if ClassType = TfrxXLExporter then
  begin
    if PropName = 'DATASOURCES' then
      Result := Integer(TfrxXLExporter(Instance).DataSources)
    else if PropName = 'PARAMS' then
      Result := Integer(TfrxXLExporter(Instance).Params)
    else if PropName = 'XLSTEMPLATE' then
      Result := TfrxXLExporter(Instance).XLSTemplate
    else if PropName = 'AUTOGENERATE' then
      Result := TfrxXLExporter(Instance).AutoGenerate
    else if PropName = 'COMPOSITEREPORT' then
      Result := TfrxXLExporter(Instance).CompositeReport            
  end else  
  if ClassType = TfrxXLDatasources then
  begin
    if PropName = 'COUNT' then
      Result := TfrxXLDatasources(Instance).Count
  end else
  if ClassType = TfrxXLParams then
  begin
    if PropName = 'COUNT' then
      Result := TfrxXLParams(Instance).Count
  end else
  if ClassType = TfrxXLItems then
  begin
    if PropName = 'COUNT' then
      Result := TfrxXLItems(Instance).Count
  end else
  if ClassType = TfrxXLParam then
  begin
    if PropName = 'NAME' then
      Result := TfrxXLParam(Instance).Name
    else
    if PropName = 'VALUE' then
      Result := TfrxXLParam(Instance).Value
  end else
  if ClassType = TfrxXLItem then
  begin
    if PropName = 'XLEXPORTER' then
      Result := Integer(TfrxXLItem(Instance).XLExporter)
    else
    if PropName = 'SHEETPREFIX' then
      Result := TfrxXLItem(Instance).SheetPrefix
  end else
  if ClassType = TfrxXLDatasource then
  begin
    if PropName = 'DATASET' then
      Result := Integer(TfrxXLDatasource(Instance).Dataset)
    else
    if PropName = 'ALIAS' then
      Result := TfrxXLDatasource(Instance).Alias
    else
    if PropName = 'RANGE' then
      Result := TfrxXLDatasource(Instance).Range
    else    
    if PropName = 'MASTERSOURCENAME' then
      Result := TfrxXLDatasource(Instance).MasterSourceName
    else
    if PropName = 'MASTERSOURCE' then
      Result := Integer(TfrxXLDatasource(Instance).MasterSourceName);
  end;
end;

procedure TFunctions.SetProp(Instance: TObject; ClassType: TClass;
  const PropName: String; Value: Variant);
begin
  if ClassType = TfrxDBFExporter then
  begin
    if PropName = 'DATASOURCES' then
      TfrxDBFExporter(Instance).DataSources := Pointer(Integer(Value))
    else if PropName = 'AUTOGENERATE' then
      TfrxDBFExporter(Instance).AutoGenerate := Boolean(Value)
  end else
  if ClassType = TfrxXLExporter then
  begin
    if PropName = 'DATASOURCES' then
      TfrxXLExporter(Instance).DataSources := Pointer(Integer(Value))
    else if PropName = 'PARAMS' then
      TfrxXLExporter(Instance).Params := Pointer(Integer(Value))
    else if PropName = 'XLSTEMPLATE' then
      TfrxXLExporter(Instance).XLSTemplate := String(Value)
    else if PropName = 'AUTOGENERATE' then
      TfrxXLExporter(Instance).AutoGenerate := Boolean(Value)
    else if PropName = 'COMPOSITEREPORT' then
      TfrxXLExporter(Instance).CompositeReport := Boolean(Value)
  end else
  if ClassType = TfrxXLParam then
  begin
    if PropName = 'NAME' then
      TfrxXLParam(Instance).Name := Value
    else if PropName = 'VALUE' then
      TfrxXLParam(Instance).Value := Value;
  end else
  if ClassType = TfrxXLItem then
  begin
    if PropName = 'XLEXPORTER' then
      TfrxXLItem(Instance).XLExporter := Pointer(Integer(Value))
    else if PropName = 'SHEETPREFIX' then
      TfrxXLItem(Instance).SheetPrefix := String(Value);
  end else
  if ClassType = TfrxXLDatasource then
  begin
    if PropName = 'DATASET' then
      TfrxXLDatasource(Instance).Dataset := Pointer(Integer(Value))
    else if PropName = 'ALIAS' then
      TfrxXLDatasource(Instance).Alias := String(Value)
    else if PropName = 'RANGE' then
      TfrxXLDatasource(Instance).Range := String(Value)
    else if PropName = 'MASTERSOURCE' then
      TfrxXLDatasource(Instance).MasterSource := Pointer(Integer(Value));
  end  
end;

initialization
  fsRTTIModules.Add(TFunctions);

end.
