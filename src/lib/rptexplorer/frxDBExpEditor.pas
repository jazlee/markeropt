unit frxDBExpEditor;

interface
{$I frx.inc}
///////////////////////////////////////////////////////////////////////////////
//         Unit: frxDBExpEditor.pas
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
  Windows, Classes, SysUtils, Forms, Dialogs, frxCustomDB, frxDsgnIntf, frxRes,
  frxDBExporter, Controls
{$IFDEF Delphi6}
, Variants
{$ENDIF};

type
  TfrxDBExpDatasourceProperty = class(TfrxClassProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    function Edit: Boolean; override;
  end;

  TfrxDBExpParamsProperty = class(TfrxClassProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    function Edit: Boolean; override;
  end;

  TfrxDBExpXLItemsProperty = class(TfrxClassProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    function Edit: Boolean; override;
  end;

  TfrxDBExpXLTemplateProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    function Edit: Boolean; override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

implementation
uses
  frmDBExpEditorU;

{ TfrxDBExpDatasourceProperty }

function TfrxDBExpDatasourceProperty.Edit: Boolean;
begin
  with TfrmDBExpEditor.Create(Designer) do
  try
    Report := TfrxCustomExporter(Component).Report;
    Datasources := TfrxCustomExporter(Component).DataSources;
    Execute;
  finally
    Free;
  end;
end;

function TfrxDBExpDatasourceProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TfrxDBExpParamsProperty }

function TfrxDBExpParamsProperty.Edit: Boolean;
begin
  with TfrmDBExpEditor.Create(Designer) do
  try
    Report := TfrxCustomExporter(Component).Report;
    Params := TfrxXLExporter(Component).Params;
    Execute(1);
  finally
    Free;
  end;
end;

function TfrxDBExpParamsProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TfrxDBExpXLItemsProperty }

function TfrxDBExpXLItemsProperty.Edit: Boolean;
begin
  with TfrmDBExpEditor.Create(Designer) do
  try
    Report := TfrxCustomExporter(Component).Report;
    XLItems := TfrxXLExporter(Component).XLSItems;
    Execute(2);
  finally
    Free;
  end;
end;

function TfrxDBExpXLItemsProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TfrxDBExpXLTemplateProperty }

function TfrxDBExpXLTemplateProperty.Edit: Boolean;
begin
  with TOpenDialog.Create(Designer) do
  try
    Filter := 'Microsoft Excel File (*.xls)|*.xls';
    Title  := 'Open Excel Template File';
    DefaultExt := '.xls';
    if Execute then
      TfrxXLExporter(Component).XLSTemplate := FileName;
  finally
    Free;
  end;
end;

function TfrxDBExpXLTemplateProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paDialog];
end;

function TfrxDBExpXLTemplateProperty.GetValue: String;
begin
  Result := TfrxXLExporter(Component).XLSTemplate;
end;

procedure TfrxDBExpXLTemplateProperty.SetValue(const Value: String);
begin
  TfrxXLExporter(Component).XLSTemplate := Value;
end;

initialization
  frxPropertyEditors.Register(TypeInfo(TfrxXLDatasources), TfrxCustomExporter,
    'Datasources', TfrxDBExpDatasourceProperty);
  frxPropertyEditors.Register(TypeInfo(TfrxXLParams), TfrxXLExporter,
    'Params', TfrxDBExpParamsProperty);
  frxPropertyEditors.Register(TypeInfo(TfrxXLParams), TfrxXLExporter,
    'XLSItems', TfrxDBExpXLItemsProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxXLExporter,
    'XLSTemplate', TfrxDBExpXLTemplateProperty);
end.
