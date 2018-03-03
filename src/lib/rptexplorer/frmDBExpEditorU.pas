unit frmDBExpEditorU;

interface
{$I frx.inc}
///////////////////////////////////////////////////////////////////////////////
//         Unit: frmDBExpEditorU.pas
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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, frxDBExporter, frxClass, frxCustomDB, frxDBSet,
  Buttons;

type
  TfrmDBExpEditor = class(TForm)
    Panel1: TPanel;
    btnAdd: TButton;
    btnRemove: TButton;
    btnApply: TButton;
    btnClose: TButton;
    Panel2: TPanel;
    ListBox1: TListBox;
    Panel3: TPanel;
    PanelDS: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    edDataset: TComboBox;
    edAlias: TEdit;
    edMastersource: TComboBox;
    PanelPRM: TPanel;
    Label4: TLabel;
    edName: TEdit;
    Label5: TLabel;
    edValue: TEdit;
    PanelITM: TPanel;
    Label6: TLabel;
    edXLReport: TComboBox;
    SpeedButton3: TSpeedButton;
    Label7: TLabel;
    edRange: TEdit;
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnApplyClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure edDatasetChange(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    FActiveDS: TfrxXLDatasource;
    FActivePRM: TfrxXLParam;
    FReport: TfrxReport;
    FDS: TfrxXLDatasources;
    FPrm: TfrxXLParams;
    FType: smallint;
    FItems: TfrxXLItems;
    FActiveITM: TfrxXLItem;
    { Private declarations }

    procedure PrepareData;

    procedure SetActiveDS(const Value: TfrxXLDatasource);
    procedure SetActivePRM(const Value: TfrxXLParam);
    procedure SetActiveITM(const Value: TfrxXLItem);

  protected
    property ActiveDS: TfrxXLDatasource read FActiveDS write SetActiveDS;
    property ActivePRM: TfrxXLParam read FActivePRM write SetActivePRM;
    property ActiveITM: TfrxXLItem read FActiveITM write SetActiveITM;
  public
    procedure Execute(const AType: smallint = 0);

    property Report: TfrxReport read FReport write FReport;
    property Datasources: TfrxXLDatasources read FDS write FDS;
    property Params: TfrxXLParams read FPrm write FPrm;
    property XLItems: TfrxXLItems read FItems write FItems;
  end;

var
  frmDBExpEditor: TfrmDBExpEditor;

implementation

{$R *.dfm}

{ TfrmDBExpEditor }

procedure TfrmDBExpEditor.Execute(const AType: smallint);
begin
  FType := AType;
  case AType of
    0:
      begin
        PanelDS.Visible := True;
        PanelPRM.Visible := False;
        PanelITM.Visible := False;
        PanelDS.Align := alClient;
        Caption := 'Datasource Editor';
        ActiveDS := nil;
      end;
    1:
      begin
        PanelDS.Visible := False;
        PanelITM.Visible := False;
        PanelPRM.Visible := True;
        PanelPRM.Align := alClient;
        Caption := 'Parameter Editor';
        ActivePRM := nil;      
      end;
    2:
      begin
        PanelDS.Visible := False;
        PanelPRM.Visible := False;
        PanelITM.Visible := True;
        PanelITM.Align := alClient;
        Caption := 'Composited Excel Report Editor';
        ActiveITM := nil;      
      end;
  end;
  PrepareData;
  ShowModal;
end;

procedure TfrmDBExpEditor.PrepareData;
var
  i: integer;
begin
  ListBox1.Clear;
  edMastersource.Clear;
  edDataset.Clear;
  edXLReport.Clear;
  case FType of
    0:
      begin
        for i := 0 to Datasources.Count-1 do
        begin
          ListBox1.AddItem(Format('Datasource #%d',[i+1]), Datasources.Items[i]);
          edMastersource.Items.AddObject(Format('Datasource #%d',[i+1]),
            Datasources.Items[i])
        end;
        Report.GetDataSetList(edDataset.Items, True);
      end;
    1:
      begin
        for i := 0 to Params.Count-1 do
          ListBox1.AddItem(Format('Params #%d',[i+1]), Params.Items[i]);
      end;
    2:
      begin
        for i := 0 to XLItems.Count-1 do
          ListBox1.AddItem(Format('XLReport #%d',[i+1]), XLItems.Items[i]);
        with frxGetExporter do
        begin
          for i := 0 to Count-1 do
          begin
            if (Items[i] <> nil) and (TObject(Items[i]) is TfrxXLExporter) then
              edXLReport.AddItem(TfrxXLExporter(Items[i]).Name,
                TfrxXLExporter(Items[i]));
          end;
        end;
      end;
  end;
end;

procedure TfrmDBExpEditor.ListBox1Click(Sender: TObject);
begin
  case FType of
    0:
      if (ListBox1.Count > 0) and (ListBox1.ItemIndex > -1) then
        ActiveDS := TfrxXLDatasource(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    1:
      if (ListBox1.Count > 0) and (ListBox1.ItemIndex > -1) then
        ActivePRM := TfrxXLParam(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    2:
      if (ListBox1.Count > 0) and (ListBox1.ItemIndex > -1) then
        ActiveITM := TfrxXLItem(ListBox1.Items.Objects[ListBox1.ItemIndex]);
  end;
end;

procedure TfrmDBExpEditor.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN: ListBox1Click(Self);
  end;
end;

procedure TfrmDBExpEditor.btnApplyClick(Sender: TObject);
begin
  case FType of
    0:
      if (ActiveDS <> nil) then
      begin
        ActiveDS.Range := edRange.Text;
        ActiveDS.Alias := edAlias.Text;
        if edDataset.ItemIndex > -1 then
          ActiveDS.Dataset :=
            TfrxDBDataSet(edDataset.Items.Objects[edDataset.ItemIndex])
        else
          ActiveDS.Dataset := nil;
        if edMastersource.ItemIndex > -1 then
          ActiveDS.MasterSource :=
            TfrxXLDatasource(edMastersource.Items.Objects[edMastersource.ItemIndex])
        else
          ActiveDS.MasterSource := nil;
        ActiveDS := nil;
      end;
    1:
      if (ActivePRM <> nil) then
      begin
        ActivePRM.Name := edName.Text;
        ActivePRM.Value := edValue.Text;
        ActivePRM := nil;
      end;
    2:
      if (ActiveITM <> nil) then
      begin
        if edXLReport.ItemIndex > -1 then
          ActiveITM.XLExporter :=
            TfrxXLExporter(edXLReport.Items.Objects[edXLReport.ItemIndex]);
        ActiveITM := nil;
      end;
  end;
end;

procedure TfrmDBExpEditor.btnAddClick(Sender: TObject);
begin
  case FType of
    0: ActiveDS := Datasources.Add;
    1: ActivePRM:= Params.Add;
    2: ActiveITM:= XLItems.Add;
  end;
  PrepareData;
end;

procedure TfrmDBExpEditor.SetActiveDS(const Value: TfrxXLDatasource);
begin
  FActiveDS := Value;
  if FActiveDS <> nil then
  begin
    if FActiveDS.Dataset <> nil then
      edDataset.ItemIndex := edDataset.Items.IndexOfObject(FActiveDS.Dataset);
    edAlias.Text := FActiveDS.Alias;
    edRange.Text := FActiveDS.Range;
    if (FActiveDS.MasterSource <> nil) then
      edMastersource.Text :=
        edMastersource.Items[
          edMastersource.Items.IndexOfObject(FActiveDS.MasterSource)
        ];
  end else
  begin
    edDataset.ItemIndex := -1;
    edAlias.Text := EmptyStr;
    edRange.Text := EmptyStr;
    edMastersource.ItemIndex := -1;
  end;
  btnApply.Enabled := (FActiveDS <> nil);
  edDataset.Enabled := (FActiveDS <> nil);
  edAlias.Enabled := (FActiveDS <> nil);
  edRange.Enabled := (FActiveDS <> nil);
  edMastersource.Enabled := (FActiveDS <> nil);
  SpeedButton1.Enabled := (FActiveDS <> nil);
  SpeedButton2.Enabled := (FActiveDS <> nil); 
end;

procedure TfrmDBExpEditor.btnRemoveClick(Sender: TObject);
begin
  case FType of
    0:
      if ActiveDS <> nil then
      begin
        Datasources.Delete(ActiveDS.Index);
        ActiveDS := nil;
      end;
    1:
      if ActivePRM <> nil then
      begin
        Params.Delete(ActivePRM.Index);
        ActivePRM := nil;
      end;
    2:
      if ActiveITM <> nil then
      begin
        XLItems.Delete(ActiveITM.Index);
        ActiveITM := nil;
      end;
  end;
  PrepareData;
end;

procedure TfrmDBExpEditor.SpeedButton1Click(Sender: TObject);
begin
  edDataset.ItemIndex := -1;
end;

procedure TfrmDBExpEditor.SpeedButton2Click(Sender: TObject);
begin
  edMastersource.ItemIndex := -1;
end;

procedure TfrmDBExpEditor.SetActivePRM(const Value: TfrxXLParam);
begin
  FActivePRM := Value;
  if FActivePRM <> nil then
  begin
    edName.Text := FActivePRM.Name;
    edValue.Text := VarToStr(FActivePRM.Value);
  end else
  begin
    edName.Text := EmptyStr;
    edValue.Text := EmptyStr; 
  end;
  btnApply.Enabled := (FActivePRM <> nil);
  edName.Enabled := (FActivePRM <> nil);
  edValue.Enabled := (FActivePRM <> nil);
end;

procedure TfrmDBExpEditor.edDatasetChange(Sender: TObject);
begin
  if edDataset.ItemIndex > -1 then
  begin
    edAlias.Text :=
      TfrxDBDataset(edDataset.Items.Objects[edDataset.ItemIndex]).UserName;
  end;
end;

procedure TfrmDBExpEditor.SetActiveITM(const Value: TfrxXLItem);
begin
  FActiveITM := Value;
  if FActiveITM <> nil then
  begin
    if FActiveITM.XLExporter <> nil then
      edXLReport.ItemIndex := edXLReport.Items.IndexOfObject(FActiveITM.XLExporter);
  end else
  begin
    edXLReport.ItemIndex := -1;
  end;
  btnApply.Enabled := (FActiveITM <> nil);
  edXLReport.Enabled := (FActiveITM <> nil);
  SpeedButton3.Enabled := (FActiveITM <> nil);  
end;

procedure TfrmDBExpEditor.SpeedButton3Click(Sender: TObject);
begin
  edXLReport.ItemIndex := -1;
end;

end.
