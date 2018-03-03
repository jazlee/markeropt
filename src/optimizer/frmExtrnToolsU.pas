unit frmExtrnToolsU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxEdit, cxLabel, cxControls, cxContainer, cxListBox, Menus,
  cxLookAndFeelPainters, StdCtrls, cxButtons, ecfextrntools;

{$I CSPDefs.inc}  

type
  TfrmExtrnTools = class(TForm)
    ListBox: TcxListBox;
    cxLabel1: TcxLabel;
    btnAdd: TcxButton;
    btnEdit: TcxButton;
    btnDelete: TcxButton;
    btnDown: TcxButton;
    btnUp: TcxButton;
    btnCLose: TcxButton;
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    FExternTools: TExternalTools;

    procedure UpdateState;
  public

    procedure AssignCollection(AColl: TExternalTools);
    
  end;

var
  frmExtrnTools: TfrmExtrnTools;

implementation

uses frmExtrnToolsEditU, ecflangutil;

{$R *.dfm}

{ TfrmExtrnTools }

procedure TfrmExtrnTools.AssignCollection(AColl: TExternalTools);
begin
  ReconstructLanguage(Self);
  FExternTools := AColl;
  UpdateState;
end;

procedure TfrmExtrnTools.btnAddClick(Sender: TObject);
var
  AItem: TExternalToolItem;
begin
  with TfrmExtrnToolsEdit.Create(Self) do
  try
    AItem := FExternTools.AddItem;
    AssignItem(AItem);
    if ShowModal <> mrOk then
      AItem.Free;
    UpdateState;
  finally
    Free;
  end;
end;

procedure TfrmExtrnTools.btnDeleteClick(Sender: TObject);
var
  AItem: TExternalToolItem;
begin
  {$IFDEF DELPHI9}
  if (ListBox.ItemIndex >= 0) and (ListBox.Count > 0) and
     (MessageDlg('Delete the selected tool?', mtConfirmation, mbYesNo, 0) = mrYes) then
  {$ELSE}
  if (ListBox.ItemIndex >= 0) and (ListBox.Count > 0) and
     (MessageDlg('Delete the selected tool?', mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  {$ENDIF}
  begin
    AItem := TExternalToolItem(ListBox.Items.Objects[ListBox.ItemIndex]);
    AItem.Free;
    ListBox.DeleteSelected;
    if ListBox.Count <= 0 then
      UpdateState;    
  end;
end;

procedure TfrmExtrnTools.btnDownClick(Sender: TObject);
var
  ANewIndex: Integer;
begin
  if (ListBox.ItemIndex < (ListBox.Count-1)) and (ListBox.ItemIndex >= 0) then
  begin
    ANewIndex := ListBox.ItemIndex + 1;
    TExternalToolItem(ListBox.Items.Objects[ListBox.ItemIndex]).Index := ANewIndex;
    ListBox.Items.Move(ListBox.ItemIndex, ANewIndex);
  end;
end;

procedure TfrmExtrnTools.btnEditClick(Sender: TObject);
var
  AItem: TExternalToolItem;
begin
  if (ListBox.ItemIndex >= 0) and (ListBox.Count > 0) then  
  with TfrmExtrnToolsEdit.Create(Self) do
  try
    AItem := TExternalToolItem(ListBox.Items.Objects[ListBox.ItemIndex]);
    AssignItem(AItem);
    ShowModal;
    UpdateState;
  finally
    Free;
  end;
end;

procedure TfrmExtrnTools.btnUpClick(Sender: TObject);
var
  ANewIndex: Integer;
begin
  if ListBox.ItemIndex > 0 then
  begin
    ANewIndex := ListBox.ItemIndex - 1;
    TExternalToolItem(ListBox.Items.Objects[ListBox.ItemIndex]).Index := ANewIndex; 
    ListBox.Items.Move(ListBox.ItemIndex, ANewIndex);
  end;
end;

procedure TfrmExtrnTools.UpdateState;
var
  I: Integer;
begin
  ListBox.Clear;
  for I := 0 to FExternTools.Count - 1 do
  begin
    ListBox.AddItem(
      TExternalToolItem(FExternTools.Items[I]).Title,
      FExternTools.Items[I]);
  end;
  btnEdit.Enabled := ListBox.Count > 0;
  btnDelete.Enabled := btnEdit.Enabled;
  btnUp.Enabled := btnEdit.Enabled;
  btnDown.Enabled := btnEdit.Enabled;     
end;

end.
