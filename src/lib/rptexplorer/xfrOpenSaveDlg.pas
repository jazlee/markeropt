unit xfrOpenSaveDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frxClass, Menus, StdCtrls, Buttons, xfrRptExplr, ExtCtrls,
  ComCtrls, ImgList;

type
  TfrxDialogType = (dtOpenDialog, dtSaveDialog);
  
  TxfrOpenSaveDialog = class(TForm)
    lblSaveIn: TLabel;
    cbFolders: TComboBox;
    btnUpOneLevel: TSpeedButton;
    btnNewFolder: TSpeedButton;
    btnListView: TSpeedButton;
    btnDetailView: TSpeedButton;
    lblItemName: TLabel;
    edItemName: TEdit;
    lblSaveAsType: TLabel;
    cbFileType: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    PopUpItems: TPopupMenu;
    itmView: TMenuItem;
    ItmViewList: TMenuItem;
    itmViewDetails: TMenuItem;
    N8: TMenuItem;
    itmNewFolder: TMenuItem;
    N10: TMenuItem;
    itmDelete: TMenuItem;
    itmRename: TMenuItem;
    Panel1: TPanel;
    ListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbFoldersChange(Sender: TObject);
    procedure cbFoldersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbFoldersDropDown(Sender: TObject);
    procedure edItemNameChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnUpOneLevelClick(Sender: TObject);
    procedure btnNewFolderClick(Sender: TObject);
    procedure btnListViewClick(Sender: TObject);
    procedure btnDetailViewClick(Sender: TObject);
    procedure itmRenameClick(Sender: TObject);
    procedure itmDeleteClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListViewEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure ListViewEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FRptExplorer: TxfrReportExplorer;
    FCurrentFolder: TxfrDirectoryItem;
    FImageList: TImageList;
    FDummyList: TStringList;
    FFolderLevels: TList;
    FFolderIndex: Integer;
    FItemName: String;
    FSelectedItem: TxfrReportItem;
    FDialogType: TfrxDialogType;
    procedure SetRptExplorer(const Value: TxfrReportExplorer);
    procedure SetCurrentFolder(const Value: TxfrDirectoryItem);
    procedure SelectNewFolder;
    procedure SelectViewStyle(Value: Boolean);
    procedure CreateNewFolder;    
    procedure UpdateDropDownList;
    procedure UpdateListView;
    function  CloseConfirmed: Boolean;
    procedure SetDialogType(const Value: TfrxDialogType);
  public
    property DialogType: TfrxDialogType read FDialogType write SetDialogType;
    property ReportExplorer: TxfrReportExplorer read FRptExplorer write SetRptExplorer;
    property CurrentFolder: TxfrDirectoryItem read FCurrentFolder write SetCurrentFolder;
    property SelectedItem: TxfrReportItem read FSelectedItem;
    property ItemName: string read FItemName;
  end;

implementation

{$R *.dfm}

type
  TxfrAbstractItemHack = class(TxfrAbstractItem);

procedure TxfrOpenSaveDialog.FormCreate(Sender: TObject);

  procedure CopyImage(Glyph: TBitmap;ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;//! for lack of a better color
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end;

begin
  FFolderLevels := TList.Create;
  FDummyList := TStringList.Create;
  ListView.ViewStyle := vsList;
  FImageList := xfrExplorerResources.MainButtonImages;
  ListView.SmallImages := FImageList;

  CopyImage(btnUpOneLevel.Glyph, FImageList, 2);
  CopyImage(btnNewFolder.Glyph,  FImageList, 3);
  CopyImage(btnListView.Glyph,  FImageList, 0);
  CopyImage(btnDetailView.Glyph,  FImageList, 1);  
end;

procedure TxfrOpenSaveDialog.FormDestroy(Sender: TObject);
begin
  FFolderLevels.Free;
  FDummyList.Free;
end;

procedure TxfrOpenSaveDialog.cbFoldersChange(Sender: TObject);
begin
  FCurrentFolder := TxfrDirectoryItem(cbFolders.Items.Objects[cbFolders.ItemIndex]);
  SelectNewFolder;
end;

procedure TxfrOpenSaveDialog.cbFoldersDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  AOffset: Integer;
  AItem: TxfrAbstractItem;
begin
  cbFolders.Canvas.FillRect(Rect);
  AOffset := 1;
  AOffset := AOffset + (Integer(FFolderLevels[Index]) * 20);
  AItem   := TxfrAbstractItem(cbFolders.Items.Objects[Index]);
  if (not cbFolders.DroppedDown) or (Index = FFolderIndex) then
    FImageList.Draw(cbFolders.Canvas, Rect.Left + AOffset, Rect.Top, Integer(AItem.ItemType) + 6)
  else
    FImageList.Draw(cbFolders.Canvas, Rect.Left + AOffset, Rect.Top, Integer(AItem.ItemType) + 6);
  Inc(AOffset, 20);
  cbFolders.Canvas.TextOut(Rect.Left + AOffset, Rect.Top, cbFolders.Items[Index]);
end;

procedure TxfrOpenSaveDialog.cbFoldersDropDown(Sender: TObject);
begin
  FFolderIndex := cbFolders.ItemIndex;
end;

procedure TxfrOpenSaveDialog.edItemNameChange(Sender: TObject);
begin
  FItemName := edItemName.Text;
end;

procedure TxfrOpenSaveDialog.btnOKClick(Sender: TObject);
begin
  if (ListView.Selected <> nil) and (ListView.Selected.Data <> nil) and
     (TxfrAbstractItem(ListView.Selected.Data).ItemType <> itmReport) then
    ListViewDblClick(Self)
  else if CloseConfirmed then
    ModalResult := mrOK;
end;

procedure TxfrOpenSaveDialog.btnUpOneLevelClick(Sender: TObject);
begin
  if (FCurrentFolder.Owner <> nil) and (FCurrentFolder.Owner is TxfrAbstractItem) then
  begin
    FCurrentFolder := TxfrDirectoryItem(FCurrentFolder.Owner);
    SelectNewFolder;
  end;
end;

procedure TxfrOpenSaveDialog.btnNewFolderClick(Sender: TObject);
begin
  CreateNewFolder;
end;

procedure TxfrOpenSaveDialog.btnListViewClick(Sender: TObject);
begin
  SelectViewStyle(True);
end;

procedure TxfrOpenSaveDialog.btnDetailViewClick(Sender: TObject);
begin
  SelectViewStyle(False);
end;

procedure TxfrOpenSaveDialog.SetRptExplorer(
  const Value: TxfrReportExplorer);
begin
  FRptExplorer := Value;
end;

procedure TxfrOpenSaveDialog.UpdateDropDownList;
var
  AFolders: TStringList;
  AParentFolders: TStringList;
  i, AIndex, ALevel, ASelected: integer;
  AParentName: string;
  AItem: TxfrAbstractItem;
begin
  AFolders := TStringList.Create;
  AParentFolders := TStringList.Create;
  try
    FFolderLevels.Clear;
    TxfrAbstractItemHack(FRptExplorer.Root).GetChildFolders(AFolders);
    for i := 0 to AFolders.Count-1 do
      FFolderLevels.Add(TObject(1));
    TxfrAbstractItemHack(FCurrentFolder).GetParentFolders(AParentFolders);
    if AParentFolders.Count > 0 then
    begin
      if AParentFolders.Count > 0 then
        AParentFolders.Delete(AParentFolders.Count-1);
      if AParentFolders.Count > 0 then
        AParentName := AParentFolders[AParentFolders.Count-1]
      else
        AParentName := '';
      AIndex := AFolders.IndexOf(AParentName);
      if AIndex <> -1 then
        AParentFolders.Delete(AParentFolders.Count-1)
      else
        AIndex := 0;
      for i := 0 to AParentFolders.Count-1 do
      begin
        AItem  := TxfrAbstractItem(AParentFolders.Objects[i]);
        AFolders.InsertObject(AIndex+1, AItem.ReportName, AItem);
        ALevel := AParentFolders.Count - i + 1;
        FFolderLevels.Insert(AIndex+1, TObject(ALevel)); 
      end;
    end;
    cbFolders.Items.BeginUpdate;
    try
      cbFolders.Items.Clear;
      ASelected := -1;
      AFolders.InsertObject(0, FRptExplorer.Root.ReportName, FRptExplorer.Root);
      FFolderLevels.Insert(0, TObject(0));
      for i := 0 to AFolders.Count-1 do
      begin
        AItem := TxfrAbstractItem(AFolders.Objects[i]);
        cbFolders.Items.AddObject(AItem.ReportName, AItem);
        if FCurrentFolder = TxfrDirectoryItem(AItem) then
          ASelected := i;
      end;
      cbFolders.ItemIndex := ASelected;
    finally
      cbFolders.Items.EndUpdate;
    end;
  finally
    AFolders.Free;
    AParentFolders.Free;
  end;
end;

procedure TxfrOpenSaveDialog.SetCurrentFolder(
  const Value: TxfrDirectoryItem);
begin
  FCurrentFolder := Value;
  SelectNewFolder;
end;

procedure TxfrOpenSaveDialog.SelectNewFolder;
begin
  UpdateDropDownList;
  UpdateListView;  
end;

procedure TxfrOpenSaveDialog.SelectViewStyle(Value: Boolean);
begin
  if Value then
    begin
      ItmViewList.Checked := True;
      btnListView.Down := True;
      btnDetailView.Down := False;

      ListView.Columns[0].Width := ListView.Width;
      ListView.ViewStyle := vsList;
    end
  else
    begin
      itmViewDetails.Checked := True;
      btnListView.Down := False;
      btnDetailView.Down := True;

      ListView.Columns[0].Width := 250;
      ListView.ViewStyle := vsReport;
    end;
end;

procedure TxfrOpenSaveDialog.CreateNewFolder;
begin
  if (FCurrentFolder <> nil) then
  begin
    with TxfrDirectoryItem.Create(TxfrAbstractItem(FCurrentFolder)) do;
    SelectNewFolder;    
  end;
end;

procedure TxfrOpenSaveDialog.UpdateListView;
var
  i: integer;
  ARoot: TxfrDirectoryItem;
  AListItem: TListItem;
begin
  if FCurrentFolder <> nil then
  begin
    ListView.Items.BeginUpdate;
    try
      ListView.Clear;
      ARoot := FCurrentFolder;
      if ARoot <> nil then
      begin
        for i := 0 to ARoot.ComponentCount-1 do
        begin
          if (TxfrAbstractItem(ARoot.Components[i]).ItemType in [itmRoot, itmDirectory, itmReport]) then
          begin
{
          if (not(ftHidden in TxfrAbstractItem(ARoot.Components[i]).ReportAttr)) or
             (ShowHiddenObjects = True) then
          begin
}
            AListItem := ListView.Items.Add;
            AListItem.ImageIndex := Integer(
              TxfrAbstractItem(ARoot.Components[i]).ItemType) + 6;
            AListItem.Data := ARoot.Components[i];
            AListItem.Caption := TxfrAbstractItem(ARoot.Components[i]).ReportName;
            if TxfrAbstractItem(ARoot.Components[i]).ItemType = itmReport then
              AListItem.SubItems.Add(
                Format('%f kb',[TxfrReportItem(ARoot.Components[i]).Size / 1024]))
            else
              AListItem.SubItems.Add(EmptyStr);
            case TxfrAbstractItem(ARoot.Components[i]).ItemType of
              itmRoot, itmDirectory: AListItem.SubItems.Add('Folder');
              itmRecycleDir: AListItem.SubItems.Add('Recycle Bin');
              itmReport: AListItem.SubItems.Add('Report File');
            end;
            AListItem.SubItems.Add(DateTimeToStr(
              TxfrAbstractItem(ARoot.Components[i]).TimeStamp)
            );
          end;
        end;
      end;
    finally
      ListView.Items.EndUpdate;
    end;
  end;
end;

procedure TxfrOpenSaveDialog.itmRenameClick(Sender: TObject);
begin
  if (ListView.Selected <> nil) and (ListView.Selected.Data <> nil) then
    ListView.Selected.EditCaption;
end;

procedure TxfrOpenSaveDialog.itmDeleteClick(Sender: TObject);
var
  i: integer;
begin
  if ListView.Focused then
  begin
    if (ListView.SelCount > 0) and
       (MessageDlg(
          Format('Are you sure you want to send these %d items to the Recycle Bin?',
            [ListView.SelCount]),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      for i := 0 to ListView.Items.Count-1 do
      begin
        if ListView.Items[i].Selected and (ListView.Items[i].Data <> nil) then
        case TxfrAbstractItem(ListView.Items[i].Data).ItemType of
        itmDirectory, itmReport:
          begin
            if TxfrAbstractItem(ListView.Items[i].Data).Deleted then
              TxfrAbstractItem(ListView.Items[i].Data).Free
            else
              TxfrAbstractItem(ListView.Items[i].Data).MoveTo(
                TxfrReportExplorer(Owner).Root.RecycleDir)            
          end;
        end;
      end;
      SelectNewFolder;
    end;
  end;
end;

procedure TxfrOpenSaveDialog.ListViewDblClick(Sender: TObject);
var
  AItem: TxfrAbstractItem;
begin
  if (ListView.Selected <> nil) and (ListView.Selected.Data <> nil) then
  begin
    AItem := TxfrAbstractItem(ListView.Selected.Data);
    case AItem.ItemType of
      itmRoot, itmDirectory:
        begin
          FCurrentFolder := TxfrDirectoryItem(AItem);
          SelectNewFolder;
        end;
      itmReport:
        begin
          FItemName := AItem.ReportName;
          if (FSelectedItem = nil) or (FSelectedItem <> AItem) then
            FSelectedItem := TxfrReportItem(AItem);
          if CloseConfirmed then
            ModalResult := mrOK;
        end;
    end;
  end;
end;

procedure TxfrOpenSaveDialog.ListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if (Change = ctState) and (Item.Data <> nil) then
  begin
    if (TxfrAbstractItem(Item.Data).ItemType = itmReport) and
       (TxfrAbstractItem(Item.Data).ReportName <> EmptyStr) then
    begin
      edItemName.Text := TxfrAbstractItem(Item.Data).ReportName;
      FSelectedItem := TxfrReportItem(Item.Data);
    end;
  end;
end;

function TxfrOpenSaveDialog.CloseConfirmed: Boolean;
var
  AIndex: Integer;
  AFound: Boolean;
  AReport: TxfrAbstractItem;
begin

  if (FItemName = '') then
    begin
      Result := False;
      Exit;
    end;

  AIndex := 0;
  AFound := False;

  while (AIndex < ListView.Items.Count) and not(AFound) do
    begin
      AReport := TxfrAbstractItem(ListView.Items[AIndex].Data);
      if (AReport <> nil) and (AReport.ItemType = itmReport) and
         (CompareText(FItemName, AReport.ReportName) = 0) then
        AFound := True
      else
        Inc(AIndex);
    end;

  if DialogType = dtOpenDialog then
  begin
    Result := AFound;
    if not AFound then
      MessageDlg('Cound not found the requested report', mtInformation, [mbOK], 0);
  end else
  begin
    if AFound then
      Result := (MessageDlg('The report with same name has already exists, replace it anyway?',
        mtConfirmation, mbYesNoCancel, 0) = mrYes)
    else
      Result := True;
  end;
end;

procedure TxfrOpenSaveDialog.ListViewEdited(Sender: TObject;
  Item: TListItem; var S: String);
begin
  if (Item.Data <> nil) and
     (not(ftReadOnly in TxfrAbstractItem(Item.Data).ReportAttr)) and
     (TxfrAbstractItem(Item.Data).ReportName <> S) then
  begin
    TxfrAbstractItem(Item.Data).ReportName := S;
    TxfrAbstractItemHack(Item.Data).SaveMe;
    if (TxfrAbstractItem(Item.Data).LinkedNode <> nil) then
    begin
      TTreeNode(TxfrAbstractItem(Item.Data).LinkedNode).Text :=
        TxfrAbstractItem(Item.Data).ReportName;
    end;
    S := TxfrAbstractItem(Item.Data).ReportName;
  end;
end;

procedure TxfrOpenSaveDialog.ListViewEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  if (Item.Data <> nil) and
     (ftReadOnly in TxfrAbstractItem(Item.Data).ReportAttr) then
    AllowEdit := False;
end;

procedure TxfrOpenSaveDialog.ListViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    itmDeleteClick(Self);
end;

procedure TxfrOpenSaveDialog.SetDialogType(const Value: TfrxDialogType);
begin
  FDialogType := Value;
  case FDialogType of
    dtOpenDialog:
      begin
        Caption := 'Open';
        btnOK.Caption := 'Open';
      end;
    dtSaveDialog:
      begin
        Caption := 'Save As';
        btnOK.Caption := 'Save';
      end;      
  end;
end;

end.
