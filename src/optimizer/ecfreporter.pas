unit ecfreporter;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, ComCtrls, frxDesgn, Menus, dxDockControl, dxBar,
  ImgList, ActnList, cxTreeView, cxContainer, cxListView, dxDockPanel,
  cxControls, dxStatusBar, xfrRptExplr, frxClass, frxRes,
  fs_idbrtti, fs_ichartrtti, fs_idialogsrtti, fs_igraphicsrtti, fs_iinirtti,
  fs_idbctrlsrtti, fs_iextctrlsrtti, fs_iformsrtti, fs_isysrtti, frxChBoxRTTI,
  frxBarcodeRTTI, frxClassRTTI, frxCustomDBRTTI, frxChartRTTI, frxCrossRTTI,
  frxRichRTTI, frxGradientRTTI, frxDCtrlRTTI, frxOLERTTI,
  frxDBExporter, fs_dbexprtti, frxDBExpEditor, frxVariables, frxExportODF,
  frxExportTXT, frxExportMail, frxExportCSV, frxExportText, frxExportImage,
  frxExportRTF, frxExportXML, frxExportXLS, frxExportHTML, frxExportPDF,
  frxCrypt, frxGZip, frxDMPExport, cxClasses, CSPConsts, frxvariants;

const
  APP_READMEFILE='readme.rtf';

  WM_APPCMD = WM_USER + 1;

type
  Tecfrptexplorer = class(TxfrCustomExplorer)
    dxStatusBar: TdxStatusBar;
    dxDockSite1: TdxDockSite;
    dxLayoutDockSite2: TdxLayoutDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    ContentPanel: TdxDockPanel;
    RLV: TcxListView;
    FolderTreePanel: TdxDockPanel;
    FTV: TcxTreeView;
    ActionList1: TActionList;
    actNewFolder: TAction;
    actNewReport: TAction;
    actOpen: TAction;
    actPrint: TAction;
    actDelete: TAction;
    actExit: TAction;
    actViewList: TAction;
    actViewDetails: TAction;
    actUpLevel: TAction;
    actAbout: TAction;
    actRename: TAction;
    actRefresh: TAction;
    actFolderTree: TAction;
    actEmptyRecycleBin: TAction;
    actRestore: TAction;
    actProperties: TAction;
    actShowOrigFile: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actImport: TAction;
    actExport: TAction;
    STDImages: TImageList;
    BarManager: TdxBarManager;
    File1: TdxBarSubItem;
    New1: TdxBarSubItem;
    Folder1: TdxBarButton;
    Report1: TdxBarButton;
    dxBarButton1: TdxBarButton;
    OpenReport1: TdxBarButton;
    dxBarButton3: TdxBarButton;
    View1: TdxBarSubItem;
    itmViewFolderTree: TdxBarButton;
    itmShowHidden: TdxBarButton;
    Uponelevel1: TdxBarButton;
    Help1: TdxBarSubItem;
    About1: TdxBarButton;
    dxBarButton4: TdxBarButton;
    List1: TdxBarButton;
    Detail1: TdxBarButton;
    Print1: TdxBarButton;
    Exit1: TdxBarButton;
    dxBarSubItem1: TdxBarSubItem;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    DeleteReport1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxDockingManager1: TdxDockingManager;
    SImage: TImageList;
    LImages: TImageList;
    PopupMenu1: TPopupMenu;
    Open1: TMenuItem;
    N5: TMenuItem;
    Folder2: TMenuItem;
    Report2: TMenuItem;
    Open2: TMenuItem;
    N2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N1: TMenuItem;
    ImportExport1: TMenuItem;
    ImportFiles1: TMenuItem;
    ExportFiles1: TMenuItem;
    Rename1: TMenuItem;
    Delete1: TMenuItem;
    N3: TMenuItem;
    Restore1: TMenuItem;
    EmptyRecyclebin1: TMenuItem;
    N4: TMenuItem;
    Properties1: TMenuItem;
    BrowseFile1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    AdmOptionList: TdxBarSubItem;
    actSecureRpt: TAction;
    dxBarButton15: TdxBarButton;
    actSaveRpts: TAction;
    dxBarButton16: TdxBarButton;
    procedure actEmptyRecycleBinExecute(Sender: TObject);
    procedure actRestoreExecute(Sender: TObject);
    procedure actPropertiesExecute(Sender: TObject);
    procedure actShowOrigFileExecute(Sender: TObject);
    procedure actRenameExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actNewFolderExecute(Sender: TObject);
    procedure actNewReportExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actImportExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actViewListExecute(Sender: TObject);
    procedure actViewDetailsExecute(Sender: TObject);
    procedure actUpLevelExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actFolderTreeExecute(Sender: TObject);
    procedure RLVChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure RLVEdited(Sender: TObject; Item: TListItem; var S: String);
    procedure RLVEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure RLVKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RLVEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure RLVDblClick(Sender: TObject);
    procedure RLVDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure RLVDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FTVChange(Sender: TObject; Node: TTreeNode);
    procedure FTVCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FTVDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FTVDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FTVEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure FTVEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure itmShowHiddenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSecureRptExecute(Sender: TObject);
    procedure actSaveRptsExecute(Sender: TObject);
  private
    FOldDirItem: TxfrDirectoryItem;
    FRptClipboard: TStringList;
    FFirstTime: Boolean;
    FTemplatePath: string;
    FEmbedded: boolean;

    function GetCollapsedNodes: String;
    function GetSelectedNode: TTreeNode;
    function GetFocused: boolean;
    procedure EnlargeWindow;
    procedure ReadSettings;
    function GetFolderTreeVisibility: boolean;
    procedure SetFolderTreeVisibility(const Value: boolean);
    procedure SetAppCommand(const Value: integer);

  private
    procedure WMAppCommand(var message: TMessage); message WM_APPCMD;
    procedure InternalValidateReport(AReport: TxfrReportItem; const AEvent: TxfrExplorerEventTypes);
    procedure CheckOnSave(Sender: TObject);
    procedure ConstructResources;
  protected
    procedure InternalUpdateState;

    property OldDirItem: TxfrDirectoryItem read FOldDirItem write FOldDirItem;
    property SelectedFolderNode: TTreeNode read GetSelectedNode;
    property TreeFocused: boolean read GetFocused;
    property FolderTreeVisible: boolean read GetFolderTreeVisibility write SetFolderTreeVisibility;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AppCommand: integer write SetAppCommand;

    procedure WriteSettings;

    procedure InitExplorer; override;
    procedure FinalizeExplorer;
    procedure Lock; override;
    procedure UpdateFolderTree; override;
    procedure UpdateReportList; override;
    procedure UpdateItems; override;
  end;

function TrimAll(const AValue: string): string;

var
  ecfrptexplorer: Tecfrptexplorer;
  FirstConnected: boolean = true;

implementation

uses
  ShlObj, xfrAboutFrm, xfrPropsFrm, xfrFViewerFrm,
  StrUtils, NativeRegXML, NativeRegIniXML, ecfcrypt, ShellAPI, cspapputil, gnugettext;

resourcestring
  MSG_INF_100 = 'Connection has been established successfully';

  MSG_ERR_100 = 'Initialization could not be done';
  MSG_ERR_101 = 'This application is not connected to the database';
  MSG_ERR_102 = 'Previous record has not been saved or canceled';
  MSG_ERR_103 = 'The record has already exist on the database';
  MSG_ERR_104 = 'Could not found specified record';

  MSG_CNF_100 = 'Are you sure to exit from this application?';
  MSG_CNF_101 = 'Are you sure to delete this record?';
  MSG_CNF_102 = 'You are about to abort the process, are you sure?';
  MSG_CNF_103 = 'Do you want to save this record?';

type
  TxfrHackedReportExplorer = class(TxfrReportExplorer);
  TxfrHackedItem = class(TxfrAbstractItem);
  TxfrHackedExplorer = class(TxfrReportExplorer);

var
  CollapsedNodes: String;


{$R *.dfm}
{$R frxXplrButtons.res}

function TrimAll(const AValue: string): string;
var
  nLen, i: integer;
  ch: char;
begin
  if AValue = '' then exit;
  nLen := PCardinal(Cardinal(AValue)-4)^;
  for i := 1 to nLen do
  begin
    ch := AValue[i];
    if ch <> #32 then
      Result := Result + ch;
  end;
end;

procedure CheckDir(ADir: string);
begin
  try
    if not DirectoryExists(ADir) then
      ForceDirectories(ADir);
  except; end;
end;

function BrowseCallback(Wnd: HWND; uMsg: UINT; lParam,lpData: LPARAM): Integer; stdcall;
begin
   Result:=0;
   if (uMsg=BFFM_INITIALIZED) then
      begin
      if (StrLen(PChar(lpData)) > 0) then
         SendMessage(Wnd,BFFM_SETSELECTION,1,lpData);
      end;
end;

function BrowseDirectory(var DirectoryStr: string): Boolean;
var
   WindowList: Pointer;
   Buffer: array[0..MAX_PATH] of Char;
   ItemIdList: PItemIDList;
   BrowseInfo: TBrowseInfo;
begin
   Result:=False;
   with BrowseInfo do
      begin
      hwndOwner:=Screen.ActiveForm.Handle;
      pidlRoot:=nil;
      pszDisplayName:=Buffer;
      lpszTitle:='Select the directory';
      ulFlags:=BIF_RETURNONLYFSDIRS;
      lpfn:=BrowseCallback;
      lParam:=Integer(PChar(DirectoryStr));
      end;
   WindowList:=DisableTaskWindows(0);
   try
      ItemIdList:=ShBrowseForFolder(BrowseInfo);
   finally
      EnableTaskWindows(WindowList);
   end;
   if (ItemIDList=nil) then
      Exit;
   if SHGetPathFromIDList(ItemIDList,@Buffer) then
      begin
      Result:=True;
      DirectoryStr:=StrPas(@Buffer);
      end;
end;

procedure Tecfrptexplorer.actEmptyRecycleBinExecute(Sender: TObject);
begin
  if (Explorer.Root.RecycleDir.ComponentCount > 0) and
     (MessageDlg(Format('Are you sure you want to delete this %d items',
      [Explorer.Root.RecycleDir.ComponentCount]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    Explorer.Root.RecycleDir.ReleaseAll;
    actRefreshExecute(Self);
  end;
end;

procedure Tecfrptexplorer.actRestoreExecute(Sender: TObject);
  function ValidateSelection: boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to RLV.Items.Count-1 do
    begin
      if (RLV.Items[i].Selected) and (RLV.Items[i].Data <> nil) and
         (TxfrAbstractItem(RLV.Items[i].Data).Deleted <> True) then
        exit;
    end;
    Result := True;
  end;
var
  i: integer;
begin
  if RLV.Focused then
  begin
    if (RLV.SelCount > 0) and
       (RLV.Items[0].Data <> nil) and
       (ValidateSelection) and
       (MessageDlg(Format('Are you sure you want to restore these %d items?',[RLV.SelCount]),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      for i := 0 to RLV.Items.Count-1 do
        if RLV.Items[i].Selected and (RLV.Items[i].Data <> nil) then
          TxfrHackedItem(RLV.Items[i].Data).Restore;
      UpdateItems;
    end;
  end;
end;

procedure Tecfrptexplorer.actPropertiesExecute(Sender: TObject);
var
  AItem: TxfrAbstractItem;
begin
  AItem := nil;
  if (RLV.Focused) and (RLV.Selected <> nil) and
     (RLV.Selected.Data <> nil) then
    AItem := TxfrAbstractItem(RLV.Selected.Data)
  else
  if (FTV.Focused) and (SelectedFolderNode <> nil) and
     (SelectedFolderNode.Data <> nil) then
    AItem := TxfrAbstractItem(SelectedFolderNode.Data);
  if (AItem = nil) then
    exit;

  TxfrHackedReportExplorer(Explorer).
    ValidateReport(TxfrReportItem(AItem), etShowProperties);
  with TfrxExplorerPropForm.Create(Self) do
  try
    ReportExplorer := self.Explorer;
    Item := AItem;
    Caption := AItem.ReportName +' Properties';
    edFileName.Text := AItem.ReportName;
    edDesc.Text := AItem.Description;
    edLocation.Text := AItem.Path;
    if AItem is TxfrReportItem then
      edRptID.Text := IntToStr(TxfrReportItem(AItem).ReportID)
    else begin
      edRptID.Enabled := False;
      // edRptID.Color   := clBtnFace;
    end;
    if (ftReadOnly in AItem.ReportAttr) then
    begin
      edFileName.Properties.ReadOnly := True;
      edRptID.Properties.ReadOnly    := True;
      edDesc.Properties.ReadOnly     := True;
    end;
    if (AItem.ItemType = itmReport) then
      edSize.Text := TxfrReportItem(AItem).SizeStr
    else
      edSize.Text := '-';
    edType.Text := AItem.TypeStr;
    edDate.Text := AItem.TimestampStr;
    edTag.Text  := IntToStr(AItem.Tag);
    ckReadonly.Checked := (ftReadOnly in AItem.ReportAttr);
    ckHidden.Checked := (ftHidden in AItem.ReportAttr);
    ckSystem.Checked := (ftSystem in AItem.ReportAttr);
    if (AItem.ItemType in [itmRoot, itmRecycleDir]) then
    begin
      ckReadonly.Enabled := False;
      ckHidden.Enabled := False;
      ckSystem.Enabled := False;
      edFileName.Properties.ReadOnly := True;
      edRptID.Properties.ReadOnly := True;
    end;
    xfrExplorerResources.MainButtonImages.GetBitmap(Integer(AItem.ItemType)+6,
      Image1.Picture.Bitmap);
    Image1.Transparent := True;
    Image1.AutoSize := True;
    if ShowModal = mrOk then
    begin
      if AItem is TxfrReportItem then
        TxfrReportItem(AItem).ReportID := StrToInt(edRptID.Text);
      AItem.ReportName := edFileName.Text;
      AItem.Description := edDesc.Text;
      if ckReadonly.Checked then
        AItem.ReportAttr := AItem.ReportAttr + [ftReadOnly]
      else
        AItem.ReportAttr := AItem.ReportAttr - [ftReadOnly];
      if ckHidden.Checked then
        AItem.ReportAttr := AItem.ReportAttr + [ftHidden]
      else
        AItem.ReportAttr := AItem.ReportAttr - [ftHidden];
      if ckSystem.Checked then
        AItem.ReportAttr := AItem.ReportAttr + [ftSystem]
      else
        AItem.ReportAttr := AItem.ReportAttr - [ftSystem];
      AItem.Tag := StrToInt(edTag.Text);
      UpdateItems;
    end;
  finally
    Free;
  end;
end;

procedure Tecfrptexplorer.actShowOrigFileExecute(Sender: TObject);
var
  AItem: TxfrAbstractItem;
  AStream: TMemoryStream;
begin
  // SecChk.AssertCheck(SECSTR_EL_ADMRPT);
  if (RLV.Selected <> nil) and (RLV.Selected.Data <> nil) then
  begin
    AItem := TxfrAbstractItem(RLV.Selected.Data);
    case AItem.ItemType of
      itmReport:
        begin
          if TxfrReportItem(AItem).Size > 0 then
          begin
            AStream := TMemoryStream.Create;
            try
              TxfrReportItem(AItem).SaveToStream(AStream);
              AStream.Position := 0;
              with TxfrFileViewerForm.Create(Self) do
              try
                Edit1.Text := AItem.ReportName;
                CodeWindow.Lines.LoadFromStream(AStream);
                if Execute then
                begin
                  AStream.Clear;
                  CodeWindow.Lines.SaveToStream(AStream);
                  TxfrReportItem(AItem).LoadFromStream(AStream);
                end;
              finally
                Free;
              end;
            finally
              AStream.Free;
            end;
          end;
        end;
    end;
    actRefreshExecute(Self);
  end;
end;

procedure Tecfrptexplorer.actRenameExecute(Sender: TObject);
begin
  if TreeFocused then
  begin
    if (SelectedFolderNode <> nil) and (SelectedFolderNode.Data <> nil) then
      SelectedFolderNode.EditText;
  end else
  if RLV.Focused then
    if (RLV.Selected <> nil) and (RLV.Selected.Data <> nil) then
      RLV.Selected.EditCaption;
end;

procedure Tecfrptexplorer.actCutExecute(Sender: TObject);
var
  i: integer;
begin
  FRptClipboard.Clear;
  if RLV.SelCount > 1 then
    for i := 0 to RLV.Items.Count-1 do
    begin
      if RLV.Items[i].Selected and (RLV.Items[i].Data <> nil) and
        (TxfrAbstractItem(RLV.Items[i].Data).ItemType in [itmDirectory, itmReport]) then
        FRptClipboard.AddObject('CUT',RLV.Items[i].Data)
    end
  else if (RLV.Selected <> nil) and (RLV.Selected.Data <> nil) and
    (TxfrAbstractItem(RLV.Selected.Data).ItemType in [itmDirectory, itmReport]) then
    FRptClipboard.AddObject('CUT', RLV.Selected.Data);
end;

procedure Tecfrptexplorer.actCopyExecute(Sender: TObject);
var
  i: integer;
begin
  FRptClipboard.Clear;
  if RLV.SelCount > 1 then
    for i := 0 to RLV.Items.Count-1 do
    begin
      if RLV.Items[i].Selected and (RLV.Items[i].Data <> nil) and
        (TxfrAbstractItem(RLV.Items[i].Data).ItemType in [itmDirectory, itmReport]) then
        FRptClipboard.AddObject('COPY',RLV.Items[i].Data)
    end
  else if (RLV.Selected <> nil) and (RLV.Selected.Data <> nil) and
    (TxfrAbstractItem(RLV.Selected.Data).ItemType in [itmDirectory, itmReport]) then
    FRptClipboard.AddObject('COPY', RLV.Selected.Data)
  else if (SelectedFolderNode <> nil) and (SelectedFolderNode.Data <> nil) and
    (TxfrAbstractItem(SelectedFolderNode.Data).ItemType in [itmDirectory, itmReport]) then
    FRptClipboard.AddObject('COPY', SelectedFolderNode.Data);
end;

procedure Tecfrptexplorer.actPasteExecute(Sender: TObject);
var
  i: integer;
  AItemDst, AItemSrc: TxfrAbstractItem;
  ANewItem: TxfrAbstractItem;
  AOper: integer;
begin
  AOper := 0;
  AItemDst := nil;
  if (RLV.Focused) and (RLV.Selected <> nil) and
          (RLV.Selected.Data <> nil) and
          (TxfrAbstractItem(RLV.Selected.Data).ItemType in
            [itmRoot, itmDirectory, itmRecycleDir]) then
    AItemDst := TxfrAbstractItem(RLV.Selected.Data)
  else if (SelectedFolderNode <> nil) and (SelectedFolderNode.Data <> nil) then
    AItemDst := TxfrAbstractItem(SelectedFolderNode.Data);
  if AItemDst = nil then
    exit;
  for i := 0 to FRptClipboard.Count-1 do
  begin
    AItemSrc := TxfrAbstractItem(FRptClipboard.Objects[i]);
    if not SameText(FRptClipboard[i], 'COPY') then AOper := 1;
    if (AItemSrc <> nil) and (AItemSrc.ItemType in [itmDirectory, itmReport]) then
    begin
      case AItemDst.ItemType of
        itmRoot, itmDirectory:
          begin
            ANewItem := nil;
            if AOper = 0 then
            begin
              case AItemSrc.ItemType of
                itmDirectory: ANewItem := TxfrDirectoryItem.Create(AItemDst);
                itmReport: ANewItem := TxfrReportItem.Create(AItemDst);
              end;
              if ANewItem <> nil then
                ANewItem.Assign(AItemSrc);
            end else
              TxfrHackedItem(AItemSrc).MoveTo(TxfrDirectoryItem(AItemDst));
          end;
      end;
    end;
  end;
  if AOper <> 0 then
    FRptClipboard.Clear;
  actRefreshExecute(Self);
end;

procedure Tecfrptexplorer.actNewFolderExecute(Sender: TObject);
var
  AItem: TxfrDirectoryItem;
  ANode: TTreeNode;
begin
  if (SelectedFolderNode <> nil) and (SelectedFolderNode.Data <> nil) and
     (TxfrAbstractItem(SelectedFolderNode.Data).ItemType in [itmRoot, itmDirectory]) then
  begin
    AItem := TxfrDirectoryItem.Create(TxfrDirectoryItem(SelectedFolderNode.Data));
    ANode := FTV.Items.AddChildObject(
      SelectedFolderNode,
      AItem.ReportName, AItem);
    ANode.ImageIndex := Integer(AItem.ItemType) + 6;
    ANode.SelectedIndex := Integer(AItem.ItemType) + 6;
    AItem.LinkedNode := ANode;
    actRefreshExecute(Self);
  end;
end;

procedure Tecfrptexplorer.actNewReportExecute(Sender: TObject);
begin
  if (SelectedFolderNode <> nil) and (SelectedFolderNode.Data <> nil) and
     (TxfrAbstractItem(SelectedFolderNode.Data).ItemType <> itmRecycleDir) then
  begin
    TxfrReportItem.Create(
      TxfrDirectoryItem(SelectedFolderNode.Data)
    );
    actRefreshExecute(Self);
  end;
end;

procedure Tecfrptexplorer.actOpenExecute(Sender: TObject);
var
  AItem: TxfrAbstractItem;
begin
  if (RLV.Selected <> nil) and (RLV.Selected.Data <> nil) then
  begin
    AItem := TxfrAbstractItem(RLV.Selected.Data);
    case AItem.ItemType of
      itmRoot, itmDirectory, itmRecycleDir:
        begin
          if (AItem.LinkedNode <> nil) then
          begin
            FTV.Selected := TTreeNode(AItem.LinkedNode);
            UpdateItems;
          end;
        end;
      itmReport:
        begin
          // SecChk.AssertCheck(SECSTR_EL_ADMRPT);
          TxfrHackedExplorer(Explorer).DesignReport(TxfrReportItem(AItem));
        end;
    end;
    actRefreshExecute(Self);
  end;
end;

procedure Tecfrptexplorer.actPrintExecute(Sender: TObject);
var
  AItem: TxfrAbstractItem;
begin
  if (RLV.Selected <> nil) and (RLV.Selected.Data <> nil) then
  begin
    AItem := TxfrAbstractItem(RLV.Selected.Data);
    case AItem.ItemType of
      itmReport: TxfrHackedExplorer(Explorer).PreviewReport(TxfrReportItem(AItem));
    end;
    UpdateItems;
  end;
end;

procedure Tecfrptexplorer.actDeleteExecute(Sender: TObject);
var
  i: integer;
begin
  if TreeFocused then
  begin
    if (SelectedFolderNode <> nil) and (SelectedFolderNode.Data <> nil) and
       (MessageDlg('Are you sure you want to send this item to the Recycle Bin?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      case TxfrAbstractItem(SelectedFolderNode.Data).ItemType of
        itmDirectory:
          begin
            if TxfrAbstractItem(SelectedFolderNode.Data).Deleted then
              TxfrAbstractItem(SelectedFolderNode.Data).Free
            else
            begin
              TxfrAbstractItem(SelectedFolderNode.Data).MoveTo(
                  Explorer.Root.RecycleDir
                );
              FTV.Items.Delete(SelectedFolderNode);
              FTV.Selected :=
                FTV.Items.GetFirstNode;
            end;
            actRefreshExecute(Self);
          end;
        itmReport:
          begin
            if TxfrAbstractItem(SelectedFolderNode.Data).Deleted then
              TxfrAbstractItem(SelectedFolderNode.Data).Free
            else
              TxfrAbstractItem(SelectedFolderNode.Data).MoveTo(
                Explorer.Root.RecycleDir);
          end;
      end;
    end;
  end else
  if RLV.Focused then
  begin
    if (RLV.SelCount > 0) and
       (MessageDlg(
          Format('Are you sure you want to send these %d items to the Recycle Bin?',
            [RLV.SelCount]),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      for i := 0 to RLV.Items.Count-1 do
      begin
        if RLV.Items[i].Selected and (RLV.Items[i].Data <> nil) then
        case TxfrAbstractItem(RLV.Items[i].Data).ItemType of
        itmDirectory, itmReport:
          begin
            if TxfrAbstractItem(RLV.Items[i].Data).Deleted then
              TxfrAbstractItem(RLV.Items[i].Data).Free
            else
              TxfrAbstractItem(RLV.Items[i].Data).MoveTo(
                Explorer.Root.RecycleDir)
          end;
        end;
      end;
      actRefreshExecute(Self);
    end;
  end;
end;

procedure Tecfrptexplorer.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure Tecfrptexplorer.actImportExecute(Sender: TObject);
var
  i: integer;
  AName: string;
  AStream: TMemoryStream;
  AParent: TxfrAbstractItem;
begin
  // SecChk.AssertCheck(SECSTR_EL_ADMRPT);
  if (SelectedFolderNode = nil) or (SelectedFolderNode.Data = nil) then
    exit;
  AParent := TxfrAbstractItem(SelectedFolderNode.Data);
  if (AParent.ItemType in [itmRecycleDir, itmReport]) then
    exit;
  if frxDesignerComp <> nil then
    OpenDialog.InitialDir := frxDesignerComp.SaveDir;
  OpenDialog.Filter :=  frxResources.Get('dsRepFilter');
  if frxCompressorClass <> nil then
    OpenDialog.Filter := OpenDialog.Filter + '|' + frxResources.Get('dsComprRepFilter');
  OpenDialog.Options := OpenDialog.Options + [ofAllowMultiSelect];
  if OpenDialog.Execute then
  begin
    AStream := TMemoryStream.Create;
    try
      for i := 0 to OpenDialog.Files.Count-1 do
      begin
        if (OpenDialog.Files[i] <> EmptyStr) and
            FileExists(OpenDialog.Files[i]) then
        begin
          AStream.Clear;
          AStream.LoadFromFile(OpenDialog.Files[i]);
          AName := ExtractFileName(OpenDialog.Files[i]);
          AStream.Position := 0;
          with TxfrReportItem.Create(TxfrDirectoryItem(AParent)) do
          begin
            ReportName := AName;
            LoadFromStream(AStream);
          end;
        end;
      end;
    finally
      AStream.Free;
    end;
    actRefreshExecute(Self);
  end;
end;

procedure Tecfrptexplorer.actExportExecute(Sender: TObject);
var
  i: integer;
  ADir: string;
  AStream: TMemoryStream;
  AItem: TxfrReportItem;
begin
  // SecChk.AssertCheck(SECSTR_EL_ADMRPT);
  if RLV.SelCount > 0 then
  begin
    if frxDesignerComp <> nil then
      ADir := frxDesignerComp.OpenDir;
    if BrowseDirectory(ADir) then
    begin
      AStream := TMemoryStream.Create;
      try
        for i := 0 to RLV.Items.Count-1 do
        begin
          if (RLV.Items[i].Selected) and (RLV.Items[i].Data <> nil) and
             (TxfrAbstractItem(RLV.Items[i].Data).ItemType = itmReport) then
          begin
            AItem := TxfrReportItem(RLV.Items[i].Data);
            AStream.Clear;
            AItem.SaveToStream(AStream);
            AStream.SaveToFile(ADir + '\'+ AItem.ReportName + '.fr3');
          end;
        end;
      finally
        AStream.Free;
      end;
      actRefreshExecute(Self);
    end;
  end;
end;

procedure Tecfrptexplorer.actAboutExecute(Sender: TObject);
begin
  with TxfrAboutForm.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure Tecfrptexplorer.actViewListExecute(Sender: TObject);
begin
  RLV.ViewStyle := vsList;
end;

procedure Tecfrptexplorer.actViewDetailsExecute(Sender: TObject);
begin
  RLV.ViewStyle := vsReport;
end;

procedure Tecfrptexplorer.actUpLevelExecute(Sender: TObject);
begin
  if (FTV.Selected <> nil) and (FTV.Selected.Parent <> nil) then
    FTV.Selected := FTV.Selected.Parent;
end;

procedure Tecfrptexplorer.actRefreshExecute(Sender: TObject);
begin
  AppCommand := CMD_UPDATESTATE;
end;

procedure Tecfrptexplorer.actFolderTreeExecute(Sender: TObject);
begin
  if actFolderTree.Checked <> itmViewFolderTree.Down then
    actFolderTree.Checked := itmViewFolderTree.Down;
  FolderTreeVisible := actFolderTree.Checked;
end;

procedure Tecfrptexplorer.InitExplorer;
var
  MainImages, DisabledImages: TImageList;
begin
  Explorer.OnValidateReport := InternalValidateReport;
  Explorer.OnSaveData := CheckOnSave;
  MainImages := xfrExplorerResources.MainButtonImages;
  DisabledImages := xfrExplorerResources.DisabledButtonImages;
  RLV.SmallImages := MainImages;
  FTV.Images := MainImages;
  ReadSettings;
  AppCommand := CMD_UPDATESTATE;
end;

procedure Tecfrptexplorer.Lock;
begin
end;

procedure Tecfrptexplorer.UpdateFolderTree;
var
  ASelectedNode: TTreeNode;
  s, Collapsed: String;

  procedure AddSubNode(AParent: TTreeNode; AItem: TxfrDirectoryItem);
  var
    i: integer;
    ANode: TTreeNode;
    AbsItem: TxfrAbstractItem;
  begin
    if (ftHidden in AItem.ReportAttr) and
       (ShowHiddenObjects <> True) then
      exit;
    if AParent = nil then
      ANode := FTV.Items.AddObject(nil, AItem.ReportName, AItem)
    else
      ANode := FTV.Items.AddChildObject(AParent, AItem.ReportName, AItem);
    ANode.ImageIndex := Integer(AItem.ItemType) + 6;
    ANode.SelectedIndex := Integer(AItem.ItemType) + 6;
    if (FOldDirItem <> nil) and (FOldDirItem = AItem) then
      ASelectedNode := ANode;
    AItem.LinkedNode := ANode;
    if (AItem.ComponentCount > 0) then
    for i := 0 to AItem.ComponentCount-1 do
    begin
      AbsItem := TxfrAbstractItem(AItem.Components[i]);
      if (
{
            (
              SecChk.SecurityCheck(SECSTR_EL_ADMSYS) or
              SecChk.SecurityCheck(SECSTR_EL_ADMRPT)
             ) or
}
             (AbsItem.Tag = 0) or
             (
              (AbsItem.Tag <> 0)
{
              and (SecChk.SecurityCheck(AbsItem.Tag))
}
             ))
             and
            ((not(ftHidden in AbsItem.ReportAttr)) or
             (ShowHiddenObjects = True)) then
      case AbsItem.ItemType of
        itmRoot,
        itmDirectory,
        itmRecycleDir: AddSubNode(ANode, TxfrDirectoryItem(AItem.Components[i]));
      end;
    end
  end;

var
  i: integer;
begin
  ASelectedNode := nil;
  if FFirstTime then
    Collapsed := CollapsedNodes
  else
    Collapsed := GetCollapsedNodes;
  if (FTV.Selected <> nil) and (FTV.Selected.Data <> nil) then
    FOldDirItem := TxfrDirectoryItem(FTV.Selected.Data);
  FTV.Items.BeginUpdate;
  try
    FTV.Items.Clear;
    if Explorer <> nil then
      AddSubNode(nil, Explorer.Root);
    FTV.Items[0].Expanded := True;
    for i := 0 to FTV.Items[0].Count - 1 do
    begin
      s := FTV.Items[0][i].Text;
      if Pos(s + ',', Collapsed) = 0 then
        FTV.Items[0][i].Expanded := True;
    end;
    if ASelectedNode <> nil then
      FTV.Selected := ASelectedNode
    else
      FTV.Selected := FTV.Items.GetFirstNode;
  finally
    FTV.Items.EndUpdate;
  end;
end;

procedure Tecfrptexplorer.UpdateItems;
begin
  UpdateFolderTree;
  UpdateReportList;
end;

procedure Tecfrptexplorer.UpdateReportList;
var
  i: integer;
  ARoot: TxfrDirectoryItem;
  AListItem: TListItem;
  AItem: TxfrAbstractItem;
begin
  if SelectedFolderNode <> nil then
  begin
    RLV.Items.BeginUpdate;
    try
      RLV.Clear;
      ARoot := TxfrDirectoryItem(SelectedFolderNode.Data);
      if ARoot <> nil then
      begin
        ContentPanel.Caption := 'Content of '+QuotedStr(ARoot.ReportName);
        for i := 0 to ARoot.ComponentCount-1 do
        begin
          AItem := TxfrAbstractItem(ARoot.Components[i]);
          if (
{
            (
              SecChk.SecurityCheck(SECSTR_EL_ADMSYS) or
              SecChk.SecurityCheck(SECSTR_EL_ADMRPT)
             ) or
}
             (AItem.Tag = 0) or
             ((AItem.Tag <> 0)
//             and (SecChk.SecurityCheck(AItem.Tag))
              ))
             and
            ((not(ftHidden in TxfrAbstractItem(ARoot.Components[i]).ReportAttr)) or
             (ShowHiddenObjects = True)) then
          begin
            AListItem := RLV.Items.Add;
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
      RLV.Items.EndUpdate;
    end;
  end;
end;

constructor Tecfrptexplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ConstructResources;
  FRptClipboard := TStringList.Create;
  FTemplatePath := GetAppIDClass.ApplicationReportPath + '\templates';
  if frxDesignerComp <> nil then
    frxDesignerComp.TemplateDir := FTemplatePath;
  FFirstTime := True;
  FEmbedded := False;
end;

destructor Tecfrptexplorer.Destroy;
begin
  FinalizeExplorer;
  FRptClipboard.Free;
  inherited Destroy;
end;

function Tecfrptexplorer.GetCollapsedNodes: String;
var
  i: Integer;
  s: String;
begin
  Result := '';
  if FTV.Items.Count > 0 then
    for i := 0 to FTV.Items[0].Count - 1 do
    begin
      s := FTV.Items[0][i].Text;
      if not FTV.Items[0][i].Expanded then
        Result := Result + s + ',';
    end;
end;

function Tecfrptexplorer.GetFocused: boolean;
begin
  Result := FTV.Focused;
end;

function Tecfrptexplorer.GetSelectedNode: TTreeNode;
begin
  Result := FTV.Selected;
end;


procedure Tecfrptexplorer.RLVChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (Change = ctState) and (Item.Data <> nil) then
  begin
    if TxfrAbstractItem(Item.Data).Description <> EmptyStr then
      dxStatusBar.Panels[1].Text := TxfrAbstractItem(Item.Data).Description
    else
      dxStatusBar.Panels[1].Text := TxfrAbstractItem(Item.Data).ReportName;
  end;
end;

procedure Tecfrptexplorer.RLVEdited(Sender: TObject; Item: TListItem;
  var S: String);
begin
  if (Item.Data <> nil) and
     (not(ftReadOnly in TxfrAbstractItem(Item.Data).ReportAttr)) and
     (TxfrAbstractItem(Item.Data).ReportName <> S) then
  begin
    TxfrAbstractItem(Item.Data).ReportName := S;
    TxfrHackedItem(Item.Data).SaveMe;
    if (TxfrAbstractItem(Item.Data).LinkedNode <> nil) then
    begin
      TTreeNode(TxfrAbstractItem(Item.Data).LinkedNode).Text :=
        TxfrAbstractItem(Item.Data).ReportName;
    end;
    S := TxfrAbstractItem(Item.Data).ReportName;
  end;
end;

procedure Tecfrptexplorer.RLVEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  if (Item.Data <> nil) and
     (ftReadOnly in TxfrAbstractItem(Item.Data).ReportAttr) then
    AllowEdit := False;
end;

procedure Tecfrptexplorer.RLVKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DELETE: actDeleteExecute(Self);
    VK_RETURN: RLVDblClick(Self);
  end;
end;

procedure Tecfrptexplorer.RLVEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target <> nil then
    RLV.Update;
end;

procedure Tecfrptexplorer.RLVDblClick(Sender: TObject);
var
  AItem: TxfrAbstractItem;
begin
  if (RLV.Selected <> nil) and (RLV.Selected.Data <> nil) then
  begin
    AItem := TxfrAbstractItem(RLV.Selected.Data);
    case AItem.ItemType of
      itmRoot, itmDirectory, itmRecycleDir:
        begin
          if (AItem.LinkedNode <> nil) then
          begin
            FTV.Selected := TTreeNode(AItem.LinkedNode);
            if AItem.ItemType <> itmRecycleDir then
              Explorer.CurrentFolder := TxfrDirectoryItem(AItem);
            actRefreshExecute(Sender);
          end;
        end;
      itmReport:
        begin
          if TxfrReportItem(AItem).Size = 0 then
            actOpenExecute(Self)
          else
            actPrintExecute(Self);
        end;
    end;
  end;
end;

procedure Tecfrptexplorer.RLVDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  i : integer;
  ALV: TcxListView;
  AOldName: string;
  ANode: TListItem;
  AItem1, AItem2: TxfrAbstractItem;
begin
  if
     (Sender is TcxListView) and (Source is TcxDragControlObject) and
     (TcxDragControlObject(Source).Control is TcxListView) and
     (TcxListView(TcxDragControlObject(Source).Control).SelCount > 0) and
     (RLV.GetItemAt(X, Y) <> nil) and
     (RLV.GetItemAt(X, Y).Data <> nil) and
     (TxfrAbstractItem(RLV.GetItemAt(X,Y).Data).ItemType in
        [itmRoot, itmDirectory, itmRecycleDir])
    then
  begin
    ANode := RLV.GetItemAt(X, Y);
    AItem1:= TxfrAbstractItem(ANode.Data);
    ALV := TcxListView(TcxDragControlObject(Source).Control);
    try
      for i := 0 to ALV.Items.Count-1 do
      begin
        if ALV.Items[i].Selected then
        begin
          AItem2 := TxfrAbstractItem(ALV.Items[i].Data);
          if (AItem2 <> nil) and (AItem2.ItemType in [itmReport, itmDirectory]) then
          begin
            if (AItem2.ItemType = itmDirectory) and (AItem1 = AItem2) then
              raise Exception.Create('Cannot move: the destination folder is the same as the source folder');
            if (AItem2.ItemType = itmDirectory) and AItem1.FindParent(TxfrDirectoryItem(AItem2)) then
              raise Exception.Create('Cannot move the parent folder to its child folder');
            if (AItem1 is TxfrRecycleDir) then
              AItem2.MoveTo(AItem1)
            else if (AItem1 is TxfrDirectoryItem) then
            begin
              AOldName := AItem2.ReportName;
              AItem2.MoveTo(AItem1);
              AItem2.ReportName := '$$TEMP$$';
              AItem2.ReportName := AOldName;
            end;
          end;
        end;
      end;
    finally
      actRefreshExecute(Self);
    end;
  end;
end;

procedure Tecfrptexplorer.RLVDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept :=
     (Sender is TcxListView) and (Source is TcxDragControlObject) and
     (TcxDragControlObject(Source).Control is TcxListView) and
     (TcxListView(TcxDragControlObject(Source).Control).SelCount > 0) and
     (RLV.GetItemAt(X, Y) <> nil) and
     (RLV.GetItemAt(X, Y).Data <> nil) and
     (TxfrAbstractItem(RLV.GetItemAt(X, Y).Data).ItemType in [
      itmRoot, itmDirectory, itmRecycleDir
     ]);
end;

procedure Tecfrptexplorer.FTVChange(Sender: TObject; Node: TTreeNode);
var
  AItem: TxfrAbstractItem;
begin
  if (Node.Data = nil) then
    exit;
  AItem := TxfrAbstractItem(Node.Data);
  if FTV.Selected <> Node then
    FTV.Selected := Node;
  if (AItem.ItemType in [itmRoot, itmDirectory]) then
    Explorer.CurrentFolder := TxfrDirectoryItem(AItem);
  UpdateReportList;
  if (Node.Data <> nil) then
  begin
    with dxStatusBar do
    begin
      if TxfrAbstractItem(Node.Data).Description <> EmptyStr then
        Panels[1].Text := TxfrAbstractItem(Node.Data).Description
      else
        Panels[1].Text := TxfrAbstractItem(Node.Data).ReportName;
      Panels[2].Text := TxfrAbstractItem(Node.Data).Path;
    end;
  end;
end;

procedure Tecfrptexplorer.FTVCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.Count <> 0 then
    Sender.Canvas.Font.Style := [fsBold];
end;

procedure Tecfrptexplorer.FTVDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  i : integer;
  AOldName: string;
  ALV: TcxListView;
  ANode: TTreeNode;
  AItem1, AItem2: TxfrAbstractItem;
begin
  if (Sender is TcxTreeView) and (Source is TcxDragControlObject) and
     (TcxDragControlObject(Source).Control is TcxListView) and
     (TcxListView(TcxDragControlObject(Source).Control).SelCount > 0) and
     (FTV.GetNodeAt(X, Y) <> nil) and
     (FTV.GetNodeAt(X, Y).Data <> nil) and
     (TxfrAbstractItem(FTV.GetNodeAt(X, Y).Data).ItemType in [itmRoot, itmDirectory, itmRecycleDir])
  then
  begin
    ANode := FTV.GetNodeAt(X, Y);
    AItem1:= TxfrAbstractItem(ANode.Data);
    ALV := TcxListView(TcxDragControlObject(Source).Control);
    try
      for i := 0 to ALV.Items.Count-1 do
      begin
        if ALV.Items[i].Selected then
        begin
          AItem2 := TxfrAbstractItem(ALV.Items[i].Data);
          if (AItem2 <> nil) and (AItem2.ItemType in [itmReport, itmDirectory]) then
          begin
            if (AItem2.ItemType = itmDirectory) and (AItem1 = AItem2) then
              raise Exception.Create('Cannot move: the destination folder is the same as the source folder');
            if (AItem2.ItemType = itmDirectory) and AItem1.FindParent(TxfrDirectoryItem(AItem2)) then
              raise Exception.Create('Cannot move the parent folder to its child folder');
            if (AItem1 is TxfrRecycleDir) then
              AItem2.MoveTo(AItem1)
            else if (AItem1 is TxfrDirectoryItem) then
            begin
              AOldName := TxfrAbstractItem(AItem2).ReportName;
              AItem2.MoveTo(TxfrDirectoryItem(AItem1));
              AItem2.ReportName := '$$TEMP$$';
              AItem2.ReportName := AOldName;
            end;
          end;
        end;
      end;
    finally
      UpdateItems;
    end;
  end;
end;


procedure Tecfrptexplorer.FTVDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept :=
            (Sender is TcxTreeView) and (Source is TcxDragControlObject) and
            (TDragControlObject(Source).Control is TcxListView) and
            (TcxListView(TDragControlObject(Source).Control).Selected.Data <> nil) and
            (FTV.GetNodeAt(X, Y) <> nil) and
            (FTV.GetNodeAt(X, Y).Data <> nil) and
            (TxfrAbstractItem(FTV.GetNodeAt(X, Y).Data).ItemType in
              [itmRoot, itmDirectory, itmRecycleDir]);
end;

procedure Tecfrptexplorer.FTVEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
begin
  if S <> Node.Text then
    Node.Text := S;
  if (Node.Data <> nil) and
     (not(ftReadOnly in TxfrAbstractItem(Node.Data).ReportAttr)) and
     (TxfrAbstractItem(Node.Data).ReportName <> Node.Text) then
  begin
    TxfrAbstractItem(Node.Data).ReportName := Node.Text;
    TxfrHackedItem(Node.Data).SaveMe;
    Node.Text := TxfrAbstractItem(Node.Data).ReportName;
    S := TxfrAbstractItem(Node.Data).ReportName;
  end;
end;

procedure Tecfrptexplorer.FTVEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  if (Node.Data <> nil) and
     (ftReadOnly in TxfrAbstractItem(Node.Data).ReportAttr) then
    AllowEdit := False;
end;

procedure Tecfrptexplorer.itmShowHiddenClick(Sender: TObject);
begin
  ShowHiddenObjects := itmShowHidden.Down;
end;

procedure Tecfrptexplorer.EnlargeWindow;
begin
  left  := 0;
  top   := 0;
  width := GetSystemMetrics(SM_CXMAXIMIZED) - 4;
  height:= GetSystemMetrics(SM_CYMAXIMIZED) - 4;
end;

type
  TdxBarManagerHack = class(TdxBarManager);

procedure Tecfrptexplorer.ReadSettings;
var
  bOldPrompt: boolean;
  ARegIniXML: TRegXMLIniFile;
begin
  ARegIniXML := TRegXMLIniFile.Create(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile,
    GetAppIDClass.RegistryPath+ '\ReportExplorer\MenuBar');
  try
    TdxBarManagerHack(BarManager).LoadBarManager(ARegIniXML, ARegIniXML.BasePath, skReg);
  finally
    ARegIniXML.Free;
  end;
  with TRegXML.Create(Self) do
  try
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath, True) then
    begin
      if not ValueExists('FolderTreeVisible') then
        WriteBool('FolderTreeVisible',FolderTreeVisible)
      else
        FolderTreeVisible := ReadBool('FolderTreeVisible');
      if not ValueExists('WindowTop') then
        WriteInteger('WindowTop',Top)
      else
        Top := ReadInteger('WindowTop');
      if not ValueExists('WindowLeft') then
        WriteInteger('WindowLeft',Left)
      else
        Left := ReadInteger('WindowLeft');
      if not ValueExists('WindowHeight') then
        WriteInteger('WindowHeight',Height)
      else
      begin
        Height := ReadInteger('WindowHeight');
        if Height = 0 then
          Height := GetSystemMetrics(SM_CYMAXIMIZED) - 4;
      end;
      if not ValueExists('WindowWidth') then
        WriteInteger('WindowWidth',Width)
      else
      begin
        Width := ReadInteger('WindowWidth');
        if Width = 0 then
          Width := GetSystemMetrics(SM_CXMAXIMIZED) - 4;
      end;
      if not ValueExists('AutoUpdate') then
      begin
        WriteBool('AutoUpdate',False);
      end
    end;
  finally
    Free;
  end;
end;

procedure Tecfrptexplorer.WriteSettings;
var
  ARegIniXML: TRegXMLIniFile;
begin
  ARegIniXML := TRegXMLIniFile.Create(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile,
    GetAppIDClass.RegistryPath+ '\ReportExplorer\MenuBar');
  try
    TdxBarManagerHack(BarManager).SaveBarManager(ARegIniXML, ARegIniXML.BasePath, skReg); 
    ARegIniXML.RegIniFile.Close;
  finally
    ARegIniXML.Free;
  end;
  with TRegXML.Create(Self) do
  try
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath, True) then
    begin
      WriteBool('FolderTreeVisible',actFolderTree.Checked);
      WriteInteger('WindowTop',Top);
      WriteInteger('WindowLeft',Left);
      WriteInteger('WindowHeight',Height);
      WriteInteger('WindowWidth',Width);
    end;
    Close;
  finally
    Free;
  end;
end;

procedure Tecfrptexplorer.FinalizeExplorer;
begin
  WriteSettings;
  Explorer.OnValidateReport := nil;
  Explorer.OnSaveData := nil;
end;

function Tecfrptexplorer.GetFolderTreeVisibility: boolean;
begin
  Result := FolderTreePanel.Visible;
end;

procedure Tecfrptexplorer.SetFolderTreeVisibility(const Value: boolean);
begin
  if Value <> FolderTreePanel.Visible then
    FolderTreePanel.Visible := Value;
  if actFolderTree.Checked <> Value then
    actFolderTree.Checked := Value;
end;

procedure Tecfrptexplorer.WMAppCommand(var message: TMessage);
begin
  case message.WParam of
    CMD_UPDATESTATE: InternalUpdateState;
  end;
end;

procedure Tecfrptexplorer.InternalUpdateState;
var
  i: integer;
begin
  for i := 0 to ActionList1.ActionCount-1 do
  begin
    TAction(ActionList1.Actions[i]).Enabled := True;
  end;
  actExit.Enabled := True;
  actAbout.Enabled := True;
  actViewList.Enabled := True;
  actViewDetails.Enabled := True;
{
  if AConnected then
  begin
    if SecChk.SecurityCheck(SECSTR_EL_ADMSYS) or
       SecChk.SecurityCheck(SECSTR_EL_ADMRPT) then
}
    AdmOptionList.Visible := ivAlways;
    // actSecureRpt.Visible := SecChk.SecurityCheck(SECSTR_EL_ADMRPT);
    // actSaveRpts.Visible := SecChk.SecurityCheck(SECSTR_EL_ADMRPT);
    actSecureRpt.Visible := True;
    actSaveRpts.Visible := True;
{
  end else
  begin
    AdmOptionList.Visible := ivNever;
    actSecureRpt.Visible := False;
    actSaveRpts.Visible := False;
  end;
}
  UpdateItems;
end;

procedure Tecfrptexplorer.SetAppCommand(const Value: integer);
begin
  SendMessage(Handle, WM_APPCMD, Value, 0);
end;

procedure Tecfrptexplorer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FModalFinished := True;
end;

procedure Tecfrptexplorer.InternalValidateReport(AReport: TxfrReportItem;
  const AEvent: TxfrExplorerEventTypes);
begin
  case AEvent of
    etDesign, etTagAuth: ; // SecChk.AssertCheck(SECSTR_EL_ADMRPT);
    etPreview:
      begin
        // if AReport.Tag > 0 then
          // SecChk.AssertCheck(AReport.Tag);
      end;
  end;
end;

procedure Tecfrptexplorer.actSecureRptExecute(Sender: TObject);
var
  ASecured: boolean;
begin
  // SecChk.AssertCheck(SECSTR_EL_ADMRPT);
  ASecured := Self.Explorer.SecureArchive;
  Self.Explorer.SecureArchive := True;
  try
    with SaveDialog do
    begin
      Filter := _('Report File (.rpt)|*.rpt');
      DefaultExt := '.rpt';
      Title := _('Save secured report file as');
      if Execute then
        Self.Explorer.SaveToFile(FileName);
    end;
  finally
  end;
end;

procedure Tecfrptexplorer.actSaveRptsExecute(Sender: TObject);
begin
  // SecChk.AssertCheck(SECSTR_EL_ADMRPT);
  TxfrHackedReportExplorer(Self.Explorer).SaveData;
  ShowMessage('Report file(s) has been successfully saved');
end;

procedure Tecfrptexplorer.CheckOnSave(Sender: TObject);
begin
  // SecChk.AssertCheck(SECSTR_EL_ADMRPT);
end;

procedure Tecfrptexplorer.ConstructResources;
begin
  actNewFolder.Caption := _('New folder');
  actNewReport.Caption := _('New report');
  actOpen.Caption := _('Open');
  actPrint.Caption := _('Print report');
  actDelete.Caption := _('Delete');
  actExit.Caption := _('Exit');
  actViewList.Caption := _('List');
  actViewDetails.Caption := _('Details');
  actUpLevel.Caption := _('Up one level');
  actAbout.Caption := _('About');
  actRename.Caption := _('Rename...');
  actRefresh.Caption := _('Refresh');
  actFolderTree.Caption := _('View folder tree');
  actEmptyRecycleBin.Caption := _('Empty recycle bin');
  actRestore.Caption := _('Restore');
  actProperties.Caption := _('Properties...');
  actShowOrigFile.Caption := _('Browse file...');
  actCut.Caption := _('Cut');
  actCopy.Caption := _('Copy');
  actPaste.Caption := _('Paste');
  actImport.Caption := _('Import file...');
  actExport.Caption := _('Export file...');
  actSecureRpt.Caption := _('Encrypt report file');
  actSaveRpts.Caption := _('Save report file');
  New1.Caption := _('New');
  AdmOptionList.Caption := _('Admin options');
  itmShowHidden.Caption := _('Show hidden files');
  FolderTreePanel.Caption := _('Folders');
  File1.Caption := _('File');
  dxBarSubItem1.Caption := _('Edit');
  View1.Caption := _('View');
  Help1.Caption := _('Help');
  ImportExport1.Caption := _('Import/Export');
end;

initialization
   frxExplorerClass := Tecfrptexplorer;

end.
