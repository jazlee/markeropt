unit frmBrowserU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ToolWin, ActnMan, ActnCtrls, cxClasses, dxBar,
  dxBarExtItems, cxControls, cxPC, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, cxGridLevel, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, dxStatusBar;


{$DEFINE COMPATIBLE}

type
  TBrowserField = class(TCollectionItem)
  private
    FFieldName: string;
    FCaption: string;
  public
    property FieldName: string read FFieldName write FFieldName;
    property Caption: string read FCaption write FCaption;
  end;

  TBrowserFields = class(TOwnedCollection)
  private
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;

    function AddFieldDef(const AField, ACaption: string): TBrowserField;
    procedure GetCaptionList(AList: TStrings);
  end;
  
  TfrmBrowser = class(TForm)
    ActionList: TActionList;
    actNew: TAction;
    actOpen: TAction;
    actDelete: TAction;
    actClose: TAction;
    BarManager: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    stdBtnNew: TdxBarLargeButton;
    stdBtnOpen: TdxBarLargeButton;
    stdBtnDelete: TdxBarLargeButton;
    stdBtnClose: TdxBarLargeButton;
    actFind: TAction;
    stdBtnFind: TdxBarLargeButton;
    cxTabControl1: TcxTabControl;
    cxPageControl1: TcxPageControl;
    cxTabSheet1: TcxTabSheet;
    BrowseGridDBTableView1: TcxGridDBTableView;
    BrowseGridLevel1: TcxGridLevel;
    BrowseGrid: TcxGrid;
    StatusBar: TdxStatusBar;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure BrowseGridDBTableView1DblClick(Sender: TObject);
    procedure BrowseGridDBTableView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actPrintExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    FFields: TBrowserFields;
    FForLookup: Boolean;
    FOnCreateNewRecord: TDataSetNotifyEvent;
    FOnDeleteRecord: TDataSetNotifyEvent;
    FOnEditRecord: TDataSetNotifyEvent;
    FOnPrintData: TDataSetNotifyEvent;

    function GridItemIndex(const AIndex: integer): integer;

    function GetDatasource: TDataSource;
    procedure SetDatasource(const Value: TDataSource);    
    procedure ConstructFieldView;

    function IsActive: boolean;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure ShowBrowser(AParent: TComponent;
      AFields, ACaptions, ATitle: string;
      ADatasource: TDataSource;
      AOnNewRecord, AOnEditRecord, AOnDeleteRecord, AOnPrint: TDataSetNotifyEvent); overload;

    class procedure ShowBrowserForLookup(AParent: TComponent;
      AFields, ACaptions, ATitle: string;
      ADatasource: TDataSource;
      AOnNewRecord, AOnEditRecord, AOnDeleteRecord, AOnPrint: TDataSetNotifyEvent); overload;

{$IFDEF COMPATIBLE}
    class procedure ShowBrowser(AParent: TComponent;
      AFields, ACaptions, ATitle: string;
      ADatasource: TDataSource;
      AOnNewRecord, AOnEditRecord, AOnDeleteRecord: TDataSetNotifyEvent); overload;

    class procedure ShowBrowserForLookup(AParent: TComponent;
      AFields, ACaptions, ATitle: string;
      ADatasource: TDataSource;
      AOnNewRecord, AOnEditRecord, AOnDeleteRecord: TDataSetNotifyEvent); overload;
{$ENDIF}

    procedure ExecuteBrowser;

    procedure SetFields(const AFields, ACaptions: string);

    property Datasource: TDataSource read GetDatasource write SetDatasource;
    property FieldList: TBrowserFields read FFields;

    property OnCreateNewRecord: TDataSetNotifyEvent
      read FOnCreateNewRecord write FOnCreateNewRecord;
    property OnEditRecord: TDataSetNotifyEvent
      read FOnEditRecord write FOnEditRecord;
    property OnDeleteRecord: TDataSetNotifyEvent
      read FOnDeleteRecord write FOnDeleteRecord;
    property OnPrintData: TDataSetNotifyEvent
      read FOnPrintData write FOnPrintData;
  end;

var
  frmBrowser: TfrmBrowser;

implementation

uses
  dmResU, dmMainU, ecfutils, gnugettext, frmFindU;

{$R *.dfm}

procedure TfrmBrowser.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmBrowser.actDeleteExecute(Sender: TObject);
var
  I: integer;
  ABookmark: string;
begin
  if IsActive and Assigned(FOnDeleteRecord)
    and (not Datasource.DataSet.IsEmpty) then
  begin
    if (BrowseGridDBTableView1.Controller.SelectedRecordCount > 1) then
    begin
      for I := 0 to BrowseGridDBTableView1.Controller.SelectedRecordCount-1 do
      begin
        ABookmark := BrowseGridDBTableView1.DataController.GetSelectedBookmark(I);
        if ABookmark <> EmptyStr then        
          Datasource.DataSet.Bookmark := ABookmark;
        OnDeleteRecord(Datasource.DataSet);
      end;
    end else
    begin    
      OnDeleteRecord(Datasource.DataSet)
    end;
  end;
end;

procedure TfrmBrowser.actFindExecute(Sender: TObject);
var
  AList: TStringList;
  AText: string;
  AIndex, AFieldIndex: integer;
begin
  AList := TStringList.Create;
  try
    FFields.GetCaptionList(AList);
    if TfrmFind.ShowFindDialog(Self, AText, AIndex, AList) then
    begin
      AFieldIndex := GridItemIndex(AIndex);
      BrowseGridDBTableView1.DataController.Search.Locate(AFieldIndex, AText);           
    end;    
  finally
    AList.Free;
  end;
end;

procedure TfrmBrowser.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actPrint.Enabled := Assigned(FOnPrintData);
  Handled := True;
end;

procedure TfrmBrowser.actNewExecute(Sender: TObject);
begin
  if IsActive and Assigned(FOnCreateNewRecord) then
    OnCreateNewRecord(Datasource.DataSet)
end;

procedure TfrmBrowser.actOpenExecute(Sender: TObject);
begin
  if IsActive and Assigned(FOnDeleteRecord)
    and (not Datasource.DataSet.IsEmpty) then
      OnEditRecord(Datasource.DataSet)
end;

procedure TfrmBrowser.actPrintExecute(Sender: TObject);
begin
  if IsActive and Assigned(FOnPrintData)
    and (not Datasource.DataSet.IsEmpty) then
      OnPrintData(Datasource.DataSet)
end;

procedure TfrmBrowser.BrowseGridDBTableView1DblClick(Sender: TObject);
begin
  if not FForLookup then
    actOpenExecute(Self)
  else
    actCloseExecute(Self);
end;

procedure TfrmBrowser.BrowseGridDBTableView1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      BrowseGridDBTableView1DblClick(Sender);
    VK_DELETE:
      actDeleteExecute(Sender);
  end;
end;

procedure TfrmBrowser.ConstructFieldView;
var
  i: integer;
  AColumn: TcxGridDBColumn;
  AField: TField;
  AFieldView: TBrowserField;
  ADataset: TDataSet;
begin
  if (Datasource = nil) or (Datasource.DataSet = nil) then
    exit;
  BrowseGridDBTableView1.ClearItems;
  ADataset := Datasource.DataSet;
  for I := 0 to FFields.Count - 1 do
  begin
    AFieldView := TBrowserField(FFields.Items[I]);
    AField := ADataset.FindField(AFieldView.FieldName);
    if AField <> nil then
    begin
      AColumn := BrowseGridDBTableView1.CreateColumn;
      AColumn.DataBinding.FieldName := AField.FieldName;
      AColumn.Caption := _(AFieldView.Caption);
      AColumn.Visible := True;
    end;
  end;
end;

constructor TfrmBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForLookup := False;
  FFields := TBrowserFields.Create(Self);
  BrowseGridDBTableView1.DataController.Options :=
      [
        dcoAnsiSort, dcoCaseInsensitive, dcoAssignGroupingValues,
        dcoAssignMasterDetailKeys, dcoSaveExpanding
      ];
  BrowseGridDBTableView1.OptionsBehavior.IncSearch := True;
  BrowseGridDBTableView1.OptionsData.Deleting := False;
  BrowseGridDBTableView1.OptionsData.Editing := False;
  BrowseGridDBTableView1.OptionsData.Inserting := False;
  BrowseGridDBTableView1.OptionsSelection.HideFocusRectOnExit := False;
  BrowseGridDBTableView1.OptionsSelection.InvertSelect := False;
  BrowseGridDBTableView1.OptionsSelection.MultiSelect := True;
  BrowseGridDBTableView1.OptionsView.ColumnAutoWidth := True;
  BrowseGridDBTableView1.OptionsView.GroupFooters := gfVisibleWhenExpanded;
  BrowseGridDBTableView1.OptionsView.Navigator := True;
  BrowseGridDBTableView1.NavigatorButtons.Insert.Enabled := False;
  BrowseGridDBTableView1.NavigatorButtons.Insert.Visible := False;
  BrowseGridDBTableView1.NavigatorButtons.Edit.Enabled := False;
  BrowseGridDBTableView1.NavigatorButtons.Edit.Visible := False;
  BrowseGridDBTableView1.NavigatorButtons.Append.Enabled := False;
  BrowseGridDBTableView1.NavigatorButtons.Append.Visible := False;
  BrowseGridDBTableView1.NavigatorButtons.Delete.Enabled := False;
  BrowseGridDBTableView1.NavigatorButtons.Delete.Visible := False;
  BrowseGridDBTableView1.NavigatorButtons.Post.Enabled := False;
  BrowseGridDBTableView1.NavigatorButtons.Post.Visible := False;
  BrowseGridDBTableView1.NavigatorButtons.Cancel.Enabled := False;
  BrowseGridDBTableView1.NavigatorButtons.Cancel.Visible := False;
  BrowseGridDBTableView1.DataController.Filter.Active := True;
  BrowseGridDBTableView1.DataController.DataModeController.GridMode := False;
  BrowseGridDBTableView1.OptionsCustomize.ColumnFiltering :=
      BrowseGridDBTableView1.DataController.Filter.Active;
  BrowseGridDBTableView1.OptionsCustomize.ColumnSorting :=
      BrowseGridDBTableView1.DataController.Filter.Active;
end;

destructor TfrmBrowser.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

procedure TfrmBrowser.ExecuteBrowser;
begin
  ShowModal;
end;

function TfrmBrowser.GetDatasource: TDataSource;
begin
  Result := nil;
  if BrowseGridDBTableView1.DataController <> nil then
    Result := BrowseGridDBTableView1.DataController.DataSource;
end;

function TfrmBrowser.GridItemIndex(const AIndex: integer): integer;
var
  AItem: TBrowserField;
  I: integer;
begin
  Result := -1;
  AItem := TBrowserField(FFields.Items[AIndex]);
  for I := 0 to BrowseGridDBTableView1.ItemCount - 1 do
    if SameText(AItem.FieldName,
          TcxGridDBColumn(
            BrowseGridDBTableView1.Items[i]).DataBinding.FieldName
        ) then
    begin
      Result := I;
      break;    
    end;     
end;

function TfrmBrowser.IsActive: boolean;
begin
  Result := (Datasource <> nil)
    and (Datasource.DataSet <> nil)
    and (Datasource.DataSet.Active);
end;

procedure TfrmBrowser.SetDatasource(const Value: TDataSource);
begin
  BrowseGridDBTableView1.DataController.DataSource := Value;
  if Value <> nil then
  begin
    ConstructFieldView;
  end;
end;

procedure TfrmBrowser.SetFields(const AFields, ACaptions: string);
var
  APos, BPos: integer;
  AField, ACaption: string;
begin
  APos := 1;
  BPos := 1;
  while APos <= Length(AFields) do
  begin
    AField := ExtractFieldName(AFields, APos);
    if BPos <= Length(ACaptions)  then
      ACaption := ExtractFieldName(ACaptions, BPos);
    FieldList.AddFieldDef(AField, ACaption);
  end;  
end;

class procedure TfrmBrowser.ShowBrowser(AParent: TComponent;
      AFields, ACaptions, ATitle: string;
      ADatasource: TDataSource;
      AOnNewRecord, AOnEditRecord, AOnDeleteRecord, AOnPrint: TDataSetNotifyEvent);
var
  ABrowser: TfrmBrowser;
begin
  ABrowser := TfrmBrowser.Create(AParent);
  try
    ABrowser.Caption := ATitle;
    ABrowser.SetFields(AFields, ACaptions);
    ABrowser.Datasource := ADatasource;
    ABrowser.OnCreateNewRecord := AOnNewRecord;
    ABrowser.OnEditRecord := AOnEditRecord;
    ABrowser.OnDeleteRecord := AOnDeleteRecord;
    ABrowser.OnPrintData := AOnPrint;
    ABrowser.ExecuteBrowser;
  finally
    ABrowser.Free;
  end;
end;

class procedure TfrmBrowser.ShowBrowserForLookup(AParent: TComponent; AFields,
  ACaptions, ATitle: string; ADatasource: TDataSource; AOnNewRecord,
  AOnEditRecord, AOnDeleteRecord, AOnPrint: TDataSetNotifyEvent);
var
  ABrowser: TfrmBrowser;
begin
  ABrowser := TfrmBrowser.Create(AParent);
  try
    ABrowser.FForLookup := True;
    ABrowser.Caption := ATitle;
    ABrowser.SetFields(AFields, ACaptions);
    ABrowser.Datasource := ADatasource;
    ABrowser.OnCreateNewRecord := AOnNewRecord;
    ABrowser.OnEditRecord := AOnEditRecord;
    ABrowser.OnDeleteRecord := AOnDeleteRecord;
    ABrowser.OnPrintData := AOnPrint;
    ABrowser.ExecuteBrowser;
  finally
    ABrowser.Free;
  end;
end;

{ TBrowserFields }

function TBrowserFields.AddFieldDef(const AField,
  ACaption: string): TBrowserField;
begin
  Result := TBrowserField(Add);
  Result.FieldName := AField;
  Result.Caption := ACaption;
end;

constructor TBrowserFields.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TBrowserField);
end;

destructor TBrowserFields.Destroy;
begin
  inherited;
end;

procedure TBrowserFields.GetCaptionList(AList: TStrings);
var
  I: integer;
begin
  AList.Clear;
  for I := 0 to Count - 1 do
    AList.Add(TBrowserField(Items[i]).Caption);
end;

{$IFDEF COMPATIBLE}
class procedure TfrmBrowser.ShowBrowser(AParent: TComponent; AFields, ACaptions,
  ATitle: string; ADatasource: TDataSource; AOnNewRecord, AOnEditRecord,
  AOnDeleteRecord: TDataSetNotifyEvent);
var
  ABrowser: TfrmBrowser;
begin
  ABrowser := TfrmBrowser.Create(AParent);
  try
    ABrowser.Caption := ATitle;
    ABrowser.SetFields(AFields, ACaptions);
    ABrowser.Datasource := ADatasource;
    ABrowser.OnCreateNewRecord := AOnNewRecord;
    ABrowser.OnEditRecord := AOnEditRecord;
    ABrowser.OnDeleteRecord := AOnDeleteRecord;
    ABrowser.OnPrintData := nil;
    ABrowser.ExecuteBrowser;
  finally
    ABrowser.Free;
  end;
end;

class procedure TfrmBrowser.ShowBrowserForLookup(AParent: TComponent; AFields,
  ACaptions, ATitle: string; ADatasource: TDataSource; AOnNewRecord,
  AOnEditRecord, AOnDeleteRecord: TDataSetNotifyEvent);
var
  ABrowser: TfrmBrowser;
begin
  ABrowser := TfrmBrowser.Create(AParent);
  try
    ABrowser.FForLookup := True;
    ABrowser.Caption := ATitle;
    ABrowser.SetFields(AFields, ACaptions);
    ABrowser.Datasource := ADatasource;
    ABrowser.OnCreateNewRecord := AOnNewRecord;
    ABrowser.OnEditRecord := AOnEditRecord;
    ABrowser.OnDeleteRecord := AOnDeleteRecord;
    ABrowser.OnPrintData := nil;
    ABrowser.ExecuteBrowser;
  finally
    ABrowser.Free;
  end;
end;
{$ENDIF}

end.
