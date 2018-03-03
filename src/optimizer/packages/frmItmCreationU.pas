unit frmItmCreationU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ToolWin, ActnMan, ActnCtrls, cxClasses, dxBar,
  dxBarExtItems, cxControls, cxPC, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, cxGridLevel, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, dxStatusBar,
  NativeRegXML, cxGridCustomPopupMenu, cxGridPopupMenu, Menus, dmPackageU;

type
  TItemDataSource = class(TcxCustomDataSource)
  private
    FItemRows: TItemRowCollection;
    FModified: boolean;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Modified: boolean read FModified;
    property ItemRows: TItemRowCollection read FItemRows;
  end;
    
  TfrmItmCreation = class(TForm)
    ActionList: TActionList;
    actClose: TAction;
    BarManager: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    stdBtnClose: TdxBarLargeButton;
    cxTabControl1: TcxTabControl;
    cxPageControl1: TcxPageControl;
    cxTabSheet1: TcxTabSheet;
    BrowseGridLevel1: TcxGridLevel;
    BrowseGrid: TcxGrid;
    StatusBar: TdxStatusBar;
    stdBtnCreate: TdxBarLargeButton;
    actCreate: TAction;
    BrowseGridTableView1: TcxGridTableView;
    cxGridPopupMenu1: TcxGridPopupMenu;
    PopupMenu1: TPopupMenu;
    Setallforcurrentrow1: TMenuItem;
    Setallforcurentcolumn1: TMenuItem;
    Setall1: TMenuItem;
    N1: TMenuItem;
    Unsetallforcurrentrow1: TMenuItem;
    Unsetallforcurrentcolumn1: TMenuItem;
    UnsetAll1: TMenuItem;
    N2: TMenuItem;
    CreateItems1: TMenuItem;
    N3: TMenuItem;
    Closewindow1: TMenuItem;
    procedure actCloseExecute(Sender: TObject);
    procedure UnsetAll1Click(Sender: TObject);
    procedure actCreateExecute(Sender: TObject);
    procedure UpdateStatus(Sender: TObject; const AText: string);
  private
    FXFeatures: TStringList;
    FYFeatures: TStringList;
    FValues: TStringList;
    FDatasource: TItemDataSource;

    procedure ConstructFields(ADataset: TDataSet);
    procedure ConstructGrid;
    procedure ConstructDatasource;
    procedure LoadData(AStream: TStream);
    procedure SaveData(AStream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure ShowItemGeneration(AParent: TComponent; ADataset: TDataSet);

    procedure ExecuteViewer;
  end;

var
  frmItmCreation: TfrmItmCreation;

implementation

uses
  dmResU, dmMainU, ecfutils, gnugettext;

{$R *.dfm}

procedure TfrmItmCreation.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmItmCreation.actCreateExecute(Sender: TObject);
begin
  dmPackage.CreateItems(FDatasource.ItemRows, UpdateStatus);
end;

procedure TfrmItmCreation.ConstructDatasource;
begin
  FDatasource.ItemRows.AssignFields(FXFeatures);
  FDatasource.ItemRows.SetColors(FYFeatures);
end;

procedure TfrmItmCreation.ConstructFields(ADataset: TDataSet);
var
  AVal, AFlag: integer;
  AFeatureDataset, AItemDataset: TDataSet;
  AList: TStringList;
begin
  AFeatureDataset := dmPackage.GetACRTable('stfeature', 'pk_stfeature');
  AItemDataset := dmPackage.GetACRTable('stitmfeatures', 'idx_stitmfeatures_1');
  try
    AFeatureDataset.Filter := Format('(stname = %s)',
        [
          QuotedStr(ADataset.FieldByName('stname').AsString)
        ]
      );
    AFeatureDataset.Filtered := True;
    AFeatureDataset.Open;
    AFlag := 0;
    while not AFeatureDataset.Eof do
    begin
      AVal := AFeatureDataset.FieldByName('ftgrp').AsInteger + 1;
      if (AFlag and AVal) <> AVal then
      begin
        AFlag := AFlag or AVal;
        AItemDataset.Filtered := False;
        AItemDataset.Active := False;
        AItemDataset.Filter := Format('(stname = %s) and (ftname = %s)',
            [
              QuotedStr(AFeatureDataset.FieldByName('stname').AsString),
              QuotedStr(AFeatureDataset.FieldByName('ftname').AsString)
            ]
          );
        AItemDataset.Filtered := True;
        AItemDataset.Active := True;
        if AVal = 1 then
          AList := FXFeatures
        else
          AList := FYFeatures;
        AList.Clear;
        while not AItemDataset.Eof do
        begin
          AList.Add(AItemDataset.FieldByName('fitname').AsString);
          AItemDataset.Next;
        end;
      end;
      AFeatureDataset.Next;
    end;
  finally
    AFeatureDataset.Free;
    AItemDataset.Free;
  end;  
end;

procedure TfrmItmCreation.ConstructGrid;
var
  i: integer;
begin
  BrowseGridTableView1.ClearItems;
  with BrowseGridTableView1.CreateColumn as TcxGridColumn do
  begin
    Caption := 'Color';
    DataBinding.ValueTypeClass := TcxStringValueType;
    Width := 200;
    Options.Editing := False;
  end;
  for i := 0 to FXFeatures.Count - 1 do
  begin
    with BrowseGridTableView1.CreateColumn as TcxGridColumn do
    begin
      Caption := FXFeatures[i];
      DataBinding.ValueTypeClass := TcxBooleanValueType;
      Width := 50;
    end;
  end;    
end;

constructor TfrmItmCreation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXFeatures := TStringList.Create;
  FYFeatures := TStringList.Create;
  FValues := TStringList.Create;
  FDatasource := TItemDataSource.Create;
  BrowseGridTableView1.DataController.Options :=
      [
        dcoAnsiSort, dcoCaseInsensitive, dcoAssignGroupingValues,
        dcoAssignMasterDetailKeys, dcoSaveExpanding
      ];
  BrowseGridTableView1.OptionsBehavior.IncSearch := True;
  BrowseGridTableView1.OptionsData.Deleting := False;
  BrowseGridTableView1.OptionsData.Editing := True;
  BrowseGridTableView1.OptionsData.Inserting := False;
  BrowseGridTableView1.OptionsSelection.HideFocusRectOnExit := False;
  BrowseGridTableView1.OptionsSelection.InvertSelect := False;
  BrowseGridTableView1.OptionsSelection.MultiSelect := False;
  BrowseGridTableView1.OptionsSelection.CellSelect := True;
  BrowseGridTableView1.OptionsSelection.CellMultiSelect := False;
  BrowseGridTableView1.OptionsView.ColumnAutoWidth := True;
  BrowseGridTableView1.OptionsView.GroupFooters := gfVisibleWhenExpanded;
  BrowseGridTableView1.OptionsView.Navigator := False;
  BrowseGridTableView1.DataController.Filter.Active := True;
  BrowseGridTableView1.OptionsCustomize.ColumnFiltering :=
      BrowseGridTableView1.DataController.Filter.Active;
  BrowseGridTableView1.OptionsCustomize.ColumnSorting :=
      BrowseGridTableView1.DataController.Filter.Active;
  BrowseGridTableView1.DataController.CustomDataSource := FDatasource;
end;

destructor TfrmItmCreation.Destroy;
begin
  FDatasource.Free;
  FXFeatures.Free;
  FYFeatures.Free;
  FValues.Free;
  inherited Destroy;
end;

procedure TfrmItmCreation.ExecuteViewer;
begin
  ShowModal;
end;

procedure TfrmItmCreation.LoadData(AStream: TStream);
var
  AItemXML: TRegXML;
  AItem: TItemRow;
  I, J: integer;
begin
  AItemXML := TRegXML.Create(Self);
  try
    try
      if AStream.Size > 0 then
        AItemXML.LoadFromStream(AStream);
    except; end;
    for i := 0 to FYFeatures.Count - 1 do
    begin
      if AItemXML.openKey(Format('%s',[FYFeatures[i]]), False) then
      begin
        for J := 0 to FXFeatures.Count - 1 do
        begin
          if AItemXML.valueExists(FXFeatures[j]) then
            FDatasource.ItemRows.Values[FYFeatures[i],FXFeatures[j]] :=
              BoolToStr(AItemXML.readBool(FXFeatures[j]), True);
        end;
        AItemXML.closeKey;
      end;
    end;
  finally
    AItemXML.Free;
  end;
end;

procedure TfrmItmCreation.SaveData(AStream: TStream);
var
  AItemXML: TRegXML;
  AItem: TItemRow;
  I, J: integer;
begin
  AItemXML := TRegXML.Create(Self);
  try
    for i := 0 to FYFeatures.Count - 1 do
    begin
      if AItemXML.openKey(Format('%s',[FYFeatures[i]]), True) then
      begin
        for J := 0 to FXFeatures.Count - 1 do
        begin
          AItemXML.writeBool(FXFeatures[j],
            StrToBool(FDatasource.ItemRows.Values[FYFeatures[i],FXFeatures[j]]));
        end;
        AItemXML.closeKey;
      end;
    end;
    AItemXML.SaveToStream(AStream);
  finally
    AItemXML.Free;
  end;
end;

class procedure TfrmItmCreation.ShowItemGeneration(AParent: TComponent;
  ADataset: TDataSet);
var
  AMemoryStream: TMemoryStream;
  AForm: TfrmItmCreation;
begin
  AMemoryStream := TMemoryStream.Create;
  AForm := TfrmItmCreation.Create(AParent);
  try
    TBlobField(ADataset.FieldByName('itmgen')).SaveToStream(AMemoryStream);
    AMemoryStream.Position := 0;
    AForm.ConstructFields(ADataset);
    AForm.ConstructGrid;
    AForm.ConstructDatasource;
    AForm.LoadData(AMemoryStream);
    AForm.FDatasource.ItemRows.Style := ADataset.FieldByName('stname').AsString;
    AForm.FDatasource.ItemRows.StyleFormat := ADataset.FieldByName('itmfmt').AsString;
    AForm.FDatasource.ItemRows.Dataset := ADataset;
    AForm.FDatasource.DataChanged;
    AMemoryStream.Clear;
    AForm.ExecuteViewer;
    AForm.SaveData(AMemoryStream);
    AMemoryStream.Position := 0;
    TBlobField(ADataset.FieldByName('itmgen')).LoadFromStream(AMemoryStream);
  finally
    AForm.Free;
    AMemoryStream.Free;
  end;
end;

procedure TfrmItmCreation.UnsetAll1Click(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
//
  end;
end;

procedure TfrmItmCreation.UpdateStatus(Sender: TObject; const AText: string);
begin
  StatusBar.Panels[0].Text := AText;
end;

{ TItemDataSource }

constructor TItemDataSource.Create;
begin
  FItemRows := TItemRowCollection.Create;
end;

destructor TItemDataSource.Destroy;
begin
  FItemRows.Free;
  inherited Destroy;
end;

function TItemDataSource.GetRecordCount: Integer;
begin
  Result := FItemRows.Count;
end;

function TItemDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  AColumnId: Integer;
  AItem: TItemRow;
begin
  AItem := TItemRow(FItemRows.Items[Integer(ARecordHandle)]);
  AColumnId := GetDefaultItemID(Integer(AItemHandle));
  Result := AItem.Values[AColumnId];
end;

procedure TItemDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  AColumnId: Integer;
  AItem: TItemRow;
begin
  AItem := TItemRow(FItemRows.Items[Integer(ARecordHandle)]);
  AColumnId := GetDefaultItemID(Integer(AItemHandle));
  AItem.Values[AColumnId] := VarToStr(AValue);
end;

end.
