unit frmMarkerInfoU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxSplitter, cxMaskEdit, cxDropDownEdit, cxCalendar, cxContainer,
  cxEdit, cxTextEdit, StdCtrls, ExtCtrls, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxGridCustomView, cxGridCustomTableView, NativeXML,
  cxGridTableView, cxGridLevel, cxGrid, ieview, imageenview, imageenio;

type
  TPanelDatasource = class(TcxCustomDataSource)
  private
    FControl: TcxComboBox;
    FModified: boolean;
    FPanel: TStringList;

    function GetRoot: TXmlNode;
    procedure MapPanel;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
  public
    constructor Create(AControl: TcxComboBox);
    destructor Destroy; override;

    property Modified: boolean read FModified;
  end;
  
  TfrmMarkerInfo = class(TForm)
    ActionList: TActionList;
    actClose: TAction;
    StatusBar: TdxStatusBar;
    Panel2: TPanel;
    Label3: TLabel;
    Label2: TLabel;
    edStyle: TcxTextEdit;
    pnlMarker: TPanel;
    MarkerSplitter: TcxSplitter;
    Panel3: TPanel;
    pnlPanel: TPanel;
    cxSplitter1: TcxSplitter;
    GridPanelLevel1: TcxGridLevel;
    GridPanel: TcxGrid;
    GridPanelTableView1: TcxGridTableView;
    Label1: TLabel;
    edMaterial: TcxTextEdit;
    cbSize: TcxComboBox;
    Label4: TLabel;
    edPcs: TcxTextEdit;
    procedure cbSizePropertiesChange(Sender: TObject);
  private
    FOnInitialization: boolean;
    FMarkerViewer: TImageEnView;
    FPanelViewer: TImageEnView;
    FDocument: TNativeXml;
    FPanelDatasource: TPanelDatasource;
    function GetDocumentRoot: TXmlNode;
    procedure SetDocumentRoot(const Value: TXmlNode);

    procedure InitializeInfo;
    procedure CreatePanelGridColumn;

    procedure OnFocusRecordChanged(Sender: TcxCustomGridTableView; 
      APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; 
      ANewItemRecordFocusingChanged: Boolean);
    
    function GetStyleRoot: TXmlNode;    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure ShowMarkerInfo(AOwner: TComponent; AInfo: TXmlNode);

    property Document: TNativeXml read FDocument;
    property DocumentRoot: TXmlNode read GetDocumentRoot write SetDocumentRoot;
    property StyleRoot: TXmlNode read GetStyleRoot;
  end;

var
  frmMarkerInfo: TfrmMarkerInfo;

implementation

uses
  dmMainU;

{$R *.dfm}

const
  APluginName: string = 'imagemagick.dll';

{ TfrmMarkerInfo }

procedure TfrmMarkerInfo.cbSizePropertiesChange(Sender: TObject);
var
  ANode, AItem: TXmlNode;
begin
  if FOnInitialization then
    exit;
  FPanelDatasource.MapPanel;
  FPanelDatasource.DataChanged;
  if (cbSize.ItemIndex > -1) then
  begin
    ANode := TXmlNode(cbSize.Properties.Items.Objects[cbSize.ItemIndex]);
    AItem := nil;
    if ANode <> nil then      
      AItem := ANode.FindNode('NB_OF_SETS');
    if AItem <> nil then
      edPcs.Text := AItem.ValueAsString;
  end;  
end;

constructor TfrmMarkerInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnInitialization := False;
  FMarkerViewer := TImageEnView.Create(Self);
  FMarkerViewer.Parent := pnlMarker;
  FMarkerViewer.Align := alClient;
  FMarkerViewer.BorderStyle := bsNone;
  FPanelViewer := TImageEnView.Create(Self);
  FPanelViewer.Parent := pnlPanel;
  FPanelViewer.Align := alClient;
  FPanelViewer.BorderStyle := bsNone;
  FDocument := TNativeXml.Create; 
  FPanelDatasource := TPanelDatasource.Create(cbSize);
  GridPanelTableView1.DataController.CustomDataSource := FPanelDatasource;
  GridPanelTableView1.OnFocusedRecordChanged := OnFocusRecordChanged;
end;

procedure TfrmMarkerInfo.CreatePanelGridColumn;
begin
  GridPanelTableView1.ClearItems;
  with GridPanelTableView1.CreateColumn as TcxGridColumn do
  begin
    Caption := 'Panel';
    DataBinding.ValueTypeClass := TcxStringValueType;
    PropertiesClass := TcxTextEditProperties;
    Properties.ReadOnly := True;
    Options.Editing := False;
    Options.Focusing:= False;
  end;
  with GridPanelTableView1.CreateColumn as TcxGridColumn do
  begin
    Caption := 'Code';
    DataBinding.ValueTypeClass := TcxStringValueType;
    PropertiesClass := TcxTextEditProperties;
    Properties.ReadOnly := True;
    Options.Editing := False;
    Options.Focusing:= False;
  end;
  with GridPanelTableView1.CreateColumn as TcxGridColumn do
  begin
    Caption := 'Material';
    DataBinding.ValueTypeClass := TcxStringValueType;
    PropertiesClass := TcxTextEditProperties;
    Properties.ReadOnly := True;
    Options.Editing := False;
    Options.Focusing:= False;
  end;
  GridPanelTableView1.OptionsView.ColumnAutoWidth := True;
end;

destructor TfrmMarkerInfo.Destroy;
begin
  FDocument.Free;
  FPanelViewer.Free;
  FMarkerViewer.Free;
  inherited Destroy;
end;

function TfrmMarkerInfo.GetDocumentRoot: TXmlNode;
begin
  Result := Document.Root;
end;

function TfrmMarkerInfo.GetStyleRoot: TXmlNode;
begin
  Result := DocumentRoot.FindNode('STYLE');
end;

procedure TfrmMarkerInfo.InitializeInfo;
var
  I: integer;
  AStyle, AItem, ANode, AName: TXmlNode;
  AMarkerSvg: TNativeXml;
  AMemStream: TMemoryStream;
  AFileFormatInfo: TIEFileFormatInfo;
begin  
  AStyle := StyleRoot;
  if AStyle <> nil then
  begin
    AItem := AStyle.FindNode('NAME');
    if AItem <> nil then
      edStyle.Text := AItem.ValueAsString;
    AItem := AStyle.FindNode('MATERIAL');
    if AItem <> nil then
      edMaterial.Text := AItem.ValueAsString;
    for I := 0 to AStyle.NodeCount - 1 do
    begin
      AItem := AStyle.Nodes[I];
      if SameText(AItem.Name, 'SIZE') then
      begin
        ANode := AItem.FindNode('NB_OF_SETS');
        if (ANode <> nil) and (ANode.ValueAsInteger > 0) then
        begin
          ANode := AItem.FindNode('NAME');
          cbSize.Properties.Items.AddObject(ANode.ValueAsString, AItem);
        end;
      end;      
    end;
    if cbSize.Properties.Items.Count > 0 then
    begin
      cbSize.ItemIndex := 0;
      ANode := TXmlNode(cbSize.Properties.Items.Objects[0]);
      AItem := nil;
      if ANode <> nil then      
        AItem := ANode.FindNode('NB_OF_SETS');
      if AItem <> nil then
        edPcs.Text := AItem.ValueAsString;
    end;
  end;
  AMemStream := TMemoryStream.Create;
  AMarkerSvg := TNativeXml.Create;
  try
    AMarkerSvg.EncodingString := 'UTF-8';
    ANode := AMarkerSvg.RootNodeList[0];
    if not assigned(ANode) or (ANode.ElementType <> xeDeclaration) then
    begin
      ANode := TXmlNode.CreateType(AMarkerSvg, xeDeclaration);
      AMarkerSvg.RootNodeList.NodeInsert(0, ANode);
    end;
    if assigned(ANode) then
      ANode.AttributeByName['standalone'] := 'yes';
    ANode := AMarkerSvg.RootNodeList[1];
    if not assigned(ANode) or (ANode.ElementType <> xeDoctype) then
    begin
      ANode := TXmlNode.CreateType(AMarkerSvg, xeDoctype);
      AMarkerSvg.RootNodeList.NodeInsert(1, ANode);
    end;
    if assigned(ANode) then
    begin
      ANode.Name := 'svg';
      ANode.ValueDirect := 'PUBLIC "-//W3C//DTD SVG 1.1//EN" '+
        '"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"';
    end;    
    ANode := DocumentRoot.FindNode('VIEW');
    if ANode <> nil then
      AItem := ANode.FindNode('svg');
    if AItem <> nil then
      AMarkerSvg.Root.Assign(AItem);
    AFileFormatInfo := IEFileFormatGetInfo2('.svg');
    AMarkerSvg.SaveToStream(AMemStream);
    AMemStream.Position := 0;
    FMarkerViewer.IO.NativePixelFormat := True;
    FMarkerViewer.IO.Params.Dpi := 300;
    if AMemStream.Size > 0 then
      FMarkerViewer.IO.LoadFromStreamFormat(AMemStream, AFileFormatInfo.FileType); 
  finally
    AMarkerSvg.Free;
    AMemStream.Free;
  end;
end;

procedure TfrmMarkerInfo.OnFocusRecordChanged(Sender: TcxCustomGridTableView;
  APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord;
  ANewItemRecordFocusingChanged: Boolean);
var
  ANode, AItem: TXmlNode;
  AMarkerSvg: TNativeXml;
  AMemStream: TMemoryStream;
  AFileFormatInfo: TIEFileFormatInfo;  
begin
  if not ANewItemRecordFocusingChanged then
  begin
    if (AFocusedRecord <> nil) and
       (AFocusedRecord.RecordIndex > -1) and 
       (AFocusedRecord.RecordIndex < FPanelDatasource.FPanel.Count) then
    begin
      AItem := nil;
      ANode := TXmlNode(FPanelDatasource.FPanel.Objects[AFocusedRecord.RecordIndex]);
      if ANode <> nil then
        AItem := ANode.FindNode('SHAPE');
      if AItem <> nil then
      begin
        AMemStream := TMemoryStream.Create;
        AMarkerSvg := TNativeXml.Create;
        try
          AMarkerSvg.EncodingString := 'UTF-8';
          ANode := AMarkerSvg.RootNodeList[0];
          if not assigned(ANode) or (ANode.ElementType <> xeDeclaration) then
          begin
            ANode := TXmlNode.CreateType(AMarkerSvg, xeDeclaration);
            AMarkerSvg.RootNodeList.NodeInsert(0, ANode);
          end;
          if assigned(ANode) then
            ANode.AttributeByName['standalone'] := 'yes';
          ANode := AMarkerSvg.RootNodeList[1];
          if not assigned(ANode) or (ANode.ElementType <> xeDoctype) then
          begin
            ANode := TXmlNode.CreateType(AMarkerSvg, xeDoctype);
            AMarkerSvg.RootNodeList.NodeInsert(1, ANode);
          end;
          if assigned(ANode) then
          begin
            ANode.Name := 'svg';
            ANode.ValueDirect := 'PUBLIC "-//W3C//DTD SVG 1.1//EN" '+
              '"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"';
          end;
          AItem := AItem.FindNode('svg');
          if AItem <> nil then
            AMarkerSvg.Root.Assign(AItem);
          AFileFormatInfo := IEFileFormatGetInfo2('.svg');
          AMarkerSvg.SaveToStream(AMemStream);
          AMemStream.Position := 0;
          FPanelViewer.IO.NativePixelFormat := True;
          FPanelViewer.IO.Params.Dpi := 300;
          if AMemStream.Size > 0 then
            FPanelViewer.IO.LoadFromStreamFormat(AMemStream, AFileFormatInfo.FileType); 
        finally
          AMarkerSvg.Free;
          AMemStream.Free;
        end;
      end;      
    end else
      FPanelViewer.Clear;
  end;
end;

procedure TfrmMarkerInfo.SetDocumentRoot(const Value: TXmlNode);
begin
  DocumentRoot.Clear;
  DocumentRoot.Assign(Value);
end;

class procedure TfrmMarkerInfo.ShowMarkerInfo(AOwner: TComponent;
  AInfo: TXmlNode);
begin
  if (AInfo <> nil) then
  begin
    with TfrmMarkerInfo.Create(AOwner) do
    try
      FOnInitialization := True;
      try
        DocumentRoot := AInfo;
        InitializeInfo;        
        CreatePanelGridColumn;        
        FPanelDatasource.MapPanel;
      finally
        FOnInitialization := False;
      end;
      FPanelDatasource.DataChanged;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

{ TPanelDatasource }

constructor TPanelDatasource.Create(AControl: TcxComboBox);
begin
  FControl := AControl;
  FPanel := TStringList.Create;
end;

destructor TPanelDatasource.Destroy;
begin
  FPanel.Free;
  inherited Destroy;
end;

function TPanelDatasource.GetRecordCount: Integer;
begin
  Result := FPanel.Count;   
end;

function TPanelDatasource.GetRoot: TXmlNode;
begin
  Result := nil;
  if (FControl.ItemIndex > -1) and (FControl.Properties.Items.Count > 0) then
    Result := TXmlNode(FControl.Properties.Items.Objects[FControl.ItemIndex]);
end;

function TPanelDatasource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  ARow, ACol: integer;
  ARoot, ANode: TXmlNode;
begin
  Result := null;
  ARoot := GetRoot;
  if ARoot <> nil then
  begin
    ARow := Integer(ARecordHandle);
    ACol := GetDefaultItemID(Integer(AItemHandle));
    if ARow < FPanel.Count then
    begin
      ANode := TXmlNode(FPanel.Objects[ARow]);
      case ACol of
        0: Result := FPanel[ARow];
        1:
          begin
            if ANode <> nil then
              ANode := ANode.FindNode('CODE');
            if ANode <> nil then
              Result := ANode.ValueAsString;
          end;
        2:
          begin
            if ANode <> nil then
              ANode := ANode.FindNode('MATERIAL');
            if ANode <> nil then
              Result := ANode.ValueAsString;
          end;
      end;
    end;
  end;
end;

procedure TPanelDatasource.MapPanel;
var
  I: integer;
  ARoot, AItem: TXmlNode;
begin
  FPanel.Clear;
  ARoot := GetRoot;
  if ARoot <> nil then
  begin
    for I := 0 to ARoot.NodeCount - 1 do
    begin
      AItem := ARoot.Nodes[i];
      if SameText(AItem.Name, 'PIECE') then
        FPanel.AddObject(AItem.FindNode('NAME').ValueAsString, AItem);
    end;
  end;
end;

initialization
  IEFileFormatRemove(ioRAW);
  IEAddExtIOPlugin(APluginName);

end.
