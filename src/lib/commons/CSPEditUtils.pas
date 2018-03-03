unit CSPEditUtils;

interface

uses
  SysUtils, Classes, DB;


type
  TDBEventType = ( etDsAfterCancel, etDsAfterClose, etDsAfterDelete,
                   etDsAfterEdit, etDsAfterInsert, etDsAfterOpen,
                   etDsAfterPost, etDsAfterRefresh, etDsAfterScroll,
                   etDsBeforeCancel, etDsBeforeClose, etDsBeforeDelete,
                   etDsBeforeEdit, etDsBeforeInsert, etDsBeforeOpen,
                   etDsBeforePost, etDsBeforeRefresh, etDsBeforeScroll,
                   etDsOnCalcFields, etDsOnNewRecord,
                   etFldOnChange);

  TCSPDBEvent = class(TCollectionItem)
  private
    FEventType: TDBEventType;
    FFieldName: string;
    FEvent: TNotifyEvent;
  public
    property EventType: TDBEventType read FEventType;
    property FieldName: string read FFieldName;
    property Event: TNotifyEvent read FEvent;
  end;

  TDatasetEventHandler = class(TComponent)
  private
    FEventCollection: TCollection;
    FDatasource: TDataSource;

    procedure RegisterEvents;
    procedure UnregisterEvents;

    function FindEvent(const AEventType: TDBEventType): TCSPDBEvent; overload;
    function FindEvent(const AEventType: TDBEventType; const AFieldName: string): TCSPDBEvent; overload;

    procedure ExecuteDBEvent(const AEventType: TDBEventType;
                             Sender: TObject);
    procedure ExecuteFieldEvent(const AEventType: TDBEventType;
                                const AFieldName: string;
                                Sender: TObject);
  protected
    procedure InternalHandleEvent(const AEventType: TDBEventType;
                                  Sender: TObject); virtual;
    procedure HandleFieldOnChange(Sender: TField); virtual;
    procedure HandleDatasetAfterCancel(DataSet: TDataSet); virtual;
    procedure HandleDatasetAfterClose(DataSet: TDataSet); virtual;
    procedure HandleDatasetAfterDelete(DataSet: TDataSet); virtual;
    procedure HandleDatasetAfterEdit(DataSet: TDataSet); virtual;
    procedure HandleDatasetAfterInsert(DataSet: TDataSet); virtual;
    procedure HandleDatasetAfterOpen(DataSet: TDataSet); virtual;
    procedure HandleDatasetAfterPost(DataSet: TDataSet); virtual;
    procedure HandleDatasetAfterRefresh(DataSet: TDataSet); virtual;
    procedure HandleDatasetAfterScroll(DataSet: TDataSet); virtual;
    procedure HandleDatasetBeforeCancel(DataSet: TDataSet); virtual;
    procedure HandleDatasetBeforeClose(DataSet: TDataSet); virtual;
    procedure HandleDatasetBeforeDelete(DataSet: TDataSet); virtual;
    procedure HandleDatasetBeforeEdit(DataSet: TDataSet); virtual;
    procedure HandleDatasetBeforeInsert(DataSet: TDataSet); virtual;
    procedure HandleDatasetBeforeOpen(DataSet: TDataSet); virtual;
    procedure HandleDatasetBeforePost(DataSet: TDataSet); virtual;
    procedure HandleDatasetBeforeRefresh(DataSet: TDataSet); virtual;
    procedure HandleDatasetBeforeScroll(DataSet: TDataSet); virtual;
    procedure HandleDatasetOnCalcFields(DataSet: TDataSet); virtual;
    procedure HandleDatasetOnNewRecord(DataSet: TDataSet); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterDataset(ADataset: TDataset);
    procedure UnregisterDataset;

    procedure RegisterDatasetEvent(const AEventType: TDBEventType;
                                   AEvent: TNotifyEvent);
    procedure RegisterFieldEvent(const AEventType: TDBEventType;
                                 const AFieldName: string;
                                 AEvent: TNotifyEvent);
  end;

implementation

{ TDatasetEventHandler }

constructor TDatasetEventHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatasource := TDataSource.Create(Self);
  FEventCollection := TCollection.Create(TCSPDBEvent);
end;

destructor TDatasetEventHandler.Destroy;
begin
  UnregisterDataset;
  FEventCollection.Free;
  FDatasource.Free;
  inherited Destroy;
end;

procedure TDatasetEventHandler.ExecuteDBEvent(const AEventType: TDBEventType;
  Sender: TObject);
var
  I: integer;
  AItem: TCSPDBEvent;
begin
  for I := 0 to FEventCollection.Count - 1 do
  begin
    AItem := TCSPDBEvent(FEventCollection.Items[i]);
    if (AItem.EventType = AEventType) and (Assigned(AItem.Event)) then
    begin
      AItem.FEvent(Sender);
      Break;
    end;
  end;   
end;

procedure TDatasetEventHandler.ExecuteFieldEvent(const AEventType: TDBEventType;
  const AFieldName: string; Sender: TObject);
var
  I: integer;
  AItem: TCSPDBEvent;
begin
  for I := 0 to FEventCollection.Count - 1 do
  begin
    AItem := TCSPDBEvent(FEventCollection.Items[i]);
    if (AItem.EventType = AEventType) and
       (SameText(AItem.FieldName, AFieldName)) and
       (Assigned(AItem.Event)) then
    begin
      AItem.FEvent(Sender);
      Break;
    end;
  end;   
end;

function TDatasetEventHandler.FindEvent(
  const AEventType: TDBEventType): TCSPDBEvent;
var
  I: integer;
  AItem: TCSPDBEvent;
begin
  Result := nil;
  for I := 0 to FEventCollection.Count - 1 do
  begin
    AItem := TCSPDBEvent(FEventCollection.Items[i]);
    if (AItem.EventType = AEventType) then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TDatasetEventHandler.FindEvent(const AEventType: TDBEventType;
  const AFieldName: string): TCSPDBEvent;
var
  I: integer;
  AItem: TCSPDBEvent;
begin
  Result := nil;
  for I := 0 to FEventCollection.Count - 1 do
  begin
    AItem := TCSPDBEvent(FEventCollection.Items[i]);
    if (AItem.EventType = AEventType) and
       (SameText(AFieldName, AItem.FieldName)) then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

procedure TDatasetEventHandler.HandleDatasetAfterCancel(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsAfterCancel, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetAfterClose(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsAfterClose, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetAfterDelete(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsAfterDelete, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetAfterEdit(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsAfterEdit, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetAfterInsert(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsAfterInsert, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetAfterOpen(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsAfterOpen, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetAfterPost(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsAfterPost, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetAfterRefresh(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsAfterRefresh, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetAfterScroll(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsAfterScroll, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetBeforeCancel(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsBeforeCancel, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetBeforeClose(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsBeforeClose, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetBeforeDelete(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsBeforeDelete, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetBeforeEdit(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsBeforeEdit, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetBeforeInsert(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsBeforeInsert, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetBeforeOpen(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsBeforeOpen, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetBeforePost(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsBeforePost, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetBeforeRefresh(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsBeforeRefresh, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetBeforeScroll(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsBeforeScroll, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetOnCalcFields(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsOnCalcFields, DataSet);
end;

procedure TDatasetEventHandler.HandleDatasetOnNewRecord(DataSet: TDataSet);
begin
  InternalHandleEvent(etDsOnNewRecord, DataSet);
end;

procedure TDatasetEventHandler.HandleFieldOnChange(Sender: TField);
begin
  InternalHandleEvent(etFldOnChange, Sender);
end;

procedure TDatasetEventHandler.InternalHandleEvent(
  const AEventType: TDBEventType; Sender: TObject);
var
  AFieldName: string;
begin
  if (AEventType = etFldOnChange) then
  begin
    AFieldName := TField(Sender).FieldName;
    ExecuteFieldEvent(AEventType, AFieldName, Sender);
  end else
    ExecuteDBEvent(AEventType, Sender);
end;

procedure TDatasetEventHandler.RegisterDataset(ADataset: TDataset);
begin
  FDatasource.DataSet := ADataset;
  RegisterEvents;
end;

procedure TDatasetEventHandler.RegisterDatasetEvent(const AEventType: TDBEventType;
  AEvent: TNotifyEvent);
var
  AItem: TCSPDBEvent;
begin
  AItem := FindEvent(AEventType);
  if AItem = nil then
  begin
    AItem := TCSPDBEvent(FEventCollection.Add);
    AItem.FEventType := AEventType;
    AItem.FEvent := AEvent;
  end else
    AItem.FEvent := AEvent;  
end;

procedure TDatasetEventHandler.RegisterEvents;
  procedure RegisterFieldEvents;
  var
    I: integer;
  begin
    for I := 0 to FDatasource.DataSet.FieldCount - 1 do
      FDatasource.DataSet.Fields[I].OnChange := HandleFieldOnChange;
  end;
begin
  if Assigned(FDatasource.DataSet) then
  begin
    FDatasource.DataSet.AfterCancel := Self.HandleDatasetAfterCancel;
    FDatasource.DataSet.AfterClose := Self.HandleDatasetAfterClose;
    FDatasource.DataSet.AfterDelete := Self.HandleDatasetAfterDelete;
    FDatasource.DataSet.AfterEdit := Self.HandleDatasetAfterEdit;
    FDatasource.DataSet.AfterInsert := Self.HandleDatasetAfterInsert;
    FDatasource.DataSet.AfterOpen := Self.HandleDatasetAfterOpen;
    FDatasource.DataSet.AfterPost := Self.HandleDatasetAfterPost;
    FDatasource.DataSet.AfterRefresh := Self.HandleDatasetAfterRefresh;
    FDatasource.DataSet.AfterScroll := Self.HandleDatasetAfterScroll;
    FDatasource.DataSet.BeforeCancel := Self.HandleDatasetBeforeCancel;
    FDatasource.DataSet.BeforeClose := Self.HandleDatasetBeforeClose;
    FDatasource.DataSet.BeforeDelete := Self.HandleDatasetBeforeDelete;
    FDatasource.DataSet.BeforeEdit := Self.HandleDatasetBeforeEdit;
    FDatasource.DataSet.BeforeInsert := Self.HandleDatasetBeforeInsert;
    FDatasource.DataSet.BeforeOpen := Self.HandleDatasetBeforeOpen;
    FDatasource.DataSet.BeforePost := Self.HandleDatasetBeforePost;
    FDatasource.DataSet.BeforeRefresh := Self.HandleDatasetBeforeRefresh;
    FDatasource.DataSet.BeforeScroll := Self.HandleDatasetBeforeScroll;
    FDatasource.DataSet.OnCalcFields := Self.HandleDatasetOnCalcFields;
    FDatasource.DataSet.OnNewRecord := Self.HandleDatasetOnNewRecord;
    RegisterFieldEvents;
  end;
end;

procedure TDatasetEventHandler.RegisterFieldEvent(const AEventType: TDBEventType;
  const AFieldName: string; AEvent: TNotifyEvent);
var
  AItem: TCSPDBEvent;
begin
  AItem := FindEvent(AEventType, AFieldName);
  if AItem = nil then
  begin
    AItem := TCSPDBEvent(FEventCollection.Add);
    AItem.FEventType := AEventType;
    AItem.FEvent := AEvent;
    AItem.FFieldName := AFieldName;
  end else
    AItem.FEvent := AEvent;  
end;

procedure TDatasetEventHandler.UnregisterDataset;
begin
  UnregisterEvents;
  FDatasource.DataSet := nil;
end;

procedure TDatasetEventHandler.UnregisterEvents;
  procedure UnregisterFieldEvents;
  var
    I: integer;
  begin
    for I := 0 to FDatasource.DataSet.FieldCount - 1 do
      FDatasource.DataSet.Fields[I].OnChange := nil;
  end;
begin
  if Assigned(FDatasource.DataSet) then
  begin
    FDatasource.DataSet.AfterCancel := nil;
    FDatasource.DataSet.AfterClose := nil;
    FDatasource.DataSet.AfterDelete := nil;
    FDatasource.DataSet.AfterEdit := nil;
    FDatasource.DataSet.AfterInsert := nil;
    FDatasource.DataSet.AfterOpen := nil;
    FDatasource.DataSet.AfterPost := nil;
    FDatasource.DataSet.AfterRefresh := nil;
    FDatasource.DataSet.AfterScroll := nil;
    FDatasource.DataSet.BeforeCancel := nil;
    FDatasource.DataSet.BeforeClose := nil;
    FDatasource.DataSet.BeforeDelete := nil;
    FDatasource.DataSet.BeforeEdit := nil;
    FDatasource.DataSet.BeforeInsert := nil;
    FDatasource.DataSet.BeforeOpen := nil;
    FDatasource.DataSet.BeforePost := nil;
    FDatasource.DataSet.BeforeRefresh := nil;
    FDatasource.DataSet.BeforeScroll := nil;
    FDatasource.DataSet.OnCalcFields := nil;
    FDatasource.DataSet.OnNewRecord := nil;
    UnregisterFieldEvents;
  end;
end;

end.
