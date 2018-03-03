unit frxACREditor;

interface

{$I frx.inc}

implementation

uses
  Windows, Classes, frxACRComponents, frxCustomDB, frxDsgnIntf, DB, ACRMain
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TfrxDataBaseNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
  end;

  TfrxSessionNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
  end;

  TfrxTableNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
    procedure SetValue(const Value: String); override;
  end;

  TfrxIndexNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
  end;


{ TfrxDataBaseNameProperty }

function TfrxDataBaseNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

procedure TfrxDataBaseNameProperty.GetValues;
var
  Session: TACRSession;
begin
  inherited;
  Session := Sessions.FindSession(TACRDataSet(TfrxCustomDataset(Component).DataSet).SessionName);
  if Session <> nil then
    Session.GetDatabaseNames(Values);
end;


{ TfrxSessionNameProperty }

function TfrxSessionNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

procedure TfrxSessionNameProperty.GetValues;
begin
  Sessions.GetSessionNames(Values);
end;


{ TfrxTableNameProperty }

function TfrxTableNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

procedure TfrxTableNameProperty.GetValues;
var
  t: TACRTable;
  Session: TACRSession;
begin
  inherited;
  t := TfrxACRTable(Component).Table;
  Session := Sessions.FindSession(t.SessionName);
  if (Session <> nil) and (t.DatabaseName <> '') then
    try
      Session.GetTableNames(t.DatabaseName, Values);
    except
    end;
end;

procedure TfrxTableNameProperty.SetValue(const Value: String);
begin
  inherited;
  Designer.UpdateDataTree;
end;


{ TfrxIndexProperty }

function TfrxIndexNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

procedure TfrxIndexNameProperty.GetValues;
var
  i: Integer;
begin
  inherited;
  try
    with TfrxACRTable(Component).Table do
      if (TableName <> '') and (IndexDefs <> nil) then
      begin
        IndexDefs.Update;
        for i := 0 to IndexDefs.Count - 1 do
          if IndexDefs[i].Name <> '' then
            Values.Add(IndexDefs[i].Name);
      end;
  except
  end;
end;


initialization
  frxPropertyEditors.Register(TypeInfo(String), TfrxACRTable, 'DatabaseName',
    TfrxDataBaseNameProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxACRTable, 'SessionName',
    TfrxSessionNameProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxACRTable, 'TableName',
    TfrxTableNameProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxACRTable, 'IndexName',
    TfrxIndexNameProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxACRQuery, 'DatabaseName',
    TfrxDataBaseNameProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxACRQuery, 'SessionName',
    TfrxSessionNameProperty);

end.

