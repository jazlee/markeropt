unit frxecfeditor;

interface
{$I frx.inc}

implementation

uses
  Windows, Classes, frxCustomDB, frxDsgnIntf, DB, frxEditQueryParams,
  ecfconnection, ecfdataset, ecfapiproc, frxECFComponents, inifiles, frxRes,
  Controls 
{$IFDEF Delphi6}
, Variants
{$ENDIF};

type
  TfrxParamsProperty = class(TfrxClassProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    function Edit: Boolean; override;
  end;

  TfrxAPIObjectNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
  end;

  TfrxAPIMethodNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
  end;

{ TfrxParamsProperty }

function TfrxParamsProperty.Edit: Boolean;
var
  q: TfrxECFAPIObject;
begin
  Result := False;
  q := TfrxECFAPIObject(Component);
  if q.Params.Count <> 0 then
    with TfrxParamsEditorForm.Create(Designer) do
    begin
      Params := q.Params;
      Result := ShowModal = mrOk;
      if Result then
      begin
        q.UpdateParams;
        Self.Designer.UpdateDataTree;
      end;
      Free;
    end;
end;

function TfrxParamsProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TfrxAPIMethodNameProperty }

function TfrxAPIMethodNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TfrxAPIMethodNameProperty.GetValues;
var
  t: TECFAPIObject;
  v: TStringList;
begin
  inherited;
  t := TfrxECFAPIObject(Component).APIObject;
  v := TStringList.Create;
  if (t <> nil) and (t.ConnectionPool <> nil) then
    try
      t.GetMethodList(t.APIObjectName, v);
      Values.Assign(v);
    except
    end;
  v.Free;
end;

{ TfrxAPIObjectNameProperty }

function TfrxAPIObjectNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TfrxAPIObjectNameProperty.GetValues;
var
  t: TECFAPIObject;
  v: TStringList;
begin
  inherited;
  t := TfrxECFAPIObject(Component).APIObject;
  v := TStringList.Create;
  if (t <> nil) and (t.ConnectionPool <> nil) then
    try
      t.GetObjectList(v);
      Values.Assign(v);
    except
    end;
  v.Free;
end;

initialization
  frxPropertyEditors.Register(TypeInfo(TfrxParams), TfrxECFAPIObject, 'Params',
    TfrxParamsProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxECFAPIObject, 'APIObjectName',
    TfrxAPIObjectNameProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxECFAPIObject, 'APIMethodName',
    TfrxAPIMethodNameProperty);
end.

