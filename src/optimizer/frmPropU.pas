unit frmPropU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxPC, cxControls, cxContainer, cxEdit, cxTextEdit, cxMemo,
  DB, NativeRegXML;

type
  TfrmProperties = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    cxPageControl1: TcxPageControl;
    cxTabSheet1: TcxTabSheet;
    cxTextEdit1: TcxTextEdit;
    Label1: TLabel;
    Label2: TLabel;
    cxTextEdit2: TcxTextEdit;
    Label3: TLabel;
    cxTextEdit3: TcxTextEdit;
    Label4: TLabel;
    cxTextEdit4: TcxTextEdit;
    Label5: TLabel;
    cxTextEdit5: TcxTextEdit;
    Label6: TLabel;
    cxTextEdit6: TcxTextEdit;
    Label7: TLabel;
    cxMemo1: TcxMemo;
  private
    procedure ReadProps(XMLReg: TRegXML);
    procedure WriteProps(XMLReg:  TRegXML);
  public
    class procedure ShowProperties;
  end;

var
  frmProperties: TfrmProperties;

implementation

uses
  dmPackageU, cspapputil, ecfutils, StrUtils, frmMainU;

{$R *.dfm}

function ConstructFilter(const AFilterFields: string; AValues: Variant): string;
var
  AParams: TECFParams;

  function ConstructFilterStr: string;
  var
    i: integer;
    AStr: string;
  begin
    Result := '';
    for i := 0 to AParams.Count - 1 do
    begin
      AStr := Format('(%s = %s)',[AParams[I].Name, QuotedStr(AParams[I].AsString)]);
      Result := IfThen(Result = '', AStr, Result + ' and '+ AStr);
    end;
  end;
    
begin
  AParams := TECFParams.Create(nil);
  try
    AParams.ParseParams(AFilterFields);
    AParams.ParamValues[AFilterFields] := AValues;
    Result := ConstructFilterStr;
  finally
    AParams.Free;
  end;  
end;

{ TfrmPropertiesU }

procedure TfrmProperties.ReadProps(XMLReg: TRegXML);
var
  i: integer;
  AName, AValue: string;
begin
  if XMLReg.openKey('properties', False) then
  begin
    for I := 0 to ComponentCount - 1 do
    begin
      if (Components[i] is TcxCustomTextEdit) then
      begin
        AName := Format('data_%d',[Components[i].Tag]);
        AValue := XMLReg.readString(AName);
        if (Components[i] is TcxMemo) then
          TcxMemo(Components[i]).Lines.Text := AValue
        else if (Components[i] is TcxTextEdit) then
          TcxTextEdit(Components[i]).Text := AValue;
      end;      
    end;
  end;
end;

class procedure TfrmProperties.ShowProperties;
var
  ADataset: TDataset;
  XMLReg: TRegXML;
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  XMLReg := TRegXML.Create(nil);
  ADataset := dmPackage.GetACRTable('settings', 'pk_settings');
  try
    ADataset.Filter := ConstructFilter('keyname;refid_1;refid_2;refid_3',
      VarArrayOf(['RDB$PROPS', 0, 0, 0]));
    ADataset.Open;
    if ADataset.IsEmpty then
      ADataset.AppendRecord(['RDB$PROPS', 0, 0, 0,'']);
    TBlobField(ADataset.FieldByName('data')).SaveToStream(AStream);
    AStream.Position := 0;
    if AStream.Size > 0 then
      XMLReg.LoadFromStream(AStream);
    with TfrmProperties.Create(Application.MainForm) do
    try
      ReadProps(XMLReg);
      if ShowModal = mrOk then
      begin
        WriteProps(XMLReg);
        AStream.Clear;
        XMLReg.SaveToStream(AStream);
        AStream.Position := 0;
        ADataset.Edit;
        TBlobField(ADataset.FieldByName('data')).LoadFromStream(AStream);
        ADataset.Post;
        dmPackage.RefreshPackageInfo;
        frmMain.AdjustTitle(dmPackage.PackageInfo.Values['data_0']);
      end;
    finally
      Free;
    end;
  finally
    ADataset.Free;
    XMLReg.Free;
    AStream.Free;    
  end;  
end;

procedure TfrmProperties.WriteProps(XMLReg: TRegXML);
var
  i: integer;
  AName, AValue: string;
begin
  if XMLReg.openKey('properties', true) then
  begin
    for I := 0 to ComponentCount - 1 do
    begin
      if (Components[i] is TcxCustomTextEdit) then
      begin
        AName := Format('data_%d',[Components[i].Tag]);
        if (Components[i] is TcxMemo) then
          AValue := TcxMemo(Components[i]).Lines.Text
        else if (Components[i] is TcxTextEdit) then
          AValue := TcxTextEdit(Components[i]).Text;
        XMLReg.writeString(AName, AValue);
      end;      
    end;
  end;
end;

end.
