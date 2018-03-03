unit frmOptionsU;

interface
{$I CSPDefs.inc}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxPC, cxControls, Menus, cxLookAndFeelPainters, StdCtrls, cxButtons,
  cxContainer, cxEdit, cxGroupBox, cxCheckBox, cxLabel, ExtCtrls, cxGraphics,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxButtonEdit, cxSpinEdit;

type
  TfrmOptions = class(TForm)
    cxPageControl1: TcxPageControl;
    PreferenceSheet: TcxTabSheet;
    btnOK: TcxButton;
    btnCancel: TcxButton;
    ckSplash: TcxCheckBox;
    Bevel2: TBevel;
    cxLabel1: TcxLabel;
    ckMultiInstance: TcxCheckBox;
    Bevel3: TBevel;
    cxLabel5: TcxLabel;
    cxLabel6: TcxLabel;
    Bevel4: TBevel;
    cxLabel7: TcxLabel;
    cxLabel8: TcxLabel;
    cbTheme: TcxComboBox;
    cbLang: TcxComboBox;
    ckRibbon: TcxCheckBox;
    GeneralSheet: TcxTabSheet;
    cxLabel2: TcxLabel;
    Bevel1: TBevel;
    cxLabel3: TcxLabel;
    edDSN: TcxTextEdit;
    cxLabel4: TcxLabel;
    edDSP: TcxTextEdit;
    cxLabel9: TcxLabel;
    edBTF: TcxTextEdit;
    cxLabel10: TcxLabel;
    edXML: TcxTextEdit;
    cxLabel11: TcxLabel;
    Bevel5: TBevel;
    cxLabel12: TcxLabel;
    edNestApp: TcxButtonEdit;
    cxLabel13: TcxLabel;
    Bevel6: TBevel;
    cxLabel14: TcxLabel;
    edNestTime: TcxSpinEdit;
    cxLabel15: TcxLabel;
    btnRegister: TcxButton;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure edNestAppPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure FormCreate(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOptions: TfrmOptions;

implementation
uses
  CSPAppUtil, dmMainU, dxSkinsDefaultPainters, gnugettext,
  languagecodes, cxLookAndFeels, StrUtils, ecflangutil, frmMainU,
  {$ifdef COMMERCIAL}
  frmRegistrationU,
  {$endif}  
  CSPConsts;

{$R *.dfm}

procedure TfrmOptions.btnRegisterClick(Sender: TObject);
begin
  {$ifdef COMMERCIAL}
  TfrmRegistration.DoRegister(Self);
  {$endif}
end;

procedure TfrmOptions.edNestAppPropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  dmMain.OpenDialog.DefaultExt := SCSPExeExt;
  dmMain.OpenDialog.Filter := SCSPExeFilter;
  dmMain.OpenDialog.Title := SCSPSelectApp;
  dmMain.OpenDialog.FileName := EmptyStr;
  if dmMain.OpenDialog.Execute then
    edNestApp.Text := dmMain.OpenDialog.FileName;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  {$ifndef COMMERCIAL}
  btnRegister.Visible := False;
  {$else}
  btnRegister.Visible := True;  
  {$endif}
end;

procedure TfrmOptions.FormHide(Sender: TObject);
var
  AList: TStringList;
begin
  if ModalResult = mrOk then
  begin    
    if cbTheme.ItemIndex = 0 then
    begin
      if not ckRibbon.Checked then
        dmMain.SkinCollection.UseSkin := False
      else
        dmMain.SkinCollection.SkinName := 'Black';
    end else
      dmMain.SkinCollection.SkinName := cbTheme.Properties.Items[cbTheme.ItemIndex];
    AList := TStringList.Create;
    try
      DefaultInstance.GetListOfLanguages(DefaultTextDomain, AList);
      GetAppIDClass.DefaultLanguangeUsed := AList[cbLang.ItemIndex];
    finally
      AList.Free;
    end;
    GetAppIDClass.Properties.SetProperty('splashscreen', IfThen(ckSplash.Checked, 'true', 'false'));
    GetAppIDClass.Properties.SetProperty('multiinstance', IfThen(ckMultiInstance.Checked, 'true', 'false'));
    GetAppIDClass.Properties.SetProperty('ribbonbar', IfThen(ckRibbon.Checked, 'true', 'false'));

    GetAppIDClass.Properties.SetProperty('dsnext', edDSN.Text);
    GetAppIDClass.Properties.SetProperty('dspext', edDSP.Text);
    GetAppIDClass.Properties.SetProperty('btfext', edBTF.Text);
    GetAppIDClass.Properties.SetProperty('xmlext', edXML.Text);
    GetAppIDClass.Properties.SetProperty('nestapp', edNestApp.Text);
    GetAppIDClass.Properties.SetProperty('nesttime', edNestTime.Value);
  end;  
end;

procedure TfrmOptions.FormShow(Sender: TObject);
var
  I: integer;
  AList: TStringList;
begin
  ckSplash.Checked := SameText(GetAppIDClass.Properties.GetProperty('splashscreen', 'true'), 'true');
  ckMultiInstance.Checked := SameText(GetAppIDClass.Properties.GetProperty('multiinstance', 'true'), 'true');
  ckRibbon.Checked := SameText(GetAppIDClass.Properties.GetProperty('ribbonbar', 'false'), 'true');
  cbTheme.Clear;
  cbTheme.Properties.Items.Add('Standard');
  for I := 0 to dmMain.SkinCollection.SkinCount - 1 do
    cbTheme.Properties.Items.Add(dmMain.SkinCollection.SkinNames[I]);
  if dmMain.SkinController.UseSkins then
    cbTheme.ItemIndex := cbTheme.Properties.Items.IndexOf(dmMain.SkinCollection.SkinName)
  else
    cbTheme.ItemIndex := 0;
  cbLang.Clear;
  AList := TStringList.Create;
  try
    DefaultInstance.GetListOfLanguages(DefaultTextDomain, AList);
    for i := 0 to AList.Count - 1 do
      cbLang.Properties.Items.Add(getlanguagename(AList[i]));
    cbLang.ItemIndex := cbLang.Properties.Items.IndexOf(getlanguagename(GetAppIDClass.DefaultLanguangeUsed));
  finally
    AList.Free;
  end;
  edDSN.Text := GetAppIDClass.Properties.GetProperty('dsnext', DSNFileExt);
  edDSP.Text := GetAppIDClass.Properties.GetProperty('dspext', DSPFileExt);
  edBTF.Text := GetAppIDClass.Properties.GetProperty('btfext', BTFFileExt);
  edXML.Text := GetAppIDClass.Properties.GetProperty('xmlext', XMLFileExt);
  edNestApp.Text := GetAppIDClass.Properties.GetProperty('nestapp', NESTAppFileName);
  edNestTime.Value := StrToInt(GetAppIDClass.Properties.GetProperty('nesttime',
    IntToStr(DEF_NESTING_TIME)));
  ReconstructLanguage(Self);
end;

end.
