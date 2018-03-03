unit dmMainU;

{$I CSPDefs.inc}

interface

uses
  SysUtils, Classes, ImgList, Controls, DB, Forms,
  dxSkinsCore, dxSkinsDefaultPainters, cxEdit,
  cxLookAndFeels, IApplication, PCLogWriter,
  PCMemoryLog, PCLog, Dialogs, CSPCustomForm,
  dxSkinsForm, dxSkinCollector;

type
  TStringConversion = (scLocale, scFixed);
  
  TdmMain = class(TDataModule, IMainDataModule)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SkinController: TdxSkinController;
  private
    FFirstTime: boolean;
    FRegDBPath: string;
    FLogger: TPCLogWriter;
    FMemLog: TPCMemoryLog;
    FInternalLogger: TInternalLogger;
    FDMInterface: IMainDataModule;
    FConnectionName: string;
    FSkinCollection: TdxSkinCollector;

    function GetInternalLogger: TInternalLogger; safecall;
    procedure SetMsgID(const Value: Cardinal); safecall;
    function GetOpenDialog: TOpenDialog; safecall;
    function GetSaveDialog: TSaveDialog; safecall;
  protected
    procedure ReadConnSettings;
    procedure WriteConnSettings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {Flag FirstTime hanya digunakan untuk mendeteksi aplikasi yang baru saja
     diinstall dan dijalankan pertama kalinya}
    property FirstTime: boolean read FFirstTime;

    {Bind Registry Database Path}
    property REG_DBPATH: string read FRegDBPath;

    property PCLogWriter: TPCLogWriter read FLogger;
    property PCMemoryLog: TPCMemoryLog read FMemLog;

    property Logger: TInternalLogger read GetInternalLogger;

    property AppMsgID: Cardinal write SetMsgID;
    property ConnectionName: string read FConnectionName write FConnectionName;
    property DMInterface: IMainDataModule read FDMInterface;
    property SkinCollection: TdxSkinCollector read FSkinCollection;
  end;

var
  dmMain: TdmMain;

implementation
uses
  NativeRegXML, CSPConsts, CSPAppUtil, ecfutils,
  gnugettext, variants;

{$R *.dfm}

type
  TInternalLoggerHack = class(TInternalLogger);

{ TdmMain }

constructor TdmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSkinCollection := TdxSkinCollector.Create(Self);
  FSkinCollection.SkinController := SkinController;
  FRegDBPath := GetAppIDClass.RegistryPath + '\Server\ecfsrv';
  FFirstTime := True;
  FLogger := TPCLogWriter.Create(Self);
  FMemLog := TPCMemoryLog.Create(Self);
  FLogger.Target := FMemLog;
  FDMInterface := Self;
  FInternalLogger := TInternalLogger.Create(Self);
  TInternalLoggerHack(FInternalLogger).SetLogger(FLogger);
  FInternalLogger.MainDataModule := Self;
  SetMainDataModule(Self);
  PCMemoryLog.Active := True;
  SkinCollection.Refresh;
  SkinCollection.UseSkin := True;
  SkinCollection.SkinName := 'Office 2007 Blue';
  // SkinController.Kind := lfOffice11;
  ReadConnSettings;
end;

destructor TdmMain.Destroy;
begin
  try
    WriteConnSettings;
  except end;
  PCMemoryLog.Clear;
  PCMemoryLog.Active := False;
  FInternalLogger.Free;
  FLogger.Free;
  FMemLog.Free;
  inherited Destroy;
end;

procedure TdmMain.ReadConnSettings;
begin
  with TRegXML.Create(Self) do
  try
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath, True) then
    begin
      if not ValueExists('SkinName') then
        WriteString('SkinName', SkinCollection.SkinName)
      else
        SkinCollection.SkinName := ReadString('SkinName');
      if not ValueExists('UseSkins') then
        WriteBool('UseSkins', SkinCollection.UseSkin)
      else
        SkinCollection.UseSkin := ReadBool('UseSkins');
      if not SkinController.UseSkins then
        SkinController.Kind := lfOffice11;
      if not ValueExists('Language') then
        WriteString('Language', GetAppIDClass.DefaultLanguangeUsed)
      else
        GetAppIDClass.DefaultLanguangeUsed := ReadString('Language');
    end;
  finally
    Free;
  end;
  UseLanguage(GetAppIDClass.DefaultLanguangeUsed);
end;

procedure TdmMain.WriteConnSettings;
begin
  with TRegXML.Create(Self) do
  try
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath, True) then
    begin
      WriteString('SkinName', SkinCollection.SkinName);
      WriteBool('UseSkins', SkinCollection.UseSkin);
    end;
    Close;
  finally
    Free;
  end;
end;

function TdmMain.GetInternalLogger: TInternalLogger;
begin
  Result := FInternalLogger;
end;

procedure TdmMain.SetMsgID(const Value: Cardinal);
begin
  if Assigned(Application.MainForm) and
    (Application.Mainform is TCSPForm) then
    TCSPForm(Application.MainForm).MessageID := Value;
end;

function TdmMain.GetOpenDialog: TOpenDialog;
begin
  Result := OpenDialog;
end;

function TdmMain.GetSaveDialog: TSaveDialog;
begin
  Result := SaveDialog;
end;

function ExtractParameter(const AString:string; var AOfs:integer):string;
var
   quoted:boolean;
   l:integer;
   dataofs:integer;
begin
     Result:='';
     l:=length(AString);

     // Check if quoted.
     quoted:=(AString[AOfs]='"');
     if quoted then inc(AOfs);
     dataofs:=AOfs;

     while AOfs<=l do
     begin
          // Got quote?
          case AString[AOfs] of
             '"':
              begin
                   // Check if quoted.
                   if quoted then
                   begin
                        // Check for dblquote.
                        if (AOfs<length(AString)) and (AString[AOfs+1]='"') then
                        begin
                             Result:=Result+Copy(AString,dataofs,AOfs-dataofs+1);
                             inc(AOfs,2);
                             dataofs:=AOfs;
                             continue;
                        end;

                        // End quote.
                        Result:=Result+Copy(AString,dataofs,AOfs-dataofs);
                        inc(AOfs);

                        // Check if ; seperator, drop it.
                        if (AOfs<=l) and (AString[AOfs]=';') then inc(AOfs);
                        exit;
                   end;
              end;

             // Got seperator on unquoted string?
             ';':
              if not quoted then
              begin
                   Result:=Result+Copy(AString,dataofs,AOfs-dataofs);
                   inc(AOfs);
                   exit;
              end;
          end;
          inc(AOfs);
     end;

     // Will only come here if non quoted and no remaining ;
     // Check if remaining data, add it.
     Result:=Result+Copy(AString,dataofs,AOfs-1);
end;

end.
