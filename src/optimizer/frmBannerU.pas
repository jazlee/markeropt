unit frmBannerU;
{$I CSPDefs.inc}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, jpeg;

type
  TfrmBanner = class(TForm)
    Image: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    function GetAppVersionInfo: string;
    function OSVerInfo: string;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

var
  frmBanner: TfrmBanner;

implementation
uses
  CSPAppUtil;

{$R *.dfm}

{ TfrmBanner }

procedure TfrmBanner.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

function TfrmBanner.GetAppVersionInfo: string;
var
  Info: Pointer;
  InfoSize: DWORD;
  FileInfo: PVSFixedFileInfo;
  FileInfoSize: DWORD;
  Tmp: DWORD;
  Major1, Major2, Minor1, Minor2: integer;
begin
  // Get the size of the FileVersionInformatioin
  InfoSize := GetFileVersionInfoSize(PChar(Application.ExeName), Tmp);
  // If InfoSize = 0, then the file may not exist, or
  // it may not have file version information in it.
  if InfoSize = 0 then
    raise Exception.Create('Can''t get file version information for '
      + Application.ExeName);
  // Allocate memory for the file version information
  GetMem(Info, InfoSize);
  try
    // Get the information
    GetFileVersionInfo(PChar(Application.ExeName), 0, InfoSize, Info);
    // Query the information for the version
    VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize);
    // Now fill in the version information
    Major1 := FileInfo.dwFileVersionMS shr 16;
    Major2 := FileInfo.dwFileVersionMS and $FFFF;
    Minor1 := FileInfo.dwFileVersionLS shr 16;
    Minor2 := FileInfo.dwFileVersionLS and $FFFF;

    Result := Format('Version: %d.%d.%d (Build %d)', [Major1, Major2, Minor1, Minor2])
  finally
    FreeMem(Info, FileInfoSize);
  end;
end;

procedure TfrmBanner.FormCreate(Sender: TObject);
begin
  Label1.Caption := 'Copyright © 2009. All rights reserved.';
  Label2.Caption := Format('%s'#13#10+'%s',[GetAppVersionInfo, OSVerInfo]);
  {$IFDEF COMMERCIAL}
  Image.Picture.Bitmap.LoadFromResourceName(HInstance, 'SPLASH');
  {$ELSE}
  Image.Picture.Bitmap.LoadFromResourceName(HInstance, 'SPLASH_BAG');
  {$ENDIF}
end;

function TfrmBanner.OSVerInfo: string;
const
  BUILDSTR = 'Build %d %s';
  PLATFORM_W9x = 'Windows 9x';
  PLATFORM_NT  = 'Windows NT';
var
  VersionInfo: Windows.OSVERSIONINFO;
  Build: String;
begin
  ZeroMemory(@VersionInfo, SizeOf(VersionInfo));
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);

  with VersionInfo do
  begin
    if dwPlatformID = VER_PLATFORM_WIN32_NT then
    begin
      build := Format (BUILDSTR, [LoWord(dwBuildNumber), szCSDVersion]);
      Result := Format('%s %d.%d (%s)', [PLATFORM_NT, dwMajorVersion, dwMinorVersion, Build]);
    end
    else
      Result := Format('%s', [PLATFORM_W9X]);
  end;
end;

end.
