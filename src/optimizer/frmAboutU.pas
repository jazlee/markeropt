unit frmAboutU;
{$I CSPDefs.inc}
interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: frmAboutU.pas
//       Author: Jaimy Azle (jaimy@usg.co.id)
//    Date Code: 02.07.2003
// ========================================================================
// Source Owner: PT. Ungaran Sari Garments, 2003 IT Department
//    Copyright: Seluruh isi dari file ini dilindungi oleh undang-undang
//               hak cipta dan kesepakatan internasional. Segala bentuk
//               reproduksi, reverse-engineering, dan distribusi atas
//               seluruh atau bagian apapun dari kode yang ada dalam file
//               ini tanpa ijin tertulis merupakan pelanggaran hukum dan
//               akan dituntut ke pengadilan dengan sanksi maksimum yang
//               ada.
//
//  Restriction: SEGALA BENTUK KODE SUMBER (SOURCE CODE) YANG TERDAPAT DALAM
//               DISTRIBUSI YANG MENGHASILKAN KODE INTERMEDIATE (DCU, OBJ,
//               SO, DAN LAIN-LAIN) MERUPAKAN ASSET PENTING DAN RAHASIA
//               PT. UNGARAN SARI GARMENTS DAN HANYA UNTUK DIGUNAKAN DALAM
//               LINGKUP INTERNAL PT. UNGARAN SARI GARMENTS. TIDAK DIIJINKAN
//               KEPADA PIHAK LUAR UNTUK MEMBAWA FILE SOURCE CODE INI,
//               ATAUPUN BAGIAN-BAGIAN DARI FILE SOURCE, MAUPUN FILE
//               INTERMEDIATE YANG DIHASILKAN OLEH KOMPILER DALAM MEDIA APAPUN
//               KELUAR DARI LINGKUNGAN PT. UNGARAN SARI GARMENTS.
//
// Code Version: (3rd Generation Code)
// ========================================================================
//  Description:
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls, cxControls, cxContainer,
  cxEdit, cxImage, cxTextEdit, cxMemo, Menus, cxLookAndFeelPainters,
  cxButtons;

type
  TfrmAbout = class(TForm)
    Bevel1: TBevel;
    Label4: TLabel;
    Button2: TcxButton;
    Label1: TLabel;
    Bevel2: TBevel;
    Label3: TLabel;
    Image1: TcxImage;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FCaption: string;

    function GetAppVersionInfo(): string;
    function OSVerInfo: string;
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation
uses
  ShellAPI, CSPConsts, CSPAppUtil, gnugettext, ecflangutil, frmMainU;

{$R *.dfm}

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  AStr: string;
begin
  ReconstructLanguage(Self);
  FCaption := Label1.Caption;
  {$IFDEF COMMERCIAL}
  Image1.Picture.Bitmap.LoadFromResourceName(HInstance, 'ABOUT');
  AStr := EmptyStr;
  if not frmMain.LicenseManager.TrialMode then
    AStr := Format(#13#10'Registered to: %s (%s)',
      [frmMain.LicenseManager.RegisteredName,
       frmMain.LicenseManager.RegisteredCompany]);
  Label1.Caption := Format(FCaption,
    [Application.Title,GetAppVersionInfo, AStr,
      OSVerInfo]);
  {$ELSE}
  Image1.Picture.Bitmap.LoadFromResourceName(HInstance, 'ABOUT_BAG');
  Label1.Caption := Format(FCaption,
    [Application.Title,GetAppVersionInfo,GetAppIDClass.ProjectLeader,
      OSVerInfo]);
  {$ENDIF}
end;

procedure TfrmAbout.Button3Click(Sender: TObject);
var
  sContact: string;
begin
  sContact := Format(
            'mailto:%s?Subject=URGENT: Need Help On %s',
            ['support@usg.co.id', GetAppIDClass.ProjectName]);
  sContact := sContact + '&BODY=Hi,%0D%0A%0D%0A';
  ShellExecute(0,'Open',PChar(sContact),nil,nil,SW_SHOW);
end;

function TfrmAbout.GetAppVersionInfo: string;
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


function TfrmAbout.OSVerInfo: string;
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
