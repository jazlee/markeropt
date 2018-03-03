unit CSPAppUtil;

interface
uses
  Windows, Messages, TPropertiesUnit;

type
  TECFAppGUID = class
  private
    FInitialized: boolean;
    FAppGUID: TGUID;
    FProjectLeader: string;
    FCategory: string;
    FProjectName: string;
    FTempPath: string;
    FRegistryBasePath: string;
    FTitle: string;
    FAppPath: string;
    FCommonDataPath: string;
    FCommonEnvPath: string;
    FCommonReportPath: string;
    FDefLang: string;
    FProperties: TProperties;
    FSystemDrive: string;
    FWinDir: string;
    FCommonFiles32Dir: string;
    FProgramFiles32Dir: string;
    FCommonFiles64Dir: string;
    FWinSystemDir: string;
    FProgramFiles64Dir: string;
    FWinSysWow64Dir: string;
    FIsWin64: boolean;
    FIsNT: boolean;
    FSysUserInfoName: string;
    FUserEnvPath: string;
    FUserDataPath: string;
    FUserReportPath: string;
    FAppReportPath: string;
    procedure SetAppGUID(const Value: TGUID);
    function GetRegPath: string;
    procedure SetTitle(const Value: string);
    procedure InitIsWin64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetupAppDirectory;

    procedure ReadSettings;
    procedure WriteSettings;

    property DefaultLanguangeUsed: string read FDefLang write FDefLang;
    property ProjectCategory: string read FCategory write FCategory;
    property ProjectName: string read FProjectName write FProjectName;
    property ProjectLeader: string read FProjectLeader write FProjectLeader;
    property Title: string read FTitle write SetTitle;
    property RegistryBasePath: string read FRegistryBasePath;
    property RegistryPath: string read GetRegPath;
    property TempPath: string read FTempPath;
    property CommonEnvPath: string read FCommonEnvPath;
    property CommonDataPath: string read FCommonDataPath;
    property CommonReportPath: string read FCommonReportPath;
    property CommonFiles32Dir: string read FCommonFiles32Dir;
    property CommonFiles64Dir: string read FCommonFiles64Dir;
    property UserEnvPath: string read FUserEnvPath;
    property UserDataPath: string read FUserDataPath;
    property UserReportPath: string read FUserReportPath;
    property WinDir: string read FWinDir;
    property WinSystemDir: string read FWinSystemDir;
    property WinSysWow64Dir: string read FWinSysWow64Dir;
    property SystemDrive: string read FSystemDrive;
    property ProgramFiles32Dir: string read FProgramFiles32Dir;
    property ProgramFiles64Dir: string read FProgramFiles64Dir;
    property SysUserInfoName: string read FSysUserInfoName;
    property IsWin64: boolean read FIsWin64;
    property IsNT: boolean read FIsNT;

    property ApplicationPath: string read FAppPath;
    property ApplicationReportPath: string read FAppReportPath;
    property ApplicationID: TGUID read FAppGUID write SetAppGUID;
    property Properties: TProperties read FProperties;
  end;

  TECFWindowsVersion =
   (wvUnknown, wvWin95, wvWin95OSR2, wvWin98, wvWin98SE, wvWinME,
    wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4, wvWin2000, wvWinXP,
    wvWin2003, wvWinXP64, wvWin2003R2, wvWinVista, wvWinServer2008);

const
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_INTEL}
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_AMD64}
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_IA32_ON_WIN64}
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_IA64}

function GetApplicationID: TGUID;
function AppIDInitialized: Boolean;
procedure SetApplicationID(Value: TGUID);
function GetAppIDClass: TECFAppGUID;
function GetWindowsVersion: TECFWindowsVersion;
function GetNativeSystemInfo(var SystemInfo: TSystemInfo): Boolean;
function GetAppVersionInfo: string;
function TrimAll(const AValue: string): string;

procedure AppIDErrorInit;

const
  VER_NT_WORKSTATION       = $0000001;
  {$EXTERNALSYM VER_NT_WORKSTATION}


resourcestring
  SECFDataDir                        = 'data';
  SECFReportDir                      = 'reports';

implementation
uses
  SysUtils, Classes, Forms, SHFolder, ShellAPI, ShlObj,
  CSPConsts, NativeRegXML;

type
  TRegView = (rvDefault, rv32Bit, rv64Bit);
  TShellFolderID = (sfDesktop, sfStartMenu, sfPrograms, sfStartup, sfSendTo,
    sfFonts, sfAppData, sfDocs, sfTemplates, sfFavorites, sfLocalAppData);

const
  RegViews64Bit = [rv64Bit];
  PathSlash = ['\', '/'];
  NEWREGSTR_PATH_SETUP = 'Software\Microsoft\Windows\CurrentVersion';

{$IFNDEF DELPHI12}
type
  POSVersionInfoEx = ^TOSVersionInfoEx;
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array [0..127] of CHAR;     // Maintenance string for PSS usage
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
{$IFNDEF DELPHI11}
const
  KEY_WOW64_64KEY        = $0100;
  {$EXTERNALSYM KEY_WOW64_64KEY}
{$ENDIF}
{$ENDIF}  

var
  InternalAppID: TECFAppGUID;
  FsRedirectionFunctionsAvailable: Boolean;
  Wow64DisableWow64FsRedirectionFunc: function(var OldValue: Pointer): BOOL; stdcall;
  Wow64RevertWow64FsRedirectionFunc: function(OldValue: Pointer): BOOL; stdcall;

function TrimAll(const AValue: string): string;
var
  nLen, i: integer;
  ch: char;
begin
  Result := EmptyStr;
  if AValue = '' then exit;
  nLen := Length(AValue);
  for i := 1 to nLen do
  begin
    ch := AValue[i];
    if ch <> #32 then
      Result := Result + ch;
  end;
end;  

function UsingWinNT: Boolean;
{ Returns True if system is running any version of Windows NT. Never returns
  True on Windows 95 or 3.1. }
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

function PathExpand(const Filename: String): String;
{ Like Delphi's ExpandFileName, but does proper error checking. }
var
  Res: Integer;
  FilePart: PChar;
  Buf: array[0..4095] of Char;
begin
  DWORD(Res) := GetFullPathName(PChar(Filename), SizeOf(Buf), Buf, FilePart);
  if (Res > 0) and (Res < SizeOf(Buf)) then
    SetString(Result, Buf, Res)
  else
    Result := Filename;
end;

function AdjustLength(var S: String; const Res: Cardinal): Boolean;
{ Returns True if successful. Returns False if buffer wasn't large enough,
  and called AdjustLength to resize it. }
begin
  Result := Integer(Res) < Length(S);
  SetLength(S, Res);
end;

function PathCharLength(const S: String; const Index: Integer): Integer;
{ Returns the length in bytes of the character at Index in S.
  Notes:
  1. If Index specifies the last character in S, 1 will always be returned,
     even if the last character is a lead byte.
  2. If a lead byte is followed by a null character (e.g. #131#0), 2 will be
     returned. This mimics the behavior of MultiByteToWideChar and CharPrev,
     but not CharNext(P)-P, which would stop on the null. }
begin
  if IsDBCSLeadByte(Ord(S[Index])) and (Index < Length(S)) then
    Result := 2
  else
    Result := 1;
end;

function PathLastChar(const S: String): PChar;
{ Returns pointer to last character in the string. Is MBCS-aware. Returns nil
  if the string is empty. }
begin
  if S = '' then
    Result := nil
  else
    Result := CharPrev(Pointer(S), @S[Length(S)+1]);
end;

function AddBackslash(const S: String): String;
{ Returns S plus a trailing backslash, unless S is an empty string or already
  ends in a backslash/slash. }
begin
  {$IFDEF UNICODE}
  if (S <> '') and not CharInSet(PathLastChar(S)^, PathSlash) then
  {$ELSE}
  if (S <> '') and not(PathLastChar(S)^ in PathSlash) then
  {$ENDIF}
    Result := S + '\'
  else
    Result := S;
end;

function PathDrivePartLengthEx(const Filename: String;
  const IncludeSignificantSlash: Boolean): Integer;
{ Returns length of the drive portion of Filename, or 0 if there is no drive
  portion.
  If IncludeSignificantSlash is True, the drive portion can include a trailing
  slash if it is significant to the meaning of the path (i.e. 'x:' and 'x:\'
  are not equivalent, nor are '\' and '').
  If IncludeSignificantSlash is False, the function works as follows:
    'x:file'              -> 2  ('x:')
    'x:\file'             -> 2  ('x:')
    '\\server\share\file' -> 14 ('\\server\share')
    '\file'               -> 0  ('')
  If IncludeSignificantSlash is True, the function works as follows:
    'x:file'              -> 2  ('x:')
    'x:\file'             -> 3  ('x:\')
    '\\server\share\file' -> 14 ('\\server\share')
    '\file'               -> 1  ('\')
  Note: This is MBCS-safe, unlike the Delphi's ExtractFileDrive function.
  (Computer and share names can include multi-byte characters!) }
var
  Len, I, C: Integer;
begin
  Len := Length(Filename);

  { \\server\share }
  {$IFDEF UNICODE}
  if (Len >= 2) and (CharInSet(Filename[1], PathSlash)) and (CharInSet(Filename[2], PathSlash)) then begin
  {$ELSE}
  if (Len >= 2) and (Filename[1] in PathSlash) and (Filename[2] in PathSlash) then begin
  {$ENDIF}
    I := 3;
    C := 0;
    while I <= Len do begin
      {$IFDEF UNICODE}
      if CharInSet(Filename[I], PathSlash) then begin
      {$ELSE}
      if Filename[I] in PathSlash then begin
      {$ENDIF}
        Inc(C);
        if C >= 2 then
          Break;
        repeat
          Inc(I);
          { And skip any additional consecutive slashes: }
        {$IFDEF UNICODE}
        until (I > Len) or not(CharInSet(Filename[I], PathSlash));
        {$ELSE}
        until (I > Len) or not(Filename[I] in PathSlash);
        {$ENDIF}
      end
      else
        Inc(I, PathCharLength(Filename, I));
    end;
    Result := I - 1;
    Exit;
  end;

  { \ }
  { Note: Test this before 'x:' since '\:stream' means access stream 'stream'
    on the root directory of the current drive, not access drive '\:' }
  {$IFDEF UNICODE}
  if (Len >= 1) and (CharInSet(Filename[1], PathSlash)) then begin
  {$ELSE}
  if (Len >= 1) and (Filename[1] in PathSlash) then begin
  {$ENDIF}
    if IncludeSignificantSlash then
      Result := 1
    else
      Result := 0;
    Exit;
  end;

  { x: }
  if Len > 0 then begin
    I := PathCharLength(Filename, 1) + 1;
    if (I <= Len) and (Filename[I] = ':') then begin
      {$IFDEF UNICODE}
      if IncludeSignificantSlash and (I < Len) and (CharInSet(Filename[I+1], PathSlash)) then
      {$ELSE}
      if IncludeSignificantSlash and (I < Len) and (Filename[I+1] in PathSlash) then
      {$ENDIF}
        Result := I+1
      else
        Result := I;
      Exit;
    end;
  end;

  Result := 0;
end;

function PathDrivePartLength(const Filename: String): Integer;
begin
  Result := PathDrivePartLengthEx(Filename, False);
end;

function PathExtractDrive(const Filename: String): String;
{ Returns the drive portion of Filename (either 'x:' or '\\server\share'),
  or an empty string if there is no drive portion. }
var
  L: Integer;
begin
  L := PathDrivePartLength(Filename);
  if L = 0 then
    Result := ''
  else
    Result := Copy(Filename, 1, L);
end;

function RemoveBackslashUnlessRoot(const S: String): String;
{ Returns S minus any trailing slashes, unless S specifies the root directory
  of a drive (i.e. 'C:\' or '\'), in which case it leaves 1 slash. }
var
  DrivePartLen, I: Integer;
begin
  DrivePartLen := PathDrivePartLengthEx(S, True);
  I := Length(S);
  {$IFDEF UNICODE}
  while (I > DrivePartLen) and (CharInSet(CharPrev(Pointer(S), @S[I+SizeOf(Char)])^ , PathSlash)) do
  {$ELSE}
  while (I > DrivePartLen) and (CharPrev(Pointer(S), @S[I+1])^ in PathSlash) do
  {$ENDIF}
    Dec(I);
  if I = Length(S) then
    Result := S
  else
    Result := Copy(S, 1, I);
end;

function InternalGetFileAttr(const Name: String): Integer;
begin
  Result := GetFileAttributes(PChar(RemoveBackslashUnlessRoot(Name)));
end;

function DirExists(const Name: String): Boolean;
{ Returns True if the specified directory name exists. The specified name
  may include a trailing backslash.
  NOTE: Delphi's FileCtrl unit has a similar function called DirectoryExists.
  However, the implementation is different between Delphi 1 and 2. (Delphi 1
  does not count hidden or system directories as existing.) }
var
  Attr: Integer;
begin
  Attr := InternalGetFileAttr(Name);
  Result := (Attr <> -1) and (Attr and faDirectory <> 0);
end;

function GetEnv(const EnvVar: String): String;
{ Gets the value of the specified environment variable. (Just like TP's GetEnv) }
var
  Res: DWORD;
begin
  SetLength(Result, 255);
  repeat
    Res := GetEnvironmentVariable(PChar(EnvVar), PChar(Result), Length(Result));
    if Res = 0 then begin
      Result := '';
      Break;
    end;
  until AdjustLength(Result, Res);
end;

function GetWinDir: String;
{ Returns fully qualified path of the Windows directory. Only includes a
  trailing backslash if the Windows directory is the root directory. }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetWindowsDirectory(Buf, SizeOf(Buf));
  Result := StrPas(Buf);
end;

function GetSystemDir: String;
{ Returns fully qualified path of the Windows System directory. Only includes a
  trailing backslash if the Windows System directory is the root directory. }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetSystemDirectory(Buf, SizeOf(Buf));
  Result := StrPas(Buf);
end;

function GetSysWow64Dir: String;
{ Returns fully qualified path of the SysWow64 directory on 64-bit Windows.
  Returns '' if there is no SysWow64 directory (e.g. running 32-bit Windows). }
var
  GetSystemWow64DirectoryFunc: function(lpBuffer: PAnsiChar;
    uSize: UINT): UINT; stdcall;
  Res: Integer;
  Buf: array[0..MAX_PATH] of AnsiChar;
begin
  Result := '';
  GetSystemWow64DirectoryFunc := GetProcAddress(GetModuleHandle(kernel32),
    'GetSystemWow64DirectoryA');
  { Note: This function does exist on 32-bit XP, but always returns 0 }
  if Assigned(GetSystemWow64DirectoryFunc) then begin
    Res := GetSystemWow64DirectoryFunc(Buf, SizeOf(Buf) div SizeOf(Buf[0]));
    if (Res > 0) and (Res < SizeOf(Buf) div SizeOf(Buf[0])) then
      Result := String(Buf);
  end;
end;

function RegOpenKeyExView(const RegView: TRegView; hKey: HKEY; lpSubKey: PChar;
  ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY): Longint;
begin
  if RegView = rv64Bit then
    samDesired := samDesired or KEY_WOW64_64KEY;
  Result := RegOpenKeyEx(hKey, lpSubKey, ulOptions, samDesired, phkResult);
end;

function InternalRegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String;
  Type1, Type2: DWORD): Boolean;
var
  Typ, Size: DWORD;
  S: String;
  ErrorCode: Longint;
label 1;
begin
  Result := False;
1:Size := 0;
  if (RegQueryValueEx(H, Name, nil, @Typ, nil, @Size) = ERROR_SUCCESS) and
     ((Typ = Type1) or (Typ = Type2)) then begin
    if Size = 0 then begin
      { It's an empty string with no null terminator.
        (Must handle those here since we can't pass a nil lpData pointer on
        the second RegQueryValueEx call.) }
      ResultStr := '';
      Result := True;
    end
    else begin
      SetString(S, nil, Size);
      ErrorCode := RegQueryValueEx(H, Name, nil, @Typ, @S[1], @Size);
      if ErrorCode = ERROR_MORE_DATA then begin
        { The data must've increased in size since the first RegQueryValueEx
          call. Start over. }
        goto 1;
      end;
      if (ErrorCode = ERROR_SUCCESS) and
         ((Typ = Type1) or (Typ = Type2)) then begin
        { Remove any null terminators from the end and trim the string to the
          returned Size.
          Note: We *should* find 1 null terminator, but it's possible for
          there to be more or none if the value was written that way. }
        while (Size <> 0) and (S[Size] = #0) do
          Dec(Size);
        { In a REG_MULTI_SZ value, each individual string is null-terminated,
          so add 1 null (back) to the end, unless there are no strings (Size=0) }
        if (Typ = REG_MULTI_SZ) and (Size <> 0) then
          Inc(Size);
        SetLength(S, Size);
        if (Typ = REG_MULTI_SZ) and (Size <> 0) then
          S[Size] := #0;
        ResultStr := S;
        Result := True;
      end;
    end;
  end;
end;

function RegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
{ Queries the specified REG_SZ or REG_EXPAND_SZ registry key/value, and returns
  the value in ResultStr. Returns True if successful. When False is returned,
  ResultStr is unmodified. }
begin
  Result := InternalRegQueryStringValue(H, Name, ResultStr, REG_SZ,
    REG_EXPAND_SZ);
end;

function GetTempDir: String;
{ Returns fully qualified path of the temporary directory, with trailing
  backslash. This does not use the Win32 function GetTempPath, due to platform
  differences. }
label 1;
begin
  Result := GetEnv('TMP');
  if (Result <> '') and DirExists(Result) then
    goto 1;
  Result := GetEnv('TEMP');
  if (Result <> '') and DirExists(Result) then
    goto 1;
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    { Like Windows 2000's GetTempPath, return USERPROFILE when TMP and TEMP
      are not set }
    Result := GetEnv('USERPROFILE');
    if (Result <> '') and DirExists(Result) then
      goto 1;
  end;
  Result := GetWinDir;
1:Result := AddBackslash(PathExpand(Result));
end;

function GetAppVersionInfo: string;
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

    Result := Format('%d.%d.%d (Build %d)', [Major1, Major2, Minor1, Minor2])
  finally
    FreeMem(Info, FileInfoSize);
  end;
end;

function GetEnvVarValue(const VarName: string): string;
  {Returns the value for the given environment variable or '' if the variable
  does not exist}
var
  BufSize: Integer;  // buffer size required for value (including terminal #0)
begin
  // Get required buffer size (includes space for terminal #0)
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  // Read env var value into result string
  SetLength(Result, BufSize - 1); // space for #0 automatically added
  GetEnvironmentVariable(PChar(VarName), PChar(Result), BufSize);
end;

function AreFsRedirectionFunctionsAvailable: Boolean;
begin
  Result := FsRedirectionFunctionsAvailable;
end;

function GetApplicationID: TGUID;
begin
  Result := InternalAppID.ApplicationID;
end;

function AppIDInitialized: Boolean;
begin
  Result := InternalAppID.FInitialized;
end;

procedure AppIDErrorInit;
begin
  raise Exception.Create('ApplicationID has not been initialized');
end;

procedure SetApplicationID(Value: TGUID);
begin
  InternalAppID.ApplicationID := Value;
end;

function GetAppIDClass: TECFAppGUID;
begin
  Result := InternalAppID;
end;

function GetNativeSystemInfo(var SystemInfo: TSystemInfo): Boolean;
type
  TGetNativeSystemInfo = procedure (var SystemInfo: TSystemInfo) stdcall;
var
  LibraryHandle: HMODULE;
  _GetNativeSystemInfo: TGetNativeSystemInfo;
begin
  Result := False;
  LibraryHandle := GetModuleHandle(kernel32);

  if LibraryHandle <> 0 then
  begin
    _GetNativeSystemInfo := GetProcAddress(LibraryHandle,'GetNativeSystemInfo');
    if Assigned(_GetNativeSystemInfo) then
    begin
      _GetNativeSystemInfo(SystemInfo);
      Result := True;
    end
    else
      GetSystemInfo(SystemInfo);
  end
  else
    GetSystemInfo(SystemInfo);
end;

var
  KernelVersionHi: DWORD;

function GetWindowsVersion: TECFWindowsVersion;
var
  TrimmedWin32CSDVersion: string;
  SystemInfo: TSystemInfo;
  OSVersionInfoEx: TOSVersionInfoEx;
  OSVersionInfo: POSVersionInfo;
const
  SM_SERVERR2 = 89;
begin
  Result := wvUnknown;
  TrimmedWin32CSDVersion := Trim(Win32CSDVersion);
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      case Win32MinorVersion of
        0..9:
          if (TrimmedWin32CSDVersion = 'B') or (TrimmedWin32CSDVersion = 'C') then
            Result := wvWin95OSR2
          else
            Result := wvWin95;
        10..89:
          // On Windows ME Win32MinorVersion can be 10 (indicating Windows 98
          // under certain circumstances (image name is setup.exe). Checking
          // the kernel version is one way of working around that.
          if KernelVersionHi = $0004005A then // 4.90.x.x
            Result := wvWinME
          else
          if (TrimmedWin32CSDVersion = 'A') or (TrimmedWin32CSDVersion = 'B') then
            Result := wvWin98SE
          else
            Result := wvWin98;
        90:
          Result := wvWinME;
      end;
    VER_PLATFORM_WIN32_NT:
      case Win32MajorVersion of
        3:
          case Win32MinorVersion of
            1:
              Result := wvWinNT31;
            5:
              Result := wvWinNT35;
            51:
              Result := wvWinNT351;
          end;
        4:
          Result := wvWinNT4;
        5:
          case Win32MinorVersion of
            0:
              Result := wvWin2000;
            1:
              Result := wvWinXP;
            2:
              begin
                OSVersionInfo := @OSVersionInfoEx;
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                GetNativeSystemInfo(SystemInfo);
                if GetSystemMetrics(SM_SERVERR2) <> 0 then
                  Result := wvWin2003R2
                else
                if (SystemInfo.wProcessorArchitecture <> PROCESSOR_ARCHITECTURE_INTEL) and
                  GetVersionEx(OSVersionInfo^) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWinXP64
                else
                  Result := wvWin2003;
              end;
          end;
        6:
          if Win32MinorVersion = 0 then
          begin
            OSVersionInfo := @OSVersionInfoEx;
            OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
            if GetVersionEx(OSVersionInfo^) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
              Result := wvWinVista
            else
              Result := wvWinServer2008;
          end;
      end;
  end;
end;

procedure CheckDir(ADir: string);
begin
  try
    if not DirectoryExists(ADir) then
      ForceDirectories(ADir);
  except; end;
end;

function GetTempPath: string;
var
  TmpDir: PChar;
begin
  TmpDir := StrAlloc(MAX_PATH);
  Windows.GetTempPath(MAX_PATH, TmpDir);
  Result := string(TmpDir);
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
  StrDispose(TmpDir);
end;


{ TAppGUID }
constructor TECFAppGUID.Create;
  function GetPath(const RegView: TRegView; const Name: PChar): String;
  var
    H: HKEY;
  begin
    if RegOpenKeyExView(RegView, HKEY_LOCAL_MACHINE, NEWREGSTR_PATH_SETUP, 0,
       KEY_QUERY_VALUE, H) = ERROR_SUCCESS then begin
      if not RegQueryStringValue(H, Name, Result) then
        Result := '';
      RegCloseKey(H);
    end
    else
      Result := '';
  end;
begin
  FInitialized := False;
  CreateGUID(FAppGUID);
  FProjectLeader := 'Jaimy Azle';
  FRegistryBasePath := '\SOFTWARE\';
  FDefLang := 'en';
  FProperties := TProperties.Create;
  Properties.SetProperty('splashscreen','true');
  Properties.SetProperty('multiinstance','true');
  Properties.SetProperty('ribbonbar','true');

  Properties.SetProperty('dsnext', DSNFileExt);
  Properties.SetProperty('dspext', DSPFileExt);
  Properties.SetProperty('btfext',BTFFileExt);
  Properties.SetProperty('xmlext',XMLFileExt);
  Properties.SetProperty('nestapp',NESTAppFileName);
  Properties.SetProperty('nesttime',IntToStr(DEF_NESTING_TIME));

  InitIsWin64;
  FIsNT := UsingWinNT;
  FWinDir := GetWinDir;
  FWinSystemDir := GetSystemDir;
  FWinSysWow64Dir := GetSysWow64Dir;
  { Get system drive }
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    FSystemDrive := GetEnv('SystemDrive')  {don't localize}
  else
    FSystemDrive := '';
  if FSystemDrive = '' then begin
    FSystemDrive := PathExtractDrive(WinDir);
    if FSystemDrive = '' then
      { In some rare case that PathExtractDrive failed, just default to C }
      FSystemDrive := 'C:';
  end;
  { Get 32-bit Program Files and Common Files dirs }
  FProgramFiles32Dir := GetPath(rv32Bit, 'ProgramFilesDir');
  if FProgramFiles32Dir = '' then
    FProgramFiles32Dir := SystemDrive + '\Program Files';  {don't localize}
  FCommonFiles32Dir := GetPath(rv32Bit, 'CommonFilesDir');
  if FCommonFiles32Dir = '' then
    FCommonFiles32Dir := AddBackslash(ProgramFiles32Dir) + 'Common Files';  {don't localize}
  { Get 64-bit Program Files and Common Files dirs }
  if IsWin64 then begin
    FProgramFiles64Dir := GetPath(rv64Bit, 'ProgramFilesDir');
    FCommonFiles64Dir := GetPath(rv64Bit, 'CommonFilesDir');
  end;
end;


destructor TECFAppGUID.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TECFAppGUID.GetRegPath: string;
begin
  Result := Format('%s\%s\%s',
    [ FRegistryBasePath, FCategory, ProjectName ]);
end;

procedure TECFAppGUID.InitIsWin64;
const
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
var
  KernelModule: HMODULE;
  GetNativeSystemInfoFunc: procedure(var lpSystemInfo: TSystemInfo); stdcall;
  IsWow64ProcessFunc: function(hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;
  Wow64Process: BOOL;
  SysInfo: TSystemInfo;
begin
  { The system is considered a "Win64" system if all of the following
    conditions are true:
    1. GetNativeSystemInfo is available.
    2. IsWow64Process is available, and returns True for the current process.
    3. Wow64DisableWow64FsRedirection is available.
    4. Wow64RevertWow64FsRedirection is available.
    5. GetSystemWow64DirectoryA is available.
    6. RegDeleteKeyExA is available.
    The system does not have to be one of the known 64-bit architectures
    (AMD64, IA64) to be considered a "Win64" system. }

  FIsWin64 := False;
  KernelModule := GetModuleHandle(kernel32);
  GetNativeSystemInfoFunc := GetProcAddress(KernelModule, 'GetNativeSystemInfo');
  if Assigned(GetNativeSystemInfoFunc) then begin
    GetNativeSystemInfoFunc(SysInfo);
    IsWow64ProcessFunc := GetProcAddress(KernelModule, 'IsWow64Process');
    if Assigned(IsWow64ProcessFunc) and
       IsWow64ProcessFunc(GetCurrentProcess, Wow64Process) and
       Wow64Process then begin
      if AreFsRedirectionFunctionsAvailable and
         (GetProcAddress(KernelModule, 'GetSystemWow64DirectoryA') <> nil) and
         (GetProcAddress(GetModuleHandle(advapi32), 'RegDeleteKeyExA') <> nil) then
        FIsWin64 := True;
    end;
  end
  else
    GetSystemInfo(SysInfo);
end;

procedure TECFAppGUID.ReadSettings;
var
  I: integer;
  AProps: TStringList;
begin
  with TRegXML.Create(nil) do
  try
    Open(UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath, True) then
    begin
      if not ValueExists('Language') then
        WriteString('Language', GetAppIDClass.DefaultLanguangeUsed)
      else
        GetAppIDClass.DefaultLanguangeUsed := ReadString('Language');
      AProps := TStringList(Properties.GetPropertyNames);
      try
        for I := 0 to AProps.Count - 1 do
          if not ValueExists(AProps[I]) then
            WriteString(AProps[I], Properties.GetProperty(AProps[I]))
          else
            Properties.SetProperty(AProps[I], ReadString(AProps[I]));
      finally
        AProps.Free;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TECFAppGUID.SetAppGUID(const Value: TGUID);
begin
  if FInitialized then
    raise Exception.Create('GUID has been assigned');
  FAppGUID := Value;
  FInitialized := True;
end;

procedure TECFAppGUID.SetTitle(const Value: string);
begin
  if FTitle <> value then
  begin
    FTitle := Value;
    Application.Title := FTitle;
  end;
end;

function GetCSIDLShellFolder(CSIDLFolder : integer) : string;
begin
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderPath(0, PChar(Result), CSIDLFolder, True);
  SetLength(Result, StrLen(PChar(Result)));
  if (Result <> '') then
    Result := IncludeTrailingBackslash(Result);
end;

procedure TECFAppGUID.SetupAppDirectory;
var
  AWinVer: TECFWindowsVersion;
begin
  FAppPath := ExtractFilePath(ParamStr(0));
  FTempPath := GetTempPath;
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    FCommonEnvPath := FCommonFiles32Dir +'\' + FCategory
  else if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    AWinVer := GetWindowsVersion;
    if AWinVer > wvWinNT4 then
      FCommonEnvPath := GetCSIDLShellFolder(CSIDL_COMMON_APPDATA) + FCategory
    else
      FCommonEnvPath := GetEnvVarValue('CommonProgramFiles') +'\' + FCategory;
  end;
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    FUserEnvPath := FCommonFiles32Dir +'\' + FCategory
  else if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    AWinVer := GetWindowsVersion;
    if AWinVer > wvWinNT4 then
      FUserEnvPath := GetCSIDLShellFolder(CSIDL_APPDATA) + FCategory
    else
      FUserEnvPath := GetEnvVarValue('CommonProgramFiles') +'\' + FCategory;
  end;
  CheckDir(FUserEnvPath);
  FCommonDataPath := FCommonEnvPath + '\' + SECFDataDir;
  FCommonReportPath := FCommonEnvPath + '\' + SECFReportDir;
  FUserDataPath := FUserEnvPath + '\' + SECFDataDir;
  FUserReportPath := FUserEnvPath + '\' + SECFReportDir;
  FAppReportPath := FAppPath + '\' + SECFReportDir;
  CheckDir(FCommonReportPath);
  CheckDir(FCommonDataPath);
  CheckDir(FUserReportPath);
  CheckDir(FUserDataPath);
  CheckDir(FAppReportPath);
end;

procedure TECFAppGUID.WriteSettings;
var
  I: integer;
  AProps: TStringList;
begin
  with TRegXML.Create(nil) do
  try
    Open(UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath, True) then
    begin
      WriteString('Language', GetAppIDClass.DefaultLanguangeUsed);
      AProps := TStringList(Properties.GetPropertyNames);
      try
        for I := 0 to AProps.Count - 1 do
          WriteString(AProps[I], Properties.GetProperty(AProps[I]))
      finally
        AProps.Free;
      end;
      Close;
    end;
  finally
    Free;
  end;
end;

initialization
  Wow64DisableWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64DisableWow64FsRedirection');
  Wow64RevertWow64FsRedirectionFunc := GetProcAddress(GetModuleHandle(kernel32),
    'Wow64RevertWow64FsRedirection');
  FsRedirectionFunctionsAvailable := Assigned(Wow64DisableWow64FsRedirectionFunc) and
    Assigned(Wow64RevertWow64FsRedirectionFunc);
  InternalAppID := TECFAppGUID.Create;

finalization
  InternalAppID.Free;

end.

