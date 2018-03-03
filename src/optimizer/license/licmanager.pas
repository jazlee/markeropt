unit licmanager;

interface

uses
  SysUtils, Classes, Forms, Windows, Controls;

{$I CSPDefs.inc}  

type
  TCSPLicenseManager = class(TComponent)
  private
    FTrialMode: boolean;
    FRegName: string;
    FRegCompany: string;
    FCustomData: string;
  protected
    procedure DoTerminateApp;
    procedure DoCheckLicense;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Loaded; override;

    property TrialMode: boolean read FTrialMode;
    property RegisteredName: string read FRegName;
    property RegisteredCompany: string read FRegCompany;
    property RegisteredCustomData: string read FCustomData;
  end;

implementation

{$IFDEF COMMERCIAL}
uses
  WinlicenseSDK, frmRegistrationU;

resourcestring
  swLicenseRequired         = 'A valid license key is required to use this software';
  swlInvalidHardwareLicense = 'This software was licensed but not for this machine';
  swlNoMoreHwdChanges       = 'Sorry, No more HW-ID changes allowed. Please re-register this application with new key';
  swlTrialExpired           = 'Your trial period for this software is expired, please register';
  swlInvalidLicense         = 'Your license key is invalid, please supply with a valid license key';
  swlLockedMachineLicense   = 'The key that you entered is locked to different machine';
  swlNDaysLeft              = 'You have %d days left for trying this application';
  swlLicenseExpired         = 'Your license is currently expired, please re-register this application with new key';

{$ENDIF}              

{ TCSPLicenseManager }

constructor TCSPLicenseManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCSPLicenseManager.Destroy;
begin
  inherited Destroy;
end;

procedure TCSPLicenseManager.DoCheckLicense;
{$IFDEF COMMERCIAL}
var
  PStatus, PExtStatus: integer;
  AMsg: string;
  ARegName: array [0..255] of char;
  ACompanyName: array [0..255] of char;
  ACustomData: array [0..255] of char;
begin
{$I Encode_Start.inc}
  FTrialMode := False;
  PStatus := WLRegGetStatus(PExtStatus);
  if PStatus <> 1 then
  begin
    case PStatus of
      0: AMsg := swLicenseRequired;
      2: AMsg := swlInvalidLicense;
      3: AMsg := swlLockedMachineLicense;
      4: AMsg := swlNoMoreHwdChanges;
      5: AMsg := swlLicenseExpired;      
    end;
    MessageBox(0, PAnsiChar(AMsg), 'Error', MB_OK or MB_ICONERROR);
    if not TfrmRegistration.DoRegister(Self) then
      DoTerminateApp
    else
      WLRestartApplication;
  end
  else
{
  if PStatus = 0 then
  begin
    FTrialMode := True;
    if WLTrialFirstRun then
    begin
      PStatus := WLTrialDaysLeft;
      AMsg := Format('You have %d days left for trying this application', [PStatus]);
      MessageBox(0, PAnsiChar(AMsg), 'Info', MB_OK or MB_ICONINFORMATION);
    end else
    begin
      PStatus := WLTrialGetStatus(PExtStatus);
      if PStatus <> 0 then
      begin
        PStatus := WLTrialExtGetStatus;
        if PStatus <> 1 then
        begin
          MessageBox(0, 'Your trial period has been expired, please register this application or uninstall it from your computer', 'Error', MB_OK or MB_ICONERROR);
          if not TfrmRegistration.DoRegister(Self) then
            DoTerminateApp
          else
            WLRestartApplication;
        end;
      end else
      begin
        PStatus := WLTrialDaysLeft;
        if (PStatus mod 10) = 0 then
        begin
          AMsg := Format(swlNDaysLeft, [PStatus]);
          MessageBox(0, PAnsiChar(AMsg), 'Info', MB_OK or MB_ICONINFORMATION);
        end;
      end;
    end;
  end else
}  
  begin
    WLRegGetLicenseInfo(ARegName, ACompanyName, ACustomData);
    FRegName := ARegName;
    FRegCompany := ACompanyName;
    FCustomData := ACustomData;
  end;
{$I Encode_End.inc}
end;
{$ELSE}
begin
  FTrialMode := False;
  FRegName := '';
  FRegCompany := 'PT. Ungaran Sari Garments';
  FCustomData := '';
end;
{$ENDIF}

procedure TCSPLicenseManager.DoTerminateApp;
begin
  Application.Terminate;  
end;

procedure TCSPLicenseManager.Loaded;
begin
  inherited Loaded;
  DoCheckLicense;
end;

end.
