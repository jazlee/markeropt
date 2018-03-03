object frmBanner: TfrmBanner
  Left = 193
  Top = 167
  AlphaBlendValue = 128
  BorderStyle = bsNone
  ClientHeight = 250
  ClientWidth = 422
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 0
    Width = 422
    Height = 250
    Align = alClient
  end
  object Label1: TLabel
    Left = 154
    Top = 230
    Width = 14
    Height = 13
    Caption = 'sdf'
    Transparent = True
  end
  object Label2: TLabel
    Left = 150
    Top = 128
    Width = 51
    Height = 13
    Caption = 'Version: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
end
