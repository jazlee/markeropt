object frmAbout: TfrmAbout
  Left = 190
  Top = 133
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 306
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 12
    Top = 236
    Width = 435
    Height = 7
    Shape = bsTopLine
  end
  object Label4: TLabel
    Left = 10
    Top = 246
    Width = 334
    Height = 48
    Caption = 
      'Perhatian: Aplikasi ini dilindungi oleh undang-undang hak cipta ' +
      'dan kesepakatan '#13#10'internasional. Segala bentuk reproduksi, atau ' +
      'distribusi dari aplikasi ini, atau'#13#10'bagian dari aplikasi ini tan' +
      'pa ijin tertulis adalah melanggar hukum dan akan '#13#10'dituntut ke m' +
      'eja hijau.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label1: TLabel
    Left = 100
    Top = 12
    Width = 343
    Height = 95
    AutoSize = False
    Caption = '%s'#13#10'%s'#13#10#13#10'Copyright '#169' 2009 All Rights Reserved.'#13#10'%s'
    Transparent = True
  end
  object Bevel2: TBevel
    Left = 100
    Top = 106
    Width = 345
    Height = 7
    Shape = bsTopLine
  end
  object Label3: TLabel
    Left = 100
    Top = 112
    Width = 155
    Height = 13
    Caption = 'Portions of copyright informations'
    Transparent = True
  end
  object Label2: TLabel
    Left = 100
    Top = 130
    Width = 349
    Height = 97
    AutoSize = False
    Caption = 
      'ECF Business Server by Jaimy Azle '#169' 2007 All rights reserved. Ex' +
      'pressBar, Express Editors, Express Quantum Grid by Developer Exp' +
      'ress Inc. '#169' 2007 All rights reserved. XL Report 4.0 with XLOptio' +
      'nPack Technology by Afalina Co., Ltd. '#169' 1998,2002 All rights res' +
      'erved. Scripting && Reporting tools by FastReport Software '#169' 200' +
      '7 All rights reserved. Embedded Web Browser from BSalsa Producti' +
      'ons '#169' 2007 All rights reserved. TeeChart by Steema Software '#169' 20' +
      '01 All rights reserved. ZLibPas by PasZLib '#169' 2000 All rights res' +
      'erved. Tdbf by Stephen Stewart '#169' 2004 All rights reserved.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object Image1: TcxImage
    Left = 13
    Top = 13
    Properties.ReadOnly = True
    Properties.ShowFocusRect = False
    Style.BorderStyle = ebsUltraFlat
    TabOrder = 1
    Height = 216
    Width = 80
  end
  object Button2: TcxButton
    Left = 348
    Top = 244
    Width = 99
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
end
