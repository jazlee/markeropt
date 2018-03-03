object frmExtrnToolsEdit: TfrmExtrnToolsEdit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'External Tools Editor'
  ClientHeight = 143
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object cxLabel1: TcxLabel
    Left = 8
    Top = 12
    Caption = 'Title:'
    Transparent = True
  end
  object cxTextEdit1: TcxTextEdit
    Left = 74
    Top = 8
    TabOrder = 1
    Width = 245
  end
  object edProgName: TcxButtonEdit
    Left = 74
    Top = 31
    Properties.Buttons = <
      item
        Default = True
        Kind = bkEllipsis
      end>
    Properties.OnButtonClick = edProgNamePropertiesButtonClick
    TabOrder = 2
    Width = 327
  end
  object cxLabel2: TcxLabel
    Left = 8
    Top = 35
    Caption = 'Program:'
    Transparent = True
  end
  object cxLabel3: TcxLabel
    Left = 8
    Top = 58
    Caption = 'Working Dir:'
    Transparent = True
  end
  object cxTextEdit2: TcxTextEdit
    Left = 74
    Top = 54
    TabOrder = 5
    Width = 245
  end
  object cxLabel4: TcxLabel
    Left = 8
    Top = 80
    Caption = 'Parameters:'
    Transparent = True
  end
  object cxTextEdit3: TcxTextEdit
    Left = 74
    Top = 76
    TabOrder = 7
    Width = 245
  end
  object btnOK: TcxButton
    Left = 245
    Top = 111
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 8
    OnClick = btnOKClick
  end
  object btnCancel: TcxButton
    Left = 326
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.exe'
    Filter = 'Executable Program (*.exe)|*.exe|All Files (*.*)|*.*'
    Title = 'Open Tool'
    Left = 372
    Top = 54
  end
end
