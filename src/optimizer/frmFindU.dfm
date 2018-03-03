object frmFind: TfrmFind
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 174
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object cxPageControl1: TcxPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 416
    Height = 120
    ActivePage = cxTabSheet1
    Align = alTop
    Focusable = False
    ShowFrame = True
    TabOrder = 0
    ClientRectBottom = 119
    ClientRectLeft = 1
    ClientRectRight = 415
    ClientRectTop = 26
    object cxTabSheet1: TcxTabSheet
      Caption = 'Find what'
      ImageIndex = 0
      object Label1: TLabel
        Left = 10
        Top = 26
        Width = 60
        Height = 13
        Caption = 'Text to find:'
        Transparent = True
      end
      object Label2: TLabel
        Left = 243
        Top = 26
        Width = 26
        Height = 13
        Caption = 'Field:'
        Transparent = True
      end
      object edFindText: TcxTextEdit
        Left = 10
        Top = 42
        TabOrder = 0
        Width = 227
      end
      object edFields: TcxComboBox
        Left = 243
        Top = 42
        Properties.DropDownListStyle = lsFixedList
        TabOrder = 1
        Width = 154
      end
    end
  end
  object btnOK: TButton
    Left = 253
    Top = 137
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 334
    Top = 137
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
