object frmProperties: TfrmProperties
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Properties'
  ClientHeight = 404
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 162
    Top = 366
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 243
    Top = 366
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object cxPageControl1: TcxPageControl
    Left = 8
    Top = 12
    Width = 319
    Height = 343
    ActivePage = cxTabSheet1
    Focusable = False
    ShowFrame = True
    TabOrder = 0
    ClientRectBottom = 342
    ClientRectLeft = 1
    ClientRectRight = 318
    ClientRectTop = 24
    object cxTabSheet1: TcxTabSheet
      Caption = 'Summary'
      ImageIndex = 0
      object Label1: TLabel
        Left = 12
        Top = 20
        Width = 24
        Height = 13
        Caption = 'Title:'
        Transparent = True
      end
      object Label2: TLabel
        Left = 12
        Top = 47
        Width = 40
        Height = 13
        Caption = 'Subject:'
        Transparent = True
      end
      object Label3: TLabel
        Left = 12
        Top = 74
        Width = 37
        Height = 13
        Caption = 'Author:'
        Transparent = True
      end
      object Label4: TLabel
        Left = 12
        Top = 101
        Width = 46
        Height = 13
        Caption = 'Manager:'
        Transparent = True
      end
      object Label5: TLabel
        Left = 12
        Top = 128
        Width = 49
        Height = 13
        Caption = 'Company:'
        Transparent = True
      end
      object Label6: TLabel
        Left = 12
        Top = 164
        Width = 49
        Height = 13
        Caption = 'Category:'
        Transparent = True
      end
      object Label7: TLabel
        Left = 12
        Top = 191
        Width = 49
        Height = 13
        Caption = 'Comment:'
        Transparent = True
      end
      object cxTextEdit1: TcxTextEdit
        Left = 102
        Top = 16
        TabOrder = 0
        Width = 205
      end
      object cxTextEdit2: TcxTextEdit
        Tag = 1
        Left = 102
        Top = 45
        TabOrder = 1
        Width = 205
      end
      object cxTextEdit3: TcxTextEdit
        Tag = 2
        Left = 102
        Top = 72
        TabOrder = 2
        Width = 205
      end
      object cxTextEdit4: TcxTextEdit
        Tag = 3
        Left = 102
        Top = 99
        TabOrder = 3
        Width = 205
      end
      object cxTextEdit5: TcxTextEdit
        Tag = 4
        Left = 102
        Top = 126
        Enabled = False
        TabOrder = 4
        Width = 205
      end
      object cxTextEdit6: TcxTextEdit
        Tag = 5
        Left = 102
        Top = 162
        TabOrder = 5
        Width = 205
      end
      object cxMemo1: TcxMemo
        Tag = 6
        Left = 102
        Top = 191
        TabOrder = 6
        Height = 89
        Width = 205
      end
    end
  end
end
