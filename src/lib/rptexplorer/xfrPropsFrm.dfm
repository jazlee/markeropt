object frxExplorerPropForm: TfrxExplorerPropForm
  Left = 225
  Top = 139
  BorderStyle = bsDialog
  Caption = 'Object Properties'
  ClientHeight = 310
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 275
    Width = 343
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOK: TcxButton
      Left = 180
      Top = 4
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TcxButton
      Left = 260
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 343
    Height = 275
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 8
      Top = 8
      Width = 327
      Height = 259
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Properties'
        DesignSize = (
          319
          231)
        object Image1: TImage
          Left = 19
          Top = 15
          Width = 24
          Height = 24
          Transparent = True
        end
        object Bevel1: TBevel
          Left = 14
          Top = 94
          Width = 287
          Height = 9
          Anchors = [akLeft, akBottom]
          Shape = bsTopLine
        end
        object Label1: TLabel
          Left = 16
          Top = 46
          Width = 14
          Height = 13
          Caption = 'ID:'
        end
        object Label2: TLabel
          Left = 16
          Top = 103
          Width = 27
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Type:'
        end
        object Label4: TLabel
          Left = 16
          Top = 123
          Width = 44
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Location:'
        end
        object Label5: TLabel
          Left = 16
          Top = 143
          Width = 23
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Size:'
        end
        object Label6: TLabel
          Left = 16
          Top = 163
          Width = 26
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Date:'
        end
        object Bevel4: TBevel
          Left = 16
          Top = 189
          Width = 287
          Height = 9
          Anchors = [akLeft, akBottom]
          Shape = bsTopLine
        end
        object Label7: TLabel
          Left = 16
          Top = 198
          Width = 47
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Attributes:'
        end
        object Label3: TLabel
          Left = 16
          Top = 72
          Width = 28
          Height = 13
          Caption = 'Desc:'
        end
        object Label8: TLabel
          Left = 98
          Top = 46
          Width = 22
          Height = 13
          Caption = 'Tag:'
        end
        object edFileName: TcxTextEdit
          Left = 62
          Top = 16
          TabOrder = 0
          Width = 238
        end
        object edRptID: TcxTextEdit
          Left = 62
          Top = 42
          TabOrder = 1
          Width = 33
        end
        object edType: TEdit
          Left = 85
          Top = 103
          Width = 204
          Height = 21
          Anchors = [akLeft, akBottom]
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 4
          Text = 'Edit1'
        end
        object edLocation: TEdit
          Left = 85
          Top = 123
          Width = 204
          Height = 21
          Anchors = [akLeft, akBottom]
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 5
          Text = 'Edit1'
        end
        object edSize: TEdit
          Left = 85
          Top = 143
          Width = 204
          Height = 21
          Anchors = [akLeft, akBottom]
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 6
          Text = 'Edit1'
        end
        object edDate: TEdit
          Left = 85
          Top = 163
          Width = 204
          Height = 21
          Anchors = [akLeft, akBottom]
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 7
          Text = 'Edit1'
        end
        object ckReadonly: TCheckBox
          Left = 72
          Top = 198
          Width = 71
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'Read-only'
          TabOrder = 8
        end
        object ckHidden: TCheckBox
          Left = 150
          Top = 198
          Width = 59
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'Hidden'
          TabOrder = 9
        end
        object ckSystem: TCheckBox
          Left = 220
          Top = 198
          Width = 59
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'System'
          TabOrder = 10
        end
        object edTag: TcxTextEdit
          Left = 128
          Top = 42
          TabOrder = 2
          Width = 39
        end
        object edDesc: TcxTextEdit
          Left = 62
          Top = 68
          TabOrder = 3
          Width = 237
        end
        object btnValidate: TcxButton
          Left = 172
          Top = 42
          Width = 73
          Height = 23
          Caption = 'Validate...'
          TabOrder = 11
          OnClick = btnValidateClick
        end
      end
    end
  end
end
