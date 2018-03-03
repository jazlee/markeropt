object frmDBExpEditor: TfrmDBExpEditor
  Left = 328
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Datasource Editor'
  ClientHeight = 294
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 334
    Top = 0
    Width = 94
    Height = 294
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object btnAdd: TButton
      Left = 12
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 0
      OnClick = btnAddClick
    end
    object btnRemove: TButton
      Left = 12
      Top = 62
      Width = 75
      Height = 25
      Caption = 'Remove'
      TabOrder = 1
      OnClick = btnRemoveClick
    end
    object btnApply: TButton
      Left = 12
      Top = 34
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 2
      OnClick = btnApplyClick
    end
    object btnClose: TButton
      Left = 12
      Top = 90
      Width = 75
      Height = 25
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 3
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 334
    Height = 294
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object ListBox1: TListBox
      Left = 4
      Top = 119
      Width = 326
      Height = 171
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
      OnDblClick = ListBox1Click
      OnKeyDown = ListBox1KeyDown
    end
    object Panel3: TPanel
      Left = 4
      Top = 4
      Width = 326
      Height = 115
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object PanelDS: TPanel
        Left = 0
        Top = 0
        Width = 326
        Height = 109
        Align = alTop
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object Label1: TLabel
          Left = 12
          Top = 14
          Width = 40
          Height = 13
          Caption = 'Dataset:'
        end
        object Label2: TLabel
          Left = 12
          Top = 38
          Width = 25
          Height = 13
          Caption = 'Alias:'
        end
        object Label3: TLabel
          Left = 12
          Top = 86
          Width = 35
          Height = 13
          Caption = 'Master:'
        end
        object SpeedButton2: TSpeedButton
          Left = 305
          Top = 81
          Width = 15
          Height = 22
          Flat = True
          Glyph.Data = {
            E6000000424DE60000000000000076000000280000000E0000000E0000000100
            0400000000007000000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3300333333333333330033003300330033003300330033003300333333333333
            3300333333333333330033333333333333003333333333333300333333333333
            3300333333333333330033333333333333003333333333333300333333333333
            33003333333333333300}
          OnClick = SpeedButton2Click
        end
        object SpeedButton1: TSpeedButton
          Left = 305
          Top = 9
          Width = 15
          Height = 22
          Flat = True
          Glyph.Data = {
            E6000000424DE60000000000000076000000280000000E0000000E0000000100
            0400000000007000000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3300333333333333330033003300330033003300330033003300333333333333
            3300333333333333330033333333333333003333333333333300333333333333
            3300333333333333330033333333333333003333333333333300333333333333
            33003333333333333300}
          OnClick = SpeedButton1Click
        end
        object Label7: TLabel
          Left = 12
          Top = 62
          Width = 35
          Height = 13
          Caption = 'Range:'
        end
        object edDataset: TComboBox
          Left = 60
          Top = 10
          Width = 243
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = edDatasetChange
        end
        object edAlias: TEdit
          Left = 60
          Top = 34
          Width = 243
          Height = 21
          TabOrder = 1
        end
        object edMastersource: TComboBox
          Left = 60
          Top = 82
          Width = 243
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
        end
        object edRange: TEdit
          Left = 60
          Top = 58
          Width = 243
          Height = 21
          TabOrder = 2
        end
      end
      object PanelPRM: TPanel
        Left = -12
        Top = 10
        Width = 311
        Height = 71
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 1
        object Label4: TLabel
          Left = 10
          Top = 16
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object Label5: TLabel
          Left = 10
          Top = 42
          Width = 30
          Height = 13
          Caption = 'Value:'
        end
        object edName: TEdit
          Left = 56
          Top = 12
          Width = 243
          Height = 21
          TabOrder = 0
        end
        object edValue: TEdit
          Left = 56
          Top = 38
          Width = 243
          Height = 21
          TabOrder = 1
        end
      end
      object PanelITM: TPanel
        Left = 70
        Top = 6
        Width = 327
        Height = 41
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 2
        object Label6: TLabel
          Left = 6
          Top = 14
          Width = 51
          Height = 13
          Caption = 'XL Report:'
        end
        object SpeedButton3: TSpeedButton
          Left = 305
          Top = 9
          Width = 15
          Height = 22
          Flat = True
          Glyph.Data = {
            E6000000424DE60000000000000076000000280000000E0000000E0000000100
            0400000000007000000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3300333333333333330033003300330033003300330033003300333333333333
            3300333333333333330033333333333333003333333333333300333333333333
            3300333333333333330033333333333333003333333333333300333333333333
            33003333333333333300}
          OnClick = SpeedButton3Click
        end
        object edXLReport: TComboBox
          Left = 60
          Top = 10
          Width = 243
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = edDatasetChange
        end
      end
    end
  end
end
