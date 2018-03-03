object frmMarkerInfo: TfrmMarkerInfo
  Left = 0
  Top = 0
  Caption = 'Marker Info'
  ClientHeight = 444
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TdxStatusBar
    Left = 0
    Top = 424
    Width = 673
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        MinWidth = 250
        Width = 400
      end
      item
        PanelStyleClassName = 'TdxStatusBarKeyboardStatePanelStyle'
        PanelStyle.CapsLockKeyAppearance.ActiveCaption = 'CAPS'
        PanelStyle.CapsLockKeyAppearance.InactiveCaption = 'CAPS'
        PanelStyle.NumLockKeyAppearance.ActiveCaption = 'NUM'
        PanelStyle.NumLockKeyAppearance.InactiveCaption = 'NUM'
        PanelStyle.ScrollLockKeyAppearance.ActiveCaption = 'SCRL'
        PanelStyle.ScrollLockKeyAppearance.InactiveCaption = 'SCRL'
        PanelStyle.InsertKeyAppearance.ActiveCaption = 'OVR'
        PanelStyle.InsertKeyAppearance.InactiveCaption = 'INS'
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end>
    PaintStyle = stpsUseLookAndFeel
    LookAndFeel.Kind = lfOffice11
    LookAndFeel.NativeStyle = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 667
    Height = 65
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      667
      65)
    object Label3: TLabel
      Left = 18
      Top = 40
      Width = 23
      Height = 13
      Caption = 'Size:'
      Transparent = True
    end
    object Label2: TLabel
      Left = 18
      Top = 14
      Width = 28
      Height = 13
      Caption = 'Style:'
      Transparent = True
    end
    object Label1: TLabel
      Left = 336
      Top = 14
      Width = 42
      Height = 13
      Caption = 'Material:'
      Transparent = True
    end
    object Label4: TLabel
      Left = 336
      Top = 41
      Width = 34
      Height = 13
      Caption = 'Pieces:'
      Transparent = True
    end
    object edStyle: TcxTextEdit
      Left = 90
      Top = 10
      Enabled = False
      TabOrder = 0
      Width = 240
    end
    object edMaterial: TcxTextEdit
      Left = 380
      Top = 10
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 1
      Width = 269
    end
    object cbSize: TcxComboBox
      Left = 90
      Top = 37
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbSizePropertiesChange
      TabOrder = 2
      Width = 240
    end
    object edPcs: TcxTextEdit
      Left = 380
      Top = 37
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 3
      Width = 269
    end
  end
  object pnlMarker: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 230
    Width = 667
    Height = 191
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 3
  end
  object MarkerSplitter: TcxSplitter
    Left = 0
    Top = 219
    Width = 673
    Height = 8
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    HotZoneClassName = 'TcxMediaPlayer9Style'
    AlignSplitter = salBottom
    Control = pnlMarker
    ExplicitTop = 205
  end
  object Panel3: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 74
    Width = 667
    Height = 142
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitHeight = 128
    object pnlPanel: TPanel
      Left = 377
      Top = 0
      Width = 290
      Height = 142
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 128
    end
    object cxSplitter1: TcxSplitter
      Left = 369
      Top = 0
      Width = 8
      Height = 142
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      HotZoneClassName = 'TcxMediaPlayer9Style'
      Control = GridPanel
      ExplicitHeight = 128
    end
    object GridPanel: TcxGrid
      Left = 0
      Top = 0
      Width = 369
      Height = 142
      Align = alLeft
      TabOrder = 2
      ExplicitHeight = 128
      object GridPanelTableView1: TcxGridTableView
        NavigatorButtons.ConfirmDelete = False
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsView.ColumnAutoWidth = True
        OptionsView.GroupByBox = False
      end
      object GridPanelLevel1: TcxGridLevel
        GridView = GridPanelTableView1
      end
    end
  end
  object ActionList: TActionList
    Images = dmResources.SmallImages
    Left = 564
    Top = 104
    object actClose: TAction
      Caption = '&Close window'
      ImageIndex = 14
      ShortCut = 32856
    end
  end
end
