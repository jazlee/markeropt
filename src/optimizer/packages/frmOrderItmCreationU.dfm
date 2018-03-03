object frmOrderItmCreation: TfrmOrderItmCreation
  Left = 0
  Top = 0
  Caption = 'Create Order Items'
  ClientHeight = 414
  ClientWidth = 678
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
  object cxTabControl1: TcxTabControl
    AlignWithMargins = True
    Left = 0
    Top = 51
    Width = 678
    Height = 343
    Margins.Left = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 0
    ClientRectBottom = 343
    ClientRectRight = 678
    ClientRectTop = 0
    object cxPageControl1: TcxPageControl
      AlignWithMargins = True
      Left = 3
      Top = 48
      Width = 672
      Height = 292
      ActivePage = cxTabSheet1
      Align = alClient
      Focusable = False
      ShowFrame = True
      TabOrder = 0
      ClientRectBottom = 291
      ClientRectLeft = 1
      ClientRectRight = 671
      ClientRectTop = 24
      object cxTabSheet1: TcxTabSheet
        Caption = 'Create Order Items'
        ImageIndex = 0
        object BrowseGrid: TcxGrid
          Left = 0
          Top = 0
          Width = 670
          Height = 267
          Align = alClient
          BorderStyle = cxcbsNone
          TabOrder = 0
          object BrowseGridTableView1: TcxGridTableView
            NavigatorButtons.ConfirmDelete = False
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
          end
          object BrowseGridLevel1: TcxGridLevel
            GridView = BrowseGridTableView1
          end
        end
      end
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 678
      Height = 45
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 28
        Height = 13
        Caption = 'Style:'
        Transparent = True
      end
      object edStyle: TcxButtonEdit
        Left = 50
        Top = 12
        Properties.Buttons = <
          item
            Default = True
            Kind = bkEllipsis
          end>
        Properties.OnButtonClick = edStylePropertiesButtonClick
        TabOrder = 0
        Width = 121
      end
      object edStyleDesc: TcxTextEdit
        Left = 175
        Top = 12
        Enabled = False
        TabOrder = 1
        Width = 263
      end
      object btnPrepare: TButton
        Left = 444
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Load'
        TabOrder = 2
        OnClick = btnPrepareClick
      end
    end
  end
  object StatusBar: TdxStatusBar
    Left = 0
    Top = 394
    Width = 678
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
  object ActionList: TActionList
    Images = dmResources.LargeImages
    Left = 220
    Top = 74
    object actClose: TAction
      Caption = '&Close window'
      ImageIndex = 14
      ShortCut = 32856
      OnExecute = actCloseExecute
    end
    object actCreate: TAction
      Caption = 'Create Items'
      ImageIndex = 12
      OnExecute = actCreateExecute
    end
  end
  object BarManager: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    ImageOptions.Images = dmResources.SmallImages
    ImageOptions.LargeImages = dmResources.LargeImages
    PopupMenuLinks = <>
    Style = bmsUseLookAndFeel
    UseSystemFont = True
    Left = 192
    Top = 74
    DockControlHeights = (
      0
      0
      48
      0)
    object dxBarManager1Bar1: TdxBar
      Caption = 'Standard'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 326
      FloatTop = 216
      FloatClientWidth = 81
      FloatClientHeight = 181
      ItemLinks = <
        item
          Visible = True
          ItemName = 'stdBtnCreate'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnClose'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      UseRestSpace = True
      Visible = True
      WholeRow = False
    end
    object stdBtnCreate: TdxBarLargeButton
      Action = actCreate
      Category = 0
    end
    object stdBtnClose: TdxBarLargeButton
      Action = actClose
      Category = 0
    end
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = BrowseGrid
    PopupMenus = <
      item
        GridView = BrowseGridTableView1
        HitTypes = []
        Index = 0
        PopupMenu = PopupMenu1
      end>
    AlwaysFireOnPopup = True
    Left = 248
    Top = 74
  end
  object PopupMenu1: TPopupMenu
    Left = 276
    Top = 74
    object Setallforcurrentrow1: TMenuItem
      Tag = 1
      Caption = 'Set all for current row'
      OnClick = UnsetAll1Click
    end
    object Setallforcurentcolumn1: TMenuItem
      Tag = 2
      Caption = 'Set all for curent column'
      OnClick = UnsetAll1Click
    end
    object Setall1: TMenuItem
      Tag = 3
      Caption = 'Set all'
      OnClick = UnsetAll1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Unsetallforcurrentrow1: TMenuItem
      Tag = 4
      Caption = 'Unset all for current row'
      OnClick = UnsetAll1Click
    end
    object Unsetallforcurrentcolumn1: TMenuItem
      Tag = 5
      Caption = 'Unset all for current column'
      OnClick = UnsetAll1Click
    end
    object UnsetAll1: TMenuItem
      Tag = 6
      Caption = 'Unset all'
      OnClick = UnsetAll1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object CreateItems1: TMenuItem
      Action = actCreate
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Closewindow1: TMenuItem
      Action = actClose
    end
  end
end
