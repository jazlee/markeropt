object frmMarkerOptimizer: TfrmMarkerOptimizer
  Left = 0
  Top = 0
  Caption = 'Marker Optimizer'
  ClientHeight = 436
  ClientWidth = 731
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TdxStatusBar
    Left = 0
    Top = 416
    Width = 731
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
  object EngineTabControl: TcxTabControl
    AlignWithMargins = True
    Left = 3
    Top = 51
    Width = 725
    Height = 362
    Align = alClient
    MultiLine = True
    ShowFrame = True
    TabIndex = 0
    TabOrder = 5
    Tabs.Strings = (
      'N-Top Optimisation')
    OnChange = EngineTabControlChange
    OnChanging = EngineTabControlChanging
    ExplicitTop = 53
    ExplicitHeight = 360
    ClientRectBottom = 361
    ClientRectLeft = 1
    ClientRectRight = 724
    ClientRectTop = 26
    object dxBarDockControl1: TdxBarDockControl
      Left = 1
      Top = 26
      Width = 723
      Height = 26
      Align = dalTop
      BarManager = BarManager
    end
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 117
      Width = 720
      Height = 241
      Margins.Top = 0
      Margins.Right = 0
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Splitter: TcxSplitter
        AlignWithMargins = True
        Left = 522
        Top = 0
        Width = 8
        Height = 238
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        HotZoneClassName = 'TcxMediaPlayer9Style'
        AlignSplitter = salRight
        Control = TabProperties
        ExplicitHeight = 240
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 522
        Height = 241
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Panel4: TPanel
          Left = 0
          Top = 198
          Width = 522
          Height = 43
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          object ErrorMemo: TcxMemo
            Left = 0
            Top = 0
            Align = alClient
            Properties.ReadOnly = True
            Properties.ScrollBars = ssVertical
            Style.Color = clBtnFace
            TabOrder = 0
            Height = 43
            Width = 522
          end
        end
        object ErrorSplitter: TcxSplitter
          Left = 0
          Top = 190
          Width = 522
          Height = 8
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          HotZoneClassName = 'TcxMediaPlayer9Style'
          AlignSplitter = salBottom
          Control = Panel4
          ExplicitTop = 192
          ExplicitWidth = 8
        end
        object DistributionGrid: TcxGrid
          Left = 0
          Top = 0
          Width = 522
          Height = 190
          Align = alClient
          TabOrder = 2
          RootLevelOptions.DetailTabsPosition = dtpTop
          ExplicitHeight = 192
          object DistributionGridBandedTableView: TcxGridBandedTableView
            PopupMenu = DistPopupMenu
            OnDblClick = DistributionGridBandedTableViewDblClick
            NavigatorButtons.ConfirmDelete = False
            OnCanFocusRecord = DistributionGridBandedTableViewCanFocusRecord
            OnCustomDrawCell = DistributionGridBandedTableViewCustomDrawCell
            OnFocusedRecordChanged = DistributionGridBandedTableViewFocusedRecordChanged
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            OptionsData.Inserting = False
            Bands = <
              item
              end>
          end
          object ConsumptionGridBandedTableView: TcxGridBandedTableView
            PopupMenu = DistPopupMenu
            OnDblClick = ConsumptionGridBandedTableViewDblClick
            NavigatorButtons.ConfirmDelete = False
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            DataController.Summary.OnAfterSummary = ConsumptionGridTableViewDataControllerSummaryAfterSummary
            OptionsData.Inserting = False
            Bands = <
              item
              end>
          end
          object DistributionGridLevel: TcxGridLevel
            Caption = 'Distribution Plan'
            GridView = DistributionGridBandedTableView
          end
          object ConsumptionGridLevel: TcxGridLevel
            Caption = 'Fabric Consumption'
            GridView = ConsumptionGridBandedTableView
          end
        end
      end
      object TabProperties: TcxTabControl
        AlignWithMargins = True
        Left = 530
        Top = 0
        Width = 187
        Height = 241
        Margins.Left = 0
        Margins.Top = 0
        Margins.Bottom = 0
        Align = alRight
        Focusable = False
        ShowFrame = True
        TabIndex = 0
        TabOrder = 2
        Tabs.Strings = (
          'Default'
          'Detail')
        OnChange = TabPropertiesChange
        OnChanging = TabPropertiesChanging
        ExplicitHeight = 243
        ClientRectBottom = 240
        ClientRectLeft = 1
        ClientRectRight = 186
        ClientRectTop = 26
        object PropertyGrid: TcxVerticalGrid
          Left = 1
          Top = 26
          Width = 185
          Height = 214
          Margins.Left = 0
          Margins.Top = 0
          BorderStyle = cxcbsNone
          Align = alClient
          TabOrder = 0
          ExplicitTop = 24
          ExplicitHeight = 218
        end
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 52
      Width = 723
      Height = 65
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        723
        65)
      object Label3: TLabel
        Left = 18
        Top = 38
        Width = 58
        Height = 13
        Caption = 'Order Desc:'
        Transparent = True
      end
      object Label2: TLabel
        Left = 18
        Top = 14
        Width = 48
        Height = 13
        Caption = 'Order No:'
        Transparent = True
      end
      object Label1: TLabel
        Left = 336
        Top = 14
        Width = 58
        Height = 13
        Caption = 'Order Date:'
        Transparent = True
      end
      object edOrderDesc: TcxTextEdit
        Left = 90
        Top = 34
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 0
        Width = 615
      end
      object edOrderNo: TcxTextEdit
        Left = 90
        Top = 10
        Enabled = False
        TabOrder = 1
        Width = 240
      end
      object edOrderDate: TcxDateEdit
        Left = 404
        Top = 10
        Enabled = False
        TabOrder = 2
        Width = 121
      end
    end
  end
  object ActionList: TActionList
    Images = dmResources.SmallImages
    OnUpdate = ActionListUpdate
    Left = 564
    Top = 104
    object actSave: TAction
      Caption = '&Save'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actCancel: TAction
      Caption = 'Clear'
      ImageIndex = 7
      OnExecute = actCancelExecute
    end
    object actOptimize: TAction
      Caption = 'Optimize All'
      ImageIndex = 22
      ShortCut = 16463
      OnExecute = actOptimizeExecute
    end
    object actClose: TAction
      Caption = '&Close window'
      ImageIndex = 14
      ShortCut = 32856
      OnExecute = actCloseExecute
    end
    object actOptimizeItem: TAction
      Caption = 'Reoptimize Worksheet'
      ImageIndex = 12
      ShortCut = 16471
      OnExecute = actOptimizeItemExecute
    end
    object actExport: TAction
      Caption = 'Export Batch'
      ImageIndex = 28
      OnExecute = actExportExecute
    end
    object actLoad: TAction
      Caption = 'Load XML'
      ImageIndex = 1
      OnExecute = actLoadExecute
    end
    object actReset: TAction
      Caption = 'Reset'
      ImageIndex = 8
      OnExecute = actResetExecute
    end
    object actLockUnlock: TAction
      Caption = 'Lock/Unlock value'
      ImageIndex = 32
      ShortCut = 16470
      OnExecute = actLockUnlockExecute
    end
    object actLockRow: TAction
      Caption = 'Lock/unlock row'
      ImageIndex = 32
      ShortCut = 32850
      OnExecute = actLockRowExecute
    end
    object actPrint: TAction
      Caption = 'Print'
      ImageIndex = 3
      OnExecute = actPrintExecute
    end
    object actExportExcel: TAction
      Caption = 'Export to Excel'
      ImageIndex = 49
      OnExecute = actExportExcelExecute
    end
    object actShowMarkerInfo: TAction
      Caption = 'Show info'
      ImageIndex = 23
      OnExecute = actShowMarkerInfoExecute
    end
  end
  object BarManager: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Categories.Strings = (
      'Default'
      'Detail')
    Categories.ItemsVisibles = (
      2
      2)
    Categories.Visibles = (
      True
      True)
    ImageOptions.Images = dmResources.SmallImages
    ImageOptions.LargeImages = dmResources.LargeImages
    PopupMenuLinks = <>
    Style = bmsUseLookAndFeel
    UseSystemFont = True
    Left = 536
    Top = 104
    DockControlHeights = (
      0
      0
      48
      0)
    object dxBarManager1Bar1: TdxBar
      AllowClose = False
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
          ItemName = 'stdBtnOptimize'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnSave'
        end
        item
          Visible = True
          ItemName = 'stdBtnCancel'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnClose'
        end>
      NotDocking = [dsNone, dsLeft, dsRight, dsBottom]
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      UseRestSpace = True
      Visible = True
      WholeRow = False
    end
    object BarManagerBar1: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      Caption = 'Item Toolbar'
      CaptionButtons = <>
      DockControl = dxBarDockControl1
      DockedDockControl = dxBarDockControl1
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 303
      FloatTop = 300
      FloatClientWidth = 51
      FloatClientHeight = 22
      ItemLinks = <
        item
          Visible = True
          ItemName = 'lblDetail'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnReset'
        end
        item
          Visible = True
          ItemName = 'btnReoptimize'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnExportBatch'
        end
        item
          Visible = True
          ItemName = 'stdBtnLoadXML'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnShowInfo'
        end
        item
          BeginGroup = True
          UserDefine = [udPaintStyle]
          UserPaintStyle = psCaptionGlyph
          Visible = True
          ItemName = 'stdBtnPrint'
        end
        item
          UserDefine = [udPaintStyle]
          UserPaintStyle = psCaptionGlyph
          Visible = True
          ItemName = 'stdBtnExportExcel'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = True
    end
    object stdBtnClose: TdxBarLargeButton
      Action = actClose
      Category = 0
    end
    object stdBtnSave: TdxBarLargeButton
      Action = actSave
      Category = 0
    end
    object stdBtnCancel: TdxBarLargeButton
      Action = actCancel
      Category = 0
    end
    object stdBtnOptimize: TdxBarLargeButton
      Action = actOptimize
      Category = 0
    end
    object lblDetail: TdxBarStatic
      Caption = 'Detail functions:'
      Category = 1
      Hint = 'Detail functions:'
      Visible = ivAlways
    end
    object stdBtnReset: TdxBarButton
      Action = actReset
      Category = 1
      PaintStyle = psCaptionGlyph
    end
    object btnReoptimize: TdxBarButton
      Action = actOptimizeItem
      Category = 1
      PaintStyle = psCaptionGlyph
    end
    object stdBtnExportBatch: TdxBarButton
      Action = actExport
      Category = 1
      PaintStyle = psCaptionGlyph
    end
    object stdBtnLoadXML: TdxBarButton
      Action = actLoad
      Category = 1
      PaintStyle = psCaptionGlyph
    end
    object stdBtnPrint: TdxBarButton
      Action = actPrint
      Category = 1
    end
    object stdBtnExportExcel: TdxBarButton
      Action = actExportExcel
      Category = 1
    end
    object stdBtnShowInfo: TdxBarButton
      Action = actShowMarkerInfo
      Category = 1
      PaintStyle = psCaptionGlyph
    end
  end
  object DistPopupMenu: TPopupMenu
    Images = dmResources.SmallImages
    Left = 592
    Top = 104
    object OptimizeAll1: TMenuItem
      Action = actOptimize
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Save1: TMenuItem
      Action = actSave
    end
    object Cancel1: TMenuItem
      Action = actCancel
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Reset1: TMenuItem
      Action = actReset
    end
    object ReoptimizeWorksheet1: TMenuItem
      Action = actOptimizeItem
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object ExportBatch1: TMenuItem
      Action = actExport
    end
    object LoadXML1: TMenuItem
      Action = actLoad
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Showmarkerinformation1: TMenuItem
      Action = actShowMarkerInfo
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object LockUnlockvalue1: TMenuItem
      Action = actLockUnlock
    end
    object Lockunlockrow1: TMenuItem
      Action = actLockRow
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Closewindow1: TMenuItem
      Action = actClose
    end
  end
end
