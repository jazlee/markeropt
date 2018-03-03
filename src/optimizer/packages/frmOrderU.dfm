object frmOrder: TfrmOrder
  Left = 0
  Top = 0
  Caption = 'Order'
  ClientHeight = 367
  ClientWidth = 669
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TdxStatusBar
    Left = 0
    Top = 347
    Width = 669
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        MinWidth = 250
        Width = 250
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
  object cxTabControl1: TcxTabControl
    AlignWithMargins = True
    Left = 0
    Top = 51
    Width = 669
    Height = 296
    Margins.Left = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 0
    ClientRectBottom = 296
    ClientRectRight = 669
    ClientRectTop = 0
    object cxPageControl1: TcxPageControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 663
      Height = 290
      ActivePage = cxTabSheet1
      Align = alClient
      Focusable = False
      ShowFrame = True
      TabOrder = 0
      ClientRectBottom = 289
      ClientRectLeft = 1
      ClientRectRight = 662
      ClientRectTop = 26
      object cxTabSheet1: TcxTabSheet
        Caption = 'Order Header'
        ImageIndex = 0
        ParentShowHint = False
        ShowHint = True
        object Label1: TLabel
          Left = 14
          Top = 45
          Width = 48
          Height = 13
          Caption = 'Order No:'
          Transparent = True
        end
        object Label4: TLabel
          Left = 14
          Top = 69
          Width = 57
          Height = 13
          Caption = 'Description:'
          Transparent = True
        end
        object Label5: TLabel
          Left = 14
          Top = 91
          Width = 50
          Height = 13
          Caption = 'Customer:'
          Transparent = True
        end
        object Label2: TLabel
          Left = 14
          Top = 115
          Width = 43
          Height = 13
          Caption = 'DSN File:'
          Transparent = True
        end
        object Label3: TLabel
          Left = 14
          Top = 138
          Width = 64
          Height = 13
          Caption = 'Max. Length:'
          Enabled = False
          Transparent = True
          Visible = False
        end
        object Label8: TLabel
          Left = 14
          Top = 21
          Width = 27
          Height = 13
          Caption = 'Date:'
          Transparent = True
        end
        object Label6: TLabel
          Left = 14
          Top = 162
          Width = 89
          Height = 13
          Caption = 'Marker Len/Width:'
          Transparent = True
        end
        object Label10: TLabel
          Left = 14
          Top = 187
          Width = 49
          Height = 13
          Caption = 'Algorithm:'
          Transparent = True
        end
        object cxDBTextEdit1: TcxDBTextEdit
          Left = 105
          Top = 41
          DataBinding.DataField = 'ordno'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 1
          Width = 121
        end
        object cxDBTextEdit4: TcxDBTextEdit
          Left = 105
          Top = 65
          DataBinding.DataField = 'orddesc'
          DataBinding.DataSource = DataSource
          TabOrder = 2
          Width = 540
        end
        object cxDBButtonEdit1: TcxDBButtonEdit
          Left = 105
          Top = 89
          DataBinding.DataField = 'cstcode'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit1PropertiesButtonClick
          TabOrder = 3
          Width = 121
        end
        object cxDBButtonEdit2: TcxDBButtonEdit
          Left = 105
          Top = 113
          DataBinding.DataField = 'dsnfile'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Caption = 'Browse file'
              Default = True
              Glyph.Data = {
                36050000424D3605000000000000360400002800000010000000100000000100
                08000000000000010000620B0000620B0000000100000001000018A5C60018A5
                CE0029A5D60031A5D60018ADD60021ADD60029ADD60018ADDE0031B5DE0039BD
                E70052BDE7004AC6E7004AC6EF009CDEEF00ADDEEF006BDEF70073DEF700A5EF
                F700FF00FF0084EFFF008CEFFF0094EFFF008CF7FF0094F7FF00A5F7FF0094FF
                FF009CFFFF00ADFFFF00C6FFFF00D6FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00121212121212
                121212121212121212121206080512121212121212121212121212060D130B05
                0000001212121212121212060A1A19101010100B05001212121212060A1B1614
                14141010100C051212121206060E17171414141410100C121212120610061A16
                16161414141010001212120617060E111118141414101009121212061A100600
                00000E1613131010001212061A171010100F060E13131315031212061A171717
                1710100600020202001212061C191A1D1D14151010071212121212050D1C1C0D
                0400000000011212121212120806000012121212121212121212121212121212
                1212121212121212121212121212121212121212121212121212}
              Hint = 'Browse file'
              Kind = bkGlyph
            end
            item
              Caption = 'Attach'
              Glyph.Data = {
                BE020000424DBE02000000000000BE0100002800000010000000100000000100
                08000000000000010000120B0000120B0000620000006200000000000000FFFF
                FF00FF00FF00B7818300A4787400986B6600B4817600BA8E8500CB9A8200DAA4
                8200DCA88700E3B18E00EDBD9200FDF7F000DF993E00FCF6EE00FDF9F400FEFB
                F700F2D6AD00F3D9B300F5DEBD00F5DFBE00F4DEBD00F8E8D100F8E9D300F8EA
                D500FAEEDD00FAF0E200FAF1E400FCF5EB00FDF7EE00FEFAF400EECD9A00EFCF
                9D00EFD09E00F0D1A000F0D1A100F0D2A300F0D2A400F0D3A500F1D4A800F0D4
                A700F1D5A900F1D6AA00F2D7AE00F2D8AF00F1D8AF00F2D9B200F3DBB400F3DC
                B600F3DCB800F4DDBA00F4DEBC00F5E0BF00F5E1C200F5E2C300F5E2C400F5E3
                C600F7E5C900F6E4C800F6E5CA00F7E6CC00F7E7CE00F7E8D000F8E9D200F8EA
                D400F9EDDA00F9EEDD00FAF0E000FCF7EF00F5E2C200F5E4C700F8ECD700F8EC
                D800FAF0DE00FAF1E200FBF4E800FBF5EA00FDF9F200FBF4E700FAF3E600FBF6
                EC00FFFEFC00FDFCF800CBCECC00CCD4D600CED8DF003FA3F60040A3F6003C90
                D7004495D7004FABF5005AB1F500359DF600CFDDE900CFDCE800CFDBE6001153
                9300020202020202020202020202020202020203040404040404040404040405
                020202033B3616322F12282623212005020202063F3C383533302E2B27242205
                0202020648400E0E0E0E0E0E0E292505020202074A42413E3B461432132C2A05
                0202020750440E0E0E0E0E0E0E302D0502020208514F4B1A49183D4736343105
                020202084E1E4D61616161613A3715050202020A531F0D615D5D5D61173D3905
                0202020A526161615858576161613E050202020B015F61595B5B5B5961541905
                0202020B01015E615A5C5A61551B43050202020C0101015E615A61560F4C1C05
                0202020C010101015E61601110451D050202020C090909090909090909090905
                0202}
              Hint = 'Attach file'
              Kind = bkGlyph
            end
            item
              Caption = 'Download'
              Glyph.Data = {
                D6020000424DD602000000000000D60100002800000010000000100000000100
                08000000000000010000120B0000120B0000680000006800000000000000FFFF
                FF00FF00FF00A46769008E5D590080504B00A0675B00A7756B00BC826800CF8E
                6800D1926D00DA9D7500FFFCFA00E7AB7900FBF4ED00FEFCFA00D5812700FCF4
                EB00F6E3C900F8E7D000FEF8F000FCF8F300FEFBF700E9BF8200EAC18600EAC2
                8700EBC48A00EBC58C00EBC58E00EBC68F00EBC79100EDC99300EECC9A00EFD1
                A500F2D5AC00F0D4AA00F0D4AB00F2D8B100F2D9B400F2DAB600F4DEBD00F6E1
                C400F6E2C500F7E7CF00F8E9D300F8EBD900FAF0E100FBF2E500FCF4E900FBF4
                EA00FCF7F000FEFAF400EBC48900EFD1A300F2D9B200F3DCB800F4DDBA00F3DD
                BB00F4E0C000F4E1C200F4E3C700F7E6CA00F6E6CC00F7E9D300FAEFDE00F8ED
                DC00FAF0E000FBF3E600F6E6CB00F8EBD400FAEEDA00F8EDD900FCF7EE00B4A7
                8E00F8EDD800F8EFDE00FAF2E300FBF4E700FAF3E600FCFAF600FFFEFC00B5AA
                9100FBF7EE00B5AB9300B5AC9700B4AA8F00FFFEFB00FCFBF600B7B4A300FEFE
                FC00B8BAB000FEFFFE00BCC0BD003596EF003696F000409DF2001978D800288A
                EE001B7FEB000962D0000C65D1001174EA000F6AD30007397A00020202020202
                0202020202020202020202030404040404040404040404050202020337252421
                5467491C34181705020202063B392658676367551D1A190502020206442A5A67
                66656367511E1B0502020207455C67605E61626467531F05020202074B676767
                5F5F5D6767672005020202084E4247675F5F5F67252335050202020848304C67
                67676767383622050202020A5714112F404A133D292827050202020A56161552
                4D2E462C3E3C3A050202020B0159101010101010102B12050202020B01015B0F
                4F140E43422D3F050202020D0101101010101010102E41050202020D01010101
                01500C3332312F050202020D0909090909090909090909050202}
              Hint = 'Download file'
              Kind = bkGlyph
            end
            item
              Caption = 'Clear'
              Glyph.Data = {
                36050000424D3605000000000000360400002800000010000000100000000100
                08000000000000010000220B0000220B000000010000000100000031DE000031
                E7000031EF000031F700FF00FF000031FF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00040404040404
                0404040404040404000004000004040404040404040404000004040000000404
                0404040404040000040404000000000404040404040000040404040402000000
                0404040400000404040404040404000000040000000404040404040404040400
                0101010004040404040404040404040401010204040404040404040404040400
                0201020304040404040404040404030201040403030404040404040404050203
                0404040405030404040404040303050404040404040303040404040303030404
                0404040404040403040403030304040404040404040404040404030304040404
                0404040404040404040404040404040404040404040404040404}
              Hint = 'Clear'
              Kind = bkGlyph
            end>
          Properties.ReadOnly = True
          Properties.OnButtonClick = cxDBButtonEdit2PropertiesButtonClick
          TabOrder = 5
          Width = 330
        end
        object cxDBTextEdit2: TcxDBTextEdit
          Left = 228
          Top = 89
          DataBinding.DataField = 'cstname'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.ReadOnly = False
          TabOrder = 4
          Width = 417
        end
        object cxDBDateEdit1: TcxDBDateEdit
          Left = 105
          Top = 18
          DataBinding.DataField = 'orddate'
          DataBinding.DataSource = DataSource
          TabOrder = 0
          Width = 121
        end
        object cxDBSpinEdit1: TcxDBSpinEdit
          Left = 105
          Top = 136
          DataBinding.DataField = 'maxlen'
          DataBinding.DataSource = DataSource
          Enabled = False
          TabOrder = 6
          Visible = False
          Width = 121
        end
        object cxDBSpinEdit2: TcxDBSpinEdit
          Left = 105
          Top = 160
          DataBinding.DataField = 'mrklen'
          DataBinding.DataSource = DataSource
          TabOrder = 7
          Width = 121
        end
        object cxDBSpinEdit3: TcxDBSpinEdit
          Left = 228
          Top = 160
          DataBinding.DataField = 'mrkwidth'
          DataBinding.DataSource = DataSource
          TabOrder = 8
          Width = 121
        end
        object cxDBComboBox1: TcxDBComboBox
          Left = 105
          Top = 183
          DataBinding.DataField = 'algorithm'
          DataBinding.DataSource = DataSource
          Properties.Items.Strings = (
            'ANT'
            'QUALITY')
          TabOrder = 9
          Width = 121
        end
      end
    end
  end
  object ActionList: TActionList
    Images = dmResources.LargeImages
    OnUpdate = ActionListUpdate
    Left = 220
    Top = 74
    object actNew: TAction
      Caption = '&New'
      ImageIndex = 0
      ShortCut = 16462
    end
    object actSave: TAction
      Caption = '&Save'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actDelete: TAction
      Caption = '&Delete'
      ImageIndex = 7
      OnExecute = actDeleteExecute
    end
    object actClear: TAction
      Caption = 'Clear Calculation'
      ImageIndex = 7
      OnExecute = actClearExecute
    end
    object actClose: TAction
      Caption = '&Close window'
      ImageIndex = 14
      ShortCut = 32856
      OnExecute = actCloseExecute
    end
    object actReff_1: TAction
      Caption = 'Order Items'
      ImageIndex = 50
      OnExecute = actReff_1Execute
    end
    object actReff_2: TAction
      Caption = 'Styles'
      ImageIndex = 50
      OnExecute = actReff_2Execute
    end
    object actPrint: TAction
      Caption = 'Print'
      ImageIndex = 3
      OnExecute = actPrintExecute
    end
    object actGenerateItems: TAction
      Caption = 'Generate Items'
      ImageIndex = 12
      OnExecute = actGenerateItemsExecute
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
          ItemName = 'stdBtnOpen'
        end
        item
          Visible = True
          ItemName = 'stdBtnPrint'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnDelete'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnClose'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnReff_2'
        end
        item
          Visible = True
          ItemName = 'stdBtnReff_1'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnGenerateItems'
        end>
      NotDocking = [dsNone, dsLeft, dsRight, dsBottom]
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      UseRestSpace = True
      Visible = True
      WholeRow = False
    end
    object stdBtnNew: TdxBarLargeButton
      Action = actNew
      Category = 0
    end
    object stdBtnOpen: TdxBarLargeButton
      Action = actSave
      Category = 0
    end
    object stdBtnDelete: TdxBarLargeButton
      Action = actDelete
      Category = 0
    end
    object stdBtnClose: TdxBarLargeButton
      Action = actClose
      Category = 0
    end
    object stdBtnReff_2: TdxBarLargeButton
      Action = actReff_2
      Category = 0
    end
    object stdBtnReff_1: TdxBarLargeButton
      Action = actReff_1
      Category = 0
    end
    object stdBtnPrint: TdxBarLargeButton
      Action = actPrint
      Category = 0
    end
    object stdBtnGenerateItems: TdxBarLargeButton
      Action = actGenerateItems
      Category = 0
    end
  end
  object DataSource: TDataSource
    Left = 248
    Top = 74
  end
end
