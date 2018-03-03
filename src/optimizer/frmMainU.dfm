object frmMain: TfrmMain
  Left = 223
  Top = 117
  ClientHeight = 553
  ClientWidth = 858
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TdxStatusBar
    Left = 0
    Top = 533
    Width = 858
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
  object DockSite: TdxDockSite
    Left = 0
    Top = 196
    Width = 858
    Height = 337
    Align = alClient
    OnHideControl = HideDockControl
    OnShowControl = ShowDockControl
    DockType = 0
    OriginalWidth = 858
    OriginalHeight = 337
    object dxLayoutDockSite3: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 858
      Height = 229
      DockType = 1
      OriginalWidth = 300
      OriginalHeight = 200
      object dxLayoutDockSite1: TdxLayoutDockSite
        Left = 0
        Top = 0
        Width = 858
        Height = 229
        DockType = 1
        OriginalWidth = 300
        OriginalHeight = 200
      end
      object DockMainPanel: TdxDockPanel
        Left = 0
        Top = 0
        Width = 858
        Height = 229
        AllowDockClients = [dtLeft, dtTop, dtRight, dtBottom]
        AllowFloating = False
        AllowDock = [dtClient]
        AutoHide = False
        Caption = 'Orders'
        CaptionButtons = []
        ShowCaption = False
        DockType = 1
        OriginalWidth = 185
        OriginalHeight = 140
        object OrderNavBar: TdxNavBar
          Left = 0
          Top = 0
          Width = 854
          Height = 225
          Align = alClient
          ActiveGroupIndex = 0
          TabOrder = 0
          View = 0
          OptionsView.Common.ShowGroupCaptions = False
          OptionsView.NavigationPane.ShowOverflowPanel = False
          object OrderNavBarOrderGroup: TdxNavBarGroup
            Caption = 'Order list'
            SelectedLinkIndex = -1
            TopVisibleLinkIndex = 0
            OptionsGroupControl.ShowControl = True
            OptionsGroupControl.UseControl = True
            Links = <>
          end
          object OrderNavBarOrderGroupControl: TdxNavBarGroupControl
            Left = 0
            Top = 0
            Width = 854
            Height = 225
            Caption = 'OrderNavBarOrderGroupControl'
            TabOrder = 1
            GroupIndex = 0
            OriginalHeight = 225
            object GridOrder: TcxGrid
              AlignWithMargins = True
              Left = 0
              Top = 2
              Width = 854
              Height = 223
              Margins.Left = 0
              Margins.Top = 2
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alClient
              BevelEdges = []
              BevelInner = bvNone
              BevelOuter = bvNone
              TabOrder = 0
              object GridOrderDBTableView1: TcxGridDBTableView
                PopupMenu = MainPopupMenu
                OnDblClick = GridOrderDBTableView1DblClick
                OnKeyDown = GridOrderDBTableView1KeyDown
                NavigatorButtons.ConfirmDelete = False
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                OptionsView.GroupByBox = False
              end
              object GridOrderLevel1: TcxGridLevel
                GridView = GridOrderDBTableView1
              end
            end
          end
        end
      end
    end
    object DockLogPanel: TdxDockPanel
      Left = 0
      Top = 229
      Width = 858
      Height = 108
      OnVisibleChanged = DockLogPanelVisibleChanged
      AllowFloating = True
      AllowDock = [dtLeft, dtRight, dtBottom]
      AutoHide = False
      Caption = 'Log Panel'
      DockType = 5
      OriginalWidth = 185
      OriginalHeight = 108
    end
  end
  object Ribbon: TdxRibbon
    Left = 0
    Top = 0
    Width = 858
    Height = 147
    ApplicationButton.Glyph.Data = {
      36100000424D3610000000000000360000002800000020000000200000000100
      2000000000000010000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0001020101030100000400000000000000000000000000000000000000000000
      0000000000000000000001000004010100030000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010100020000
      00000000000000000000040404091007012A1A0C00431D0D004E1D0D004E190B
      00410F0700270303030600000000000000000000000000000002000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000002010002000000000101
      0102190F053E381E0B8D63462DCE816750ED947D68F8A18A78FDA08A77FD937C
      67F780654DEB5F4229CA371E0A88160B01380000000000000000010000020000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000170B013C4A2D
      14B18C725CF9B8ADA3FFD5D3D2FFE9EDEFFFF2F9FDFDF5FBFBFBF4FBFBFBF1F7
      FBFCE8ECEEFFD2D0CCFFB5A99EFF866C54F6462B13A90F030133000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000100000000010100064C2C077F7F6148F6C3BC
      B5FFEBEEF0FBF6F9F9F9F7FAFBFBEEEBE9FCE2D6D0FDD9CAC4FDD9CAC5FDE2D9
      D2FDEFEEECFCF8FAFAFAF7F9F9F9E9EDEDFCC0B7AFFF7F6448F1230F02730000
      0003000000000000000100000000000000000000000000000000000000000000
      00000000000000000000000000000401000D3E2006A993847AFFE7EAEDFCF4F9
      F9F9F2F0EEFCD1BDB1FDA67C66FE885234FE783D1CFD733514FE723414FE763A
      1AFE835137FFA47E6BFED3C2BBFEEEEDEEFDEEF2F5FAE2E2E2FF99836FFF3A1F
      089A030100080000000000000000000000000000000000000000000000000000
      000000000000000000010000000644290DA9A99A8EFFF1F7F8F8F2F2F1FCDCC4
      B0FD996748FD73340FFD73330DFE7D4019FE834B23FE874D25FF834A22FF7A3D
      1AFE6B2D0AFE622301FD63280BFF8F705DFEF1EEEAFDE7E8E8FBF0F4F5F9A18E
      7FFF341A059A0201000201000001000000000000000000000000000000000000
      0000010101010000000031190685978274FFE3E8EDF8DDDAD4FDC29674FD7E3E
      17FD773913FE8A522BFE976138FE9D683FFEA26B41FF9F6A40FE986238FF8F57
      2FFE6D4F41FF6B5044FF77635AFF927F78FF897D7BFF7C6A65FFCBC9C8FEE1E6
      EAF9927B6AFF2817087300000000000000010000000000000000000000000000
      000100000000241C0A40775E49F7BCC0C2FAB9BEC0FCB0896CFD78360DFD7F45
      23FE925C33FEA16C41FEAF7A51FEB68157FFB68159FEB37D55FFA97349FF9A69
      34FF7E6D67FFC2B8B5FFDFD6D4FFE3DCDAFFE5DFDDFFAB9C9AFF958F8EFEBABB
      BBFBBABDBFFC71553DF1130B0233000000000000000100000000000000010000
      000000000006462814BA9C9894FFA4A5A6F9AAAAA9FE9A6D4FFD83461EFE9059
      31FEA46E44FEB7835AFFC18D65FFC49068FEC48F66FFBD885FFF907059FFA390
      8AFFC4B6B2FFCDBFBBFFCFC3BFFFD2C7C3FFD6CBC8FFDCD2D0FFAEA29FFF8B79
      75FFA4AAADF999928CFF402813A9010000020000000000000000020101060000
      000016090246836E5CFAA9B0B7FBA79587FDB1A79FFD9A7154FE8E5025FF9D68
      3EFEB47F56FEC49068FECC9770FECD9870FFC9946BFFC08A61FF674E3AFFA192
      8CFFC1B2ADFFC8B9B5FFD1C5C3FFE1D9D7FFC9BBB8FFCABDB8FFD5CBC9FF8C79
      70FFB4A9A1FDA6ACB1FB7C6552F30B0602370000000001000002010101020000
      00003923119AA9A09AFFB3B7BBFAA77E5EFEB6B2B0FDA37D61FE975B2DFEA571
      47FFB7845DFFB38663FFD09B73FFCF9B71FFC9946BFFBF8258FF8A7C75FFA897
      8FFFB5A29CFFBBABA6FFFDFDFDFFFFFFFFFFE9E3E1FFD5C9C6FFD0C3BFFFA599
      94FF927969FFB9C1C7F9A1968DFF321D0D880000000002010104000000001011
      111166503DD8CACDCEFEBAAFA8FC99633EFECBD1D4FE927766FF8E572DFFAA74
      4AFF6F584EFF816A63FFBA8B66FFCE9A70FFA57958FF9B755AFF827772FFA08B
      86FFA8928BFFB4A09AFFF9F7F7FFFFFFFFFFE0D8D6FFD2C7C4FFDDD5D2FFBBB3
      B2FF8A5D40FEC9C8C8FBC3C3C2FE59402EC90707070700000000000000000700
      00388E7664F2E1E9EFFEAF9583FE925B3AFED0D7DCFF937A73FF77625BFF7A65
      5DFF998581FF9F8D89FF857069FF735E56FF967E77FF84746BFFA29F9AFFA192
      8EFF9E857DFFA69088FFBFB0ABFFD1C5C1FFC5B6B2FFCBBDB9FFDCD4D1FFB6AE
      ACFF7B390EFDC5B7ADFCDDE1E5FF846A57EB0200002700000000000000001E0E
      0252B09886FCE8F2F9FCA17B61FC976746FDBABBBDFF968179FFB8AAA7FFD3D3
      C8FFC1CBB3FFC0CCB1FFD1D6C6FFD3CECAFFA3918CFF7F726DFF8C8A8BFFAAA3
      A1FF9E8A84FFA48D86FFAB968FFFB39F99FFBAA9A3FFC9BCB8FFCEC7C7FFC0BB
      B9FF6F2D03FDB59A8BFCE8F0F6FDA78C76F7120500410000000000000000301E
      0E61C3B09FFFECF6FBFB7E6352FF76655CFF8B7B75FFC9BFB9FF9EB18AFF8FAA
      77FFA6B994FFABBD9BFF9CB68AFF94B17EFFD0D6C6FFA79A96FF6E615CFF695F
      5BFFAAA5A4FFB2A7A3FFAE9A93FFB5A49EFFC2B6B2FFE0DBDAFFB7B9BAFFC6B5
      AEFF6B2700FEAB8B79FDEEF7FBFCBEA997FD2315084D00000000000000003A2A
      1D63CEBCAFFFF2F8F8FB786458FFAE9A93FFBBACA8FF92A779FFA2B391FFCCCB
      C7FFB6C1ACFFC3C9BDFFD2D1CEFFBFC7B6FF8FAC78FFD3D3C7FFAB9B95FF8778
      73FF7E7B7AFFA59C94FFA49E9AFFD5D2D1FF857E79FFB5B5B3FFE6EAEBFFC9B9
      B3FE672100FEAA8A7AFCF5FCFCFCC8B7A9FD2B1D144E0000000000000000372C
      2354D5C7BDFDF9FAFBFB9B775CFEA0908CFFAAAD93FF9AAC85FFCECCC9FFB5BF
      AAFF83A66DFFA0B591FFCDCBC8FFCECCCAFFBCC2B3FF95AC7EFFC3B9B6FF8B81
      7CFFF0CDADFFBB885EFF68482EFF8F6241FF9C5E31FFCDBAAFFFF8FAFBFFCAB8
      AEFE611D00FDB99D8FFDFBFCFCFDCEBEB1F8281F194300000000000000002822
      1E3BD6CBC1F3E4E3E3FF87786EFFA59490FF899E6EFFB8BBADFFBCC1B2FF92AE
      7FFFC8D7C4FF98BA89FFDDDFDDFFD1D0CDFFCECDCAFF8CA474FFC4C4B4FF8C7D
      78FFC59570FFB27D51FFA66F43FF9C643BFF9B6034FEA67757FEFEFEFEFEC6AE
      A2FE611D00FDD1BFB5FCFDFEFEFECDC1B8ED1713102A00000000000000001010
      1014C4BFBADB908683FFC1B1ACFFA69590FF7A925CFFBFBEB6FFB9BDAEFFDBDF
      DCFFE4E9E7FF97BA86FFD6DFD4FFD0CECCFFD5D4D2FFA1B690FFBABEA8FFC1B1
      ACFF9C8F89FFC29067FFB57F55FFA16B40FF9A6036FF955D34FEEEE7E4FEB89C
      91FE702F14FEECE5E0FBFBFBFCFEBBB7B2CE0909090900000000020202020000
      0000919090A0CAC8C8FF948A87FFA08F88FF79905BFFB7B7ACFFC4BFBBFFD3D2
      CFFFE5E9E8FFBACFB2FFB6CBAEFFCECCC9FFD2D2CFFF9EB58BFFCCD0BFFFBFB4
      B1FFA48974FFC7916AFFBA845CFFA87248FF986137FE8C4F24FEE0D0C6FEA984
      71FD996C53FEFAFAFAFAFBFAF9FF7D7C7C8F0000000004040404060606060000
      00003232324DF4F3F4FCD9D9D9FCA2938DFF848E65FF9DA589FFC0BBB7FFC9C6
      C3FFDBDCDAFFDADEDAFF93B081FFCDCCC7FFCBCCC8FF8DA974FFDCD8D5FFAB9D
      97FFCB966EFFC48F68FEB7815AFEA46E44FE925C34FE85461CFED8C3B8FDAA80
      64FDD4BFB1FDFCFCFCFCEDECEBF73232323D0000000003030303020202020000
      000009090909B1B1B1C2B7B3B3FFC9BEBAFF9B8A7DFF768B54FFB0AFA3FFC0BB
      B6FFC3BEBBFFC6C3C0FFB5BCABFFB5BDA9FF9BAE88FFB0BB9CFFDFDAD8FFB7B0
      ABFFC69570FFBD885FFFAF794FFF996238FE874F28FE81441CFECDB5A4FDD5BD
      ACFEF6F5F4FAFDFDFDFFA1A1A1B2040404040000000000000000000000000101
      0101000000004040404AA29E9CFEB6ADABFFAFA29EFF8F8971FF768B55FF9CA4
      87FFB6B6ABFFBCBBB3FFAFB4A2FF8DA274FF96A87FFFD5CFCBFFD4D0CEFFD0CB
      C8FFAF7C55FFAC774EFE9B663BFE8A522AFE7B4220FE7B3A0FFDD7BFACFDFAFA
      FAFCF9F8F8FBEAEAEAF53333333C000000000101010100000000000000000000
      000001010101000000007F7F7F91FBFBFBFFACABA9FCB7AEAAFF9D9181FF8992
      6AFF7C925DFF7D965FFF8A9E70FFABAE95FFD1CAC6FFB8B2ADFFA5724BFFA670
      46FFA06A41FE966036FE87512AFE773B18FE7A3911FDB3805BFDF3EEE7FDF3F4
      F5F8F8F8F8FF6B6B6B8000000000010101010000000000000000000000000000
      000000000000010101011111110C9C9C9CB7FAFBFBFFDBD5D3FFD5D0CDFFBCB3
      AFFFB8ACA8FFBFB3AFFFC6BDBAFFD3CFCEFFF2EEEDFFA39287FF905930FF8B54
      2CFF834A23FE793B14FE75360FFD95582FFDD2AD90FDF5F3EFFCEEF0F1F8F9F8
      F8FF969696A80505050501010101000000000000000000000000000000000000
      000000000000000000000000000013131315A3A4A4B9B6B2B2FFD8D7D7FDEAED
      EFFBBCB8B8FFE4E1E0FF815E4DFE7B4021FE9C8B82FF7F5237FF783812FF7B3D
      17FD864B28FDA17151FECEAF96FEF2E6DAFDEEF3F6F9EFF0F1FCFFFFFFFF9595
      95AA0B0B0B0D0000000000000000000000000000000000000000000000000000
      0000000000000000000001010101000000000A0A0A0A7C7C7C90EEEEEEFAF1F0
      F0FFCBCCCEFCBDBEBEFDEFEDEBFCE3D8D2FDD5C1B6FDCDB8ABFDCEBAABFDD8C6
      BBFDE6DED7FDEFEEEBFBECEFF1F9E8EBEDFBF2F1F0FFEBEBEBF8525252830606
      0606000000000101010100000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000908D8D074C4C4C4FAFAF
      AFC1F3F2F1FFEDEDECFFECEDEDFFEAEDEEFFE8EBECFDE9EDEFFCEAEEF1FAEAED
      EFFBEAECEEFEECECECFFEEEDEDFFF2F1F0FDA8A8A8B936363641000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000101010101010101000000000606
      06083D3D3D4E939393A1CCCCCCDBE5E4E4F4EEEDEDFEEDECECFFEEEDECFFEDEC
      ECFDE3E2E2F2CACAC9D88C8C8C9A383838470303030500000000000000010000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000020202020000
      00000000000000000000080808152E2E2E3A4848485455555563555555624646
      46522C2C2C380606061100000000000000000000000002020202000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0002040404060303030300000000000000000000000000000000000000000000
      0000000000000000000004040404040404060000000200000000000000000000
      0000000000000000000000000000000000000000000000000000}
    ApplicationButton.Menu = BarAppMenu
    ApplicationButton.StretchGlyph = False
    BarManager = BarManager
    ColorSchemeName = 'Black'
    QuickAccessToolbar.Toolbar = rbnBarQuickAccess
    SupportNonClientDrawing = True
    TabOrder = 2
    TabStop = False
    object RibbonTabPackage: TdxRibbonTab
      Caption = 'Package'
      Groups = <
        item
          ToolbarName = 'rbnBarPackage'
        end
        item
          ToolbarName = 'rbnBarOrders'
        end
        item
          ToolbarName = 'rbnBarReferences'
        end
        item
          ToolbarName = 'rbnBarMisceleanous'
        end>
    end
    object RibbonTabHelp: TdxRibbonTab
      Active = True
      Caption = 'Help'
      Groups = <
        item
          ToolbarName = 'rbnBarOnlineGuide'
        end>
    end
  end
  object STDActions: TActionList
    Images = dmResources.SmallImages
    OnUpdate = STDActionsUpdate
    Left = 366
    Top = 340
    object actPackageNew: TAction
      Category = 'File'
      Caption = 'New package'
      ImageIndex = 0
      OnExecute = actPackageNewExecute
    end
    object actPackageOpen: TAction
      Category = 'File'
      Caption = 'Open'
      ImageIndex = 1
      OnExecute = actPackageOpenExecute
    end
    object actPackageClose: TAction
      Tag = 1
      Category = 'File'
      Caption = 'Close'
      ImageIndex = 38
      OnExecute = actPackageCloseExecute
    end
    object actPackageProperties: TAction
      Tag = 1
      Category = 'File'
      Caption = 'Properties'
      ImageIndex = 23
      OnExecute = actPackagePropertiesExecute
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      ImageIndex = 14
      OnExecute = actExitExecute
    end
    object actEditCut1: TAction
      Category = 'Edit'
      Caption = 'Cut'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 4
      ShortCut = 16472
      OnExecute = actEditCut1Execute
    end
    object actEditCopy1: TAction
      Category = 'Edit'
      Caption = 'Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 5
      ShortCut = 16451
      OnExecute = actEditCopy1Execute
    end
    object actEditPaste1: TAction
      Category = 'Edit'
      Caption = 'Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 6
      ShortCut = 16470
      OnExecute = actEditPaste1Execute
    end
    object actEditSelectAll1: TAction
      Category = 'Edit'
      Caption = 'Select All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
      OnExecute = actEditSelectAll1Execute
    end
    object actEditUndo1: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 36
      ShortCut = 16474
      OnExecute = actEditUndo1Execute
    end
    object HelpContent: TAction
      Category = 'Help'
      Caption = 'Contents'
      ImageIndex = 25
      OnExecute = HelpContents1Execute
    end
    object HelpSearch: TAction
      Category = 'Help'
      Caption = 'Search for Help'
      ImageIndex = 25
      OnExecute = HelpTopicSearch1Execute
    end
    object HelpOnHelp: TAction
      Category = 'Help'
      Caption = 'How to Use Help'
      OnExecute = HelpOnHelp1Execute
    end
    object HelpReadme: TAction
      Category = 'Help'
      Caption = 'Readme'
      ImageIndex = 25
      OnExecute = HelpReadmeExecute
    end
    object HelpAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      ImageIndex = 13
      OnExecute = HelpAboutExecute
    end
    object actWelcome: TAction
      Category = 'Help'
      Caption = 'Welcome'
      ImageIndex = 17
      OnExecute = actWelcomeExecute
    end
    object actOptions: TAction
      Category = 'Tools'
      Caption = 'Options...'
      OnExecute = actOptionsExecute
    end
    object actRptExplorer: TAction
      Category = 'Tools'
      Caption = 'Report Explorer...'
      ImageIndex = 26
      OnExecute = actRptExplorerExecute
    end
    object actExternTools: TAction
      Category = 'Tools'
      Caption = 'Manage external tools...'
      OnExecute = actExternToolsExecute
    end
    object actOrderNew: TAction
      Tag = 1
      Category = 'Orders'
      Caption = 'New order'
      ImageIndex = 40
      ShortCut = 16462
      OnExecute = actOrderNewExecute
    end
    object actOrderEdit: TAction
      Tag = 1
      Category = 'Orders'
      Caption = 'Edit order'
      ImageIndex = 41
      ShortCut = 16453
      OnExecute = actOrderEditExecute
    end
    object actOrderDelete: TAction
      Tag = 1
      Category = 'Orders'
      Caption = 'Delete order'
      ImageIndex = 42
      ShortCut = 16452
      OnExecute = actOrderDeleteExecute
    end
    object actOrderFind: TAction
      Tag = 1
      Category = 'Orders'
      Caption = 'Find'
      ImageIndex = 9
      ShortCut = 16454
      OnExecute = actOrderFindExecute
    end
    object actOrderOptimize: TAction
      Tag = 1
      Category = 'Orders'
      Caption = 'Optimize'
      ImageIndex = 22
      ShortCut = 16463
      OnExecute = actOrderOptimizeExecute
    end
    object actOrderPrint: TAction
      Tag = 1
      Category = 'Orders'
      Caption = 'Print'
      ImageIndex = 3
      ShortCut = 16464
      OnExecute = actOrderPrintExecute
    end
    object actRefresh: TAction
      Tag = 1
      Category = 'Orders'
      Caption = 'Refresh View'
      ImageIndex = 11
      ShortCut = 116
      OnExecute = actRefreshExecute
    end
    object actRefUoM: TAction
      Category = 'References'
      Caption = 'Unit of Measurement'
      ImageIndex = 43
      OnExecute = actRefUoMExecute
    end
    object actRefFeatures: TAction
      Category = 'References'
      Caption = 'Features'
      ImageIndex = 44
      OnExecute = actRefFeaturesExecute
    end
    object actRefLayingRule: TAction
      Category = 'References'
      Caption = 'Laying Rule'
      Enabled = False
      ImageIndex = 45
      Visible = False
      OnExecute = actRefLayingRuleExecute
    end
    object actRefItems: TAction
      Tag = 1
      Category = 'References'
      Caption = 'Items'
      ImageIndex = 46
      OnExecute = actRefItemsExecute
    end
    object actRefCustomers: TAction
      Category = 'References'
      Caption = 'Customers'
      ImageIndex = 47
      OnExecute = actRefCustomersExecute
    end
    object actRefStyles: TAction
      Tag = 1
      Category = 'References'
      Caption = 'Styles'
      ImageIndex = 48
      OnExecute = actRefStylesExecute
    end
    object actRefMaterials: TAction
      Category = 'References'
      Caption = 'Materials'
      ImageIndex = 49
      OnExecute = actRefMaterialsExecute
    end
    object actRepairPackage: TAction
      Tag = 1
      Category = 'Tools'
      Caption = 'Repair package'
      OnExecute = actRepairPackageExecute
    end
    object actCompactPackage: TAction
      Tag = 1
      Category = 'Tools'
      Caption = 'Compact Package'
      OnExecute = actCompactPackageExecute
    end
    object actRepairCompactExternPackage: TAction
      Category = 'Tools'
      Caption = 'Repair and compact external package...'
      OnExecute = actRepairCompactExternPackageExecute
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
      'Package Menu'
      'Order Menu'
      'References Menu'
      'View Menu'
      'Tools Menu'
      'Help Menu'
      'Menus'
      'Package'
      'Orders'
      'Miscelanous'
      'Help'
      'References')
    Categories.ItemsVisibles = (
      2
      2
      2
      2
      2
      2
      2
      2
      2
      2
      2
      2
      2)
    Categories.Visibles = (
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True)
    ImageOptions.Images = dmResources.SmallImages
    ImageOptions.LargeImages = dmResources.LargeImages
    ImageOptions.SmoothGlyphs = True
    ImageOptions.UseLargeImagesForLargeIcons = True
    LookAndFeel.Kind = lfOffice11
    LookAndFeel.NativeStyle = False
    PopupMenuLinks = <>
    Style = bmsUseLookAndFeel
    UseSystemFont = True
    OnBarAfterReset = BarManagerBarAfterReset
    Left = 338
    Top = 340
    DockControlHeights = (
      0
      0
      49
      0)
    object stdBarMain: TdxBar
      Caption = 'Main Menu'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 0
      FloatTop = 0
      FloatClientWidth = 0
      FloatClientHeight = 0
      IsMainMenu = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'Package1'
        end
        item
          Visible = True
          ItemName = 'Order1'
        end
        item
          Visible = True
          ItemName = 'References1'
        end
        item
          Visible = True
          ItemName = 'View1'
        end
        item
          Visible = True
          ItemName = 'Tools1'
        end
        item
          Visible = True
          ItemName = 'Help1'
        end>
      MultiLine = True
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = True
    end
    object stdBarStandard: TdxBar
      Caption = 'Standard'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 23
      DockingStyle = dsTop
      FloatLeft = 344
      FloatTop = 244
      FloatClientWidth = 804
      FloatClientHeight = 22
      ItemLinks = <
        item
          Visible = True
          ItemName = 'stdBtnPackageNew'
        end
        item
          Visible = True
          ItemName = 'stdBtnPackageOpen'
        end
        item
          Visible = True
          ItemName = 'stdBtnPackageClose'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnPackageProperties'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnOrderNew'
        end
        item
          Visible = True
          ItemName = 'stdBtnOrderFind'
        end
        item
          Visible = True
          ItemName = 'stdBtnOrderEdit'
        end
        item
          Visible = True
          ItemName = 'stdBtnOrderDelete'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnOrderOptimize'
        end
        item
          Visible = True
          ItemName = 'stdBtnOrderPrint'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnOrderRefresh'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'dxBarButton9'
        end
        item
          Visible = True
          ItemName = 'dxBarButton3'
        end>
      OneOnRow = True
      Row = 1
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbnBarQuickAccess: TdxBar
      Caption = 'Quick Access'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 884
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'stdBtnPackageNew'
        end
        item
          Visible = True
          ItemName = 'stdBtnPackageOpen'
        end
        item
          Visible = True
          ItemName = 'stdBtnPackageProperties'
        end
        item
          Visible = True
          ItemName = 'stdBtnPackageClose'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbnBarOnlineGuide: TdxBar
      Caption = 'Online Guide'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 884
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'rbnBtnHelpContent'
        end
        item
          Visible = True
          ItemName = 'rbnBtnReadme'
        end
        item
          Visible = True
          ItemName = 'rbnBtnHelpSearch'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'rbnBtnAbout'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbnBarPackage: TdxBar
      Caption = 'Package'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 884
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'rbnBtnNewPackage'
        end
        item
          Visible = True
          ItemName = 'rbnBtnOpenPackage'
        end
        item
          Visible = True
          ItemName = 'rbnBtnPackageProps'
        end
        item
          Visible = True
          ItemName = 'rbnBtnClosePackage'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbnBarOrders: TdxBar
      Caption = 'Manage Orders'
      CaptionButtons = <>
      DockedLeft = 141
      DockedTop = 0
      FloatLeft = 884
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'rbnBtnOrderNew'
        end
        item
          Visible = True
          ItemName = 'rbnBtnOrderEdit'
        end
        item
          Visible = True
          ItemName = 'rbnBtnOrderDelete'
        end
        item
          Visible = True
          ItemName = 'rbnBtnOrderFind'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'rbnBtnOrderOptimize'
        end
        item
          Visible = True
          ItemName = 'rbnBtnOrderPrint'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbnBarMisceleanous: TdxBar
      Caption = 'Misceleanous'
      CaptionButtons = <>
      DockedLeft = 647
      DockedTop = 0
      FloatLeft = 884
      FloatTop = 8
      FloatClientWidth = 104
      FloatClientHeight = 110
      ItemLinks = <
        item
          Visible = True
          ItemName = 'rbnBtnRefresh'
        end
        item
          Visible = True
          ItemName = 'rbnBtnTools'
        end
        item
          Visible = True
          ItemName = 'rbnBtnReport'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbnBarReferences: TdxBar
      Caption = 'Manage References'
      CaptionButtons = <>
      DockedLeft = 382
      DockedTop = 0
      FloatLeft = 501
      FloatTop = 353
      FloatClientWidth = 116
      FloatClientHeight = 230
      ItemLinks = <
        item
          Visible = True
          ItemName = 'rbnBtnStyles'
        end
        item
          BeginGroup = True
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'rbnBtnRefUoM'
        end
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'rbnBtnFeatures'
        end
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = False
          ItemName = 'rbnBtnLayingRule'
        end
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'rbnBtnMaterials'
        end
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'rbnBtnCustomers'
        end
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'rbnBtnItems'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object itmToolbar: TdxBarToolbarsListItem
      Caption = 'Tool Bars'
      Category = 0
      Visible = ivAlways
    end
    object dxRibbonQuickAccessGroupButton1: TdxRibbonQuickAccessGroupButton
      Category = 0
      Visible = ivAlways
    end
    object stdBtnPackageNew: TdxBarButton
      Action = actPackageNew
      Category = 1
      PaintStyle = psCaptionGlyph
    end
    object stdBtnPackageOpen: TdxBarButton
      Action = actPackageOpen
      Category = 1
    end
    object stdBtnPackageClose: TdxBarButton
      Action = actPackageClose
      Category = 1
    end
    object stdBtnPackageProperties: TdxBarButton
      Action = actPackageProperties
      Category = 1
    end
    object stdBtnExit: TdxBarButton
      Action = actExit
      Category = 1
    end
    object stdBtnOrderNew: TdxBarButton
      Action = actOrderNew
      Category = 2
      PaintStyle = psCaptionGlyph
    end
    object stdBtnOrderEdit: TdxBarButton
      Action = actOrderEdit
      Category = 2
    end
    object stdBtnOrderDelete: TdxBarButton
      Action = actOrderDelete
      Category = 2
    end
    object stdBtnOrderFind: TdxBarButton
      Action = actOrderFind
      Category = 2
    end
    object stdBtnOrderOptimize: TdxBarButton
      Action = actOrderOptimize
      Category = 2
      PaintStyle = psCaptionGlyph
    end
    object stdBtnOrderPrint: TdxBarButton
      Action = actOrderPrint
      Category = 2
    end
    object stdBtnOrderRefresh: TdxBarButton
      Action = actRefresh
      Category = 2
      PaintStyle = psCaptionGlyph
    end
    object stdBtnRefUoM: TdxBarButton
      Action = actRefUoM
      Category = 3
    end
    object stdBtnRefFeatures: TdxBarButton
      Action = actRefFeatures
      Category = 3
    end
    object stdBtnRefCustomers: TdxBarButton
      Action = actRefCustomers
      Category = 3
    end
    object stdBtnRefLayingRule: TdxBarButton
      Action = actRefLayingRule
      Category = 3
    end
    object stdBtnRefMaterials: TdxBarButton
      Action = actRefMaterials
      Category = 3
    end
    object stdBtnRefStyle: TdxBarButton
      Action = actRefStyles
      Category = 3
    end
    object stdBtnRefItems: TdxBarButton
      Action = actRefItems
      Category = 3
    end
    object ViewLogPanel: TdxBarButton
      Caption = 'Log Panel'
      Category = 4
      Hint = 'Log Panel'
      Visible = ivAlways
      ButtonStyle = bsChecked
      Down = True
      OnClick = ViewLogPanelClick
    end
    object itmTheme: TdxBarSubItem
      Caption = 'Themes'
      Category = 4
      Visible = ivAlways
      ImageIndex = 30
      ItemLinks = <>
    end
    object itmLanguage: TdxBarSubItem
      Caption = 'Languages'
      Category = 4
      Visible = ivAlways
      ItemLinks = <>
    end
    object Options1: TdxBarButton
      Action = actOptions
      Category = 5
    end
    object dxBarButton9: TdxBarButton
      Action = actRptExplorer
      Category = 5
    end
    object itmShowDesigner: TdxBarButton
      Caption = 'Show Designer'
      Category = 5
      Hint = 'Show Designer'
      Visible = ivAlways
      ButtonStyle = bsChecked
    end
    object dxBarButton1: TdxBarButton
      Action = actExternTools
      Category = 5
    end
    object itmExternalTools: TdxBarSubItem
      Caption = 'External Tools'
      Category = 5
      Visible = ivAlways
      ItemLinks = <>
    end
    object itmMaintenance: TdxBarSubItem
      Caption = 'Maintenance'
      Category = 5
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'stdBtnRepairPackage'
        end
        item
          Visible = True
          ItemName = 'stdBtnCompactPackage'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'itmRepairCompactExtrnPkg'
        end>
    end
    object stdBtnRepairPackage: TdxBarButton
      Action = actRepairPackage
      Category = 5
    end
    object stdBtnCompactPackage: TdxBarButton
      Action = actCompactPackage
      Category = 5
    end
    object itmRepairCompactExtrnPkg: TdxBarButton
      Action = actRepairCompactExternPackage
      Category = 5
    end
    object dxBarButton3: TdxBarButton
      Action = HelpContent
      Category = 6
    end
    object About1: TdxBarButton
      Action = HelpAbout
      Category = 6
    end
    object dxBarButton4: TdxBarButton
      Action = HelpSearch
      Category = 6
    end
    object dxBarButton5: TdxBarButton
      Action = HelpOnHelp
      Category = 6
    end
    object dxBarButton6: TdxBarButton
      Action = HelpReadme
      Category = 6
    end
    object Package1: TdxBarSubItem
      Caption = 'Package'
      Category = 7
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'stdBtnPackageNew'
        end
        item
          Visible = True
          ItemName = 'stdBtnPackageOpen'
        end
        item
          Visible = True
          ItemName = 'stdBtnPackageClose'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnPackageProperties'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnExit'
        end>
    end
    object Order1: TdxBarSubItem
      Caption = 'Orders'
      Category = 7
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'stdBtnOrderNew'
        end
        item
          Visible = True
          ItemName = 'stdBtnOrderEdit'
        end
        item
          Visible = True
          ItemName = 'stdBtnOrderDelete'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnOrderFind'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnOrderOptimize'
        end
        item
          Visible = True
          ItemName = 'stdBtnOrderPrint'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnOrderRefresh'
        end>
    end
    object References1: TdxBarSubItem
      Caption = 'References'
      Category = 7
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'stdBtnRefUoM'
        end
        item
          Visible = True
          ItemName = 'stdBtnRefFeatures'
        end
        item
          Visible = True
          ItemName = 'stdBtnRefLayingRule'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnRefItems'
        end
        item
          Visible = True
          ItemName = 'stdBtnRefCustomers'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnRefStyle'
        end>
    end
    object View1: TdxBarSubItem
      Caption = 'View'
      Category = 7
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'itmShowDesigner'
        end
        item
          Visible = True
          ItemName = 'ViewLogPanel'
        end
        item
          Visible = True
          ItemName = 'itmLanguage'
        end
        item
          Visible = True
          ItemName = 'itmTheme'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'itmToolbar'
        end>
    end
    object Tools1: TdxBarSubItem
      Caption = 'Tools'
      Category = 7
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton9'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'itmExternalTools'
        end
        item
          Visible = True
          ItemName = 'itmMaintenance'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'Options1'
        end
        item
          Visible = True
          ItemName = 'dxBarButton1'
        end>
    end
    object Help1: TdxBarSubItem
      Caption = 'Help'
      Category = 7
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton3'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'dxBarButton4'
        end
        item
          Visible = True
          ItemName = 'dxBarButton5'
        end
        item
          Visible = True
          ItemName = 'dxBarButton6'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'About1'
        end>
    end
    object rbnBtnNewPackage: TdxBarLargeButton
      Action = actPackageNew
      Category = 8
    end
    object rbnBtnOpenPackage: TdxBarButton
      Action = actPackageOpen
      Category = 8
    end
    object rbnBtnPackageProps: TdxBarButton
      Action = actPackageProperties
      Category = 8
    end
    object rbnBtnClosePackage: TdxBarButton
      Action = actPackageClose
      Category = 8
    end
    object rbnBtnOrderNew: TdxBarLargeButton
      Action = actOrderNew
      Category = 9
    end
    object rbnBtnOrderEdit: TdxBarButton
      Action = actOrderEdit
      Category = 9
    end
    object rbnBtnOrderDelete: TdxBarButton
      Action = actOrderDelete
      Category = 9
    end
    object rbnBtnOrderFind: TdxBarButton
      Action = actOrderFind
      Category = 9
    end
    object rbnBtnOrderOptimize: TdxBarLargeButton
      Action = actOrderOptimize
      Category = 9
    end
    object rbnBtnOrderPrint: TdxBarLargeButton
      Action = actOrderPrint
      Category = 9
    end
    object rbnBtnView: TdxBarSubItem
      Caption = 'View'
      Category = 10
      Visible = ivAlways
      ImageIndex = 35
      LargeImageIndex = 35
      ItemLinks = <
        item
          Visible = True
          ItemName = 'itmShowDesigner'
        end
        item
          Visible = True
          ItemName = 'ViewLogPanel'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'itmTheme'
        end
        item
          Visible = True
          ItemName = 'itmLanguage'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'itmToolbar'
        end>
    end
    object rbnBtnTools: TdxBarSubItem
      Caption = 'Tools'
      Category = 10
      Visible = ivAlways
      ImageIndex = 37
      LargeImageIndex = 37
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton9'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'itmShowDesigner'
        end
        item
          Visible = True
          ItemName = 'ViewLogPanel'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'itmLanguage'
        end
        item
          Visible = True
          ItemName = 'itmTheme'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'itmExternalTools'
        end
        item
          Visible = True
          ItemName = 'itmMaintenance'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'Options1'
        end
        item
          Visible = True
          ItemName = 'dxBarButton1'
        end>
    end
    object rbnBtnReport: TdxBarLargeButton
      Action = actRptExplorer
      Category = 10
    end
    object rbnBtnRefresh: TdxBarLargeButton
      Action = actRefresh
      Category = 10
    end
    object rbnBtnHelpContent: TdxBarLargeButton
      Action = HelpContent
      Category = 11
    end
    object rbnBtnReadme: TdxBarLargeButton
      Action = HelpReadme
      Category = 11
    end
    object rbnBtnHelpSearch: TdxBarLargeButton
      Action = HelpSearch
      Category = 11
    end
    object rbnBtnAbout: TdxBarLargeButton
      Action = HelpAbout
      Category = 11
    end
    object rbnBtnRefUoM: TdxBarLargeButton
      Action = actRefUoM
      Category = 12
    end
    object rbnBtnFeatures: TdxBarLargeButton
      Action = actRefFeatures
      Category = 12
    end
    object rbnBtnLayingRule: TdxBarLargeButton
      Action = actRefLayingRule
      Category = 12
    end
    object rbnBtnMaterials: TdxBarLargeButton
      Action = actRefMaterials
      Category = 12
    end
    object rbnBtnItems: TdxBarLargeButton
      Action = actRefItems
      Category = 12
    end
    object rbnBtnCustomers: TdxBarLargeButton
      Action = actRefCustomers
      Category = 12
    end
    object rbnBtnStyles: TdxBarLargeButton
      Action = actRefStyles
      Category = 12
    end
  end
  object dxDockingManager1: TdxDockingManager
    Color = clBtnFace
    DefaultHorizContainerSiteProperties.Dockable = True
    DefaultHorizContainerSiteProperties.ImageIndex = -1
    DefaultVertContainerSiteProperties.Dockable = True
    DefaultVertContainerSiteProperties.ImageIndex = -1
    DefaultTabContainerSiteProperties.Dockable = True
    DefaultTabContainerSiteProperties.ImageIndex = -1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    LookAndFeel.Kind = lfOffice11
    LookAndFeel.NativeStyle = False
    ViewStyle = vsUseLookAndFeel
    Left = 394
    Top = 340
  end
  object MainPopupMenu: TPopupMenu
    Images = dmResources.SmallImages
    Left = 393
    Top = 311
    object Neworder1: TMenuItem
      Action = actOrderNew
    end
    object Editorder1: TMenuItem
      Action = actOrderEdit
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Deleteorder1: TMenuItem
      Action = actOrderDelete
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Find1: TMenuItem
      Action = actOrderFind
    end
    object Optimize1: TMenuItem
      Action = actOrderOptimize
    end
    object Print1: TMenuItem
      Action = actOrderPrint
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object References2: TMenuItem
      Caption = 'References'
      object Items1: TMenuItem
        Action = actRefItems
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object UnitofMeasurement1: TMenuItem
        Action = actRefUoM
      end
      object Features1: TMenuItem
        Action = actRefFeatures
      end
      object LayingRule1: TMenuItem
        Action = actRefLayingRule
      end
      object Materials1: TMenuItem
        Action = actRefMaterials
      end
      object Customers1: TMenuItem
        Action = actRefCustomers
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Styles1: TMenuItem
        Action = actRefStyles
      end
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Print2: TMenuItem
      Action = actOrderPrint
    end
    object ReportExplorer1: TMenuItem
      Action = actRptExplorer
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object RefreshView1: TMenuItem
      Action = actRefresh
    end
  end
  object BarAppMenu: TdxBarApplicationMenu
    BarManager = BarManager
    Buttons = <
      item
        Item = stdBtnExit
      end>
    ExtraPane.Items = <>
    ExtraPane.OnItemClick = BarAppMenuExtraPaneItemClick
    ItemLinks = <
      item
        Visible = True
        ItemName = 'rbnBtnNewPackage'
      end
      item
        Visible = True
        ItemName = 'rbnBtnOpenPackage'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'rbnBtnPackageProps'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'rbnBtnClosePackage'
      end>
    UseOwnFont = False
    Left = 420
    Top = 310
  end
end
