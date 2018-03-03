object xfrOpenSaveDialog: TxfrOpenSaveDialog
  Left = 194
  Top = 221
  BorderStyle = bsDialog
  Caption = 'Save As'
  ClientHeight = 291
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    414
    291)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSaveIn: TLabel
    Left = 11
    Top = 10
    Width = 39
    Height = 13
    Caption = 'Save &in:'
  end
  object btnUpOneLevel: TSpeedButton
    Left = 289
    Top = 7
    Width = 23
    Height = 22
    OnClick = btnUpOneLevelClick
  end
  object btnNewFolder: TSpeedButton
    Left = 323
    Top = 7
    Width = 23
    Height = 22
    OnClick = btnNewFolderClick
  end
  object btnListView: TSpeedButton
    Left = 356
    Top = 7
    Width = 23
    Height = 22
    GroupIndex = 1
    Down = True
    OnClick = btnListViewClick
  end
  object btnDetailView: TSpeedButton
    Left = 380
    Top = 7
    Width = 23
    Height = 22
    GroupIndex = 1
    OnClick = btnDetailViewClick
  end
  object lblItemName: TLabel
    Left = 8
    Top = 227
    Width = 52
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Item &name:'
  end
  object lblSaveAsType: TLabel
    Left = 8
    Top = 253
    Width = 65
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Save as &type:'
    Visible = False
  end
  object cbFolders: TComboBox
    Left = 54
    Top = 7
    Width = 227
    Height = 21
    Style = csOwnerDrawVariable
    ItemHeight = 15
    TabOrder = 0
    OnChange = cbFoldersChange
    OnDrawItem = cbFoldersDrawItem
    OnDropDown = cbFoldersDropDown
  end
  object edItemName: TEdit
    Left = 80
    Top = 224
    Width = 233
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    OnChange = edItemNameChange
  end
  object cbFileType: TComboBox
    Left = 80
    Top = 250
    Width = 233
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 2
    Visible = False
  end
  object btnOK: TButton
    Left = 334
    Top = 225
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Save'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 334
    Top = 253
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object Panel1: TPanel
    Left = 4
    Top = 34
    Width = 406
    Height = 183
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 5
    object ListView: TListView
      Left = 0
      Top = 0
      Width = 406
      Height = 183
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 200
        end
        item
          Caption = 'Size'
          Width = 75
        end
        item
          Caption = 'Type'
          Width = 100
        end
        item
          Caption = 'Date'
          Width = 130
        end>
      ColumnClick = False
      DragMode = dmAutomatic
      FlatScrollBars = True
      MultiSelect = True
      PopupMenu = PopUpItems
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = ListViewChange
      OnDblClick = ListViewDblClick
      OnEdited = ListViewEdited
      OnEditing = ListViewEditing
      OnKeyDown = ListViewKeyDown
    end
  end
  object PopUpItems: TPopupMenu
    Left = 379
    Top = 140
    object itmView: TMenuItem
      Caption = 'View'
      object ItmViewList: TMenuItem
        Caption = '&List'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = btnListViewClick
      end
      object itmViewDetails: TMenuItem
        Caption = '&Details'
        GroupIndex = 1
        RadioItem = True
        OnClick = btnDetailViewClick
      end
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object itmNewFolder: TMenuItem
      Caption = 'New &Folder'
      OnClick = btnNewFolderClick
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object itmDelete: TMenuItem
      Caption = '&Delete'
      OnClick = itmDeleteClick
    end
    object itmRename: TMenuItem
      Caption = 'Rena&me'
      OnClick = itmRenameClick
    end
  end
end
