object FormVCLServer: TFormVCLServer
  Left = 0
  Top = 0
  Caption = 'Tethering Server to copy DB-Tables'
  ClientHeight = 611
  ClientWidth = 724
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    Left = 0
    Top = 121
    Width = 724
    Height = 490
    Align = alClient
    Caption = 'PanelBottom'
    TabOrder = 0
    object txaProt: TMemo
      Left = 1
      Top = 1
      Width = 722
      Height = 488
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 724
    Height = 121
    Align = alTop
    BevelOuter = bvNone
    Color = clSkyBlue
    ParentBackground = False
    TabOrder = 1
    object btnClearProt: TButton
      Left = 16
      Top = 33
      Width = 154
      Height = 58
      Caption = 'Clear Prot.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnClearProtClick
    end
    object btnStartStop: TButton
      Left = 560
      Top = 33
      Width = 154
      Height = 58
      Caption = 'Start'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = btnStartStopClick
    end
    object rgbxStreamType: TRadioGroup
      Left = 227
      Top = 22
      Width = 120
      Height = 90
      Caption = 'Stream type'
      ItemIndex = 0
      Items.Strings = (
        'Bin'
        'JSON'
        'XML')
      TabOrder = 2
    end
    object rgbxStreamCompr: TRadioGroup
      Left = 372
      Top = 22
      Width = 120
      Height = 90
      Caption = 'Stream compress'
      ItemIndex = 0
      Items.Strings = (
        'Not Compress'
        'Fast Compress'
        'Default Compress'
        'Max Compress')
      TabOrder = 3
    end
  end
  object TetherServerManager: TTetheringManager
    OnEndManagersDiscovery = TetherServerManagerEndManagersDiscovery
    OnPairedFromLocal = TetherServerManagerPairedFromLocal
    OnPairedToRemote = TetherServerManagerPairedToRemote
    OnRequestManagerPassword = TetherServerManagerRequestManagerPassword
    OnNewManager = TetherServerManagerNewManager
    OnUnPairManager = TetherServerManagerUnPairManager
    OnRemoteManagerShutdown = TetherServerManagerRemoteManagerShutdown
    Text = '???'
    Enabled = False
    AllowedAdapters = 'Network'
    Left = 64
    Top = 176
  end
  object TetherServerProfile: TTetheringAppProfile
    Manager = TetherServerManager
    Text = '???'
    Group = 'TetherServerGroup'
    OnDisconnect = TetherServerProfileDisconnect
    OnAfterConnectProfile = TetherServerProfileAfterConnectProfile
    Actions = <>
    Resources = <>
    OnResourceReceived = TetherServerProfileResourceReceived
    Left = 64
    Top = 256
  end
  object bmTransfer: TFDBatchMove
    Reader = bmReader
    Writer = bmWriter
    Mappings = <>
    LogFileName = 'Data.log'
    Left = 384
    Top = 304
  end
  object bmReader: TFDBatchMoveDataSetReader
    DataSet = tblSrc
    Left = 312
    Top = 304
  end
  object Database: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        SourceDataType = dtDateTimeStamp
        TargetDataType = dtDateTime
      end>
    Connected = True
    LoginPrompt = False
    Left = 480
    Top = 200
  end
  object FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink
    Left = 344
    Top = 208
  end
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'FMX'
    ScreenCursor = gcrHourGlass
    Left = 328
    Top = 152
  end
  object mqSrcTables: TFDMetaInfoQuery
    Connection = Database
    TableKinds = [tkTable]
    ObjectScopes = [osMy, osOther]
    Left = 440
    Top = 152
  end
  object FDStanStorageBinLink: TFDStanStorageBinLink
    Left = 456
    Top = 385
  end
  object tblSrc: TFDTable
    Connection = Database
    Left = 264
    Top = 345
  end
  object bmWriter: TFDBatchMoveDataSetWriter
    Left = 464
    Top = 305
  end
  object FDStanStorageJSONLink: TFDStanStorageJSONLink
    Left = 456
    Top = 433
  end
  object FDStanStorageXMLLink: TFDStanStorageXMLLink
    Left = 456
    Top = 481
  end
end
