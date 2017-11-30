unit FMXClientForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  uTetherUtil, System.ZLib,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  IPPeerServer, System.Tether.Manager, System.Tether.AppProfile,
  TimeMem,
  FMX.ListView.Types, FMX.ListView, FMX.StdCtrls, Fmx.Bind.GenData,
  Data.Bind.GenData, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, System.Actions, FMX.ActnList, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.Objects, FMX.ScrollBox, FMX.Memo,
  FMX.Grid.Style, FMX.Grid, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.UI.Intf, FireDAC.FMXUI.Wait, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.DApt,
  FireDAC.Comp.BatchMove.DataSet, FireDAC.Comp.BatchMove,
  FireDAC.Comp.BatchMove.SQL, FireDAC.Comp.UI, FireDAC.Stan.StorageBin,
  FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageXML;

const
 c_sSQLiteFileName = 'client.s3db';
 c_sBtnTextToCopyTables = 'Get Tables from Server';

type
  TFormFMXClient = class(TForm)
    TetherClientManager: TTetheringManager;
    TetherClientProfile: TTetheringAppProfile;
    ToolBarTop: TToolBar;
    Label1: TLabel;
    ToolBar2: TToolBar;
    btnSync: TButton;
    ActionList1: TActionList;
    actGetList: TAction;
    txaProt: TMemo;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    bmWriter: TFDBatchMoveSQLWriter;
    bmTransfer: TFDBatchMove;
    bmReader: TFDBatchMoveDataSetReader;
    Database: TFDConnection;
    FDStanStorageBinLink: TFDStanStorageBinLink;
    tblSQLite: TFDTable;
    btnResetlDB: TButton;
    FDStanStorageXMLLink: TFDStanStorageXMLLink;
    FDStanStorageJSONLink: TFDStanStorageJSONLink;
    chbxWithProt: TCheckBox;
    rbtnNetwork: TRadioButton;
    rbtnBluetooth: TRadioButton;
    procedure btnSyncClick(Sender: TObject);
    procedure TetherClientManagerRemoteManagerShutdown(const Sender: TObject;
      const ManagerIdentifier: string);
    procedure TetherClientManagerRequestManagerPassword(const Sender: TObject;
      const RemoteIdentifier: string; var Password: string);
    procedure TetherClientManagerPairedToRemote(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetherClientManagerUnPairManager(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetherClientProfileDisconnect(const Sender: TObject;
      const AProfileInfo: TTetheringProfileInfo);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TetherClientProfileResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure TetherClientProfileAfterConnectProfile(const Sender: TObject;
      const AProfileInfo: TTetheringProfileInfo);
    procedure TetherClientManagerEndProfilesDiscovery(const Sender: TObject;
      const ARemoteProfiles: TTetheringProfileInfoList);
    procedure TetherClientManagerEndManagersDiscovery(const Sender: TObject;
      const ARemoteManagers: TTetheringManagerInfoList);
    procedure FormDestroy(Sender: TObject);
    procedure btnResetlDBClick(Sender: TObject);
  private
    { Private declarations }
    m_lIsConnected:  Boolean;
    m_oClientInfo: TClientInfo;
    m_oServerInfo: TServerInfo;
    m_oTimeMem       : TTimeMem;
    procedure HandleDisconnect(i_lWithTetherManagerDisable: Boolean);
    procedure CheckRemoteProfiles;
    procedure CreateDBTable(l_oMemTable: TFdMemTable; i_sTab: String);
    function  LoadTableFromStream(const AResource: TRemoteResource;
                           i_oSendDesc:  TSendDesc): Boolean;
    procedure WriteProt(i_sText: String);
  public
    { Public declarations }
  end;

var
  FormFMXClient: TFormFMXClient;

implementation

{$R *.fmx}

uses uGlobal;


procedure TFormFMXClient.FormCreate(Sender: TObject);
begin
 m_oTimeMem := TTimeMem.Create;
 m_oClientInfo := TClientInfo.Create;
 m_oServerInfo := TServerInfo.Create;
 btnSync.Text  := c_sBtnTextToCopyTables;
end;

procedure TFormFMXClient.FormDestroy(Sender: TObject);
begin
 m_oTimeMem.Free;
 m_oClientInfo.Free;
 m_oServerInfo.Free;
end;

procedure TFormFMXClient.FormShow(Sender: TObject);
var
 l_sDBFilePath: String;
begin
 m_lIsConnected := False;
 txaProt.Lines.Clear;
{$IF DEFINED(iOS) or DEFINED(ANDROID)}
 l_sDBFilePath := TPath.Combine(TPath.GetDocumentsPath, c_sSQLiteFileName);
{$ELSE}
 l_sDBFilePath := c_sSQLiteFileName;
{$ENDIF}
 if FileExists(l_sDBFilePath) then begin
   self.WriteProt(Format('Destination file [%s] exists. Deleting', [c_sSQLiteFileName]));
   DeleteFile(l_sDBFilePath);
 end;
 Database.Params.Clear;
 Database.Params.DriverID := 'SQLite';
 Database.Params.Database := l_sDBFilePath;
 TFDPhysSQLiteConnectionDefParams(Database.Params).OpenMode := TFDSQLiteOpenMode.omCreateUTF8;
 //-- TFDPhysSQLiteConnectionDefParams(conDest.Params).DateTimeFormat := TFDSQLiteDateTimeFormat.dtfDateTime;
 Database.Connected := True;
end;

procedure TFormFMXClient.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Database.Connected := False;
 TetherClientManager.Enabled := False;
end;

procedure TFormFMXClient.TetherClientProfileResourceReceived(
  const Sender: TObject; const AResource: TRemoteResource);
var
 l_lOk: Boolean;
 l_sStr, l_sServerId: String;
 l_oSendDesc:  TSendDesc;
begin
 l_lOk := True;
 self.WriteProt('Server Resource Received:');
 self.WriteProt('- Remote Resource Name: ' + aResource.Name);
 self.WriteProt('- Remote Resource Hint: ' + aResource.Hint);
 l_sServerId   := l_oSendDesc.ParseString(aResource.Hint);
 self.WriteProt('- Server Send Desc: ' + l_oSendDesc.ToString());
 if m_oServerInfo.ServerId = l_sServerId then begin
   if aResource.ResType = TRemoteResourceType.Data then begin
     self.WriteProt(' as String!');
     if l_oSendDesc.SType = TEnumSendType.MSG_SYNC_END then begin
       self.WriteProt(' Copy End - at: ' + DateTimeToStr(Now));
       self.HandleDisconnect(True)
     end;
   end
   else if aResource.ResType = TRemoteResourceType.Stream then begin
     self.WriteProt(' as Stream - at: ' + DateTimeToStr(Now) + '; Size: ' + IntToStr(AResource.Value.AsStream.Size));
     if (l_oSendDesc.SType = TEnumSendType.MSG_TBL_NEW)  or
        (l_oSendDesc.SType = TEnumSendType.MSG_TBL_DATA_CHNG) then begin
       if l_oSendDesc.SType = TEnumSendType.MSG_TBL_NEW then begin
         l_lOk := self.LoadTableFromStream(aResource,l_oSendDesc);
       end
       else if l_oSendDesc.SType = TEnumSendType.MSG_TBL_DATA_CHNG then begin
         // Erstmals als Tabelle speichern!
         // TODO:
         l_lOk := self.LoadTableFromStream(aResource,l_oSendDesc);
       end;
       if l_lOk then begin
         // Bereit zur nächsten Aktion!
         l_oSendDesc.Clear(TEnumSendType.MSG_READY_FOR_RECEIVE);
         l_sStr := l_oSendDesc.BuildString(m_oClientInfo.ClientId);
         TetherClientProfile.SendString(m_oServerInfo.ProfileInfo,l_sStr,'-');
       end;
     end;
   end;
 end;
end;

procedure TFormFMXClient.btnResetlDBClick(Sender: TObject);
begin
 txaProt.Lines.Clear;
 if FileExists(c_sSQLiteFileName) then begin
   if Database.Connected then
     Database.Connected := False;
   self.WriteProt(Format('Destination file [%s] exists. Deleting', [c_sSQLiteFileName]));
   DeleteFile(c_sSQLiteFileName);
   Database.Connected := True;
 end;
end;

procedure TFormFMXClient.btnSyncClick(Sender: TObject);
begin
 if btnSync.Text = c_sBtnTextToCopyTables then begin
   TetherClientManager.Tag := 0;
   if not TetherClientManager.Enabled then begin
     if rbtnBluetooth.IsChecked then
       TetherClientManager.AllowedAdapters := 'Bluetooth'
     else
       TetherClientManager.AllowedAdapters := 'Network';
     self.BeginUpdate;
     rbtnBluetooth.Enabled := False;
     rbtnNetwork.Enabled   := False;
     self.EndUpdate;
     TetherClientManager.Enabled := True;
   end;
   btnSync.Text := 'Stop';
   m_oServerInfo.Clear;
   if Length(m_oClientInfo.ClientId) = 0 then
     m_oClientInfo.ClientId := uTetherUtil.BuildIdent(uGlobal.g_sComputerName, uGlobal.g_sUserName);;
   TetherClientManager.Text := uTetherUtil.BuildTetherText(uTetherUtil.TetherAppIdentClient,m_oClientInfo.ClientId);
   TetherClientProfile.Group:= uTetherUtil.TetherAppIdentClient;
   TetherClientProfile.Text := m_oClientInfo.ClientId;
   TetherClientManager.DiscoverManagers();
   m_oTimeMem.Start;
 end
 else begin
   self.HandleDisconnect(True);
 end;
end;

procedure TFormFMXClient.TetherClientManagerEndManagersDiscovery(
  const Sender: TObject; const ARemoteManagers: TTetheringManagerInfoList);
var
 i: Integer;
l_sName,l_sIdent: String;
begin
 self.WriteProt('End of Searching all "Tether Managers"!');
 for i := 0 to ARemoteManagers.Count-1 do begin
   if Pos(uTetherUtil.TetherAppIdentServer,ARemoteManagers[i].ManagerText) > 0 then begin
     m_oServerInfo.ManagerInfo := ARemoteManagers[i];
     TetherClientManager.PairManager(m_oServerInfo.ManagerInfo);
     uTetherUtil.ParseTetherText(ARemoteManagers[i].ManagerText,l_sName,l_sIdent);
     self.WriteProt('- ServerAppIdent: ' + l_sName);
     self.WriteProt('- ServerIdent:   ' + l_sIdent);
     self.WriteProt('- Found Server Manager: ' + uTetherUtil.TetherAppIdentServer);
     break;
   end;
 end;
 if m_oServerInfo.ManagerInfo.ManagerText = '' then begin
   self.WriteProt('-> No found "Tether Manager"!');
   self.HandleDisconnect(True);
 end;
 m_oTimeMem.Start;
end;

procedure TFormFMXClient.TetherClientManagerEndProfilesDiscovery(
  const Sender: TObject; const ARemoteProfiles: TTetheringProfileInfoList);
var
 i: Integer;
 l_sStr: String;
 l_oSendDesc: TSendDesc;
begin
 self.WriteProt('End of Searching all "Tether Profiles" of the "Tether Manager"!');
 m_oServerInfo.ProfileInfo.ProfileText := '';
 for i := 0 to TetherClientManager.RemoteProfiles.Count - 1 do begin
   self.WriteProt('- ' + IntToStr(i) + '. ManagerIdentifier: ' + ARemoteProfiles.Items[I].ManagerIdentifier);
   self.WriteProt('-    ' + 'ProfileIdentifier: ' + ARemoteProfiles.Items[I].ProfileIdentifier);
   self.WriteProt('-    ' + 'AppIdent : ' + ARemoteProfiles.Items[I].ProfileGroup);
   self.WriteProt('-    ' + 'ServerId: ' + ARemoteProfiles.Items[I].ProfileText);
   if ARemoteProfiles.Items[i].ProfileGroup = uTetherUtil.TetherAppIdentServer then begin
     self.WriteProt('- Found a Server Profile of the App "' + uTetherUtil.TetherAppIdentServer + '"!');
     m_oServerInfo.ProfileInfo := TetherClientManager.RemoteProfiles.Items[I];
     break;
   end;
 end;
 if Length(m_oServerInfo.ProfileInfo.ProfileText) > 0 then begin
   TetherClientProfile.Connect(m_oServerInfo.ProfileInfo);
   l_oSendDesc.Clear(TEnumSendType.MSG_READY_FOR_RECEIVE);
   l_sStr := l_oSendDesc.BuildString(m_oClientInfo.ClientId);
   self.WriteProt(' - Text Message send to Server: ' + l_oSendDesc.ToString);
   TetherClientProfile.SendString(m_oServerInfo.ProfileInfo,l_sStr,'-');
   self.WriteProt(' - Now client is ready to receive data!');
 end;
 m_oTimeMem.Start;
end;

procedure TFormFMXClient.TetherClientManagerPairedToRemote(const Sender: TObject;
  const AManagerInfo: TTetheringManagerInfo);
var
l_sName,l_sIdent: String;
begin
 // Verbindung zum Remote hergestellt!
 uTetherUtil.ParseTetherText(aManagerInfo.ManagerText,l_sName,l_sIdent);
 m_oServerInfo.ServerId := l_sIdent;
 self.WriteProt('Connection to "Server Manager" (Paired to Remote):');
 self.WriteProt(' - AppIdent     : ' + l_sName);
 self.WriteProt(' - ServerIdent : ' + l_sIdent);
 TetherClientManager.DiscoverProfiles(AManagerInfo);
end;

procedure TFormFMXClient.TetherClientManagerRequestManagerPassword(
  const Sender: TObject; const RemoteIdentifier: string; var Password: string);
begin
 self.WriteProt('Request Manager Password - Identifier: ' + RemoteIdentifier);
 Password := uTetherUtil.Password;
end;

procedure TFormFMXClient.TetherClientProfileAfterConnectProfile(
  const Sender: TObject; const AProfileInfo: TTetheringProfileInfo);
begin
 self.WriteProt('Profile connect:');
 self.WriteProt(' - ManagerIdent: ' + AProfileInfo.ManagerIdentifier);
 self.WriteProt(' - ProfileIdent: ' + AProfileInfo.ProfileIdentifier);
 self.WriteProt(' - AppIdent     : ' + AProfileInfo.ProfileGroup);
 self.WriteProt(' - ServerIdent : ' + AProfileInfo.ProfileText);
 if AProfileInfo.ProfileGroup = uTetherUtil.TetherAppIdentServer then begin
   self.WriteProt(' ->  Correct "Server Profile"!');
   m_oServerInfo.ServerId    := AProfileInfo.ProfileText;
   m_oServerInfo.ProfileInfo := AProfileInfo;
 end
 else begin
   self.WriteProt(' ->  No correct "Server Profile"! -> Disconnect');
   try
     TetherClientProfile.Disconnect(AProfileInfo);
   except
   end;
 end;
end;

procedure TFormFMXClient.TetherClientProfileDisconnect(const Sender: TObject; const AProfileInfo: TTetheringProfileInfo);
begin
 self.WriteProt('Profile disconnect:');
 self.WriteProt(' - ManagerIdent: ' + AProfileInfo.ManagerIdentifier);
 self.WriteProt(' - ProfileIdent: ' + AProfileInfo.ProfileIdentifier);
 self.WriteProt(' - AppIdent     : ' + AProfileInfo.ProfileGroup);
 self.WriteProt(' - ServerIdent : ' + AProfileInfo.ProfileText);
 if AProfileInfo.ProfileGroup = uTetherUtil.TetherAppIdentServer then
   self.HandleDisconnect(False);
end;

procedure TFormFMXClient.TetherClientManagerUnPairManager(const Sender: TObject;
  const AManagerInfo: TTetheringManagerInfo);
begin
 // Verbindung zum Remote unterbrochen!
 self.WriteProt('Connection to Remote broken - ' + AManagerInfo.ManagerIdentifier + ' - ' + AManagerInfo.ManagerName + ' - ' + AManagerInfo.ConnectionString + ' - ' + aManagerInfo.ManagerText);
 self.HandleDisconnect(False);
end;

procedure TFormFMXClient.TetherClientManagerRemoteManagerShutdown(const Sender: TObject;
  const ManagerIdentifier: string);
begin
 self.WriteProt('Remote Manager Shutdown - Identifier: ' + ManagerIdentifier);
 self.HandleDisconnect(True);
end;

procedure TFormFMXClient.HandleDisconnect(i_lWithTetherManagerDisable: Boolean);
begin
 TetherClientManager.Tag := TetherClientManager.Tag + 1;
 if TetherClientManager.Tag = 1 then begin
   if btnSync.Text <> c_sBtnTextToCopyTables then begin
     try
       if Length(m_oServerInfo.ProfileInfo.ProfileText) > 0 then begin
         TetherClientProfile.Disconnect(m_oServerInfo.ProfileInfo);
         m_oServerInfo.ProfileInfo.ProfileText := '';
       end;
     except
       on E: Exception do begin
         txaProt.Lines.Add('Exception in TFormFMXClient.HandleDisconnect: ' + E.Message);
         txaProt.Lines.Add('- Exception by "Profile Disconnect"!');
       end;
     end;
     try
       if Length(m_oServerInfo.ManagerInfo.ManagerText) > 0 then begin
         TetherClientManager.UnPairManager(m_oServerInfo.ManagerInfo);
         m_oServerInfo.ManagerInfo.ManagerText := '';
       end;
     except
       on E: Exception do begin
         txaProt.Lines.Add('Exception in TFormFMXClient.HandleDisconnect: ' + E.Message);
         txaProt.Lines.Add('- Exception by "UnPair Manager"!');
       end;
     end;
     m_oClientInfo.Clear;
     m_oServerInfo.Clear;
     self.BeginUpdate;
     btnSync.Text   := c_sBtnTextToCopyTables;
     self.EndUpdate;
     {--  Bleibt ansonsten hängen!!!
     if i_lWithTetherManagerDisable then begin
       try
         if TetherClientManager.Enabled then
           TetherClientManager.Enabled := False;
       except
         on E: Exception do begin
           txaProt.Lines.Add('Exception in TFormFMXClient.HandleDisconnect: ' + E.Message);
           txaProt.Lines.Add('- Exception by "Tether Client Manager" disable!');
         end;
       end;
     end;
     --}
     m_oTimeMem.ProtInfo(txaProt.Lines,False,'>>> End of "Copy tables" <<<');
     m_oTimeMem.Stop;
   end;
 end;
 TetherClientManager.Tag := TetherClientManager.Tag - 1;
end;

procedure TFormFMXClient.CheckRemoteProfiles;
var
 i: Integer;
 l_sStr: String;
begin
 if TetherClientManager.RemoteProfiles.Count > 0 then begin
   l_sStr := '';
   for I := 0 to TetherClientManager.RemoteProfiles.Count - 1 do begin
     if Length(l_sStr) > 0 then
       l_sStr := l_sStr + '; ';
     l_sStr := l_sStr + TetherClientManager.RemoteProfiles.Items[I].ProfileIdentifier;
   end;
   if not m_lIsConnected then
     //--
   m_lIsConnected := True;
 end
 else begin
   m_lIsConnected := False;
 end;
end;

function TFormFMXClient.LoadTableFromStream(const AResource: TRemoteResource;
                           i_oSendDesc:  TSendDesc): Boolean;
var
 l_lOk: Boolean;
 l_iCount: Integer;
 l_sTableName: String;
 l_oMemTable: TFdMemTable;
 l_oStorageFormat: TFDStorageFormat;
 l_oComprLevel:    TCompressionLevel;
 l_oMemTableStream, l_oMemComprStream: TMemoryStream;
 l_arBuf: array[0..4095] of Byte;
begin
 l_lOk := True;
 l_oMemTable  := Nil;
 if Database.Connected then begin
   //
   if i_oSendDesc.SStreamType = TEnumStreamType.STR_JSON then
     l_oStorageFormat := TFDStorageFormat.sfJSON
   else if i_oSendDesc.SStreamType = TEnumStreamType.STR_XML then
     l_oStorageFormat := TFDStorageFormat.sfXML
   else
     l_oStorageFormat := TFDStorageFormat.sfBinary;
   //
   l_sTableName := Database.EncodeObjectName('', '', '',i_oSendDesc.STableName);
   if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_NO_COMPR) then begin
     try
       l_oMemTable := TFdMemTable.Create(Nil);
       AResource.Value.AsStream.Position := 0;
       l_oMemTable.LoadFromStream(AResource.Value.AsStream,l_oStorageFormat);
     except
       on E: Exception do begin
         l_lOk := False;
         txaProt.Lines.Add('Exception in TFormFMXClient.LoadTableFromStream: ' + E.Message);
         txaProt.Lines.Add('- Exception by loading a Stream!');
       end;
     end;
   end
   else begin
     if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_COMPR_FAST) then
       l_oComprLevel := TCompressionLevel.clFastest
     else if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_COMPR_DFT) then
       l_oComprLevel := TCompressionLevel.clDefault
     else if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_COMPR_MAX) then
       l_oComprLevel := TCompressionLevel.clMax
     else
       l_oComprLevel := TCompressionLevel.clNone;
     l_oMemTableStream := TMemoryStream.Create;
     l_oMemComprStream := TMemoryStream.Create;
     try
       try
         AResource.Value.AsStream.Position := 0;
         l_oMemComprStream.LoadFromStream(AResource.Value.AsStream);
         l_oMemComprStream.Position := 0;
         self.WriteProt('- Compressed Stream-Size: ' + IntToStr(l_oMemComprStream.Size));
         // Decompress
         ZDecompressStream(l_oMemComprStream, l_oMemTableStream);
         l_oMemTableStream.Position := 0;
         self.WriteProt('- Memory Table Stream-Size: ' + IntToStr(l_oMemTableStream.Size));
         l_oMemTable := TFdMemTable.Create(Nil);
         l_oMemTable.LoadFromStream(l_oMemTableStream,l_oStorageFormat);
       except
         on E: Exception do begin
           l_lOk := False;
           txaProt.Lines.Add('Exception in TFormFMXClient.LoadTableFromStream: ' + E.Message);
           txaProt.Lines.Add('- Exception by loading a compressed Stream!');
         end;
       end;
     finally
       if Assigned(l_oMemTableStream) then
         l_oMemTableStream.Free;
       if Assigned(l_oMemComprStream) then
         l_oMemComprStream.Free;
     end;
   end;
 end;
 if Assigned(l_oMemTable) then begin
   if (i_oSendDesc.SType = TEnumSendType.MSG_TBL_NEW) or
      (i_oSendDesc.SType = TEnumSendType.MSG_TBL_DATA_CHNG) then begin
     try
       bmReader.DataSet   := l_oMemTable;
       bmWriter.TableName := l_sTableName;
       self.CreateDBTable(l_oMemTable,l_sTableName);
       bmTransfer.Execute;
     except
       on E: Exception do begin
         l_lOk := False;
         self.WriteProt('Exception in TFormFMXClient.LoadTableFromStream: ' + E.Message);
         self.WriteProt('- Exception by creating a SQLite-Table or BatchMove-Processing!');
       end;
     end;
   end;
   try
     l_oMemTable.Free;
   except
     on E: Exception do begin
       l_lOk := False;
       txaProt.Lines.Add('Exception in TFormFMXClient.LoadTableFromStream: ' + E.Message);
       txaProt.Lines.Add('- Exception by delete a local Memory-Table!');
     end;
   end;
 end;
 Result := l_lOk;
end;

procedure TFormFMXClient.CreateDBTable(l_oMemTable: TFdMemTable; i_sTab: String);
var
 i, l_iIdxFieldDefsID, l_iIdxIndexDefsID: Integer;
 l_lFindColID, l_lFindPKIndexID, l_lFindPK: Boolean;
 l_oTab: TFDTable;
 l_oFieldDef: TFieldDef;
 l_oIndexDef: TIndexDef;
begin
 l_lFindColID  := False;
 l_lFindPKIndexID:= False;
 l_lFindPK     := False;
 l_iIdxFieldDefsID:= -1;
 l_iIdxIndexDefsID:= -1;
 l_oTab := TFDTable.Create(nil);
 try
   l_oTab.TableName := i_sTab;
   l_oTab.UpdateOptions.GeneratorName := 'GEN';
   l_oTab.ConnectionName := Database.ConnectionName;
   l_oTab.Connection     := Database;
   bmTransfer.Reader.Open(False);
   bmTransfer.Reader.GetTableDefs(l_oTab.FieldDefs, l_oTab.IndexDefs);
   for i := 0 to l_oTab.FieldDefs.Count-1 do begin
     l_oFieldDef := l_oTab.FieldDefs[i];
   end;
   for i := 0 to l_oTab.IndexDefs.Count-1 do begin
     l_oIndexDef := l_oTab.IndexDefs[i];
   end;
   if not Database.Connected then
     Database.Connected := True;
   l_oTab.CreateTable(False, [tpTable .. tpIndexes]);    // <- Kein Recreate, da Datenbank-File neu angelegt wurde!!!
   bmTransfer.Reader.Close(False);
 finally
   l_oTab.Free;
 end;
end;

procedure TFormFMXClient.WriteProt(i_sText: String);
begin
 if chbxWithProt.IsChecked then
   txaProt.Lines.Add(i_sText);
end;

end.
