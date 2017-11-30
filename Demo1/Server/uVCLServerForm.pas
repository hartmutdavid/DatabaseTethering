unit uVCLServerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Generics.Collections,
  uTetherUtil,
  System.Tether.NetworkAdapter,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, IPPeerServer,
  System.Actions, Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan,
  Vcl.ExtCtrls, Vcl.StdCtrls, System.Tether.Manager, System.Tether.AppProfile,
  Vcl.Imaging.jpeg, Vcl.Samples.Spin, Vcl.Mask, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait,
  FireDAC.Comp.UI, Data.DB, FireDAC.Comp.Client, FireDAC.Comp.BatchMove.DataSet,
  FireDAC.Comp.BatchMove, FireDAC.Comp.BatchMove.SQL, FireDAC.Comp.DataSet,
  FireDAC.DApt, FireDAC.Stan.StorageBin;

const
 c_sSQLiteFileName = '..\server.s3db';

type
  TFormVCLServer = class(TForm)
    TetherServerManager: TTetheringManager;
    TetherServerProfile: TTetheringAppProfile;
    txaProt: TMemo;
    PanelBottom: TPanel;
    btnStartStop: TButton;
    bmTransfer: TFDBatchMove;
    bmReader: TFDBatchMoveDataSetReader;
    Database: TFDConnection;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    mqSrcTables: TFDMetaInfoQuery;
    FDStanStorageBinLink: TFDStanStorageBinLink;
    tblSrc: TFDTable;
    bmWriter: TFDBatchMoveDataSetWriter;
    PanelTop: TPanel;
    btnClearProt: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TetherServerManagerNewManager(const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
    procedure TetherServerManagerPairedToRemote(const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
    procedure TetherServerManagerPairedFromLocal(const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
    procedure TetherServerProfileDisconnect(const Sender: TObject; const AProfileInfo: TTetheringProfileInfo);
    procedure TetherServerManagerUnPairManager(const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
    procedure btnStartStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TetherServerManagerEndManagersDiscovery(const Sender: TObject;
      const ARemoteManagers: TTetheringManagerInfoList);
    procedure TetherServerManagerRemoteManagerShutdown(const Sender: TObject;
      const AManagerIdentifier: string);
    procedure TetherServerProfileResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure TetherServerProfileAfterConnectProfile(const Sender: TObject;
      const AProfileInfo: TTetheringProfileInfo);
    procedure TetherServerManagerRequestManagerPassword(const Sender: TObject;
      const ARemoteIdentifier: string; var Password: string);
    procedure btnClearProtClick(Sender: TObject);
  private
    { Private declarations }
    m_oServerInfo:  TServerInfo;
    m_lsTClientInfoOnServers: TDictionary<String,TClientInfoOnServer>;
    procedure GetAllActions(i_oClientInfo: TClientInfoOnServer);
  public
    { Public declarations }
  end;

var
  FormVCLServer: TFormVCLServer;

implementation

{$R *.dfm}

uses uGlobal;

procedure TFormVCLServer.FormCreate(Sender: TObject);
begin
 m_oServerInfo := TServerInfo.Create;
 TClientInfoOnServer.InitListOfUserInfos(m_lsTClientInfoOnServers);
end;

procedure TFormVCLServer.FormDestroy(Sender: TObject);
begin
 m_oServerInfo.Free;
 TClientInfoOnServer.CloseAndFreeListOfUserInfos(m_lsTClientInfoOnServers);
end;

procedure TFormVCLServer.FormShow(Sender: TObject);
begin
 txaProt.Lines.Clear;
 Database.Params.Clear;
 Database.Params.DriverID := 'SQLite';
 Database.Params.Database := c_sSQLiteFileName;
 TFDPhysSQLiteConnectionDefParams(Database.Params).OpenMode := TFDSQLiteOpenMode.omReadWrite;
 //-- TFDPhysSQLiteConnectionDefParams(conDest.Params).DateTimeFormat := TFDSQLiteDateTimeFormat.dtfDateTime;
end;

procedure TFormVCLServer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if Database.Connected then
   Database.Connected := False;
 TetherServerManager.Enabled := False;
end;

procedure TFormVCLServer.TetherServerProfileAfterConnectProfile(
  const Sender: TObject; const AProfileInfo: TTetheringProfileInfo);
var
 l_oClientInfo: TClientInfoOnServer;
begin
 txaProt.Lines.Add('Profile connect:');
 txaProt.Lines.Add(' - ManagerIdent: ' + AProfileInfo.ManagerIdentifier);
 txaProt.Lines.Add(' - ProfileIdent: ' + AProfileInfo.ProfileIdentifier);
 txaProt.Lines.Add(' - AppName:      ' + AProfileInfo.ProfileGroup);
 txaProt.Lines.Add(' - ClientIdent:  ' + AProfileInfo.ProfileText);
 if AProfileInfo.ProfileGroup = uTetherUtil.TetherAppIdentClient then begin
   txaProt.Lines.Add(' ->  Korrektes "Client Profile" zur Applikation!');
   l_oClientInfo := TClientInfoOnServer.GetOrAddClientInfoToList(m_lsTClientInfoOnServers,AProfileInfo.ProfileText);
   l_oClientInfo.ClearListOfSends;
   l_oClientInfo.ProfileInfo := AProfileInfo;
 end
 else begin
   txaProt.Lines.Add(' ->  Falsches "Client Profile"!');
   try
     TetherServerProfile.Disconnect(AProfileInfo);
   except
   end;
 end;
end;

procedure TFormVCLServer.TetherServerProfileDisconnect(const Sender: TObject; const AProfileInfo: TTetheringProfileInfo);
var
 l_oClientInfo: TClientInfoOnServer;
begin
 txaProt.Lines.Add('Profile disconnect:');
 txaProt.Lines.Add(' - ManagerIdent: ' + AProfileInfo.ManagerIdentifier);
 txaProt.Lines.Add(' - ProfileIdent: ' + AProfileInfo.ProfileIdentifier);
 txaProt.Lines.Add(' - AppIdent:      ' + AProfileInfo.ProfileGroup);
 txaProt.Lines.Add(' - ClientIdent:  ' + AProfileInfo.ProfileText);
 if AProfileInfo.ProfileGroup = uTetherUtil.TetherAppIdentClient then begin
   l_oClientInfo := TClientInfoOnServer.GetClientInfoOnServerFromList(m_lsTClientInfoOnServers,AProfileInfo.ProfileText);
   if Assigned(l_oClientInfo) then begin
     l_oClientInfo.ClearListOfSends;
     l_oClientInfo.ProfileInfo.ProfileText := '';
   end;
 end;
end;

procedure TFormVCLServer.TetherServerProfileResourceReceived(
  const Sender: TObject; const AResource: TRemoteResource);
var
 l_lEndOfActions, l_lDoTrans: Boolean;
 l_sStr, l_sTabName, l_sClientId: String;
 l_oSendDesc:   TSendDesc;
 l_oClientInfo: TClientInfoOnServer;
begin
 txaProt.Lines.Add('Client Resource Received at: ' + DateTimeToStr(now));
 txaProt.Lines.Add('- Remote Resource Name: ' + aResource.Name);
 txaProt.Lines.Add('- Remote Resource Hint: ' + aResource.Hint);
 l_sClientId   := l_oSendDesc.ParseString(aResource.Hint);
 l_oClientInfo := TClientInfoOnServer.GetClientInfoOnServerFromList(m_lsTClientInfoOnServers,l_sClientId);
 txaProt.Lines.Add('- Client Send Desc: ' + l_oSendDesc.ToString());
 if Assigned(l_oClientInfo) then begin
   //
   if AResource.ResType = TRemoteResourceType.Data then begin
     l_lEndOfActions := False;
     l_sStr := aResource.Value.AsString;
     txaProt.Lines.Add('- Resource is a String: ' + l_sStr);
     if l_oSendDesc.SType = TEnumSendType.MSG_READY_FOR_RECEIVE then begin
       if Length(l_oClientInfo.SendList) = 0 then begin
         // Ermitteln aller notwendigen Aktionen
         self.GetAllActions(l_oClientInfo);
         if Length(l_oClientInfo.SendList) = 0 then
           l_lEndOfActions := True;
       end;
       if Length(l_oClientInfo.SendList) > 0 then begin
         Inc(l_oClientInfo.ActIdxOfSendList);
         if l_oClientInfo.ActIdxOfSendList >= Length(l_oClientInfo.SendList) then
           l_lEndOfActions := True
         else begin
           // Naechste Aktion ermitteln und das Gewünschte zum Client senden!
           l_oSendDesc.Assign(l_oClientInfo.SendList[l_oClientInfo.ActIdxOfSendList]);
           txaProt.Lines.Add('-> Next Server Action: ' + l_oSendDesc.ToString);
           l_sStr := l_oSendDesc.BuildString(m_oServerInfo.ServerId);
           //
           if (l_oSendDesc.SType = TEnumSendType.MSG_TBL_NEW)  or
              (l_oSendDesc.SType = TEnumSendType.MSG_TBL_DATA_CHNG) then begin
             // Eine DB-Tabelle verschicken ...
             if Database.Connected then begin
               TThread.Synchronize(nil,procedure
                 var
                  l_lOk, l_lDoTrans: Boolean;
                  l_oMemTable:  TFdMemTable;
                  l_oMemStream: TMemoryStream;
                 begin
                  l_lDoTrans := False;
                  l_oMemTable:= Nil;
                  if tblSrc.Active then
                    tblSrc.Close;
                  tblSrc.TableName  := Database.EncodeObjectName('', '', '',l_oSendDesc.SText);
                  try
                    l_oMemTable := TFdMemTable.Create(Nil);
                    bmWriter.DataSet := l_oMemTable;
                    bmTransfer.Execute;
                    l_lDoTrans := True;
                  except
                    on E: Exception do begin
                      txaProt.Lines.Add('Exception in TFormVCLServer.TetherServerProfileResourceReceived: ' + E.Message);
                      txaProt.Lines.Add('- Exception by BatchMove: SQLite-Table to Memory-Table!');
                    end;
                  end;
                  if l_lDoTrans then begin
                    if not l_oMemTable.Active then
                      l_oMemTable.Open;
                    l_oMemStream := TMemoryStream.Create;
                    try
                      try
                        l_oMemTable.SaveToStream(l_oMemStream);
                        l_lOk := TetherServerProfile.SendStream(l_oClientInfo.ProfileInfo,l_sStr,l_oMemStream);
                      except
                        on E: Exception do begin
                          txaProt.Lines.Add('Exception in TFormVCLServer.TetherServerProfileResourceReceived: ' + E.Message);
                          txaProt.Lines.Add('- Exception by Stream-Save a "Memory-Table"!');
                        end;
                      end;
                    finally
                      l_oMemStream.Free;
                      if l_oMemTable.Active then
                        l_oMemTable.Close;
                    end;
                  end;
                  if tblSrc.Active then
                    tblSrc.Close;
                  if Assigned(l_oMemTable) then
                    l_oMemTable.Free;
                 end );
             end;
           end
           else begin
             // Text verschicken ...
             TetherServerProfile.SendString(l_oClientInfo.ProfileInfo,l_sStr,'-');
           end;
         end;
       end;
       if l_lEndOfActions then begin
         l_oSendDesc.Clear(TEnumSendType.MSG_SYNC_END);
         l_sStr := l_oSendDesc.BuildString(m_oServerInfo.ServerId);
         TetherServerProfile.SendString(l_oClientInfo.ProfileInfo,l_sStr,'-');
       end;
     end;
   end
   //
   else if aResource.ResType = TRemoteResourceType.Stream then begin
     txaProt.Lines.Add('- Resource is a Stream');
     TThread.Synchronize(nil,procedure
       var
        l_lOk, l_lDoTrans: Boolean;
        l_iSize: Int64;
        l_sLastSend: String;
        l_oMemTable: TFdMemTable;
      begin
       l_lOk := True;
       l_oMemTable := Nil;
       try
         l_oMemTable := TFdMemTable.Create(Nil);
         l_iSize := Aresource.Value.AsStream.Size;
         txaProt.Lines.Add('- Size: ' + InttoStr(l_iSize));
         AResource.Value.AsStream.Position := 0;
         l_oMemTable.LoadFromStream(aResource.Value.AsStream);
       except
         on E: Exception do begin
           l_lOk := False;
           txaProt.Lines.Add('Exception: ' + E.Message);
         end;
       end;
       if l_lOk then begin
         // TODO:
       end;
       if l_lOk then
         l_oSendDesc.Clear(TEnumSendType.MSG_READY)
       else begin
         l_sLastSend := l_oSendDesc.ToString;
         l_oSendDesc.Clear(TEnumSendType.MSG_ERR);
         l_oSendDesc.SText := 'Server-Error by processing the last client send: "' + l_sLastSend + '"';
       end;
       l_sStr := l_oSendDesc.BuildString(m_oServerInfo.ServerId);
       TetherServerProfile.SendString(l_oClientInfo.ProfileInfo,l_sStr,'-');
      if Assigned(l_oMemTable) then
         l_oMemTable.Free;
      end );
   end;
 end;
end;

procedure TFormVCLServer.btnClearProtClick(Sender: TObject);
begin
 txaProt.Clear;
end;

procedure TFormVCLServer.btnStartStopClick(Sender: TObject);
begin
 if btnStartStop.Caption = 'Start' then begin
   if Length(m_oServerInfo.ServerId) = 0 then
     m_oServerInfo.ServerId := uTetherUtil.BuildIdent(uGlobal.g_sComputerName, uGlobal.g_sUserName);;
   TetherServerManager.Text := uTetherUtil.BuildTetherText(uTetherUtil.TetherAppIdentServer,m_oServerInfo.ServerId);
   TetherServerProfile.Group:= uTetherUtil.TetherAppIdentServer;
   TetherServerProfile.Text := m_oServerInfo.ServerId;
   //
   btnStartStop.Caption := 'Stop';
   TetherServerManager.Enabled := True;
   //
   if not Database.Connected then
     Database.Connected := True;
 end
 else begin
   btnStartStop.Caption := 'Start';
   TetherServerManager.Enabled := False;
 end;
end;

procedure TFormVCLServer.TetherServerManagerEndManagersDiscovery(
  const Sender: TObject; const ARemoteManagers: TTetheringManagerInfoList);
begin
 txaProt.Lines.Add('End Managers Discovery:');
end;

procedure TFormVCLServer.TetherServerManagerNewManager(const Sender: TObject; const aManagerInfo: TTetheringManagerInfo);
begin
 txaProt.Lines.Add('New Manager: ' + aManagerInfo.ManagerIdentifier + ' - ' + aManagerInfo.ManagerName + ' - ' + aManagerInfo.ConnectionString + ' - ' + aManagerInfo.ManagerText);
end;

procedure TFormVCLServer.TetherServerManagerRemoteManagerShutdown(
  const Sender: TObject; const aManagerIdentifier: string);
begin
 txaProt.Lines.Add('Remote Manager Shutdown - Identifier: ' + aManagerIdentifier);
end;

procedure TFormVCLServer.TetherServerManagerRequestManagerPassword(
  const Sender: TObject; const ARemoteIdentifier: string; var Password: string);
begin
 txaProt.Lines.Add('Request Manager Password - Identifier: ' + ARemoteIdentifier);
 Password := uTetherUtil.Password;
end;

procedure TFormVCLServer.TetherServerManagerPairedFromLocal(const Sender: TObject; const aManagerInfo: TTetheringManagerInfo);
begin
 txaProt.Lines.Add('Tether Manager - PairedFromLocal: ' + AManagerInfo.ManagerIdentifier + ' - ' + aManagerInfo.ManagerName + ' - ' + AManagerInfo.ConnectionString + ' - ' + aManagerInfo.ManagerText);
end;

procedure TFormVCLServer.TetherServerManagerPairedToRemote(const Sender: TObject; const aManagerInfo: TTetheringManagerInfo);
begin
 txaProt.Lines.Add('Tether Manager - PairedToRemote: ' + aManagerInfo.ManagerIdentifier + ' - ' + aManagerInfo.ManagerName + ' - ' + AManagerInfo.ConnectionString + ' - ' + aManagerInfo.ManagerText);
end;

procedure TFormVCLServer.TetherServerManagerUnPairManager(const Sender: TObject; const aManagerInfo: TTetheringManagerInfo);
begin
 txaProt.Lines.Add('Tether Manager - UnPaired: ' + aManagerInfo.ManagerIdentifier + ' - ' + aManagerInfo.ManagerName + ' - ' + AManagerInfo.ConnectionString + ' - ' + aManagerInfo.ManagerText);
end;

procedure TFormVCLServer.GetAllActions(i_oClientInfo: TClientInfoOnServer);
var
 i, n: Integer;
 l_sTabName: String;
 l_lstNames: TStringList;
begin
 i_oClientInfo.ActIdxOfSendList := -1;
 // Erst einmal
 l_lstNames := TStringList.Create;
 i_oClientInfo.ClearListOfSends;
 mqSrcTables.Active := True;
//  mqSrcTables.Wildcard := 'Categories';
 mqSrcTables.FetchAll;
 n := 0;
 mqSrcTables.First;
 while not mqSrcTables.Eof do begin
   Inc(n);
   l_sTabName := mqSrcTables.FieldByName('TABLE_NAME').AsString;
   l_lstNames.Add(l_sTabName);
   mqSrcTables.Next;
 end;
 mqSrcTables.Active := False;
 SetLength(i_oClientInfo.SendList,l_lstNames.Count);
 for i := 0 to l_lstNames.Count-1 do begin
   i_oClientInfo.SendList[i].SId := i + 1;
   i_oClientInfo.SendList[i].SType    := TEnumSendType.MSG_TBL_NEW;
   i_oClientInfo.SendList[i].SAnswere := TEnumAnswereType.MSGA_OK;
   i_oClientInfo.SendList[i].SText    := l_lstNames[i];
 end;
 l_lstNames.Clear;
 l_lstNames.Free;
end;

end.

