unit uVCLServerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Generics.Collections,
  uTetherUtil,
  System.Tether.NetworkAdapter, System.ZLib,
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
  FireDAC.DApt, FireDAC.Stan.StorageBin, FireDAC.Stan.StorageXML,
  FireDAC.Stan.StorageJSON;

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
    bmWriter: TFDBatchMoveDataSetWriter;
    PanelTop: TPanel;
    btnClearProt: TButton;
    FDStanStorageJSONLink: TFDStanStorageJSONLink;
    FDStanStorageXMLLink: TFDStanStorageXMLLink;
    rgbxStreamType: TRadioGroup;
    rgbxStreamCompr: TRadioGroup;
    chbxWithProt: TCheckBox;
    rgbxProtocol: TRadioGroup;
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
    function  SendTableAsStream(i_oClientInfo: TClientInfoOnServer; i_oSendDesc: TSendDesc): Boolean;
    function  SendQueryResultAsStream(i_oClientInfo: TClientInfoOnServer; i_oSendDesc: TSendDesc): Boolean;
    procedure GetAllActions(i_oClientInfo: TClientInfoOnServer);
    procedure WriteProt(i_sText: String);
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
 self.WriteProt('Profile connect:');
 self.WriteProt(' - ManagerIdent: ' + AProfileInfo.ManagerIdentifier);
 self.WriteProt(' - ProfileIdent: ' + AProfileInfo.ProfileIdentifier);
 self.WriteProt(' - AppName:      ' + AProfileInfo.ProfileGroup);
 self.WriteProt(' - ClientIdent:  ' + AProfileInfo.ProfileText);
 if AProfileInfo.ProfileGroup = uTetherUtil.TetherAppIdentClient then begin
   self.WriteProt(' ->  Korrektes "Client Profile" zur Applikation!');
   l_oClientInfo := TClientInfoOnServer.GetOrAddClientInfoToList(m_lsTClientInfoOnServers,AProfileInfo.ProfileText);
   l_oClientInfo.ClearListOfSends;
   l_oClientInfo.ProfileInfo := AProfileInfo;
 end
 else begin
   self.WriteProt(' ->  Falsches "Client Profile"!');
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
 self.WriteProt('Profile disconnect:');
 self.WriteProt(' - ManagerIdent: ' + AProfileInfo.ManagerIdentifier);
 self.WriteProt(' - ProfileIdent: ' + AProfileInfo.ProfileIdentifier);
 self.WriteProt(' - AppIdent:      ' + AProfileInfo.ProfileGroup);
 self.WriteProt(' - ClientIdent:  ' + AProfileInfo.ProfileText);
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
 l_lEndOfActions, l_lOk: Boolean;
 l_sSendStr, l_sTabName, l_sClientId: String;
 l_oSendDesc:   TSendDesc;
 l_oClientInfo: TClientInfoOnServer;
begin
 self.WriteProt('Client Resource Received at: ' + DateTimeToStr(now));
 self.WriteProt('- Remote Resource Name: ' + aResource.Name);
 self.WriteProt('- Remote Resource Hint: ' + aResource.Hint);
 l_sClientId   := l_oSendDesc.ParseString(aResource.Hint);
 l_oClientInfo := TClientInfoOnServer.GetClientInfoOnServerFromList(m_lsTClientInfoOnServers,l_sClientId);
 self.WriteProt('- Client Send Desc: ' + l_oSendDesc.ToString());
 if Assigned(l_oClientInfo) then begin
   //
   if AResource.ResType = TRemoteResourceType.Data then begin
     l_lEndOfActions := False;
     l_sSendStr := aResource.Value.AsString;
     self.WriteProt('- Resource is a String: ' + l_sSendStr);
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
            //
           if (l_oSendDesc.SType = TEnumSendType.MSG_TBL_NEW)  or
              (l_oSendDesc.SType = TEnumSendType.MSG_TBL_DATA_CHNG) then begin
             if rgbxStreamType.ItemIndex = 1 then
               l_oSendDesc.SStreamType := TEnumStreamType.STR_JSON
             else if rgbxStreamType.ItemIndex = 2 then
               l_oSendDesc.SStreamType := TEnumStreamType.STR_XML
             else
               l_oSendDesc.SStreamType := TEnumStreamType.STR_BIN;
             if rgbxStreamCompr.ItemIndex = 1 then
               l_oSendDesc.SStreamCompr := TEnumStreamCompr.STR_COMPR_FAST
             else if rgbxStreamCompr.ItemIndex = 2 then
               l_oSendDesc.SStreamCompr := TEnumStreamCompr.STR_COMPR_DFT
             else if rgbxStreamCompr.ItemIndex = 3 then
               l_oSendDesc.SStreamCompr := TEnumStreamCompr.STR_COMPR_MAX
             else
               l_oSendDesc.SStreamCompr := TEnumStreamCompr.STR_NO_COMPR;
             //
             self.WriteProt('-> Next Server Action - Send Stream: ' + l_oSendDesc.ToString);
             if l_oSendDesc.SType = TEnumSendType.MSG_TBL_NEW then begin
               l_lOk := self.SendTableAsStream(l_oClientInfo,l_oSendDesc);
             end
             else if l_oSendDesc.SType = TEnumSendType.MSG_TBL_DATA_CHNG then begin
               l_lOk := self.SendQueryResultAsStream(l_oClientInfo,l_oSendDesc);
             end;
           end
           else begin
             // Text verschicken ...
             self.WriteProt('-> Next Server Action - Send String: ' + l_oSendDesc.ToString);
             l_sSendStr := l_oSendDesc.BuildString(m_oServerInfo.ServerId);
             TetherServerProfile.SendString(l_oClientInfo.ProfileInfo,l_sSendStr,'-');
           end;
         end;
       end;
       if l_lEndOfActions then begin
         l_oSendDesc.Clear(TEnumSendType.MSG_SYNC_END);
         l_sSendStr := l_oSendDesc.BuildString(m_oServerInfo.ServerId);
         TetherServerProfile.SendString(l_oClientInfo.ProfileInfo,l_sSendStr,'-');
       end;
     end;
   end
   //
   else if aResource.ResType = TRemoteResourceType.Stream then begin
     self.WriteProt('- Resource is a Stream');
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
         self.WriteProt('- Size: ' + InttoStr(l_iSize));
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
       l_sSendStr := l_oSendDesc.BuildString(m_oServerInfo.ServerId);
       TetherServerProfile.SendString(l_oClientInfo.ProfileInfo,l_sSendStr,'-');
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
   rgbxProtocol.Enabled := False;
   if rgbxProtocol.ItemIndex = 1 then
     TetherServerManager.AllowedAdapters := 'Bluetooth'
   else
     TetherServerManager.AllowedAdapters := 'Network';
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
 self.WriteProt('End Managers Discovery:');
end;

procedure TFormVCLServer.TetherServerManagerNewManager(const Sender: TObject; const aManagerInfo: TTetheringManagerInfo);
begin
 self.WriteProt('New Manager: ' + aManagerInfo.ManagerIdentifier + ' - ' + aManagerInfo.ManagerName + ' - ' + aManagerInfo.ConnectionString + ' - ' + aManagerInfo.ManagerText);
end;

procedure TFormVCLServer.TetherServerManagerRemoteManagerShutdown(
  const Sender: TObject; const aManagerIdentifier: string);
begin
 self.WriteProt('Remote Manager Shutdown - Identifier: ' + aManagerIdentifier);
end;

procedure TFormVCLServer.TetherServerManagerRequestManagerPassword(
  const Sender: TObject; const ARemoteIdentifier: string; var Password: string);
begin
 self.WriteProt('Request Manager Password - Identifier: ' + ARemoteIdentifier);
 Password := uTetherUtil.Password;
end;

procedure TFormVCLServer.TetherServerManagerPairedFromLocal(const Sender: TObject; const aManagerInfo: TTetheringManagerInfo);
begin
 self.WriteProt('Tether Manager - PairedFromLocal: ' + AManagerInfo.ManagerIdentifier + ' - ' + aManagerInfo.ManagerName + ' - ' + AManagerInfo.ConnectionString + ' - ' + aManagerInfo.ManagerText);
end;

procedure TFormVCLServer.TetherServerManagerPairedToRemote(const Sender: TObject; const aManagerInfo: TTetheringManagerInfo);
begin
 self.WriteProt('Tether Manager - PairedToRemote: ' + aManagerInfo.ManagerIdentifier + ' - ' + aManagerInfo.ManagerName + ' - ' + AManagerInfo.ConnectionString + ' - ' + aManagerInfo.ManagerText);
end;

procedure TFormVCLServer.TetherServerManagerUnPairManager(const Sender: TObject; const aManagerInfo: TTetheringManagerInfo);
begin
 self.WriteProt('Tether Manager - UnPaired: ' + aManagerInfo.ManagerIdentifier + ' - ' + aManagerInfo.ManagerName + ' - ' + AManagerInfo.ConnectionString + ' - ' + aManagerInfo.ManagerText);
end;

function  TFormVCLServer.SendTableAsStream(i_oClientInfo: TClientInfoOnServer;
                                           i_oSendDesc:   TSendDesc): Boolean;
var
 l_lOk: Boolean;
 l_sSendStr, l_sTableName: String;
 l_oStorageFormat: TFDStorageFormat;
 l_oComprLevel:    TCompressionLevel;
begin
 l_lOk := True;
 // Eine DB-Tabelle verschicken ...
 if Database.Connected then begin
   l_sSendStr := i_oSendDesc.BuildString(m_oServerInfo.ServerId);
   //
   l_sTableName  := Database.EncodeObjectName('', '', '',i_oSendDesc.STableName);
   //
   if i_oSendDesc.SStreamType = TEnumStreamType.STR_JSON then
     l_oStorageFormat := TFDStorageFormat.sfJSON
   else if i_oSendDesc.SStreamType = TEnumStreamType.STR_XML then
     l_oStorageFormat := TFDStorageFormat.sfXML
   else
     l_oStorageFormat := TFDStorageFormat.sfBinary;
   //
   if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_NO_COMPR) then begin   // Nicht Kompromiert!
     TThread.Synchronize(nil,procedure
       var
        l_lDoTrans: Boolean;
        l_oSrcTable:  TFDTable;
        l_oMemTable:  TFdMemTable;
        l_oMemStream: TMemoryStream;
       begin
        l_lDoTrans := False;
        l_oSrcTable:= Nil;
        l_oMemTable:= Nil;
        try
          l_oMemTable := TFdMemTable.Create(Nil);
          l_oSrcTable := TFDTable.Create(Nil);
          l_oSrcTable.TableName  := l_sTableName;
          l_oSrcTable.Connection := Database;
          bmReader.DataSet := l_oSrcTable;
          bmWriter.DataSet := l_oMemTable;
          bmTransfer.Execute;
          l_lDoTrans := True;
        except
          on E: Exception do begin
            l_lOk := False;
            txaProt.Lines.Add('Exception in TFormVCLServer.SendTableAsStream: ' + E.Message);
            txaProt.Lines.Add('- Exception by BatchMove: SQLite-Table to Memory-Table!');
          end;
        end;
        if l_lOk and l_lDoTrans then begin
          if not l_oMemTable.Active then
            l_oMemTable.Open;
          l_oMemStream := TMemoryStream.Create;
          try
            try
              l_oMemTable.SaveToStream(l_oMemStream,l_oStorageFormat);
              self.WriteProt('- Memory Table Stream-Size: ' + IntToStr(l_oMemStream.Size));
              l_lOk := TetherServerProfile.SendStream(i_oClientInfo.ProfileInfo,l_sSendStr,l_oMemStream);
            except
              on E: Exception do begin
                l_lOk := False;
                txaProt.Lines.Add('Exception in TFormVCLServer.SendTableAsStream: ' + E.Message);
                txaProt.Lines.Add('- Exception by Stream-Save a "Memory-Table"!');
              end;
            end;
          finally
            l_oMemStream.Free;
          end;
        end;
        if Assigned(l_oMemTable) then begin
          if l_oMemTable.Active then
            l_oMemTable.Close;
          l_oMemTable.Free;
        end;
        if Assigned(l_oSrcTable) then begin
          if l_oSrcTable.Active then
            l_oSrcTable.Close;
          l_oSrcTable.Free;
        end;
       end );
   end
   else begin           // Kompromierte Übertragung; TCompressionLevel = (clNone, clFastest, clDefault, clMax);
     if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_COMPR_FAST) then
       l_oComprLevel := TCompressionLevel.clFastest
     else if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_COMPR_DFT) then
       l_oComprLevel := TCompressionLevel.clDefault
     else if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_COMPR_MAX) then
       l_oComprLevel := TCompressionLevel.clMax
     else
       l_oComprLevel := TCompressionLevel.clNone;
     TThread.Synchronize(nil,procedure
       var
        l_lDoTrans: Boolean;
        l_oSrcTable:  TFDTable;
        l_oMemTable:  TFdMemTable;
        l_oMemTableStream, l_oMemComprStream: TMemoryStream;
       begin
        l_lDoTrans := False;
        l_oSrcTable:= Nil;
        l_oMemTable:= Nil;
        try
          l_oMemTable := TFdMemTable.Create(Nil);
          l_oSrcTable := TFDTable.Create(Nil);
          l_oSrcTable.TableName  := l_sTableName;
          l_oSrcTable.Connection := Database;
          bmReader.DataSet := l_oSrcTable;
          bmWriter.DataSet := l_oMemTable;
          bmTransfer.Execute;
          l_lDoTrans := True;
        except
          on E: Exception do begin
            l_lOk := False;
            txaProt.Lines.Add('Exception in TFormVCLServer.SendTableAsStream: ' + E.Message);
            txaProt.Lines.Add('- Exception by BatchMove: SQLite-Table to Memory-Table!');
          end;
        end;
        if l_lOk and l_lDoTrans then begin
          if not l_oMemTable.Active then
            l_oMemTable.Open;
          l_oMemTableStream := TMemoryStream.Create;
          l_oMemComprStream := TMemoryStream.Create;
          try
            try
              l_oMemTable.SaveToStream(l_oMemTableStream,l_oStorageFormat);
              l_oMemTableStream.Position := 0;
              ZCompressStream(l_oMemTableStream, l_oMemComprStream, l_oComprLevel);
              l_oMemComprStream.Position := 0;
              self.WriteProt('- Memory Table Stream-Size: ' + IntToStr(l_oMemTableStream.Size));
              self.WriteProt('- Compressed Stream-Size: ' + IntToStr(l_oMemComprStream.Size));
              l_lOk := TetherServerProfile.SendStream(i_oClientInfo.ProfileInfo,l_sSendStr,l_oMemComprStream);
            except
              on E: Exception do begin
                l_lOk := False;
                txaProt.Lines.Add('Exception in TFormVCLServer.SendTableAsStream: ' + E.Message);
                txaProt.Lines.Add('- Exception by Compress Stream-Save a "Memory-Table"!');
              end;
            end;
          finally
            l_oMemTableStream.Free;
            l_oMemComprStream.Free;
          end;
        end;
        if Assigned(l_oMemTable) then begin
          if l_oMemTable.Active then
            l_oMemTable.Close;
          l_oMemTable.Free;
        end;
        if Assigned(l_oSrcTable) then begin
          if l_oSrcTable.Active then
            l_oSrcTable.Close;
          l_oSrcTable.Free;
        end;
       end );
   end
 end;
 Result := l_lOk;
end;

function TFormVCLServer.SendQueryResultAsStream(i_oClientInfo: TClientInfoOnServer; i_oSendDesc: TSendDesc): Boolean;
var
 l_lOk: Boolean;
 l_sSendStr, l_sSql, l_sTableName: String;
 l_oStorageFormat: TFDStorageFormat;
 l_oComprLevel:    TCompressionLevel;
begin
 l_lOk := True;
 // Eine DB-Tabelle verschicken ...
 if Database.Connected then begin
   l_sTableName := Database.EncodeObjectName('', '', '',i_oSendDesc.STableName);
   l_sSql       := i_oSendDesc.SText;
   i_oSendDesc.SText := '';
   l_sSendStr   := i_oSendDesc.BuildString(m_oServerInfo.ServerId);
   //
   if i_oSendDesc.SStreamType = TEnumStreamType.STR_JSON then
     l_oStorageFormat := TFDStorageFormat.sfJSON
   else if i_oSendDesc.SStreamType = TEnumStreamType.STR_XML then
     l_oStorageFormat := TFDStorageFormat.sfXML
   else
     l_oStorageFormat := TFDStorageFormat.sfBinary;
   //
   if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_NO_COMPR) then begin   // Nicht Kompromiert!
     TThread.Synchronize(nil,procedure
       var
        l_lDoTrans: Boolean;
        l_oQuery:     TFDQuery;
        l_oMemTable:  TFdMemTable;
        l_oMemStream: TMemoryStream;
       begin
        l_lDoTrans := False;
        l_oQuery   := Nil;
        l_oMemTable:= Nil;
        try
          l_oMemTable := TFdMemTable.Create(Nil);
          //-- l_oMemTable.TableName := l_sTableName
          l_oQuery    := TFDQuery.Create(Nil);
          l_oQuery.SQL.Add(l_sSql);
          l_oQuery.Connection := Database;
          bmReader.DataSet := l_oQuery;
          bmWriter.DataSet := l_oMemTable;
          bmTransfer.Execute;
          l_lDoTrans := True;
        except
          on E: Exception do begin
            l_lOk := False;
            txaProt.Lines.Add('Exception in TFormVCLServer.SendQueryResultAsStream: ' + E.Message);
            txaProt.Lines.Add('- Exception by BatchMove: SQLite-Table to Memory-Table!');
          end;
        end;
        if l_lOk and l_lDoTrans then begin
          if not l_oMemTable.Active then
            l_oMemTable.Open;
          l_oMemStream := TMemoryStream.Create;
          try
            try
              l_oMemTable.SaveToStream(l_oMemStream,l_oStorageFormat);
              self.WriteProt('- Memory Table Stream-Size: ' + IntToStr(l_oMemStream.Size));
              l_lOk := TetherServerProfile.SendStream(i_oClientInfo.ProfileInfo,l_sSendStr,l_oMemStream);
            except
              on E: Exception do begin
                l_lOk := False;
                txaProt.Lines.Add('Exception in TFormVCLServer.SendQueryResultAsStream: ' + E.Message);
                txaProt.Lines.Add('- Exception by Stream-Save a "Memory-Table"!');
              end;
            end;
          finally
            l_oMemStream.Free;
          end;
        end;
        if Assigned(l_oMemTable) then begin
          if l_oMemTable.Active then
            l_oMemTable.Close;
          l_oMemTable.Free;
        end;
        if Assigned(l_oQuery) then begin
          if l_oQuery.Active then
            l_oQuery.Close;
          l_oQuery.Free;
        end;
       end );
   end
   else begin           // Kompromierte Übertragung; TCompressionLevel = (clNone, clFastest, clDefault, clMax);
     if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_COMPR_FAST) then
       l_oComprLevel := TCompressionLevel.clFastest
     else if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_COMPR_DFT) then
       l_oComprLevel := TCompressionLevel.clDefault
     else if (i_oSendDesc.SStreamCompr = TEnumStreamCompr.STR_COMPR_MAX) then
       l_oComprLevel := TCompressionLevel.clMax
     else
       l_oComprLevel := TCompressionLevel.clNone;
     TThread.Synchronize(nil,procedure
       var
        l_lDoTrans: Boolean;
        l_oQuery:     TFDQuery;
        l_oMemTable:  TFdMemTable;
        l_oMemTableStream, l_oMemComprStream: TMemoryStream;
       begin
        l_lDoTrans := False;
        l_oQuery   := Nil;
        l_oMemTable:= Nil;
        try
          l_oMemTable := TFdMemTable.Create(Nil);
          l_oQuery    := TFDQuery.Create(Nil);
          l_oQuery.SQL.Add(l_sSql);
          l_oQuery.Connection := Database;
          bmReader.DataSet := l_oQuery;
          bmWriter.DataSet := l_oMemTable;
          bmTransfer.Execute;
          l_lDoTrans := True;
        except
          on E: Exception do begin
            l_lOk := False;
            txaProt.Lines.Add('Exception in TFormVCLServer.SendQueryResultAsStream: ' + E.Message);
            txaProt.Lines.Add('- Exception by BatchMove: SQLite-Table to Memory-Table!');
          end;
        end;
        if l_lOk and l_lDoTrans then begin
          if not l_oMemTable.Active then
            l_oMemTable.Open;
          l_oMemTableStream := TMemoryStream.Create;
          l_oMemComprStream := TMemoryStream.Create;
          try
            try
              l_oMemTable.SaveToStream(l_oMemTableStream,l_oStorageFormat);
              l_oMemTableStream.Position := 0;
              ZCompressStream(l_oMemTableStream, l_oMemComprStream, l_oComprLevel);
              l_oMemComprStream.Position := 0;
              self.WriteProt('- Memory Table Stream-Size: ' + IntToStr(l_oMemTableStream.Size));
              self.WriteProt('- Compressed Stream-Size: ' + IntToStr(l_oMemComprStream.Size));
              l_lOk := TetherServerProfile.SendStream(i_oClientInfo.ProfileInfo,l_sSendStr,l_oMemComprStream);
            except
              on E: Exception do begin
                l_lOk := False;
                txaProt.Lines.Add('Exception in TFormVCLServer.SendQueryResultAsStream: ' + E.Message);
                txaProt.Lines.Add('- Exception by Compress Stream-Save a "Memory-Table"!');
              end;
            end;
          finally
            l_oMemTableStream.Free;
            l_oMemComprStream.Free;
          end;
        end;
        if Assigned(l_oMemTable) then begin
          if l_oMemTable.Active then
            l_oMemTable.Close;
          l_oMemTable.Free;
        end;
        if Assigned(l_oQuery) then begin
          if l_oQuery.Active then
            l_oQuery.Close;
          l_oQuery.Free;
        end;
       end );
   end
 end;
 Result := l_lOk;
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
 SetLength(i_oClientInfo.SendList,l_lstNames.Count+1);
 for i := 0 to l_lstNames.Count-1 do begin
   i_oClientInfo.SendList[i].SId := i + 1;
   i_oClientInfo.SendList[i].SType    := TEnumSendType.MSG_TBL_NEW;
   i_oClientInfo.SendList[i].SAnswere := TEnumAnswereType.MSGA_OK;
   i_oClientInfo.SendList[i].STableName := l_lstNames[i];
   i_oClientInfo.SendList[i].SText    := '';
 end;
 // Nun ein SQL. Das Ergebnis wird als Tabelle übertragen!
 i := l_lstNames.Count;
 i_oClientInfo.SendList[i].SId := i + 1;
 i_oClientInfo.SendList[i].SType    := TEnumSendType.MSG_TBL_DATA_CHNG;
 i_oClientInfo.SendList[i].SAnswere := TEnumAnswereType.MSGA_OK;
 i_oClientInfo.SendList[i].STableName := 'SQL_ProductsWithCategories';
 i_oClientInfo.SendList[i].SText    :=
      'SELECT p.ProductID as PID, p.ProductName as PName, p.UnitPrice * p.UnitsInStock as TotalPrice,' +
      'c.CategoryID as CID, c.CategoryName as CName' +
      ' FROM Products p left join Categories c on p.CategoryID = c.CategoryID';
 //
 l_lstNames.Clear;
 l_lstNames.Free;
end;

procedure TFormVCLServer.WriteProt(i_sText: String);
begin
 if chbxWithProt.Checked then
   txaProt.Lines.Add(i_sText);
end;

end.

