unit uTetherUtil;

interface

uses
  System.SysUtils, System.IOUtils, Generics.Collections, System.Tether.Manager;

const
 DelimVal = '+';        // Achtung: '|' funktioniert nicht!
 DelimRow = #13#10;
 DftUser  = 'Default User';
 Password = 'Tether@DB@Test';
 TetherAppIdentClient = 'TetherClientAppXYZ';
 TetherAppIdentServer = 'TetherServerAppXYZ';

type
 TEnumSendType = (MSG_NONE,
                  MSG_READY,MSG_READY_FOR_RECEIVE,
                  MSG_SYNC_END,
                  MSG_ERR,MSG_ERR_DLG,MSG_WARN,MSG_WARN_DLG,
                  MSG_INFO,MSG_INFO_DLG,
                  MSG_TBL_NEW,MSG_TBL_DATA_CHNG);
 TEnumAnswereType = (MSGA_OK,MSGA_CANCEL,MSGA_ABORT);
 TEnumStreamType  = (STR_BIN,STR_JSON,STR_XML);
 TEnumStreamCompr = (STR_NO_COMPR,STR_COMPR_FAST,STR_COMPR_DFT,STR_COMPR_MAX);

 TSendDesc = record
 public
   SId:      Integer;
   SType:    TEnumSendType;
   SAnswere: TEnumAnswereType;
   SStreamType:  TEnumStreamType;
   SStreamCompr: TEnumStreamCompr;
   STableName: String;
   SText:    String;
   procedure Clear(i_oSendType: TEnumSendType);
   function  BuildString(i_sIdent: String): String;
   function  ParseString(aStr: String): String;  // Return: Ident
   function  ToString: String;
   procedure Assign(i_oSendDesc: TSendDesc);
 end;

 TClientInfo = class
 public
   ClientId:     String;
   ProgVerMajor: String;
   ProgVerMinor: String;
   ProgVersion:  String;      // T: Testversion; S: Standardversion; E: Enterpriseversion
   constructor Create;
   destructor  Destroy; override;
   procedure Clear;
   function  BuildString: String;
   procedure ParseString(aStr: String);
 end;

 TServerInfo = class
 public
   ServerId:    String;
   ManagerInfo: TTetheringManagerInfo;
   ProfileInfo: TTetheringProfileInfo;
   constructor Create;
   destructor Destroy; override;
   procedure Clear;
   function  BuildString: String;
   procedure ParseString(aStr: String);
 end;

 TClientInfoOnServer = class
 public
   ClientId:   String;
   ProfileInfo:  TTetheringProfileInfo;
   DtBegSync:  TDateTime;    // Startzeit der Sync
   DtEndSync:  TDateTime;    // Endezeit  der Sync
   ActIdxOfSendList: Integer;
   SendList:   array of TSendDesc;
   constructor Create(i_sClientId: String);
   destructor  Destroy; override;
   procedure ClearListOfSends;
   class function ListHasClientInfo(i_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>; const i_sClientId: String): Boolean;
   class function GeTClientInfoOnServerFromList(i_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>; const i_sClientId: String): TClientInfoOnServer;
   class function AddClientInfoToList(i_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>; const i_sClientId: String): TClientInfoOnServer;
   class function GetOrAddClientInfoToList(i_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>; const i_sClientId: String): TClientInfoOnServer;
   class procedure InitListOfUserInfos(var o_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>);
   class procedure CloseAndFreeListOfUserInfos(var o_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>);
 end;

function  BuildIdent(i_sClientName, i_sUserName: String): String;
function  BuildTetherText(i_sName, i_sIdent: String): String;
procedure ParseTetherText(i_sText: String; var o_sName, o_sIdent: String);

implementation

uses uGlobal, uStrUtil;

{ ---------------------------------------------------------------------------- }

function  BuildIdent(i_sClientName, i_sUserName: String): String;
begin
 if Length(i_sUserName) = 0 then
   i_sUserName := DftUser;
 Result := LowerCase(uStrUtil.GetHashMD5Key(UpperCase(i_sClientName + '_' + i_sUserName)));
end;

function  BuildTetherText(i_sName, i_sIdent: String): String;
begin
 Result := i_sName + DelimVal + i_sIdent;
end;

procedure ParseTetherText(i_sText: String; var o_sName, o_sIdent: String);
var
 l_arInfoArray: TArray<string>;
begin
 l_arInfoArray := i_sText.Split([DelimVal]);
 o_sName       := l_arInfoArray[0];
 o_sIdent      := l_arInfoArray[1];
end;

{ ----------------------------- TSendDesc ------------------------------------ }

procedure TSendDesc.Clear(i_oSendType: TEnumSendType);
begin
 self.SId   := 1;
 self.SType := i_oSendType;
 self.SAnswere := MSGA_OK;
 self.SStreamType := STR_BIN;
 self.SStreamCompr:= STR_NO_COMPR;
 self.STableName := '';
 self.SText := '';
end;

function  TSendDesc.BuildString(i_sIdent: String): String;
begin
 Result := i_sIdent + DelimVal +
           IntToStr(self.SId) + DelimVal +
           IntToStr(Ord(self.SType)) + DelimVal +
           IntToStr(Ord(self.SAnswere)) + DelimVal +
           IntToStr(Ord(self.SStreamType))  + DelimVal +
           IntToStr(Ord(self.SStreamCompr)) + DelimVal +
           self.STableName + DelimVal +
           self.SText;
end;

function TSendDesc.ParseString(aStr: String): String;
var
 l_arInfoArray: TArray<string>;
begin
 l_arInfoArray := aStr.Split([DelimVal]);
 Result     := l_arInfoArray[0];
 self.SId   := l_arInfoArray[1].ToInteger();
 self.SType := TEnumSendType(l_arInfoArray[2].ToInteger());
 self.SAnswere := TEnumAnswereType(l_arInfoArray[3].ToInteger());
 self.SStreamType  := TEnumStreamType(l_arInfoArray[4].ToInteger());
 self.SStreamCompr := TEnumStreamCompr(l_arInfoArray[5].ToInteger());
 self.STableName := l_arInfoArray[6];
 self.SText := l_arInfoArray[7];
end;

function TSendDesc.ToString: String;
var
 l_sSendType, l_sAnswereType, l_sStreamType, l_sStreamCompr: String;
begin
 case self.SType of
   MSG_NONE:
     l_sSendType := 'MSG_NONE';
   MSG_READY_FOR_RECEIVE:
     l_sSendType := 'MSG_READY_FOR_RECEIVE';
   MSG_SYNC_END:
     l_sSendType := 'MSG_SYNC_END';
   MSG_ERR:
     l_sSendType := 'MSG_ERR';
   MSG_ERR_DLG:
     l_sSendType := 'MSG_ERR_DLG';
   MSG_WARN:
     l_sSendType := 'MSG_WARN';
   MSG_WARN_DLG:
     l_sSendType := 'MSG_WARN_DLG';
   MSG_INFO:
     l_sSendType := 'MSG_INFO';
   MSG_INFO_DLG:
     l_sSendType := 'MSG_INFO_DLG';
   MSG_TBL_NEW:
     l_sSendType := 'MSG_TBL_NEW';
   MSG_TBL_DATA_CHNG:
     l_sSendType := 'MSG_TBL_DATA_CHNG';
   else
     l_sSendType := '';
 end;
 case self.SAnswere of
   MSGA_OK:
     l_sAnswereType := 'MSG_NONE';
   MSGA_CANCEL:
     l_sAnswereType := 'MSGA_CANCEL';
   MSGA_ABORT:
     l_sAnswereType := 'MSGA_ABORT';
   else
     l_sAnswereType := '';
 end;
 case self.SStreamType of
   STR_BIN:
     l_sStreamType := 'STR_BIN';
   STR_JSON:
     l_sStreamType := 'STR_JSON';
   STR_XML:
     l_sStreamType := 'STR_XML';
   else
     l_sStreamType := '';
 end;
 case self.SStreamCompr of
   STR_NO_COMPR:
     l_sStreamCompr := 'STR_NO_COMPR';
   STR_COMPR_FAST:
     l_sStreamCompr := 'STR_COMPR_FAST';
   STR_COMPR_DFT:
     l_sStreamCompr := 'STR_COMPR_DFT';
   STR_COMPR_MAX:
     l_sStreamCompr := 'STR_COMPR_MAX';
   else
     l_sStreamCompr := '';
 end;
 Result := 'SendId: ' + IntToStr(self.SId) +
           '; > SendType: ' + l_sSendType +
           ' <; SendAnswere: ' + l_sAnswereType +
           '; StreamType: ' + l_sStreamType +
           '; StreamCompr: ' + l_sStreamCompr +
           '; TableName: ' + self.STableName +
           '; SendText: ' + self.SText;
end;

procedure TSendDesc.Assign(i_oSendDesc: TSendDesc);
begin
 self.SId     := i_oSendDesc.SId;
 self.SType   := i_oSendDesc.SType;
 self.SAnswere:= i_oSendDesc.SAnswere;
 self.SStreamType:= i_oSendDesc.SStreamType;
 self.SStreamCompr:= i_oSendDesc.SStreamCompr;
 self.STableName  := i_oSendDesc.STableName;
 self.SText   := i_oSendDesc.SText;
end;

{ ----------------------------- TClientInfo ---------------------------------- }

constructor TClientInfo.Create;
begin
 inherited Create;
 self.Clear;
end;

destructor TClientInfo.Destroy();
begin
 inherited Destroy;
end;

procedure TClientInfo.Clear;
begin
 self.ClientId := '';
 self.ProgVerMajor := uGlobal.c_sProgVerMajor;
 self.ProgVerMinor := uGlobal.c_sProgVerMinor;
 self.ProgVersion  := uGlobal.c_sProgVersion;
end;

procedure TClientInfo.ParseString(aStr: String);
var
 l_arInfoArray: TArray<string>;
begin
 l_arInfoArray   := aStr.Split([DelimVal]);
 self.ClientId     := l_arInfoArray[0];
 self.ProgVerMajor := l_arInfoArray[1];
 self.ProgVerMinor := l_arInfoArray[2];
 self.ProgVersion  := l_arInfoArray[3];
end;

function  TClientInfo.BuildString: String;
begin
 Result := self.ClientId + DelimVal +
           self.ProgVerMajor + DelimVal +
           self.ProgVerMinor + DelimVal +
           self.ProgVersion + DelimVal;
end;

{ ----------------------------- TServerItem ---------------------------------- }

constructor TServerInfo.Create;
begin
 inherited Create;
 self.Clear;
end;

destructor TServerInfo.Destroy();
begin
 inherited Destroy;
end;

procedure TServerInfo.Clear;
begin
 self.ServerId := '';
 self.ProfileInfo.ProfileText := '';
 self.ManagerInfo.ManagerText := '';
end;

procedure TServerInfo.ParseString(aStr: String);
begin
 self.ServerId := aStr;
end;

function  TServerInfo.BuildString: String;
begin
 Result := self.ServerId;
end;

{ ----------------------------- TClientInfoOnServer ---------------------------------- }

constructor TClientInfoOnServer.Create(i_sClientId: String);
begin
 inherited Create;
 self.ClientId := i_sClientId;
 DtBegSync := 0.0;
 DtEndSync := 0.0;
 self.ActIdxOfSendList := -1;
 SetLength(self.SendList,0);
end;

destructor TClientInfoOnServer.Destroy();
begin
 if Assigned(self.SendList) then begin
   self.ClearListOfSends;
   SetLength(self.SendList,0);
 end;
 inherited Destroy;
end;

procedure TClientInfoOnServer.ClearListOfSends;
begin
 if Assigned(self.SendList) then begin
   SetLength(self.SendList,0);
 end;
end;

class function TClientInfoOnServer.ListHasClientInfo(i_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>; const i_sClientId: String): Boolean;
begin
 Result := False;
 if Assigned(i_lsTClientInfoOnServer) then begin
   Result := i_lsTClientInfoOnServer.ContainsKey(i_sClientId);
 end;
end;

class function TClientInfoOnServer.GeTClientInfoOnServerFromList(i_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>; const i_sClientId: String): TClientInfoOnServer;
var
 l_sKey: String;
begin
 Result := Nil;
 if Assigned(i_lsTClientInfoOnServer) then begin
   try
     Result := i_lsTClientInfoOnServer.Items[i_sClientId];
   except end;
 end;
end;

class function TClientInfoOnServer.AddClientInfoToList(i_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>; const i_sClientId: String): TClientInfoOnServer;
begin
 Result := Nil;
 if Assigned(i_lsTClientInfoOnServer) then begin
   Result := TClientInfoOnServer.Create(i_sClientId);
   i_lsTClientInfoOnServer.Add(i_sClientId,Result);
 end;
end;

class function TClientInfoOnServer.GetOrAddClientInfoToList(i_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>; const i_sClientId: String): TClientInfoOnServer;
begin
 Result := Nil;
 if Assigned(i_lsTClientInfoOnServer) then begin
   if i_lsTClientInfoOnServer.ContainsKey(i_sClientId) then begin
     try
       Result := i_lsTClientInfoOnServer.Items[i_sClientId];
     except end;
   end
   else begin
     Result := TClientInfoOnServer.Create(i_sClientId);
     i_lsTClientInfoOnServer.Add(i_sClientId,Result);
   end;
 end;
end;

class procedure TClientInfoOnServer.InitListOfUserInfos(var o_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>);
begin
 o_lsTClientInfoOnServer := TDictionary<String,TClientInfoOnServer>.Create;
end;

class procedure TClientInfoOnServer.CloseAndFreeListOfUserInfos(var o_lsTClientInfoOnServer: TDictionary<String,TClientInfoOnServer>);
var
 l_sKey: String;
 l_oUserInfo: TClientInfoOnServer;
 l_oPairEnum: TDictionary<String,TClientInfoOnServer>.TPairEnumerator;
begin
 if Assigned(o_lsTClientInfoOnServer) then begin
   l_oPairEnum := o_lsTClientInfoOnServer.GetEnumerator;
   while(l_oPairEnum.MoveNext) do begin
     l_sKey := l_oPairEnum.Current.Key;
     l_oUserInfo := l_oPairEnum.Current.Value;
     l_oUserInfo.Free;
     o_lsTClientInfoOnServer.Remove(l_sKey);
   end;
   o_lsTClientInfoOnServer.Clear;
   FreeAndNil(o_lsTClientInfoOnServer);
 end;
 FreeAndNil(o_lsTClientInfoOnServer);
end;

{ ---------------------------------------------------------------------------- }

{--
initialization
begin
end;

finalization
begin
end;
--}

end.
