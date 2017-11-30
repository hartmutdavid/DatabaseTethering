unit uGlobal;


interface

uses System.HelpIntfs, MSHTML, Data.SqlTimSt,
     System.SysUtils, System.IOUtils, uStrUtil,
     FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Consts,
     FireDAC.Phys.Intf, FireDAC.Phys.MSSQL, FireDAC.Phys.ODBCBase,
     FireDAC.Comp.Client, FireDAC.Comp.DataSet,
     FireDAC.Comp.UI, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
     FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt;

type
{$ifdef Win32}        { Definition eines 16-Bit-Integer (Unterschied: Delphi 1.0 zu 2.0 !) }
  Int16 = Integer;
{$else}
  Int16 = Integer;
{$endif}

 { Maskenstatus }
 ctMskStatus   = (mskNone, mskEdit, mskEinfuege, mskNaechster, mskAbfrage, mskListe);
 { Transferstatus }
 ctTransStates = (transNone, transValues, transSort, transRange, transViewport);

 { Spez. DB-Fehler }
 ctDBError = (enErrUnknown, enIndexErr, enTriggerErr, enOutOfView);

 { Funktion zum Feedback, ob etwas geaendert wurde }
 DataChangeFeedBack = procedure(Sender: TObject; lDataChange: Boolean;
                                sBuildText: AnsiString) of object;

 { Funktion zum Beginn einer Aktion }
 ctFktActionBegin = procedure(Sender: TObject; var lChangeAllowed: Boolean) of object;
 { Funktion zum Ende einer Aktion }
 ctFktActionEnd   = procedure(Sender: TObject; lActionOk: Boolean) of object;


Const
 c_sProgVerMajor = '1';
 c_sProgVerMinor = '1';

{$IFDEF PROG_S}
 c_sProgVersion  = 'S';      // S: Standardversion; E: Enterpriseversion
{$ELSE}
{$IFDEF PROG_E}
 c_sProgVersion  = 'E';      // E: Enterpriseversion
{$ELSE}
 c_sProgVersion  = 'T';      // T: Testversion
{$ENDIF}
{$ENDIF}

 c_iProgressFaktor = 5;

 FullDateTimeFormat = 'dd.MM.yyyy hh:nn:ss';
 FullDateFormat     = 'dd.MM.yyyy';
 FullTimeFormat     = 'hh:nn:ss';

  { Delimiter }
 c_sDelimiter = '@|@';
 c_iDelimiterSize = 3;

 { Kz. fuer Nichts }
 NilKz     = '-';

 { Kz. fuer "Alle" }
 AllKz     = 'Alle';

 { Default-Stoff-Nr., notwendig fuer Statistiken }
 sDftStoffNr = '00000';

 { Tab-Zeichen }
 cTabChar = #09;

 { Zeichen fuer das Schliessen einer Maske ueber [Alt, ...] }
 cCloseChar = 'S';
 { Zeichen fuer das Abspeichern eines Datensatzes (Main) ueber [Alt, ...] }
 cSaveChar = 'A';
 { Zeichen fuer das Verwerfen eines Datensatzes (Main) ueber [Alt, ...] }
 cCancelChar = 'V';

 { Zeichen fuer "Unchecked" bei Checkboxen }
 cUncheckedChar = 'N';
 { Zeichen fuer "Checked" bei Checkboxen }
 cCheckedChar = 'J';

 { OWD-Status: 'M': Mail erzeugt; 'S': versendet; 'R': Mail wurde vom Empfänger gelesen }
 cOWDStateMailCreate = 'M';
 cOWDStateMailSend   = 'S';
 cOWDStateMailRead   = 'R';

 { Default-Groesse fuer Listboxen }
 iDftHeightLstBx = 160;

 { System-Admin. }
 SysAdmin = 'SYSDBA';

 { Vorlagenarten }
 cDftVorlage = 'D';    { <= Default-Vorlage }
 cFrcVorlage = 'F';    { <= Zwangsvorlage }

 { Abort-Meldung; muss unterdrueckt werden! }
 sAbortMsg = 'Operation aborted';

 { DB-Fehlermeldungen (Trigger, Stored Procedure) enthalten einige Sonderzeichen }
 sDBErrSubStr = '<@>';

 { Epsilon: minWert fuer den Vergleich einer Differenz von Realzahlen gegen Null }
 f_EpsilonDM = 0.01;

var
 { Var.'s aus Registry / DB (SYSTEM) }
 g_sComputerName:    String;    { Computername }
 g_sUserName:        String;
 g_sApplicationName: String;
 g_sApplicationVersion: String;
 g_oDatabase: TFDConnection;
 g_lIsMSSQL:  Boolean;
 g_lIsOracle: Boolean;
 g_lIsSQLite: Boolean;
 sCurrencyFullValue:  String;    { Volle Waehrungsangabe, z.B. DEM, EUR }
 sCurrencyShortValue: String;   { Waehrungsangabe in Kurzform, DM }
 iDBLogging: Integer = 0;       { > 0; Protokoll der DB wird in der Tabelle DB_LOG geschrieben }
 g_lSysAdmin: Boolean = True;   { True: Der Benutzer hat SysAdm-Rechte ([Alt]-F12) }
 g_iActualYear: Integer;      { Aktuelles Jahr }
 g_iActualDay:  Integer;      { Aktueller Tag  }
 g_sActualYear: String[4];      { Aktuelles Jahr }
 g_sActualDay:  String[3];      { Aktueller Tag als String mit fuehrenden Nullen }
 iAnzRueckTage: Integer = 10;   { Anzahl Tage mit denen rueckwaertig default-maessig Daten retrieved werden sollen. }
 iAnzVorTage:   Integer = 10;   { Anzahl Tage mit denen vorwaertig default-maessig Daten retrieved werden sollen. }
 { iRAKorrekturLocking =0 : Keine Abfrage, alles implizit, d.h. erst nach einer Aenderung
                            und nach dem ersten Satzwechsel wird gelocked
   iRAKorrekturLocking =1 : Keine Abfrage, aber es wird exclusiv gelocked
   iRAKorrekturLocking =2 : Abfrage nach Locking   }
 iRAKorrekturLocking:  Integer = 2;
 lArchivWithProt: Boolean;      { Die SQL's der Archivierung werden protokolliert !}
 g_sExportPath:   String;       { Excel/SQL/Report-Ausgabe-Path }
 g_sQueriesPath:  String;       { Path zu den Queries-Files }
 g_sMailDir:   String;          { Pfad zum Mail-Verzeichnis }
 g_sArchivDir: String;          { Pfad zum Archiv-Verzeichnis }

 g_cExtensionSeparatorChar: Char;
 g_cDirectorySeparatorChar: Char;
 g_cPathSeparator: Char;
 g_cVolumeSeparatorChar: Char;

 g_sHomePath:      String;     // <- Mit DirectorySeparator am Schluss!
 g_sRunDir:        String;     // <- Mit DirectorySeparator am Schluss!
 g_sWinDirPath:    String;     // <- Mit DirectorySeparator am Schluss!
 g_sIniFileName:   String;
 g_sIniFilePath:   String;
 g_iServicePort:   Integer = 0;  // <- HTTP-Port des Service

 g_iProgMode: Integer = 1;     // <- Programmmodus:
                               //   1 : FMS ohne Browser
                               //   2 : Nur Browser-Betrieb
                               //   3 : FMS und Browser-Betrieb

 g_sDefaultURL:   String = '';
 g_sFMSHomeURL:   String = '';
 g_sFPNrReadFilter:  String = '';
 g_sFPNrWriteFilter: String = '';
 g_sKstFilter:    String = '';

 g_oHelpSystem : IHelpSystem;  // <- Hilfesystem

 // Variablen zu den Service-Job's
 g_Locked: Boolean = False;
 g_DoJobsExecute: Boolean = False;
 g_DoJobUnfallmeldungen: Boolean = False;
 g_PeriodeUnfallmeldungen: Integer = 10;     // <- in min
 g_DoJobIdentifikatoren: Boolean = False;
 g_PeriodeIdentifikatoren: Integer = 10;     // <- in min

 // ----------------------------------------------------------------------------
 // Achtung: Muessen in den Methoden "WebBrowserBeforeNavigate2",
 // ========                         "WebBrowserDocumentComplete",
 //                                  "WebBrowserNavigateError"
 //                                  "WebBrowserTitleChange"
 //          vom WebBrowser behandelt und gesetzt werden.
 g_pDisp:          IDispatch = Nil;
 g_lWaitForPage:   Boolean = False;   // <- True: Innerhalb der Methode "uWebBrowserUtil.WaitForPage"
 g_iBrowserStatus: Integer = 0;
 g_iWaitCntForDOMBuild:     Integer = 0;
 g_iCntForRefreshIfExpired: Integer = 0;
 g_sHTMLPageTitle: String = '';
 g_sLoadedURL:     String = '';
 g_sRetVal:        String = '';
 g_oHtmlDoc2:      IHTMLDocument2;  // <- Nur hier befindet sich das aktuell angezeigte HTML-Dokument
 g_oHtmlDoc3:      IHTMLDocument3;
 g_sUSID: String = '';         // User-Id.
 g_sSTID: String = '';         // Step-Id. (Wird von einem Aufruf zum naechsten weitergereicht!)
 //
 // ----------------------------------------------------------------------------
 // Browser-Kommunikationsparameter
 g_TimeOut: Cardinal = 120000;   // in Millisekunden;  vorab:  40000
 g_Sleep:   Cardinal =   1000;   // in Millisekunden;  vorab:    800
 // ----------------------------------------------------------------------------

 // Für den schnellen Programmablauf notwendig!
 g_oDBQrySystem: TFDQuery = Nil;
 g_oDBUpdSystem: TFDQuery = Nil;


implementation

initialization
begin
 g_sComputerName := uStrUtil.LocalComputerName();
 g_sUserName     := uStrUtil.WUserName();
 g_sRunDir     := uStrUtil.AddDirectorySeparatorChar(TDirectory.GetCurrentDirectory);
 g_sHomePath   := uStrUtil.AddDirectorySeparatorChar(TPath.GetHomePath);
 g_sWinDirPath := uStrUtil.AddDirectorySeparatorChar(uStrUtil.GetWinDir);
 g_sIniFileName := 'Tethering.ini';
 g_sIniFilePath := '';
 g_cExtensionSeparatorChar := TPath.ExtensionSeparatorChar;
 g_cDirectorySeparatorChar := TPath.DirectorySeparatorChar;
 g_cPathSeparator          := TPath.PathSeparator;
 g_cVolumeSeparatorChar    := TPath.VolumeSeparatorChar;
 if not GetHelpSystem(g_oHelpSystem) then
   g_oHelpSystem := Nil;
 //
 g_sExportPath := '\test\';       // Temporaer ...
 g_sQueriesPath:= '\test\';       // Temporaer ...
 //
end;

finalization

end.
