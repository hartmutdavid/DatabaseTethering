unit TimeMem;

interface

uses SysUtils, Windows, System.Classes;

type
 TTimeMem = class(TObject)
   Private
      lActive: Boolean;

      StartTime: TDateTime;
      UsedTime: TDateTime;

      // Bytes of physical memory
      TotalPhys: Integer;
      // Percent of memory in use
      StartMemoryLoad: Integer;
      EndMemoryLoad: Integer;
      // Bytes physical memory available
      StartAvailPhys: Integer;
      EndAvailPhys: Integer;
      // Bytes available in paging file
      StartAvailPageFile: Integer;
      EndAvailPageFile: Integer;
      // bytes available in the user mode portion of the virtual address space
      StartAvailVirtual: Integer;
      EndAvailVirtual: Integer;

      TimeMemMessage: String;
      function  CreateInfoText(InfoTitle: String): String;
 public
   constructor Create;  virtual;
   destructor  Destroy; override;
   function  Active: Boolean;
   procedure Start;  // Record start time & memory
   procedure Stop;   // Record stop time & memory
   // procedure DisplayInfo(InfoTitle: String);
   procedure ProtInfo(Lines: TStrings; lWithMem: Boolean; InfoTitle: String);    // Ausgabe in Protokoll-Datei
 end;

var
 g_TimeMem: TTimeMem = Nil;


implementation


constructor TTimeMem.Create;
begin
 inherited Create;
 lActive := False;
 g_TimeMem := Self;
end;

destructor TTimeMem.Destroy;
begin
 inherited Destroy;
 g_TimeMem := Nil;
end;

function TTimeMem.Active: Boolean;
begin
 Result := lActive;
end;

Procedure TTimeMem.Start;
var
   SysMemoryStatus: TMemoryStatus;
begin
 { The help file says that the dwLength field of record type
   MEMORYSTATUS should be set before calling GlobalMemoryStatus.
   But we've been using it just fine without setting it. }
 GlobalMemoryStatus(SysMemoryStatus);
 StartMemoryLoad      := SysMemoryStatus.dwMemoryLoad;
 StartAvailPhys       := SysMemoryStatus.dwAvailPhys;
 StartAvailPageFile   := SysMemoryStatus.dwAvailPageFile;
 StartAvailVirtual    := SysMemoryStatus.dwAvailVirtual;
 lActive := True;
 StartTime := NOW;
end;

procedure TTimeMem.Stop;
var
 SysMemoryStatus: TMemoryStatus;
begin
 UsedTime := NOW - StartTime;
 GlobalMemoryStatus(SysMemoryStatus);
 TotalPhys          := SysMemoryStatus.dwTotalPhys;
 EndMemoryLoad      := SysMemoryStatus.dwMemoryLoad;
 EndAvailPhys       := SysMemoryStatus.dwAvailPhys;
 EndAvailPageFile   := SysMemoryStatus.dwAvailPageFile;
 EndAvailVirtual    := SysMemoryStatus.dwAvailVirtual;
 lActive := False;
end;

function TTimeMem.CreateInfoText(InfoTitle: String): String;
var
 Hour, Min, Sec, MSec: Word;
var
 UsedMemoryLoad, UsedAvailPhys, UsedAvailPageFile, UsedAvailVirtual,
 StrStartMemoryLoad, StrStartAvailPhys, StrStartAvailPageFile,
 StrStartAvailVirtual,
 StrEndMemoryLoad, StrTotalPhys, StrEndAvailPhys, StrEndAvailPageFile,
 StrEndAvailVirtual: String;
begin
   DecodeTime(UsedTime, Hour, Min, Sec, MSec);

   UsedMemoryLoad := IntToStr(EndMemoryLoad - StartMemoryLoad);

   UsedAvailPhys :=
      FloatToStrF( (StartAvailPhys - EndAvailPhys), ffNumber, 10, 0) ;
   UsedAvailPageFile :=
      FloatToStrF( (StartAvailPageFile - EndAvailPageFile), ffNumber, 10, 0) ;
   UsedAvailVirtual :=
      FloatToStrF( (StartAvailVirtual - EndAvailVirtual), ffNumber, 10, 0) ;

   StrTotalPhys := FloatToStrF(TotalPhys, ffNumber, 13, 0);

   StrStartMemoryLoad := FloatToStrF(StartMemoryLoad, ffNumber, 13, 0);
   StrStartAvailPhys := FloatToStrF(StartAvailPhys, ffNumber, 13, 0);
   StrStartAvailPageFile := FloatToStrF(StartAvailPageFile, ffNumber, 13, 0);
   StrStartAvailVirtual := FloatToStrF(StartAvailVirtual, ffNumber, 13, 0);

   StrEndMemoryLoad := FloatToStrF(EndMemoryLoad, ffNumber, 13, 0);
   StrEndAvailPhys := FloatToStrF(EndAvailPhys, ffNumber, 13, 0);
   StrEndAvailPageFile := FloatToStrF(EndAvailPageFile, ffNumber, 13, 0);
   StrEndAvailVirtual := FloatToStrF(EndAvailVirtual, ffNumber, 13, 0);

   TimeMemMessage :=  'Elapsed time (Start):  ' + IntToStr(Min) + ' Minutes  '
                                    + IntToStr(Sec) + ' Seconds  '
                                    + IntToStr(MSec)+ ' Milliseconds'
                                    +#10+#13+#10+#13+
      'Total physical memory: ' + StrTotalPhys + #10+#13+#10+#13 +
      'Percent of memory in use:' + #10+#13 +
         #9 + StrStartMemoryLoad + '%  before' + #10+#13 +
         #9 + StrEndMemoryLoad + '% after' + #10+#13 +
         #9 + UsedMemoryLoad + '% used' + #10+#13+#10+#13 +
      'Free physical memory:' + #10+#13 +
         #9 + StrStartAvailPhys + ' before' + #10+#13 +
         #9 + StrEndAvailPhys + ' after' + #10+#13 +
         #9 + UsedAvailPhys + ' used' + #10+#13+#10+#13 +
      'Paging file free:' + #10+#13 +
         #9 + StrStartAvailPageFile + ' before' + #10+#13 +
         #9 + StrEndAvailPageFile + ' after' + #10+#13 +
         #9 + UsedAvailPageFile + ' used' + #10+#13+#10+#13 +
      'Available virtual address space:' + #10+#13 +
         #9 + StrStartAvailVirtual + ' before' + #10+#13 +
         #9 + StrEndAvailVirtual + ' after' + #10+#13 +
         #9 + UsedAvailVirtual + ' used';
 Result := InfoTitle + #10+#13+#10+#13 + TimeMemMessage;
end;

{--
procedure TTimeMem.DisplayInfo(InfoTitle: String);
// Ausgabe auf Bildschirm
begin
 UsedTime := Now - StartTime;
 ShowMessage(CreateInfoText(InfoTitle));
end;
--}

procedure TTimeMem.ProtInfo(Lines: TStrings; lWithMem: Boolean; InfoTitle: String);    // Ausgabe in Protokoll-Datei
// Ausgabe in Protokoll-Datei
var
 Hour, Min, Sec, MSec: Word;
 l_fSec: Real;
 sStr: String;
begin
 UsedTime := Now - StartTime;
 if lWithMem then
   Lines.Add(CreateInfoText(InfoTitle))
 else begin
   DecodeTime(UsedTime, Hour, Min, Sec, MSec);
   l_fSec := Min * 60 + Sec + MSec/1000.0;
   sStr := InfoTitle + #10 + '- Elapsed time (Start):  ' + IntToStr(Min) + ' Minutes  '
                                   + IntToStr(Sec) + ' Seconds  '
                                   + IntToStr(MSec)+ ' Milliseconds = '
                                   + Format('%.3f sec',[l_fSec]);
   Lines.Add(sStr);
 end;
end;

end.
