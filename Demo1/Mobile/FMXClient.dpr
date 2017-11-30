program FMXClient;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  FMXClientForm in 'FMXClientForm.pas' {FormFMXClient},
  uTetherUtil in '..\..\Common\uTetherUtil.pas',
  TimeMem in '..\..\Common\TimeMem.pas',
  uGlobal in '..\..\Common\uGlobal.PAS',
  uStrutil in '..\..\Common\uStrutil.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFMXClient, FormFMXClient);
  Application.Run;
end.
