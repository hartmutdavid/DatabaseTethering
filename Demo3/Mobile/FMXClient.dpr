program FMXClient;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  FMXClientForm in 'FMXClientForm.pas' {FormFMXClient},
  uTetherUtil in '..\..\Common\uTetherUtil.pas',
  uStrutil in '..\..\Common\uStrutil.pas',
  uGlobal in '..\..\Common\uGlobal.PAS',
  TimeMem in '..\..\Common\TimeMem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFMXClient, FormFMXClient);
  Application.Run;
end.
