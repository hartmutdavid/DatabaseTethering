program VCLServerDesktop;

uses
  Vcl.Forms,
  uVCLServerForm in 'uVCLServerForm.pas' {FormVCLServer},
  uTetherUtil in '..\..\Common\uTetherUtil.pas',
  uGlobal in '..\..\Common\uGlobal.PAS',
  uStrutil in '..\..\Common\uStrutil.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormVCLServer, FormVCLServer);
  Application.Run;
end.
