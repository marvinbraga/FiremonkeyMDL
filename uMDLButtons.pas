{ /------------------------------------------------------------------/
  / Por Marcus Vinicius Braga - Marvinbraga Youtube Channel - Brazil /
  /------------------------------------------------------------------/  }

unit uMDLButtons;

interface

uses
  { embarcadero }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  { firemonkey }
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Effects,
  FMX.Objects,
  FMX.Ani,
  { marvin }
  Marvin.Comps.MDL.Intf.Button,
  Marvin.Comps.MDL.Buttons,
  FMX.Filter.Effects;

type
  TFormPOCButtonsMDL = class(TForm)
    btnTeste: TButton;
    lblRipple: TLabel;
    lblEnabled: TLabel;
    lblMensagem: TLabel;
    lblCor: TLabel;
    swtRipple: TSwitch;
    swtEnabled: TSwitch;
    trcColor: TTrackBar;
    btnMDL: TMRVButtonMDL;
    retPainel: TRectangle;
    sdw1: TShadowEffect;
    GaussianBlurEffect1: TGaussianBlurEffect;
    retBtnDark: TRectangle;
    sdwDark: TShadowEffect;
    retClean: TRectangle;
    btnMDLClean: TMRVButtonMDL;
    sdwClean: TShadowEffect;
    procedure FormCreate(Sender: TObject);
    procedure swtEnabledClick(Sender: TObject);
    procedure swtRippleClick(Sender: TObject);
    procedure trcColorChange(Sender: TObject);
  private
    { Private declarations }
    procedure InitButton;
    procedure InternalOnClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormPOCButtonsMDL: TFormPOCButtonsMDL;

implementation

uses
  FMX.ActnList;

{$R *.fmx}

{ TFormPOCButtonsMDL }

procedure TFormPOCButtonsMDL.FormCreate(Sender: TObject);
begin
  { inicializa os botões }
  Self.InitButton;
  { inicializa mensagem }
  lblMensagem.Text := EmptyStr;
end;

procedure TFormPOCButtonsMDL.InitButton;
begin
  { eventos }
  btnMDL.AddOnClickEvent(InternalOnClick);
  btnMDLClean.AddOnClickEvent(InternalOnClick);
  btnTeste.OnClick := InternalOnClick;
end;

procedure TFormPOCButtonsMDL.InternalOnClick(Sender: TObject);
var
  LMensagem: string;
begin
  { recupera a mensagem anterior }
  LMensagem := lblMensagem.Text;
  { inicializa a mensagem }
  lblMensagem.Text := EmptyStr;
  { verifica se está em branco para exibir nova mensagem }
  if (LMensagem = EmptyStr) then
  begin
    { recupera o texto do botão clicado }
    lblMensagem.Text := 'Cliquei em ' + (TComponent(Sender) as ICaption).Text;
  end;
end;

procedure TFormPOCButtonsMDL.swtEnabledClick(Sender: TObject);
begin
  { ajusta o acesso aos botões }
  btnMDL.Enabled := swtEnabled.IsChecked;
  btnMDLClean.Enabled := swtEnabled.IsChecked;
  btnTeste.Enabled := btnMDL.Enabled;
end;

procedure TFormPOCButtonsMDL.swtRippleClick(Sender: TObject);
begin
  { efeito Ripple }
  btnMDL.Ripple := swtRipple.IsChecked;
  btnMDLClean.Ripple := swtRipple.IsChecked;
end;

procedure TFormPOCButtonsMDL.trcColorChange(Sender: TObject);
begin
  { altera a cor do botão }
  btnMDL.ButtonColor := TMRVColorMDLType(Trunc(trcColor.Value));
  btnMDLClean.ButtonColor := TMRVColorMDLType(Trunc(trcColor.Value));
  lblCor.Text := 'Cor: ' + trcColor.Value.ToString;
end;

end.


