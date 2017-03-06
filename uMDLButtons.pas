unit uMDLButtons;

interface

uses
  { embarcadero }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  { firemonkey }
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  { marvin }
  Marvin.Comps.MDL.Intf.Button,
  Marvin.Comps.MDL.Buttons;

type
  TFormPOCButtonsMDL = class(TForm)
    btnMDL: TMRVButtonMDL;
    btnTeste: TButton;
    lblRipple: TLabel;
    lblEnabled: TLabel;
    lblMensagem: TLabel;
    lblCor: TLabel;
    swtRipple: TSwitch;
    swtEnabled: TSwitch;
    trcColor: TTrackBar;
    swtCorPadrao: TSwitch;
    lblCorPadrao: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure swtEnabledClick(Sender: TObject);
    procedure swtRippleClick(Sender: TObject);
    procedure trcColorChange(Sender: TObject);
    procedure swtCorPadraoClick(Sender: TObject);
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
  { inicializa os textos dos botões }
  btnMDL.Text := 'BUTTON MDL';
  btnTeste.Text := 'BUTTON FMX';
  { bordas arredondadas }
  //btnMDL.XRadius := 5;
  //btnMDL.YRadius := 5;
  { cor }
  btnMDL.PrimaryColor := TAlphaColors.Aqua;
  btnMDL.AccentColor := TAlphaColors.Yellow;
  btnMDL.BackgroundStyle := bsDark;
  { eventos }
  btnMDL.AddOnClickEvent(InternalOnClick);
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

procedure TFormPOCButtonsMDL.swtCorPadraoClick(Sender: TObject);
begin
  if swtCorPadrao.IsChecked then
  begin
    btnMDL.BackgroundStyle := bsClean;
  end
  else
  begin
    btnMDL.BackgroundStyle := bsDark;
  end;
end;

procedure TFormPOCButtonsMDL.swtEnabledClick(Sender: TObject);
begin
  { ajusta o acesso aos botões }
  btnMDL.Enabled := swtEnabled.IsChecked;
  btnTeste.Enabled := btnMDL.Enabled;
end;

procedure TFormPOCButtonsMDL.swtRippleClick(Sender: TObject);
begin
  { efeito Ripple }
  btnMDL.Ripple := swtRipple.IsChecked;
end;

procedure TFormPOCButtonsMDL.trcColorChange(Sender: TObject);
begin
  { altera a cor do botão }
  btnMDL.ButtonColor := TMRVColorMDLType(Trunc(trcColor.Value));
  lblCor.Text := 'Cor: ' + trcColor.Value.ToString;
end;

end.


