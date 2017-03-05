unit uProgressCount;

interface

uses
  { marvin }
  Marvin.Comps.MDL.ProgressCount,
  Marvin.Comps.MDL.Intf.ProgressCount,
  { system }
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Rtti,
  { firemonkey }
  FMX.Types,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Effects,
  FMX.Ani,
  FMX.Colors,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox;

type
  TFormProgressCountApplicationPOC = class(TForm)
    tlbBotoes: TToolBar;
    btnIniciar: TButton;
    tmrProgresso: TTimer;
    btn1: TButton;
    btn2: TButton;
    lytPrincipal: TLayout;
    btnEncerrar: TButton;
    btnEstilos: TButton;
    dlgOpenStyle: TOpenDialog;
    lblProgresso: TLabel;
    stbStyle: TStyleBook;
    spbProgress: TSpinBox;
    spbMax: TSpinBox;
    edtMensagem: TEdit;
    edtClearMensagem: TClearEditButton;
    swtCoresRandomicas: TSwitch;
    clqCorPadrao: TColorQuad;
    cbxCorPadrao: TColorBox;
    cpkCorPadrao: TColorPicker;
    spbTamanho: TSpinBox;
    spbTamanhoArco: TSpinBox;
    spbDuracaoTotal: TSpinBox;
    lblInicio: TLabel;
    lblMax: TLabel;
    lblMensagem: TLabel;
    lblTamanho: TLabel;
    lblTamanhoArco: TLabel;
    lblDuracao: TLabel;
    lblCoresRandomicas: TLabel;
    lblCorPadrao: TLabel;
    lytCor: TLayout;
    lblIsRoundLines: TLabel;
    swtIsRoundLines: TSwitch;
    swtBackgroundClean: TSwitch;
    lblBackgroundClean: TLabel;
    scbBackground: TVertScrollBox;
    procedure FormDestroy(Sender: TObject);
    procedure tmrProgressoTimer(Sender: TObject);
    procedure btnIniciarClick(Sender: TObject);
    procedure btnEncerrarClick(Sender: TObject);
    procedure btnEstilosClick(Sender: TObject);
  private
    { objeto do progress count }
    FProgressCount: TMRVProgressCount;
    procedure InternalCreateInstance;
    procedure InternalOnInit;
    { método que recupera informação do progress count }
    procedure InternalOnUpdateStatus(const AProgressCount: IMRVProgressCount);
  protected
    { recupera dados da interface com o usuário }
    procedure GetFromGUI;
    { cria dinamicamente o progress count }
    procedure CreateProgressCount;
    { inicia }
    procedure InternalOnStart;
    { encerra }
    procedure InternalOnFinally;
  public
  end;

var
  FormProgressCountApplicationPOC: TFormProgressCountApplicationPOC;

implementation

uses
  FMX.Styles;

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

procedure TFormProgressCountApplicationPOC.CreateProgressCount;
begin
  { se estiver carregado então encerra }
  Self.InternalOnFinally;
  { cria nova instância e configura }
  Self.InternalCreateInstance;
end;

procedure TFormProgressCountApplicationPOC.FormDestroy(Sender: TObject);
begin
  { encerra tudo e finaliza o componente }
  Self.InternalOnFinally;
end;

procedure TFormProgressCountApplicationPOC.btnEstilosClick(Sender: TObject);
begin
  if dlgOpenStyle.Execute then
  begin
    { libera o estilo padrão inicial }
    Self.StyleBook := nil;
    { carrega o estilo selecionado }
    TStyleManager.SetStyle(TStyleStreaming.LoadFromFile(dlgOpenStyle.FileName));
  end;
end;

procedure TFormProgressCountApplicationPOC.btnIniciarClick(Sender: TObject);
begin
  { inicializa as animações }
  Self.InternalOnStart;
end;

procedure TFormProgressCountApplicationPOC.InternalOnStart;
begin
  { cria objeto }
  Self.CreateProgressCount;
  { inicializa }
  Self.InternalOnInit;
  { finaliza o timer que passa o valor do progresso }
  tmrProgresso.Enabled := True;
  { inicializa as animações }
  FProgressCount.Play;
end;

procedure TFormProgressCountApplicationPOC.InternalOnUpdateStatus(const
  AProgressCount: IMRVProgressCount);
begin
  { quando atualiza a pintura do ProgressCount }
  lblProgresso.Text := AProgressCount.Progress.ToString;
end;

procedure TFormProgressCountApplicationPOC.InternalCreateInstance;
begin
  FProgressCount := TMRVProgressCount.Create(lytPrincipal);
  FProgressCount.Parent := lytPrincipal;
  FProgressCount.Align := TAlignLayout.Contents;
end;

procedure TFormProgressCountApplicationPOC.InternalOnInit;
begin
  { recupera os valores padrão }
  Self.GetFromGUI;
  { delegate no evento }
  FProgressCount.OnUpdateStatus := InternalOnUpdateStatus;
end;

procedure TFormProgressCountApplicationPOC.tmrProgressoTimer(Sender: TObject);
begin
  FProgressCount.Progress := FProgressCount.Progress + 1;
  if FProgressCount.Progress >= FProgressCount.Max then
  begin
    tmrProgresso.Enabled := False;
    FProgressCount.Stop;
  end;
end;

procedure TFormProgressCountApplicationPOC.btnEncerrarClick(Sender: TObject);
begin
  Self.InternalOnFinally;
end;

procedure TFormProgressCountApplicationPOC.InternalOnFinally;
begin
  if Assigned(FProgressCount) then
  begin
    { finaliza o timer que passa o valor do progresso }
    tmrProgresso.Enabled := False;
    { finaliza o componente }
    FProgressCount.DisposeOf;
    FProgressCount := nil;
  end;
end;

procedure TFormProgressCountApplicationPOC.GetFromGUI;
begin
  { inicializa }
  FProgressCount.Progress := spbProgress.Value;
  FProgressCount.Max := spbMax.Value;
  FProgressCount.Text := edtMensagem.Text.Trim;
  { cores randômicas }
  FProgressCount.UseRandomColors := swtCoresRandomicas.IsChecked;
  { estilo do fundo }
  if swtBackgroundClean.IsChecked then
  begin
    { claro }
    FProgressCount.BackgroundStyle := TMRVBackgrpoundStyle.bsClear;
  end
  else
  begin
    { escuro }
    FProgressCount.BackgroundStyle := TMRVBackgrpoundStyle.bsTwilights;
  end;
  FProgressCount.DefaultColor := cbxCorPadrao.Color;
  { linhas }
  FProgressCount.IsRoundLines := swtIsRoundLines.IsChecked;
  { tamanhos }
  FProgressCount.TotalHeight := spbTamanho.Value;
  FProgressCount.TotalWidth := FProgressCount.TotalHeight;
  FProgressCount.ArcSize := spbTamanhoArco.Value;
  { duração em segundos para os giros completarem 1 ciclo }
  FProgressCount.TotalDuration := spbDuracaoTotal.Value;
end;

end.


