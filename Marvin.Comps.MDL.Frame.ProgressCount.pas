{ /------------------------------------------------------------------/
  / Por Marcus Vinicius Braga - Marvinbraga Youtube Channel - Brazil /
  /------------------------------------------------------------------/  }

unit Marvin.Comps.MDL.Frame.ProgressCount;

interface

uses
  { marvin }
  Marvin.Comps.MDL.Intf.ProgressCount,
  { system }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  { firemonkey }
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Effects,
  FMX.Controls.Presentation,
  FMX.Ani,
  FMX.Objects;

type
  { classe de opoio para instanciar objeto que implementa a interface }
  coMRVProgressCount = class
    class function Create(AOwner: TComponent): IMRVProgressCount;
  end;

  { classe que implementa o objeto de progress count }
  TfraMarvinProgressCount = class(TFrame, IMRVProgressCount)
    arcValue1: TArc;
    arcValue2: TArc;
    arcValue3: TArc;
    arcValue4: TArc;
    arcProgress: TArc;
    crcProgress: TCircle;
    retMessage: TRectangle;
    retBackground: TRectangle;
    lblMessage: TLabel;
    lblProgress: TLabel;
    sdwProgress: TShadowEffect;
    sdwArc1: TShadowEffect;
    sdwMessage: TShadowEffect;
    FloatAnimationProgressValue: TFloatAnimation;
    FloatAnimationValue4: TFloatAnimation;
    FloatAnimationValue3: TFloatAnimation;
    FloatAnimationValue2: TFloatAnimation;
    FloatAnimationWidth: TFloatAnimation;
    FloatAnimationHeight: TFloatAnimation;
    FloatAnimationValue1: TFloatAnimation;
    { eventos dos efeitos }
    procedure FloatAnimationHeightFinish(Sender: TObject);
    procedure FloatAnimationValue1Process(Sender: TObject);
    procedure FloatAnimationValue1Finish(Sender: TObject);
    procedure FloatAnimationValue2Finish(Sender: TObject);
    procedure FloatAnimationValue3Finish(Sender: TObject);
    procedure FloatAnimationValue4Finish(Sender: TObject);
    { ajuste de tamanho }
    procedure FrameResize(Sender: TObject);
  private
    FStopped: Boolean;
    FTotalDuration: Double;
    FProgress: Single;
    FTotalHeight: Single;
    FTotalWidth: Single;
    FArcSize: Single;
    FOnUpdateStatus: TOnUpdateStatus;
    FUseRandomColors: Boolean;
    FDefaultColor: TAlphaColor;
    FMax: Single;
    FMin: Single;
    FIsRoundLines: Boolean;
    FBackgroundStyle: TMRVBackgrpoundStyle;
    procedure RestartAnimation(const AAnimation: TFloatAnimation);
    procedure UpdateValue;
    procedure SetTotalDuration(const Value: Double);
    procedure SetProgress(const Value: Single);
    procedure SetTotalHeight(const Value: Single);
    procedure SetTotalWigth(const Value: Single);
    procedure SetArcSize(const Value: Single);
    procedure UpdateProgressStatus(const Value: Single);
    function GetArcSize: Single;
    function GetOnUpdateStatus: TOnUpdateStatus;
    function GetProgress: Single;
    function GetTotalDuration: Double;
    function GetTotalHeight: Single;
    function GetTotalWidth: Single;
    function GetUseRandomColors: Boolean;
    procedure SetOnUpdateStatus(const Value: TOnUpdateStatus);
    procedure SetUseRandomColors(const Value: Boolean);
    function GetDefaultColor: TAlphaColor;
    procedure SetDefaultColor(const Value: TAlphaColor);
    procedure InternalConfigureTotalSizes;
    procedure InternalAdjustMessage;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetMax: Single;
    function GetMin: Single;
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    function GetIsRoundLines: Boolean;
    procedure SetIsRoundLines(const Value: Boolean);
    function GetBackgroundStyle: TMRVBackgrpoundStyle;
    procedure SetBackgroundStyle(const Value: TMRVBackgrpoundStyle);
  protected
    procedure Init; virtual;
    procedure InternalStopAnimation;
    procedure InternalInitializeEndAngle;
    procedure InternalOnExecuteEndAnimation;
    procedure InternalShowMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { recupera o parent }
    procedure SetParentControl(const AParent: TFmxObject);
    { inicia a animação }
    procedure Play;
    { finaliza a animação }
    procedure Stop;
    { finaliza a animação de saída do círculo }
    procedure ExecuteEndAnimation;
    { propriedades }
    property DefaultColor: TAlphaColor read GetDefaultColor write SetDefaultColor;
    property TotalDuration: Double read GetTotalDuration write SetTotalDuration;
    property Progress: Single read GetProgress write SetProgress;
    property Max: Single read GetMax write SetMax;
    property Min: Single read GetMin write SetMin;
    property TotalHeight: Single read GetTotalHeight write SetTotalHeight;
    property TotalWidth: Single read GetTotalWidth write SetTotalWigth;
    property ArcSize: Single read GetArcSize write SetArcSize;
    property Text: string read GetText write SetText;
    property BackgroundStyle: TMRVBackgrpoundStyle read GetBackgroundStyle write SetBackgroundStyle;
    property UseRandomColors: Boolean read GetUseRandomColors write SetUseRandomColors;
    property IsRoundLines: Boolean read GetIsRoundLines write SetIsRoundLines;
    property OnUpdateStatus: TOnUpdateStatus read GetOnUpdateStatus write SetOnUpdateStatus;
  end;

implementation

{$R *.fmx}

{ coMRVProgressCount }

class function coMRVProgressCount.Create(AOwner: TComponent): IMRVProgressCount;
begin
  Result := TfraMarvinProgressCount.Create(AOwner);
end;

{ TfraMarvinProgressCount }

constructor TfraMarvinProgressCount.Create(AOwner: TComponent);

  procedure LConfigControl(AControl: TControl);
  begin
    AControl.Locked := True;
    AControl.Stored := False;
    AControl.HitTest := False;
  end;


begin
  inherited;
  LConfigControl(arcValue1);
  LConfigControl(arcValue2);
  LConfigControl(arcValue3);
  LConfigControl(arcValue4);
  LConfigControl(arcProgress);
  LConfigControl(crcProgress);
  LConfigControl(retMessage);
  LConfigControl(retBackground);
  LConfigControl(lblMessage);
  LConfigControl(lblProgress);

  FBackgroundStyle := TMRVBackgrpoundStyle.bsClear;
  Self.Init;
  Self.Min := 0;
  Self.Max := 100;
end;

destructor TfraMarvinProgressCount.Destroy;
begin
  { finaliza o objeto }
  inherited;
end;

procedure TfraMarvinProgressCount.ExecuteEndAnimation;
begin
  Self.InternalOnExecuteEndAnimation;
end;

procedure TfraMarvinProgressCount.InternalAdjustMessage;
begin
  { ajusta o painel de mensagem }
  if not retMessage.Visible then
  begin
    retMessage.Position.Y := Self.Height + 5;
  end;
  retMessage.Margins.Left := (Self.Width / 2) - 100;
  retMessage.Margins.Right := (Self.Width / 2) - 100;
end;

procedure TfraMarvinProgressCount.InternalConfigureTotalSizes;
begin
  { configuração dos tamanhos }
  arcValue1.Height := FTotalHeight;
  arcValue1.Width := FTotalWidth;
  FloatAnimationHeight.StopValue := FTotalHeight;
  FloatAnimationWidth.StopValue := FTotalWidth;
end;

procedure TfraMarvinProgressCount.FloatAnimationHeightFinish(Sender: TObject);
begin
  { animação do tamanho }
  FloatAnimationValue1.Enabled := True;
end;

procedure TfraMarvinProgressCount.FloatAnimationValue1Finish(Sender: TObject);
begin
  Self.RestartAnimation(FloatAnimationValue2);
  arcValue4.EndAngle := 0;
end;

procedure TfraMarvinProgressCount.FloatAnimationValue1Process(Sender: TObject);
begin
  Self.UpdateValue;
end;

procedure TfraMarvinProgressCount.FloatAnimationValue2Finish(Sender: TObject);
begin
  Self.RestartAnimation(FloatAnimationValue3);
  arcValue1.EndAngle := 0;
end;

procedure TfraMarvinProgressCount.FloatAnimationValue3Finish(Sender: TObject);
begin
  Self.RestartAnimation(FloatAnimationValue4);
  arcValue2.EndAngle := 0;
end;

procedure TfraMarvinProgressCount.FloatAnimationValue4Finish(Sender: TObject);
begin
  Self.RestartAnimation(FloatAnimationValue1);
  arcValue3.EndAngle := 0;
end;

procedure TfraMarvinProgressCount.FrameResize(Sender: TObject);
begin
  { ajusta o tamanho da mensagem }
  Self.InternalAdjustMessage;
end;

function TfraMarvinProgressCount.GetArcSize: Single;
begin
  Result := FArcSize;
end;

function TfraMarvinProgressCount.GetBackgroundStyle: TMRVBackgrpoundStyle;
begin
  Result := FBackgroundStyle;
end;

function TfraMarvinProgressCount.GetDefaultColor: TAlphaColor;
begin
  Result := FDefaultColor;
end;

function TfraMarvinProgressCount.GetIsRoundLines: Boolean;
begin
  Result := FIsRoundLines;
end;

function TfraMarvinProgressCount.GetMax: Single;
begin
  Result := FMax;
end;

function TfraMarvinProgressCount.GetMin: Single;
begin
  Result := FMin;
end;

function TfraMarvinProgressCount.GetOnUpdateStatus: TOnUpdateStatus;
begin
  Result := FOnUpdateStatus;
end;

procedure TfraMarvinProgressCount.SetParentControl(const AParent: TFmxObject);
begin
  Self.Parent := AParent;
end;

function TfraMarvinProgressCount.GetProgress: Single;
begin
  Result := FProgress;
end;

function TfraMarvinProgressCount.GetText: string;
begin
  Result := lblMessage.Text;
end;

function TfraMarvinProgressCount.GetTotalDuration: Double;
begin
  Result := FTotalDuration;
end;

function TfraMarvinProgressCount.GetTotalHeight: Single;
begin
  Result := FTotalHeight;
end;

function TfraMarvinProgressCount.GetTotalWidth: Single;
begin
  Result := FTotalWidth;
end;

function TfraMarvinProgressCount.GetUseRandomColors: Boolean;
begin
  Result := FUseRandomColors;
end;

procedure TfraMarvinProgressCount.Init;
begin
  FStopped := False;
  Self.Progress := 0;
  Self.InternalInitializeEndAngle;
  Self.InternalAdjustMessage;
end;

procedure TfraMarvinProgressCount.Play;
begin
  Self.Init;
  FloatAnimationHeight.Start;
  FloatAnimationWidth.Start;
end;

procedure TfraMarvinProgressCount.RestartAnimation(const AAnimation: TFloatAnimation);
begin
  if not FStopped then
  begin
    if FUseRandomColors then
    begin
      {$WARNINGS OFF} // usado apenas para retirar os avisos
      TArc(AAnimation.Parent).Stroke.Color := TAlphaColor((Random(MaxInt) or
        TAlphaColors.Alpha));
      {$WARNINGS ON}
    end;
    AAnimation.Start;
  end;
end;

procedure TfraMarvinProgressCount.Stop;
begin
  { informa que está parado }
  FStopped := True;
  { para as animações }
  Self.InternalStopAnimation;
  { inicializa todos os arcos  }
  Self.InternalInitializeEndAngle;
  { exibe a mensagem }
  Self.InternalShowMessage;
  { executa a animação de saída do contator }
  Self.InternalOnExecuteEndAnimation;
end;

procedure TfraMarvinProgressCount.InternalOnExecuteEndAnimation;
begin
  Application.ProcessMessages;
  arcProgress.EndAngle := 0;
  lblProgress.Visible := False;
  { diminue o tamanho do arco principal até sumir }
  TAnimator.AnimateFloat(arcValue1, 'Height', 0, FloatAnimationHeight.Duration,
    TAnimationType.Out, TInterpolationType.Linear);
  { animação com espera até finalizar }
  TAnimator.AnimateFloatWait(arcValue1, 'Width', 0, FloatAnimationHeight.Duration,
    TAnimationType.Out, TInterpolationType.Linear);
  { torna inivisível }
  arcValue1.Visible := False;
end;

procedure TfraMarvinProgressCount.InternalInitializeEndAngle;
begin
  arcValue1.EndAngle := 0;
  arcValue2.EndAngle := 0;
  arcValue3.EndAngle := 0;
  arcValue4.EndAngle := 0;
end;

procedure TfraMarvinProgressCount.InternalShowMessage;
begin
  retMessage.Visible := True;
  { executa a animação da mensagem de texto }
  TAnimator.AnimateFloatWait(retMessage, 'Position.Y', arcValue1.Position.Y - 5,
    1.5, TAnimationType.InOut, TInterpolationType.Back);
end;

procedure TfraMarvinProgressCount.InternalStopAnimation;
begin
  { para todas as animações }
  FloatAnimationHeight.Stop;
  FloatAnimationWidth.Stop;
  FloatAnimationValue4.Stop;
  FloatAnimationValue3.Stop;
  FloatAnimationValue2.Stop;
  FloatAnimationValue1.Stop;
end;

procedure TfraMarvinProgressCount.UpdateProgressStatus(const Value: Single);
var
  LPercent: Single;
begin
  { não faz nada com valores inválidos }
  if ((FProgress = 0) or (FMax = 0)) then
  begin
    Exit;
  end;
  { calcula o percentual do progresso }
  LPercent := FProgress * 100 / FMax;
  { calcula o valor do progresso relacionado ao ângulo }
  arcProgress.EndAngle := 360 * LPercent / 100;
  { exibe o valor }
  lblProgress.Text := FormatFloat('0.0', LPercent) + '%';
end;

procedure TfraMarvinProgressCount.SetArcSize(const Value: Single);

  procedure LAdjustArc(const AArc: TArc);
  begin
    AArc.Stroke.Thickness := FArcSize;
    AArc.Stroke.Thickness := FArcSize;
    AArc.Margins.Bottom := FArcSize + 2;
    AArc.Margins.Top := FArcSize + 2;
    AArc.Margins.Left := FArcSize + 2;
    AArc.Margins.Right := FArcSize + 2;
  end;

begin
  FArcSize := Value;
  { ajusta a largura dos arcos }
  LAdjustArc(arcValue1);
  LAdjustArc(arcValue2);
  LAdjustArc(arcValue3);
  LAdjustArc(arcValue4);
  { faz com o arco do progresso }
  arcProgress.Stroke.Thickness := FArcSize;
end;

procedure TfraMarvinProgressCount.SetBackgroundStyle(
  const Value: TMRVBackgrpoundStyle);
begin
  FBackgroundStyle := Value;
  case FBackgroundStyle of
    bsClear:
    begin
      { com fundo claro }
      crcProgress.Fill.Color := TAlphaColors.Azure;
      lblProgress.FontColor := TAlphaColors.Black;
    end;
    bsTwilights:
    begin
      { com fundo escuro }
      crcProgress.Fill.Color := $FF040519;
      lblProgress.FontColor := TAlphaColors.Azure;
    end;
  end;
end;

procedure TfraMarvinProgressCount.SetDefaultColor(const Value: TAlphaColor);
begin
  FDefaultColor := Value;
  { informa a cor padrão }
  arcValue1.Stroke.Color := FDefaultColor;
  arcValue2.Stroke.Color := FDefaultColor;
  arcValue3.Stroke.Color := FDefaultColor;
  arcValue4.Stroke.Color := FDefaultColor;
end;

procedure TfraMarvinProgressCount.SetIsRoundLines(const Value: Boolean);
var
  LCap: TStrokeCap;
begin
  FIsRoundLines := Value;
  LCap := TStrokeCap.Flat;
  if FIsRoundLines then
  begin
    LCap := TStrokeCap.Round;
  end;
  { informa se o arco é arredondado ou não }
  arcValue1.Stroke.Cap := LCap;
  arcValue2.Stroke.Cap := LCap;
  arcValue3.Stroke.Cap := LCap;
  arcValue4.Stroke.Cap := LCap;
  arcProgress.Stroke.Cap := LCap;
end;

procedure TfraMarvinProgressCount.SetMax(const Value: Single);
begin
  FMax := Value;
end;

procedure TfraMarvinProgressCount.SetMin(const Value: Single);
begin
  FMin := Value;
end;

procedure TfraMarvinProgressCount.SetOnUpdateStatus(const Value: TOnUpdateStatus);
begin
  FOnUpdateStatus := Value;
end;

procedure TfraMarvinProgressCount.SetProgress(const Value: Single);
begin
  { para valores válidos }
  if ((FProgress >= FMin) and (FProgress <= FMax)) then
  begin
    { recupera o progresso }
    FProgress := Value;
    { ajusta os controles }
    Self.UpdateProgressStatus(Value);
  end;
end;

procedure TfraMarvinProgressCount.SetText(const Value: string);
begin
  { recupera o texto da mensagem final }
  lblMessage.Text := Value;
end;

procedure TfraMarvinProgressCount.SetTotalDuration(const Value: Double);
begin
  FTotalDuration := Value;
  { ajusta o tempo das animações }
  FloatAnimationValue1.Duration := FTotalDuration / 4;
  FloatAnimationValue2.Duration := FTotalDuration / 4;
  FloatAnimationValue3.Duration := FTotalDuration / 4;
  FloatAnimationValue4.Duration := FTotalDuration / 4;
end;

procedure TfraMarvinProgressCount.SetTotalHeight(const Value: Single);
begin
  FTotalHeight := Value;
  Self.InternalConfigureTotalSizes;
end;

procedure TfraMarvinProgressCount.SetTotalWigth(const Value: Single);
begin
  FTotalWidth := Value;
  Self.InternalConfigureTotalSizes;
end;

procedure TfraMarvinProgressCount.SetUseRandomColors(const Value: Boolean);
begin
  FUseRandomColors := Value;
end;

procedure TfraMarvinProgressCount.UpdateValue;
begin
  { faz o updade das animações }
  sdwArc1.UpdateParentEffects;
  sdwProgress.UpdateParentEffects;
  { se existe algum evento associado para recuperar atualização de valores }
  if Assigned(FOnUpdateStatus) then
  begin
    { chama o evento e passa o progress count }
    FOnUpdateStatus(Self);
  end;
  { atualiza os controles }
  Self.UpdateProgressStatus(FProgress);
end;

initialization
  RegisterClass(TfraMarvinProgressCount);

finalization
  RegisterClass(TfraMarvinProgressCount);


end.


