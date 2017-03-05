unit Marvin.Comps.MDL.Frame.Button.Flat;

interface

uses
  { marvin }
  Marvin.Comps.MDL.Intf.Button,
  { embarcadero }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  System.Generics.Defaults,
  { firemonkey }
  FMX.Types,
  FMX.ActnList,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Ani,
  FMX.Effects,
  FMX.Objects,
  FMX.Layouts;

type
  coFactoryRaisedFlatButtonMDL = class
  public
    class function Create(AOwner: TComponent): IMRVButtonMDL;
  end;

  { classe para comparar os valores dos itens da lista }
  TNotifyEventComparer = class(TComparer<TNotifyEvent>)
  public
    function Compare(const Left, Right: TNotifyEvent): Integer; override;
  end;

  { classe do botão MDL raised, flat }
  TfraMRVRaisedFlatButtonMDL = class(TFrame, IMRVButtonMDL, IFont, ITextSettings,
    ICaption)
    retButton: TRectangle;
    sdwButton: TShadowEffect;
    FloatAnimationOpacity: TFloatAnimation;
    FloatAnimationClick: TFloatAnimation;
    lblText: TLabel;
    crcRipple: TCircle;
    FloatAnimationRippleOpacity: TFloatAnimation;
    FloatAnimationRippleSize: TFloatAnimation;
    procedure retButtonClick(Sender: TObject);
    procedure FloatAnimationRippleOpacityFinish(Sender: TObject);
    procedure FloatAnimationRippleSizeProcess(Sender: TObject);
    procedure retButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FloatAnimationOpacityProcess(Sender: TObject);
  strict private
    FOnClickEvents: TList<TNotifyEvent>;
    FRipple: Boolean;
    FPrimaryColor: TAlphaColor;
    FAccentColor: TAlphaColor;
    FButtonColor: TMRVColorMDLType;
  private
    procedure StartRipple(X: Single; Y: Single);
    { métodos da interface }
    function GetRipple: Boolean;
    procedure SetRipple(const Value: Boolean);
    function GetPrimaryColor: TAlphaColor;
    procedure SetPrimaryColor(const Value: TAlphaColor);
    function GetAccentColor: TAlphaColor;
    procedure SetAccentColor(const Value: TAlphaColor);
    function GetButtonColor: TMRVColorMDLType;
    procedure SetButtonColor(const Value: TMRVColorMDLType);
    function GetXRadius: Single;
    function GetYRadius: Single;
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    { multiplos eventos }
    function GetOnClickEvents: TList<TNotifyEvent>;
    procedure SetOnClickEvents(const Value: TList<TNotifyEvent>);
    function AddOnClickEvent(AEvent: TNotifyEvent): Boolean;
    { métodos da interface ICaption }
    function GetText: string;
    procedure SetText(const Value: string);
    function TextStored: Boolean;
    { métodos da fonte }
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetFontColor: TAlphaColor;
    procedure SetFontColor(const Value: TAlphaColor);
    function GetTextAlign: TTextAlign;
    procedure SetTextAlign(const Value: TTextAlign);
    function GetTrimming: TTextTrimming;
    procedure SetTrimming(const Value: TTextTrimming);
    function GetVertTextAlign: TTextAlign;
    procedure SetVertTextAlign(const Value: TTextAlign);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(const Value: Boolean);
    function GetPrefixStyle: TPrefixStyle;
    procedure SetPrefixStyle(const Value: TPrefixStyle);
    { métodos da interface ITextSettings }
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    function GetResultingTextSettings: TTextSettings;
    function GetStyledSettings: TStyledSettings;
    procedure SetStyledSettings(const Value: TStyledSettings);
  protected
    function MaxValue(AValue1: Single; AValue2: Single): Single;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean); override;
    procedure ChangeButtonColor; virtual;
    procedure ExecuteClickEvents(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    { propriedades da interface ITextSettings }
    property DefaultTextSettings: TTextSettings read GetDefaultTextSettings;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property ResultingTextSettings: TTextSettings read GetResultingTextSettings;
    property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings;
    { prporpiedades da interface ICaption }
    property Text: string read GetText write SetText stored TextStored;
    { propriedades da font }
    property Font: TFont read GetFont write SetFont;
    property FontColor: TAlphaColor read GetFontColor write SetFontColor default TAlphaColorRec.Black;
    property VertTextAlign: TTextAlign read GetVertTextAlign write SetVertTextAlign default TTextAlign.Center;
    property TextAlign: TTextAlign read GetTextAlign write SetTextAlign default TTextAlign.Leading;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property Trimming: TTextTrimming read GetTrimming write SetTrimming default TTextTrimming.None;
    property PrefixStyle: TPrefixStyle read GetPrefixStyle write SetPrefixStyle default TPrefixStyle.HidePrefix;
    { métodos da interface IMRVButtonMDL }
    procedure SetParentControl(const AParent: TFmxObject);
    { propriedades da interface IMRVButtonMDL}
    property Ripple: Boolean read GetRipple write SetRipple;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property PrimaryColor: TAlphaColor read GetPrimaryColor write SetPrimaryColor;
    property AccentColor: TAlphaColor read GetAccentColor write SetAccentColor;
    property ButtonColor: TMRVColorMDLType read GetButtonColor write SetButtonColor;
    property XRadius: Single read GetXRadius write SetXRadius;
    property YRadius: Single read GetYRadius write SetYRadius;
    { eventos }
    property OnClickEvents: TList<TNotifyEvent> read GetOnClickEvents write SetOnClickEvents;
  end;

implementation

{$R *.fmx}

function TfraMRVRaisedFlatButtonMDL.AddOnClickEvent(
  AEvent: TNotifyEvent): Boolean;
var
  LAchou, LResult: Boolean;
begin
  LResult := False;
  { verifica se o evento já foi incluído }
  LAchou := FOnClickEvents.Contains(AEvent);
  { se evento não foi incluído então inclui }
  if not(LAchou) then
  begin
    FOnClickEvents.Add(AEvent);
    LResult := True;
  end;
  { informa o resultado }
  Result := LResult;
end;

procedure TfraMRVRaisedFlatButtonMDL.ChangeButtonColor;
begin
  { ajusta a cor do label }
  case FButtonColor of
    { preta }
    clNone: lblText.FontColor := TAlphaColors.Black;
    { cor primária }
    clColored: lblText.FontColor := FPrimaryColor;
    { cor secundária }
    clAccent: lblText.FontColor := FAccentColor;
  end;
end;

procedure TfraMRVRaisedFlatButtonMDL.Click;
begin
  inherited;
  { falta ajustar }
  retButtonClick(Self);
end;

constructor TfraMRVRaisedFlatButtonMDL.Create(AOwner: TComponent);
var
  LComparer: TNotifyEventComparer;
begin
  inherited Create(AOwner);
  { cria multieventos }
  LComparer := TNotifyEventComparer.Create;
  FOnClickEvents := TList<TNotifyEvent>.Create(LComparer);
end;

destructor TfraMRVRaisedFlatButtonMDL.Destroy;
begin
  FOnClickEvents.Clear;
  FreeAndNil(FOnClickEvents);
  inherited;
end;

procedure TfraMRVRaisedFlatButtonMDL.FloatAnimationOpacityProcess(Sender: TObject);
begin
  sdwButton.UpdateParentEffects;
end;

procedure TfraMRVRaisedFlatButtonMDL.FloatAnimationRippleOpacityFinish(
  Sender: TObject);
begin
  crcRipple.Visible := False;
end;

procedure TfraMRVRaisedFlatButtonMDL.FloatAnimationRippleSizeProcess(Sender: TObject);
var
  LTamanho: Single;
begin
  { recupera o ajuste para o novo tamanho }
  LTamanho := crcRipple.Size.Width - crcRipple.Size.Height;
  { ajusta o tamanho }
  crcRipple.Size.Height := crcRipple.Size.Width;
  { ajusta o posicionamento para centralizar com o clique }
  crcRipple.Position.X := crcRipple.Position.X - (LTamanho / 2);
  crcRipple.Position.Y := crcRipple.Position.Y - (LTamanho / 2);
end;

function TfraMRVRaisedFlatButtonMDL.GetResultingTextSettings: TTextSettings;
begin
  Result := lblText.ResultingTextSettings;
end;

function TfraMRVRaisedFlatButtonMDL.GetRipple: Boolean;
begin
  Result := FRipple;
end;

function TfraMRVRaisedFlatButtonMDL.GetAccentColor: TAlphaColor;
begin
  Result := FAccentColor;
end;

function TfraMRVRaisedFlatButtonMDL.GetButtonColor: TMRVColorMDLType;
begin
  Result := FButtonColor;
end;

function TfraMRVRaisedFlatButtonMDL.GetDefaultTextSettings: TTextSettings;
begin
  Result := lblText.DefaultTextSettings;
end;

function TfraMRVRaisedFlatButtonMDL.GetEnabled: Boolean;
begin
  Result := inherited Enabled;
end;

function TfraMRVRaisedFlatButtonMDL.GetFont: TFont;
begin
  Result := lblText.Font;
end;

function TfraMRVRaisedFlatButtonMDL.GetFontColor: TAlphaColor;
begin
  Result := lblText.FontColor;
end;

function TfraMRVRaisedFlatButtonMDL.GetStyledSettings: TStyledSettings;
begin
  Result := lblText.StyledSettings;
end;

function TfraMRVRaisedFlatButtonMDL.GetText: string;
begin
  Result := lblText.Text;
end;

function TfraMRVRaisedFlatButtonMDL.GetTextAlign: TTextAlign;
begin
  Result := lblText.TextAlign;
end;

function TfraMRVRaisedFlatButtonMDL.GetTextSettings: TTextSettings;
begin
  Result := lblText.TextSettings;
end;

function TfraMRVRaisedFlatButtonMDL.GetTrimming: TTextTrimming;
begin
  Result := lblText.Trimming;
end;

function TfraMRVRaisedFlatButtonMDL.GetVertTextAlign: TTextAlign;
begin
  Result := lblText.VertTextAlign;
end;

function TfraMRVRaisedFlatButtonMDL.GetWordWrap: Boolean;
begin
  Result := lblText.WordWrap;
end;

function TfraMRVRaisedFlatButtonMDL.GetXRadius: Single;
begin
  Result := retButton.XRadius;
end;

function TfraMRVRaisedFlatButtonMDL.GetYRadius: Single;
begin
  Result := retButton.YRadius;
end;

function TfraMRVRaisedFlatButtonMDL.GetOnClickEvents: TList<TNotifyEvent>;
begin
  Result := FOnClickEvents;
end;

function TfraMRVRaisedFlatButtonMDL.GetPrefixStyle: TPrefixStyle;
begin
  Result := lblText.PrefixStyle;
end;

function TfraMRVRaisedFlatButtonMDL.GetPrimaryColor: TAlphaColor;
begin
  Result := FPrimaryColor;
end;

function TfraMRVRaisedFlatButtonMDL.MaxValue(AValue1, AValue2: Single): Single;
begin
  { retorna o primeiro valor }
  Result := AValue1;
  { se o segundo valor for maior que o primeiro então retorne-o. }
  if (AValue2 > AValue1) then
  begin
    Result := AValue2;
  end;
end;

procedure TfraMRVRaisedFlatButtonMDL.retButtonClick(Sender: TObject);
begin
  { executa a animação }
  FloatAnimationClick.Start;
  { executa eventos de clique }
  Self.ExecuteClickEvents(Sender);
end;

procedure TfraMRVRaisedFlatButtonMDL.retButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FRipple then
  begin
    Self.StartRipple(X, Y);
  end;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetRipple(const Value: Boolean);
begin
  FRipple := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetStyledSettings(const Value: TStyledSettings);
begin
  lblText.StyledSettings := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetText(const Value: string);
begin
  lblText.Text := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetTextAlign(const Value: TTextAlign);
begin
  lblText.TextAlign := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetTextSettings(const Value: TTextSettings);
begin
  lblText.TextSettings.Assign(Value);
end;

procedure TfraMRVRaisedFlatButtonMDL.SetAccentColor(const Value: TAlphaColor);
begin
  FAccentColor := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetButtonColor(
  const Value: TMRVColorMDLType);
begin
  FButtonColor := Value;
  { altera a cor do controle }
  Self.ChangeButtonColor;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetEnabled(const Value: Boolean);
begin
  inherited;
  lblText.Enabled := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetFont(const Value: TFont);
begin
  lblText.Font := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetFontColor(const Value: TAlphaColor);
begin
  { ajusta a cor da fonte de acordo com o tipo do botão }
  Self.ChangeButtonColor;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetTrimming(const Value: TTextTrimming);
begin
  lblText.Trimming := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetVertTextAlign(const Value: TTextAlign);
begin
  lblText.VertTextAlign := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetWordWrap(const Value: Boolean);
begin
  lblText.WordWrap := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetXRadius(const Value: Single);
begin
  retButton.XRadius := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetYRadius(const Value: Single);
begin
  retButton.YRadius := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetOnClickEvents(
  const Value: TList<TNotifyEvent>);
var
  LOnClickEvent: TNotifyEvent;
begin
  { limpa a lista }
  FOnClickEvents.Clear;
  { se existe um valor }
  if Assigned(Value) then
  begin
    { adiciona todos os itens }
    for LOnClickEvent in Value do
    begin
      { se o item ainda não foi incluído }
      Self.AddOnClickEvent(LOnClickEvent);
    end;
  end;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetParentControl(const AParent: TFmxObject);
begin
  Self.Parent := AParent;
end;

procedure TfraMRVRaisedFlatButtonMDL.ExecuteClickEvents(Sender: TObject);
var
  LNotifyEvent: TNotifyEvent;
begin
  { executa todos os eventos associados }
  if (FOnClickEvents.Count > 0) then
  begin
    { aciona todos os eventos }
    for LNotifyEvent in FOnClickEvents do
    begin
      if Assigned(LNotifyEvent) then
      begin
        TThread.CreateAnonymousThread(procedure
        begin
          TThread.Synchronize(TThread.CurrentThread, procedure
          begin
            { executa thread por evento }
            LNotifyEvent(Self.Parent);
          end)
        end).Start;
      end;
    end;
  end;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetPrefixStyle(const Value: TPrefixStyle);
begin
  lblText.PrefixStyle := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.SetPrimaryColor(const Value: TAlphaColor);
begin
  FPrimaryColor := Value;
end;

procedure TfraMRVRaisedFlatButtonMDL.StartRipple(X: Single; Y: Single);
begin
  if not FRipple then
  begin
    Exit;
  end;
  { inicia a thread }
  TThread.CreateAnonymousThread(procedure
  begin
    { sincroniza }
    TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        { centralizar o círculo com a posição do clique }
        crcRipple.Position.X := X - (crcRipple.Size.Width / 2);
        crcRipple.Position.Y := Y - (crcRipple.Size.Width / 2);
        { exibe o círculo }
        crcRipple.Visible := True;
        { configura o tamanho final do círculo }
        FloatAnimationRippleSize.StopValue := Self.MaxValue(retButton.Size.Width,
          retButton.Size.Width) + 10;
        { sincroniza a duração das animações }
        FloatAnimationRippleSize.Duration := FloatAnimationRippleOpacity.Duration;
        { anicia as animações }
        FloatAnimationRippleOpacity.Start;
        FloatAnimationRippleSize.Start;
      end);
  end).Start;
end;

function TfraMRVRaisedFlatButtonMDL.TextStored: Boolean;
begin
  Result := True;
end;

{ coFactoryFlatButtonMDL }

class function coFactoryRaisedFlatButtonMDL.Create(AOwner: TComponent): IMRVButtonMDL;
begin
  Result := TfraMRVRaisedFlatButtonMDL.Create(AOwner);
end;

{ TNotifyEventComparer }

function TNotifyEventComparer.Compare(const Left, Right: TNotifyEvent): Integer;
var
  LResult: Integer;
begin
  { define resultado como diferentes }
  LResult := -1;
  { verifica se são iguais }
  if ((TMethod(Left).Data = TMethod(Right).Data) and
      (TMethod(Left).Code = TMethod(Right).Code)) then
  begin
    { define resultado como iguais }
    LResult := 0;
  end;
  { retorna o resultado }
  Result := LResult;
end;

initialization
  RegisterClass(TfraMRVRaisedFlatButtonMDL);

finalization
  UnRegisterClass(TfraMRVRaisedFlatButtonMDL);
end.


