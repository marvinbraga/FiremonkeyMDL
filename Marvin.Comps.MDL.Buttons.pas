{ /------------------------------------------------------------------/
  / Por Marcus Vinicius Braga - Marvinbraga Youtube Channel - Brazil /
  /------------------------------------------------------------------/  }

unit Marvin.Comps.MDL.Buttons;

interface

uses
  { embarcadero }
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  { firemonkey }
  FMX.Controls,
  FMX.Types,
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.Graphics,
  FMX.ActnList,
  { marvin }
  Marvin.Comps.MDL.Frame.Button.Flat,
  Marvin.Comps.MDL.Intf.Button;

type
  { componente do botão }
  TMRVButtonMDL = class sealed(TControl, IMRVButtonMDL, IFont, ITextSettings,
    ICaption)
  strict private
    { botão }
    FButtonMDL: IMRVButtonMDL;
    { tipo e estilo }
    FButtonType: TMRVButtonMDLType;
    FButtonStyle: TMRVButtonMDLStyle;
    { propriedades da interface IMRVButtonMDL}
    FRipple: Boolean;
    FEnabled: Boolean;
    FPrimaryColor: TAlphaColor;
    FAccentColor: TAlphaColor;
    FButtonColor: TMRVColorMDLType;
    FBackgroundStyle: TMRVBackgroundStyle;
    FXRadius: Single;
    FYRadius: Single;
    { propriedades da interface ITextSettings }
    FTextSettings: TTextSettings;
    FStyledSettings: TStyledSettings;
    { prporpiedades da interface ICaption }
    FText: string;
    { propriedades da font }
    FFont: TFont;
    FFontColor: TAlphaColor;
    FVertTextAlign: TTextAlign;
    FTextAlign: TTextAlign;
    FWordWrap: Boolean;
    FTrimming: TTextTrimming;
    FPrefixStyle: TPrefixStyle;
  private
    { métodos da classe }
    procedure InitButton;
    procedure FreeButtons;
    { métodos da interface }
    procedure SetParentControl(const AParent: TFmxObject);
    function GetRipple: Boolean;
    procedure SetRipple(const Value: Boolean);
    function GetPrimaryColor: TAlphaColor;
    procedure SetPrimaryColor(const Value: TAlphaColor);
    function GetAccentColor: TAlphaColor;
    procedure SetAccentColor(const Value: TAlphaColor);
    function GetOnClickEvents: TList<TNotifyEvent>;
    procedure SetOnClickEvents(const Value: TList<TNotifyEvent>);
    function GetXRadius: Single;
    function GetYRadius: Single;
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    function GetButtonColor: TMRVColorMDLType;
    procedure SetButtonColor(const Value: TMRVColorMDLType);
    function GetBackgroundStyle: TMRVBackgroundStyle;
    procedure SetBackgroundStyle(const Value: TMRVBackgroundStyle);
    { componente }
    function GetButtonStyle: TMRVButtonMDLStyle;
    function GetButtonType: TMRVButtonMDLType;
    procedure SetButtonStyle(const Value: TMRVButtonMDLStyle);
    procedure SetButtonType(const Value: TMRVButtonMDLType);
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
    { método acionado quando o controle muda o Enabled }
    procedure EnabledChanged; override;
    procedure AtualizarInformacoesBotao;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddOnClickEvent(AEvent: TNotifyEvent): Boolean;
  published
    { MDL Button }
    property ButtonType: TMRVButtonMDLType read GetButtonType write SetButtonType;
    property ButtonStyle: TMRVButtonMDLStyle read GetButtonStyle write SetButtonStyle;
    { propriedades da interface }
    property Ripple: Boolean read GetRipple write SetRipple;
    property PrimaryColor: TAlphaColor read GetPrimaryColor write SetPrimaryColor;
    property AccentColor: TAlphaColor read GetAccentColor write SetAccentColor;
    property ButtonColor: TMRVColorMDLType read GetButtonColor write SetButtonColor;
    property BackgroundStyle: TMRVBackgroundStyle read GetBackgroundStyle write SetBackgroundStyle;
    property XRadius: Single read GetXRadius write SetXRadius;
    property YRadius: Single read GetYRadius write SetYRadius;
    { múltiplos eventos }
    property OnClickEvents: TList<TNotifyEvent> read GetOnClickEvents write SetOnClickEvents;
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
    { propriedade do TControl }
    property Width;
    property Size;
    property Height;
    property Position;
    property Align;
    property Visible;
    property Enabled;
    property CanFocus;
    property TabOrder;
    property TabStop;
    property Margins;
    property Padding;
  end;

implementation

uses
  FMX.Forms;

{ TMRVButtonMDL }

function TMRVButtonMDL.AddOnClickEvent(AEvent: TNotifyEvent): Boolean;
begin
  Result := FButtonMDL.AddOnClickEvent(AEvent);
end;

procedure TMRVButtonMDL.AtualizarInformacoesBotao;
begin
  Self.Enabled := True;
  Self.EnabledChanged;
  FButtonMDL.SetParentControl(Self);
  (FButtonMDL as IAlignableObject).Align := TAlignLayout.Client;
  { propriedades da interface IMRVButtonMDL}
  FButtonMDL.Ripple := FRipple;
  FButtonMDL.Enabled := FEnabled;
  FButtonMDL.PrimaryColor := FPrimaryColor;
  FButtonMDL.AccentColor := FAccentColor;
  FButtonMDL.ButtonColor := FButtonColor;
  FButtonMDL.BackgroundStyle := FBackgroundStyle;
  FButtonMDL.XRadius := FXRadius;
  FButtonMDL.YRadius := FYRadius;
  { propriedades da interface ITextSettings }
  (FButtonMDL as ITextSettings).TextSettings := FTextSettings;
  (FButtonMDL as ITextSettings).StyledSettings := FStyledSettings;
  { prporpiedades da interface ICaption }
  (FButtonMDL as ICaption).Text := FText;
  { propriedades da font }
  if Assigned(FFont) then
  begin
    (FButtonMDL as IFont).Font.Assign(FFont);
  end;
  (FButtonMDL as IFont).Font.Style := (FButtonMDL as IFont).Font.Style +
    [TFontStyle.fsBold];
  (FButtonMDL as IFont).FontColor := FFontColor;
  (FButtonMDL as IFont).VertTextAlign := FVertTextAlign;
  (FButtonMDL as IFont).TextAlign := FTextAlign;
  (FButtonMDL as IFont).WordWrap := FWordWrap;
  (FButtonMDL as IFont).Trimming := FTrimming;
  (FButtonMDL as IFont).PrefixStyle := FPrefixStyle;
end;

constructor TMRVButtonMDL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Size.Width := 90;
  Self.Size.Height := 30;
  Self.Align := TAlignLayout.None;
  Self.CanFocus := True;
  Self.HitTest := True;
  Self.Enabled := True;
  { inicializa o tipo e o estilo }
  FButtonType := btRaised;
  FButtonStyle := bsFlat;
  FButtonColor := clNone;
  { inicializa o botão de acordo com as indicações }
  Self.InitButton;
  { informa o parent }
  FButtonMDL.SetParentControl(Self);
  { background }
  FButtonMDL.BackgroundStyle := bsDark;
  (FButtonMDL as IFont).Font.Style := [TFontStyle.fsBold];
  { informa que o objeto trabalha com alinhamento }
  (FButtonMDL as IAlignableObject).Align := TAlignLayout.Client;
end;

destructor TMRVButtonMDL.Destroy;
begin
  { libera os botões }
  Self.FreeButtons;
  { finaliza wrapper }
  inherited;
end;

procedure TMRVButtonMDL.EnabledChanged;
begin
  inherited;
  FEnabled := Self.Enabled;
  { executa o EnabledChange do objeto instaciado }
  FButtonMDL.Enabled := FEnabled;
end;

procedure TMRVButtonMDL.FreeButtons;
begin
  { aterra }
  if Assigned(FButtonMDL) then
  begin
    TComponent(FButtonMDL).DisposeOf;
  end;
  FButtonMDL := nil;
end;

procedure TMRVButtonMDL.InitButton;
begin
  { recupera um objeto que implementa a interface }
  if Assigned(FButtonMDL) then
  begin
    Self.FreeButtons;
  end;
  { chamada de acordo com o tipo }
  case FButtonType of
    btRaised:
     { de acordo com o estilo }
      case FButtonStyle of
        { flat }
        bsFlat: FButtonMDL := coFactoryRaisedFlatButtonMDL.Create(Self);
        { plain }
        bsPlain: ;
      end;
    btFab:
      ;
    btMiniFab:
      ;
    btIcon:
      ;
  end;
  { enquanto os outros tipos não estão disponíveis }
  if not(Assigned(FButtonMDL)) then
  begin
    FButtonMDL := coFactoryRaisedFlatButtonMDL.Create(Self);
  end;
  { informar os dados }
  if Assigned(FButtonMDL) then
  begin
    { atualiza as informações do botão }
    Self.AtualizarInformacoesBotao;
  end;
end;

function TMRVButtonMDL.GetAccentColor: TAlphaColor;
begin
  Result := FButtonMDL.AccentColor;
end;

function TMRVButtonMDL.GetBackgroundStyle: TMRVBackgroundStyle;
begin
  Result := FBackgroundStyle;
end;

function TMRVButtonMDL.GetButtonColor: TMRVColorMDLType;
begin
  Result := FButtonColor;
end;

function TMRVButtonMDL.GetButtonStyle: TMRVButtonMDLStyle;
begin
  Result := FButtonStyle;
end;

function TMRVButtonMDL.GetButtonType: TMRVButtonMDLType;
begin
  Result := FButtonType;
end;

function TMRVButtonMDL.GetDefaultTextSettings: TTextSettings;
begin
  Result := (FButtonMDL as ITextSettings).DefaultTextSettings;
end;

function TMRVButtonMDL.GetFont: TFont;
begin
  Result := (FButtonMDL as IFont).Font;
end;

function TMRVButtonMDL.GetFontColor: TAlphaColor;
begin
  Result := (FButtonMDL as IFont).FontColor;
end;

function TMRVButtonMDL.GetOnClickEvents: TList<TNotifyEvent>;
begin
  Result := FButtonMDL.OnClickEvents;
end;

function TMRVButtonMDL.GetPrefixStyle: TPrefixStyle;
begin
  Result := (FButtonMDL as IFont).PrefixStyle;
end;

function TMRVButtonMDL.GetPrimaryColor: TAlphaColor;
begin
  Result := FButtonMDL.PrimaryColor;
end;

function TMRVButtonMDL.GetResultingTextSettings: TTextSettings;
begin
  Result := (FButtonMDL as ITextSettings).ResultingTextSettings;
end;

function TMRVButtonMDL.GetRipple: Boolean;
begin
  Result := FButtonMDL.Ripple;
end;

function TMRVButtonMDL.GetStyledSettings: TStyledSettings;
begin
  Result := (FButtonMDL as ITextSettings).StyledSettings;
end;

function TMRVButtonMDL.GetText: string;
begin
  Result := (FButtonMDL as ICaption).Text;
end;

function TMRVButtonMDL.GetTextAlign: TTextAlign;
begin
  Result := (FButtonMDL as IFont).TextAlign;
end;

function TMRVButtonMDL.GetTextSettings: TTextSettings;
begin
  Result := (FButtonMDL as ITextSettings).TextSettings;
end;

function TMRVButtonMDL.GetTrimming: TTextTrimming;
begin
  Result := (FButtonMDL as IFont).Trimming;
end;

function TMRVButtonMDL.GetVertTextAlign: TTextAlign;
begin
  Result := (FButtonMDL as IFont).VertTextAlign;
end;

function TMRVButtonMDL.GetWordWrap: Boolean;
begin
  Result := (FButtonMDL as IFont).WordWrap;
end;

function TMRVButtonMDL.GetXRadius: Single;
begin
  Result := FButtonMDL.XRadius;
end;

function TMRVButtonMDL.GetYRadius: Single;
begin
  Result := FButtonMDL.YRadius;
end;

procedure TMRVButtonMDL.SetAccentColor(const Value: TAlphaColor);
begin
  FAccentColor := Value;
  FButtonMDL.AccentColor := FAccentColor;
end;

procedure TMRVButtonMDL.SetBackgroundStyle(const Value: TMRVBackgroundStyle);
begin
  FBackgroundStyle := Value;
  FButtonMDL.BackgroundStyle := FBackgroundStyle;
end;

procedure TMRVButtonMDL.SetButtonColor(const Value: TMRVColorMDLType);
begin
  FButtonColor := Value;
  FButtonMDL.ButtonColor := FButtonColor;
end;

procedure TMRVButtonMDL.SetButtonStyle(const Value: TMRVButtonMDLStyle);
var
  LButtonStyle: TMRVButtonMDLStyle;
begin
  LButtonStyle := FButtonStyle;
  FButtonStyle := Value;
  { é um novo botão }
  if (LButtonStyle <> FButtonStyle) then
  begin
    Self.InitButton;
  end;
end;

procedure TMRVButtonMDL.SetButtonType(const Value: TMRVButtonMDLType);
var
  LButtonType: TMRVButtonMDLType;
begin
  LButtonType := FButtonType;
  FButtonType := Value;
  { é um novo botão }
  if (LButtonType <> FButtonType) then
  begin
    Self.InitButton;
  end;
end;

procedure TMRVButtonMDL.SetFont(const Value: TFont);
begin
  FFont := Value;
  { obriga negrito para o estilo da fonte }
  FFont.Style := FFont.Style + [TFontStyle.fsBold];
  (FButtonMDL as IFont).Font := FFont;
end;

procedure TMRVButtonMDL.SetFontColor(const Value: TAlphaColor);
begin
  FFontColor := Value;
  (FButtonMDL as IFont).FontColor := FFontColor;
end;

procedure TMRVButtonMDL.SetOnClickEvents(const Value: TList<TNotifyEvent>);
begin
  { passa os dados adiante }
  FButtonMDL.OnClickEvents := Value;
end;

procedure TMRVButtonMDL.SetParentControl(const AParent: TFmxObject);
begin
  Self.Parent := AParent;
end;

procedure TMRVButtonMDL.SetPrefixStyle(const Value: TPrefixStyle);
begin
  FPrefixStyle := Value;
  (FButtonMDL as IFont).PrefixStyle := FPrefixStyle;
end;

procedure TMRVButtonMDL.SetPrimaryColor(const Value: TAlphaColor);
begin
  FPrimaryColor := Value;
  FButtonMDL.PrimaryColor := FPrimaryColor;
end;

procedure TMRVButtonMDL.SetRipple(const Value: Boolean);
begin
  FRipple := Value;
  FButtonMDL.Ripple := FRipple;
end;

procedure TMRVButtonMDL.SetStyledSettings(const Value: TStyledSettings);
begin
  FStyledSettings := Value;
  (FButtonMDL as ITextSettings).StyledSettings := FStyledSettings;
end;

procedure TMRVButtonMDL.SetText(const Value: string);
begin
  FText := Value;
  (FButtonMDL as ICaption).Text := FText;
end;

procedure TMRVButtonMDL.SetTextAlign(const Value: TTextAlign);
begin
  FTextAlign := Value;
  (FButtonMDL as IFont).TextAlign := FTextAlign;
end;

procedure TMRVButtonMDL.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings := Value;
  (FButtonMDL as ITextSettings).TextSettings := FTextSettings;
end;

procedure TMRVButtonMDL.SetTrimming(const Value: TTextTrimming);
begin
  FTrimming := Value;
  (FButtonMDL as IFont).Trimming := FTrimming;
end;

procedure TMRVButtonMDL.SetVertTextAlign(const Value: TTextAlign);
begin
  FVertTextAlign := Value;
  (FButtonMDL as IFont).VertTextAlign := FVertTextAlign;
end;

procedure TMRVButtonMDL.SetWordWrap(const Value: Boolean);
begin
  FWordWrap:= Value;
  (FButtonMDL as IFont).WordWrap := FWordWrap;
end;

procedure TMRVButtonMDL.SetXRadius(const Value: Single);
begin
  FXRadius := Value;
  FButtonMDL.XRadius := FXRadius;
end;

procedure TMRVButtonMDL.SetYRadius(const Value: Single);
begin
  FYRadius := Value;
  FButtonMDL.YRadius := FYRadius;
end;

function TMRVButtonMDL.TextStored: Boolean;
begin
  Result := (FButtonMDL as ICaption).TextStored;
end;

end.


