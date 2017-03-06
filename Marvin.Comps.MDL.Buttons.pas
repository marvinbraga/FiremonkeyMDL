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
  TMRVButtonMDL = class(TLayout, IMRVButtonMDL, IFont, ITextSettings, ICaption)
  private
    { tipo e estilo }
    FButtonType: TMRVButtonMDLType;
    FButtonStyle: TMRVButtonMDLStyle;
    FButtonColor: TMRVColorMDLType;
    { botão }
    FButtonMDL: IMRVButtonMDL;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddOnClickEvent(AEvent: TNotifyEvent): Boolean;
  //published
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
  end;

implementation

{ TMRVButtonMDL }

function TMRVButtonMDL.AddOnClickEvent(AEvent: TNotifyEvent): Boolean;
begin
  Result := FButtonMDL.AddOnClickEvent(AEvent);
end;

constructor TMRVButtonMDL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.HitTest := False;
  Self.Size.Width := 90;
  Self.Size.Height := 30;
  { inicializa o tipo e o estilo }
  FButtonType := btRaised;
  FButtonStyle := bsFlat;
  FButtonColor := clNone;
  { só cria componentes se não estiver em tempo de design }
  if not (csDesigning in ComponentState) then
  begin
    { inicializa o botão de acordo com as indicações }
    Self.InitButton;
    { informa o parent }
    FButtonMDL.SetParentControl(Self);
    { background }
    FButtonMDL.BackgroundStyle := bsDark;
    { informa que o objeto trabalha com alinhamento }
    (FButtonMDL as IAlignableObject).Align := TAlignLayout.Client;
  end;
  Self.Align := TAlignLayout.None;
end;

destructor TMRVButtonMDL.Destroy;
begin
  Self.FreeButtons;
  { finaliza wrapper }
  inherited;
end;

procedure TMRVButtonMDL.EnabledChanged;
begin
  inherited;
  { executa o EnabledChange do objeto instaciado }
  FButtonMDL.Enabled := Self.Enabled;
end;

procedure TMRVButtonMDL.FreeButtons;
begin
  { aterra }
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
        { flat, plain }
        bsFlat:
        begin
          { de acordo a cor }
          case FButtonColor of
            clNone, clColored, clAccent:
              FButtonMDL := coFactoryRaisedFlatButtonMDL.Create(Self);
          end;
        end;
        bsPlain: ;
      end;
    btFab:
      ;
    btMiniFab:
      ;
    btIcon:
      ;
  else
    FButtonMDL := coFactoryRaisedFlatButtonMDL.Create(Self);
  end;
end;

function TMRVButtonMDL.GetAccentColor: TAlphaColor;
begin
  Result := FButtonMDL.AccentColor;
end;

function TMRVButtonMDL.GetBackgroundStyle: TMRVBackgroundStyle;
begin
  Result := FButtonMDL.BackgroundStyle;
end;

function TMRVButtonMDL.GetButtonColor: TMRVColorMDLType;
begin
  Result := FButtonColor;
  FButtonMDL.ButtonColor := FButtonColor;
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
  FButtonMDL.AccentColor := Value;
end;

procedure TMRVButtonMDL.SetBackgroundStyle(const Value: TMRVBackgroundStyle);
begin
  FButtonMDL.BackgroundStyle := Value;
end;

procedure TMRVButtonMDL.SetButtonColor(const Value: TMRVColorMDLType);
begin
  FButtonColor := Value;
  FButtonMDL.ButtonColor := FButtonColor;
end;

procedure TMRVButtonMDL.SetButtonStyle(const Value: TMRVButtonMDLStyle);
begin
  FButtonStyle := Value;
end;

procedure TMRVButtonMDL.SetButtonType(const Value: TMRVButtonMDLType);
begin
  FButtonType := Value;
end;

procedure TMRVButtonMDL.SetFont(const Value: TFont);
begin
  (FButtonMDL as IFont).Font := Value;
end;

procedure TMRVButtonMDL.SetFontColor(const Value: TAlphaColor);
begin
  (FButtonMDL as IFont).FontColor := Value;
end;

procedure TMRVButtonMDL.SetOnClickEvents(const Value: TList<TNotifyEvent>);
begin
  FButtonMDL.OnClickEvents := Value;
end;

procedure TMRVButtonMDL.SetParentControl(const AParent: TFmxObject);
begin
  Self.Parent := AParent;
end;

procedure TMRVButtonMDL.SetPrefixStyle(const Value: TPrefixStyle);
begin
  (FButtonMDL as IFont).PrefixStyle := Value;
end;

procedure TMRVButtonMDL.SetPrimaryColor(const Value: TAlphaColor);
begin
  FButtonMDL.PrimaryColor := Value;
end;

procedure TMRVButtonMDL.SetRipple(const Value: Boolean);
begin
  FButtonMDL.Ripple := Value;
end;

procedure TMRVButtonMDL.SetStyledSettings(const Value: TStyledSettings);
begin
  (FButtonMDL as ITextSettings).StyledSettings := Value;
end;

procedure TMRVButtonMDL.SetText(const Value: string);
begin
  (FButtonMDL as ICaption).Text := Value;
end;

procedure TMRVButtonMDL.SetTextAlign(const Value: TTextAlign);
begin
  (FButtonMDL as IFont).TextAlign := Value;
end;

procedure TMRVButtonMDL.SetTextSettings(const Value: TTextSettings);
begin
  (FButtonMDL as ITextSettings).TextSettings := Value;
end;

procedure TMRVButtonMDL.SetTrimming(const Value: TTextTrimming);
begin
  (FButtonMDL as IFont).Trimming := Value;
end;

procedure TMRVButtonMDL.SetVertTextAlign(const Value: TTextAlign);
begin
  (FButtonMDL as IFont).VertTextAlign := Value;
end;

procedure TMRVButtonMDL.SetWordWrap(const Value: Boolean);
begin
  (FButtonMDL as IFont).WordWrap := Value;
end;

procedure TMRVButtonMDL.SetXRadius(const Value: Single);
begin
  FButtonMDL.XRadius := Value;
end;

procedure TMRVButtonMDL.SetYRadius(const Value: Single);
begin
  FButtonMDL.YRadius := Value;
end;

function TMRVButtonMDL.TextStored: Boolean;
begin
  Result := (FButtonMDL as ICaption).TextStored;
end;

end.


