unit Marvin.Comps.MDL.Intf.Button;

interface

uses
  { embarcadero }
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  { firemonkey }
  FMX.Types,
  FMX.Graphics;

type
  { tipos de configurações dos botões }
  TMRVButtonMDLType = (btRaised, btFab, btMiniFab, btIcon);
  { estilos }
  TMRVButtonMDLStyle = (bsFlat, bsPlain);
  { cores }
  TMRVColorMDLType = (clNone, clColored, clAccent);
  { estilo do background }
  TMRVBackgroundStyle = (bsDark, bsClean);

  IFont = interface
    ['{E22F7B3D-BF3B-48A6-AFD3-B1F07140CE46}']
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
    { propriedades da font }
    property Font: TFont read GetFont write SetFont;
    property FontColor: TAlphaColor read GetFontColor write SetFontColor;
    property VertTextAlign: TTextAlign read GetVertTextAlign write SetVertTextAlign;
    property TextAlign: TTextAlign read GetTextAlign write SetTextAlign;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
    property Trimming: TTextTrimming read GetTrimming write SetTrimming;
    property PrefixStyle: TPrefixStyle read GetPrefixStyle write SetPrefixStyle;
  end;

  IMRVButtonMDL = interface
    ['{39D5393D-1187-4283-8854-48E77C831E86}']
    procedure SetParentControl(const AParent: TFmxObject);
    function GetRipple: Boolean;
    procedure SetRipple(const Value: Boolean);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetPrimaryColor: TAlphaColor;
    procedure SetPrimaryColor(const Value: TAlphaColor);
    function GetAccentColor: TAlphaColor;
    procedure SetAccentColor(const Value: TAlphaColor);
    function GetXRadius: Single;
    function GetYRadius: Single;
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    function GetButtonColor: TMRVColorMDLType;
    procedure SetButtonColor(const Value: TMRVColorMDLType);
    function GetBackgroundStyle: TMRVBackgroundStyle;
    procedure SetBackgroundStyle(const Value: TMRVBackgroundStyle);
    function GetOnClickEvents: TList<TNotifyEvent>;
    procedure SetOnClickEvents(const Value: TList<TNotifyEvent>);
    function AddOnClickEvent(AEvent: TNotifyEvent): Boolean;
    { propriedades }
    property Ripple: Boolean read GetRipple write SetRipple;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property PrimaryColor: TAlphaColor read GetPrimaryColor write SetPrimaryColor;
    property AccentColor: TAlphaColor read GetAccentColor write SetAccentColor;
    property ButtonColor: TMRVColorMDLType read GetButtonColor write SetButtonColor;
    property BackgroundStyle: TMRVBackgroundStyle read GetBackgroundStyle write SetBackgroundStyle;
    property XRadius: Single read GetXRadius write SetXRadius;
    property YRadius: Single read GetYRadius write SetYRadius;
    { eventos }
    property OnClickEvents: TList<TNotifyEvent>read GetOnClickEvents write
      SetOnClickEvents;
  end;

implementation

end.


