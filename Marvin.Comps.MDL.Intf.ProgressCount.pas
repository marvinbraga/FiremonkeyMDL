unit Marvin.Comps.MDL.Intf.ProgressCount;

interface

uses
  System.Classes,
  FMX.Types,
  System.UITypes;

type
  IMRVProgressCount = interface;
  { tipo que informa o estilo do fundo }
  TMRVBackgrpoundStyle = (bsClear, bsTwilights);
  { tipo do procedimento do evento }
  TOnUpdateStatus = procedure(const AProgressCount: IMRVProgressCount) of object;

  IMRVProgressCount = interface
    ['{C5DE62F0-3AB5-4F5A-A5A9-1C17CC3A1059}']
    procedure Play;
    procedure Stop;
    procedure SetParentControl(const AParent: TFmxObject);
    procedure ExecuteEndAnimation;
    { métodos das propriedades }
    function GetArcSize: Single;
    function GetOnUpdateStatus: TOnUpdateStatus;
    function GetProgress: Single;
    function GetTotalDuration: Double;
    function GetTotalHeight: Single;
    function GetTotalWidth: Single;
    function GetUseRandomColors: Boolean;
    procedure SetOnUpdateStatus(const Value: TOnUpdateStatus);
    procedure SetUseRandomColors(const Value: Boolean);
    procedure SetTotalDuration(const Value: Double);
    procedure SetProgress(const Value: Single);
    procedure SetTotalHeight(const Value: Single);
    procedure SetTotalWigth(const Value: Single);
    procedure SetArcSize(const Value: Single);
    function GetDefaultColor: TAlphaColor;
    procedure SetDefaultColor(const Value: TAlphaColor);
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
    { propriedades }
    property DefaultColor: TAlphaColor read GetDefaultColor write SetDefaultColor;
    property BackgroundStyle: TMRVBackgrpoundStyle read GetBackgroundStyle write SetBackgroundStyle;
    property TotalDuration: Double read GetTotalDuration write SetTotalDuration;
    property Progress: Single read GetProgress write SetProgress;
    property Max: Single read GetMax write SetMax;
    property Min: Single read GetMin write SetMin;
    property TotalHeight: Single read GetTotalHeight write SetTotalHeight;
    property TotalWidth: Single read GetTotalWidth write SetTotalWigth;
    property ArcSize: Single read GetArcSize write SetArcSize;
    property Text: string read GetText write SetText;
    property UseRandomColors: Boolean read GetUseRandomColors write SetUseRandomColors;
    property IsRoundLines: Boolean read GetIsRoundLines write SetIsRoundLines;
    property OnUpdateStatus: TOnUpdateStatus read GetOnUpdateStatus write SetOnUpdateStatus;
  end;

implementation

end.


