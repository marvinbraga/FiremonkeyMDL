unit Marvin.Comps.MDL.ProgressCount;

interface

uses
  Marvin.Comps.MDL.Intf.ProgressCount,
  Marvin.Comps.MDL.Frame.ProgressCount,
  FMX.Layouts,
  FMX.Controls,
  FMX.Types,
  System.Classes,
  System.UITypes;

type
  TMRVProgressCount = class(TLayout, IMRVProgressCount)
  private
    FProgressCount: IMRVProgressCount;
    function GetMax: Single;
    function GetMin: Single;
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
  protected
    procedure SetParentControl(const AParent: TFmxObject);
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
    function GetIsRoundLines: Boolean;
    procedure SetIsRoundLines(const Value: Boolean);
    function GetBackgroundStyle: TMRVBackgrpoundStyle;
    procedure SetBackgroundStyle(const Value: TMRVBackgrpoundStyle);
  protected
    procedure ExecuteEndAnimation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play;
    procedure Stop;
  //published
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

constructor TMRVProgressCount.Create(AOwner: TComponent);
begin
  inherited;
  Self.Width := 200;
  Self.Height := 200;
  { só cria componentes se não estiver em tempo de design }
  if not (csDesigning in ComponentState) then
  begin
    { recupera um objeto que implementa a interface }
    FProgressCount := coMRVProgressCount.Create(Self);
    FProgressCount.SetParentControl(Self);
  end;
  { informa que o objeto trabalha com alinhamento }
  (FProgressCount as IAlignableObject).Align := TAlignLayout.Client;
end;

destructor TMRVProgressCount.Destroy;
begin
  { aterra }
  FProgressCount := nil;
  { finaliza wrapper }
  inherited;
end;

procedure TMRVProgressCount.ExecuteEndAnimation;
begin
  FProgressCount.ExecuteEndAnimation;
end;

function TMRVProgressCount.GetArcSize: Single;
begin
  Result := FProgressCount.ArcSize;
end;

function TMRVProgressCount.GetBackgroundStyle: TMRVBackgrpoundStyle;
begin
  Result := FProgressCount.BackgroundStyle;
end;

function TMRVProgressCount.GetDefaultColor: TAlphaColor;
begin
  Result := FProgressCount.DefaultColor;
end;

function TMRVProgressCount.GetIsRoundLines: Boolean;
begin
  Result := FProgressCount.IsRoundLines;
end;

function TMRVProgressCount.GetMax: Single;
begin
  Result := FProgressCount.Max;
end;

function TMRVProgressCount.GetMin: Single;
begin
  Result := FProgressCount.Min;
end;

function TMRVProgressCount.GetOnUpdateStatus: TOnUpdateStatus;
begin
  Result := FProgressCount.OnUpdateStatus;
end;

procedure TMRVProgressCount.SetParentControl(const AParent: TFmxObject);
begin
  Self.Parent := AParent;
end;

function TMRVProgressCount.GetProgress: Single;
begin
  Result := FProgressCount.Progress;
end;

function TMRVProgressCount.GetText: string;
begin
  Result := FProgressCount.Text;
end;

function TMRVProgressCount.GetTotalDuration: Double;
begin
  Result := FProgressCount.TotalDuration;
end;

function TMRVProgressCount.GetTotalHeight: Single;
begin
  Result := FProgressCount.TotalHeight;
end;

function TMRVProgressCount.GetTotalWidth: Single;
begin
  Result := FProgressCount.TotalWidth;
end;

function TMRVProgressCount.GetUseRandomColors: Boolean;
begin
  Result := FProgressCount.UseRandomColors;
end;

procedure TMRVProgressCount.Play;
begin
  FProgressCount.Play;
end;

procedure TMRVProgressCount.SetArcSize(const Value: Single);
begin
  FProgressCount.ArcSize := Value;
end;

procedure TMRVProgressCount.SetBackgroundStyle(
  const Value: TMRVBackgrpoundStyle);
begin
  FProgressCount.BackgroundStyle := Value;
end;

procedure TMRVProgressCount.SetDefaultColor(const Value: TAlphaColor);
begin
  FProgressCount.DefaultColor := Value;
end;

procedure TMRVProgressCount.SetIsRoundLines(const Value: Boolean);
begin
  FProgressCount.IsRoundLines := Value;
end;

procedure TMRVProgressCount.SetMax(const Value: Single);
begin
  FProgressCount.Max := Value;
end;

procedure TMRVProgressCount.SetMin(const Value: Single);
begin
  FProgressCount.Min := Value;
end;

procedure TMRVProgressCount.SetOnUpdateStatus(const Value: TOnUpdateStatus);
begin
  FProgressCount.OnUpdateStatus := Value;
end;

procedure TMRVProgressCount.SetProgress(const Value: Single);
begin
  FProgressCount.Progress := Value;
end;

procedure TMRVProgressCount.SetText(const Value: string);
begin
  FProgressCount.Text := Value;
end;

procedure TMRVProgressCount.SetTotalDuration(const Value: Double);
begin
  FProgressCount.TotalDuration := Value;
end;

procedure TMRVProgressCount.SetTotalHeight(const Value: Single);
begin
  FProgressCount.TotalHeight := Value;
end;

procedure TMRVProgressCount.SetTotalWigth(const Value: Single);
begin
  FProgressCount.TotalWidth := Value;
end;

procedure TMRVProgressCount.SetUseRandomColors(const Value: Boolean);
begin
  FProgressCount.UseRandomColors := Value;
end;

procedure TMRVProgressCount.Stop;
begin
  FProgressCount.Stop;
end;

end.


