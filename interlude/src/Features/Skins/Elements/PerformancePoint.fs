namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play
open Prelude.Calculator

type PerformancePoint(ctx: HudContext) = 
    inherit StaticWidget(NodeType.None)

    let UPDATE_INTERVAL = 2000.0
    let mutable update_timer = UPDATE_INTERVAL

    let mutable current_pr = 0.0f
    let chart_data = ctx.State.WithColors.ToNoteData()
    let chart_diff = Difficulty.calculate(ctx.State.Scoring.Rate, chart_data)

    override this.Init(parent: Widget) =
        base.Init(parent)

    override this.Draw() =
        let point_ratio = ctx.State.Scoring.PointsScored / ctx.State.Scoring.MaxPossiblePoints * float chart_diff.Overall
        Text.fill_b(
            Style.font,
            (if current_pr = 0.0f then "-.--" else sprintf "%.2f" current_pr),
            this.Bounds,
            (Colors.white, Difficulty.color current_pr),
            Alignment.RIGHT
        )

    override this.Update (elapsed_ms: float, moved: bool): unit = 
        base.Update(elapsed_ms, moved)
        update_timer <- update_timer - elapsed_ms
        if update_timer <= 0.0 then
            current_pr <- Performance.calculate chart_diff ctx.State.Scoring
            update_timer <- UPDATE_INTERVAL

