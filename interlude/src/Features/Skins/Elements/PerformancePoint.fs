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
open Interlude.Features.Gameplay
open Prelude.Calculator

type PerformancePoint(ctx: HudContext) = 
    inherit StaticWidget(NodeType.None)

    let mutable current_pr = 0.0f

    let chart_data = ctx.State.WithColors.ToNoteData()
    let refresh_interval = max (int((float32 (chart_data.Notes.GetLength 0)) / 200.0f)) 2
    let mutable calc_timer = refresh_interval
    let chart_diff = Difficulty.calculate(ctx.State.Scoring.Rate, chart_data)

    override this.Init(parent: Widget) =
        ctx.State.Subscribe(fun (_) ->
            calc_timer <- calc_timer - 1
            if calc_timer <= 0 then
                current_pr <- Prelude.Calculator.Performance.calculate chart_diff ctx.State.Scoring
                calc_timer <- refresh_interval
        )
        |> ignore
        base.Init(parent)
    
    override this.Draw() =
        Text.fill_b(
            Style.font,
            sprintf "PR: %.2f" current_pr,
            this.Bounds,
            (Colors.white, Difficulty.color current_pr),
            Alignment.RIGHT
        )