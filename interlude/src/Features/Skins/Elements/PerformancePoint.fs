namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Skins.HudLayouts
open Interlude.Features.Play
open Prelude.Calculator
open Prelude.Gameplay.Rulesets
open System.Drawing

type PerformancePoint(ctx: HudContext) = 
    inherit StaticWidget(NodeType.None)

    let mutable calc_flag = false
    //let mutable time_taken = []
    //let mutable time_taken_avg = 0.0f
    let mutable prev_pr = 0.0f
    let mutable current_pr = 0.0f

    let mutable async_run = true

    let chart_data = ctx.State.WithColors.ToNoteData()
    let chart_too_long = chart_data.Notes.Length >= 30000
    let chart_diff = Difficulty.calculate(ctx.State.Scoring.Rate, chart_data)

    let perf_async = async {
        while async_run do
            if calc_flag then 
                calc_flag <- false
                let stopwatch = Diagnostics.Stopwatch.StartNew()
                let sc_j4_processor: Prelude.Gameplay.Scoring.ScoreProcessor =
                    let standard_ruleset = ctx.State.Scoring.RecreatePartial(SC_J4)
                    standard_ruleset.Update(ctx.State.CurrentChartTime())
                    standard_ruleset
                let new_pr = Performance.calculate chart_diff sc_j4_processor
                stopwatch.Stop()
                prev_pr <- current_pr
                current_pr <- new_pr
                //time_taken <- List.truncate 5 (float32 stopwatch.ElapsedMilliseconds :: time_taken)
                //time_taken_avg <- List.average time_taken
            do! Async.Sleep 5
        }

    let update_interval = ctx.Config.PerformancePointUpdateInterval * 1000.0f
    // let mutable use_temp_interval = false
    let mutable update_timer = update_interval
    let lerp = ctx.Config.PerformancePointLerp

    let prefix =
        match ctx.Config.PerformancePointTextType with
            | PerformancePointText.Prefix -> "PR: "
            | _ -> ""

    let suffix = 
        match ctx.Config.PerformancePointTextType with
            | PerformancePointText.Suffix -> "PR"
            | PerformancePointText.SuffixSpace -> " PR"
            | _ -> ""
    override this.Init(parent: Widget) =
        base.Init(parent)
        if not chart_too_long then Async.Start perf_async

    override this.Draw() =
        let no_data = current_pr <= 0.1f
        Text.fill_b(
            Style.font,
            (
                if chart_too_long then 
                    sprintf "%s-.--%s" prefix suffix
                elif no_data then
                    sprintf "%s0.00%s" prefix suffix
                else
                    sprintf "%s%.2f%s" prefix
                        (if lerp then 
                            (Combinators.lerp ((update_interval - update_timer) / update_interval) (prev_pr) (current_pr)) 
                            else current_pr)
                        suffix),
            this.Bounds,
            (
                if chart_too_long then
                    Colors.text_subheading
                else
                    (Colors.white, Difficulty.color current_pr)
            ),
            Alignment.RIGHT
        )

    override this.Update (elapsed_ms: float, moved: bool): unit = 
        base.Update(elapsed_ms, moved)
        update_timer <- update_timer - float32 elapsed_ms
        if update_timer <= 0.0f then
            calc_flag <- true
            update_timer <- update_interval


