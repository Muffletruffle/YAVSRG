namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Skins.HudLayouts
open Interlude.Features.Play
open Prelude.Calculator

type PerformancePoint(ctx: HudContext) = 
    inherit StaticWidget(NodeType.None)

    let mutable prev_pr = 0.0f
    let mutable current_pr = 0.0f

    let chart_data = ctx.State.WithColors.ToNoteData()
    let chart_diff = Difficulty.calculate(ctx.State.Scoring.Rate, chart_data)

    let perfAgent = MailboxProcessor.Start(fun inbox ->
        let rec loop () = async {
            let! processor = inbox.Receive()
            let new_pr = Performance.calculate_partial chart_diff processor (ctx.State.CurrentChartTime())
            prev_pr <- current_pr
            current_pr <- new_pr
            return! loop ()
        }
        loop ()
    )

    let update_interval = ctx.Config.PerformancePointUpdateInterval * 1000.0f
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

    override this.Draw() =
        Text.fill_b(
            let no_data = current_pr = 0.0f || ctx.State.Scoring.Events.Count <= 0
            Style.font,
            (
                if no_data then 
                    sprintf "%s-.--%s" prefix suffix
                else 
                    sprintf "%s%.2f%s" prefix
                        (if lerp then 
                            (Combinators.lerp ((update_interval - update_timer) / update_interval) (prev_pr) (current_pr)) 
                            else current_pr)
                        suffix),
            this.Bounds,
            (
                if no_data then
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
            perfAgent.Post ctx.State.Scoring
            update_timer <- update_interval

