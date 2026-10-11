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

    let mutable current_pr = 0.0f

    let chart_data = ctx.State.WithColors.ToNoteData()
    let chart_diff = Difficulty.calculate(ctx.State.Scoring.Rate, chart_data)

    let perfAgent = MailboxProcessor.Start(fun inbox ->
        let rec loop () = async {
            let! processor = inbox.Receive()
            let new_pr = Performance.calculate_partial chart_diff processor (ctx.State.CurrentChartTime())
            current_pr <- new_pr
            return! loop ()
        }
        loop ()
    )

    let UPDATE_INTERVAL = ctx.Config.PerformancePointUpdateInterval * 1000.0f
    let mutable update_timer = UPDATE_INTERVAL

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
            Style.font,
            (
                if current_pr = 0.0f then 
                    sprintf "%s-.--%s" prefix suffix
                else 
                    sprintf "%s%.2f%s" prefix current_pr suffix),
            this.Bounds,
            (
                if current_pr = 0.0f then
                    Colors.text_subheading
                else
                    (Colors.white, Difficulty.color current_pr)
            ),
            Alignment.RIGHT
        )

    override this.Update (elapsed_ms: float, moved: bool): unit = 
        base.Update(elapsed_ms, moved)
        update_timer <- update_timer - float32 elapsed_ms
        if update_timer <= 0.0f && not ctx.Inner.IsMultiplayer && not ctx.Inner.IsSpectate then
            perfAgent.Post ctx.State.Scoring
            update_timer <- UPDATE_INTERVAL

