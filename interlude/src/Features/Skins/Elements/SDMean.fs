namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play
open Interlude.Features.Gameplay

type SDMean(ctx: HudContext) = 
    inherit StaticWidget(NodeType.None)

    let mutable cumul_sum = 0.0f
    let mutable cumul_square_sum = 0.0f
    let mutable mean = 0.0f
    let mutable sd = 0.0f
    let mutable note_count = 0

    let calc_mean_sd() =
        mean <- cumul_sum / (float32) note_count
        sd <- sqrt((cumul_square_sum / (max 1.0f (float32 note_count))) - (mean * mean))
    
    let clear_notes() =
        cumul_sum <- 0.0f
        cumul_square_sum <- 0.0f
        mean <- 0.0f
        sd <- 0.0f
        note_count <- 0

    override this.Init(parent: Widget) =
        ctx.State.OnScoringChanged(fun () -> clear_notes()) |> ignore // clear when seeked

        ctx.State.Subscribe(fun ev ->
            let x =
                match ev.Inner with
                | Hit e when not e.Missed -> match e.Judgement with Some (j, _) -> ValueSome (j, e.Delta) | None -> ValueNone
                | Hold e when not e.Missed -> match e.Judgement with Some (j, _) -> ValueSome (j, e.Delta) | None -> ValueNone
                | Release e when not e.Missed -> match e.Judgement with Some (j, _) -> ValueSome (j, e.Delta) | None -> ValueNone
                | _ -> ValueNone

            match x with
            | ValueSome (judge, delta) ->
                cumul_sum <- cumul_sum + (float32) delta
                cumul_square_sum <- cumul_square_sum + (float32 delta * float32 delta)
                note_count <- note_count + 1
                calc_mean_sd()
            | _ -> ()
        )
        |> ignore
        base.Init(parent)
    
    override this.Draw() =
        let sd_bounds = this.Bounds.SlicePercentT 0.5f
        let mean_bounds = this.Bounds.SlicePercentB 0.5f

        Text.fill(
            Style.font,
            "SD:", 
            sd_bounds,
            Color.White,
            Alignment.LEFT
        )
        Text.fill(
            Style.font,
            sprintf "%.2fms" sd,
            sd_bounds,
            Color.White,
            Alignment.RIGHT
        )

        Text.fill(
            Style.font,
            "M:", 
            mean_bounds,
            Color.White,
            Alignment.LEFT
        )
        Text.fill(
            Style.font,
            sprintf "%.2fms" mean, 
            mean_bounds,
            Color.White,
            Alignment.RIGHT
        )