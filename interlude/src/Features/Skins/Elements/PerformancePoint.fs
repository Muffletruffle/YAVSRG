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
open Prelude.Gameplay.Scoring
open Prelude.Charts

type PerformanceProcessor(rr: Difficulty, rate: Prelude.Rate, keys: int, note_data: NoteData) =
    let running_strain = Strain.init_incremental keys
    let running_diff = Difficulty.init_incremental (note_data.Notes.Length)


    let mutable acc_v = 1.0
    let mutable current_pr = 0.0f

    let mutable pending_index = -1
    let pending_notes = ResizeArray<int * Time>()
    
    member this.CurrentPr = current_pr

    member private this.FlushPending() =
        for (col, offset) in pending_notes do
            let note = rr.NoteDifficulty.[pending_index].[col]
            let scaled = Performance.scale_note (float32 acc_v) rr.Variety.[pending_index] note
            let strain = Strain.insert_note running_strain rate col offset scaled.Total
            Difficulty.insert_note running_diff strain
        pending_notes.Clear()
        current_pr <- Difficulty.weighted_overall_difficulty_increment running_diff

    /// Call once when the replay finishes, so the very last row isn't left unscored
    member this.Flush() = this.FlushPending()

    member this.OnEvent (ev: ScoringEvent) =
        let apply_judgement (judgement: (int * float) option) =
            match judgement with
            | Some (_, value) ->
                acc_v <- Performance.ACC_SENSITIVITY * acc_v + (1.0 - Performance.ACC_SENSITIVITY) * value |> max 0.85
            | None -> ()

        let handle_note_event (judgement, col, time) =
            if ev.Index <> pending_index then
                this.FlushPending()
                pending_index <- ev.Index
            apply_judgement judgement
            pending_notes.Add((col, time))

        match ev.Inner with
        | Hit h -> handle_note_event(h.Judgement, ev.Column, ev.Time)
        | Hold h -> handle_note_event(h.Judgement, ev.Column, ev.Time)
        | Release r -> apply_judgement r.Judgement
        | GhostTap g -> apply_judgement g.Judgement
        | DropHold | RegrabHold -> ()

    // member this.CurrentPr = current_pr

    // member this.insert_note(ind: int, col: int, offset: Time) =
    //     let note = rr.NoteDifficulty.[ind].[col]    // Get precalcuated difficulty of this specific note
    //     let scaled = Performance.scale_note (float32 acc_v) rr.Variety.[ind] note
    //     let strain = Strain.insert_note running_strain rate col offset scaled.Total
    //     Difficulty.insert_note running_diff strain
    //     current_pr <- Difficulty.weighted_overall_difficulty_increment running_diff
    
    // member this.OnEvent (ev: ScoringEvent) =
    //     // match ev.Inner.Judgement with
    //     // | Some (_, value) ->
    //     //     acc_v <- Performance.ACC_SENSITIVITY * acc_v + (1.0 - Performance.ACC_SENSITIVITY) * value |> max 0.85
    //     // | None -> ()
    //     // this.insert_note(ev.Index, ev.Column, ev.Time)

    //     let apply_judgement judgement =
    //         match judgement with
    //         | Some (_, value) ->
    //             acc_v <- Performance.ACC_SENSITIVITY * acc_v + (1.0 - Performance.ACC_SENSITIVITY) * value |> max 0.85
    //         | None -> ()
        
    //     match ev.Inner with
    //     | Hit h -> apply_judgement h.Judgement; this.insert_note(ev.Index, ev.Column, ev.Time)
    //     | Hold h -> apply_judgement h.Judgement; this.insert_note(ev.Index, ev.Column, ev.Time)
    //     | Release r -> apply_judgement r.Judgement
    //     | GhostTap g -> apply_judgement g.Judgement
    //     | DropHold | RegrabHold -> ()


type PerformancePoint(ctx: HudContext) = 
    inherit StaticWidget(NodeType.None)

    let chart_data = ctx.State.WithColors.ToNoteData()
    let chart_diff = Difficulty.calculate(ctx.State.Scoring.Rate, chart_data)

    let mutable current_pr = 0.0f
    let mutable perf_processor = PerformanceProcessor(chart_diff, ctx.State.Rate, chart_data.Keys, chart_data.ToNoteData())

    // If the ruleset isn't scj4, make a second one that perf_processor can use
    let mutable scj4_processor : ScoreProcessor option = None
    let mutable subscription : IDisposable = null

    let update_interval = ctx.Config.PerformancePointUpdateInterval * 1000.0f
    let mutable update_timer = update_interval
    let lerp = ctx.Config.PerformancePointLerp

    let attach () = // create scj4 processor if not on scj4 already, otherwise just use the original processor. Then, link it to perf_processor
        if subscription <> null then subscription.Dispose()
        scj4_processor <-
            if SC_J4_HASH <> Ruleset.hash ctx.State.Scoring.Ruleset then
                Some (ctx.State.Scoring.RecreatePartial(SC_J4)) // Have to recreate ruleset if original isn't scj4
            else None
        subscription <-
            match scj4_processor with
            | Some sp -> sp.OnEvent.Subscribe perf_processor.OnEvent
            | None -> ctx.State.Subscribe perf_processor.OnEvent

    let reset () =  // Reset if replay rewound or the state otherwise changes
        perf_processor <- PerformanceProcessor(chart_diff, ctx.State.Rate, chart_data.Keys, chart_data.ToNoteData())
        current_pr <- 0.0f
        attach ()

    let scoring_changed_sub = ctx.State.OnScoringChanged(reset)

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
        attach ()

    override this.Update (elapsed_ms: float, moved: bool): unit = 
        base.Update(elapsed_ms, moved)
        if scj4_processor.IsSome then
            scj4_processor.Value.Update(ctx.State.CurrentChartTime())
        current_pr <- perf_processor.CurrentPr

    override this.Draw() =
        let no_data = current_pr <= 0.1f
        Text.fill_b(
            Style.font,
            (
                if no_data then 
                    sprintf "%s-.--%s" prefix suffix
                else
                    sprintf "%s%.2f%s" prefix
                        current_pr
                        // (if lerp then 
                        //     (Combinators.lerp ((update_interval - update_timer) / update_interval) (prev_pr) (current_pr)) 
                        //     else current_pr)
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
        // Text.fill_b(
        //     Style.font,
        //     (
        //         if no_data then 
        //             sprintf "%s-.--%s" prefix suffix
        //         else
        //             sprintf "%s%.2f%s" prefix
        //                 (if lerp then 
        //                     (Combinators.lerp ((update_interval - update_timer) / update_interval) (prev_pr) (current_pr)) 
        //                     else current_pr)
        //                 suffix),
        //     this.Bounds,
        //     (
        //         if no_data then
        //             Colors.text_subheading
        //         else
        //             (Colors.white, Difficulty.color current_pr)
        //     ),
        //     Alignment.RIGHT
        // )


