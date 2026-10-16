namespace Prelude.Calculator

open System
open Prelude
open Prelude.Charts

type Difficulty =
    {
        NoteDifficulty: NoteDifficulty array array
        Strains: RowStrain array
        Variety: float32 array
        Hands: RowStrainV2 array
        Overall: float32
    }

module Difficulty =

    let CURVE_POWER = 0.6f
    let CURVE_SCALE = 0.4056f
    /// The top 2500 notes get special weightings
    let MOST_IMPORTANT_NOTES = 2500.0f
    /// Weightings for the values are calculated by this formula
    /// x = % position through the top 2500 from 0.0 - 1.0
    /// Values below the top 2500 always have x = 0.0
    let WEIGHTING_CURVE x = 0.002f + x ** 4.0f

    /// Calculates a single number to represent a set of difficulty data points
    /// This number represents how a player would "perceive" the difficulty overall
    /// Perception is not a concrete term, so I've ruled: In my experience players perception is concentrated on the very hardest parts of a chart
    let weighted_overall_difficulty (data: float32 seq) : float32 =

        let data_array = data |> Seq.sort |> Array.ofSeq
        let length = float32 data_array.Length

        let mutable weight = 0.0f
        let mutable total = 0.0f

        for i = 0 to data_array.Length - 1 do
            let w = WEIGHTING_CURVE ((float32 i + MOST_IMPORTANT_NOTES - length) / MOST_IMPORTANT_NOTES |> max 0.0f)
            weight <- weight + w
            total <- total + data_array.[i] * w

        // Final transform on the weighted average: Power and rescale it to some arbitrary scale people like and are used to
        MathF.Pow(total / weight, CURVE_POWER) * CURVE_SCALE

    let private calculate_uncached (rate: Rate, note_data: NoteData) : Difficulty =
        let note_difficulty = NoteDifficulty.calculate_note_ratings (rate, note_data)
        let variety = Variety.calculate_variety (rate, note_data) note_difficulty
        let physical_data = Strain.calculate_finger_strains (rate, note_data) note_difficulty
        let hands = Strain.calculate_hand_strains (rate, note_data) note_difficulty
        let physical = weighted_overall_difficulty (physical_data |> Seq.map _.StrainV1Notes |> Seq.concat |> Seq.filter (fun x -> x > 0.0f))

        {
            NoteDifficulty = note_difficulty
            Strains = physical_data
            Variety = variety
            Hands = hands
            Overall = if Single.IsFinite physical then physical else 0.0f
        }

    let calculate = calculate_uncached |> cached

    let color (rating: float32) : Color =
        let red = int(rating * 0.1f * 255.0f) |> min 255 |> max 0
        let green = 255 - red
        let blue = int(rating * 0.1f * 255.0f - 255.0f) |> min 255 |> max 0

        Color.FromArgb(red, green, blue)


    type IncrDiffState =
        {
            // total_notes: int
            // unplayed_weight: float32 array
            top_values: ResizeArray<float32>    // Most valuable notes judged so far, caps at 2500 (MOST_IMPORTANT_NOTES)
            mutable floor_sum: float32  // After 2500 notes are filled, the diff weight of each note has a floor of 0.02
            mutable floor_count: int
        }

    let init_incremental (total_notes: int) : IncrDiffState = //() : IncrDiffState = //(total_notes: int) : IncrDiffState =
        // let cumul = Array.zeroCreate total_notes
        // let mutable running_weight = 0.0f
        // for i = 0 to total_notes - 1 do
        //     let wt = max 0.0f (float32 (i + (int MOST_IMPORTANT_NOTES) - total_notes) / float32 MOST_IMPORTANT_NOTES)
        //     running_weight <- running_weight + WEIGHTING_CURVE wt
        //     cumul.[i] <- running_weight
        {
            // total_notes = total_notes
            // unplayed_weight = cumul
            top_values = ResizeArray()
            floor_sum = 0.0f
            floor_count = 0
        }
    
    let private top_weight_table : float32 array =  // fixed table for the ranks of the top 2500 notes
        Array.init (int MOST_IMPORTANT_NOTES) (fun j -> WEIGHTING_CURVE (float32 j / float32 MOST_IMPORTANT_NOTES))

    let insert_note (state: IncrDiffState) (value: float32) =
        let top = state.top_values
        if top.Count < int MOST_IMPORTANT_NOTES then    // Can put value into top 2500 by default
            let indx = top.BinarySearch value   // use a binary search to insert into the top 2500 notes while keeping it sorted
                // indx = the index where an exact match was found. however, if binarysearch returns a negative number, then its
                // absolute value is the index where the value would've gone
                // (need to use ~~~indx because if not, then sometimes it's out of bounds)
            top.Insert((if indx < 0 then ~~~indx else indx), value)
        elif value > top.[0] then      // 2500 note spots filled, but the note can still be placed (will kick one out)
            let popped = top.[0]
            top.RemoveAt 0
            state.floor_sum <- state.floor_sum + popped
            state.floor_count <- state.floor_count + 1
            let indx = top.BinarySearch value   // insert note value into top 2500
            top.Insert((if indx < 0 then ~~~indx else indx), value)
        else    // Not in top 2500
            state.floor_sum <- state.floor_sum + value
            state.floor_count <- state.floor_count + 1
    
    let weighted_overall_difficulty_increment (state: IncrDiffState) : float32 =
        // EXTRAPOLATE
        let top = state.top_values
        let topct = top.Count
        let mutable weight = 0.002f * float32 state.floor_count
        let mutable total = 0.002f * state.floor_sum

        if topct = int MOST_IMPORTANT_NOTES then
            for j = 0 to topct - 1 do
                let w = top_weight_table.[j]
                weight <- weight + w
                total <- total + top.[j] * w
        else
            for j = 0 to topct - 1 do
                let x = max 0.0f (float32 (j + int MOST_IMPORTANT_NOTES - topct) / float32 MOST_IMPORTANT_NOTES)
                let w = WEIGHTING_CURVE x
                weight <- weight + w
                total <- total + top.[j] * w
        
        MathF.Pow(total / weight, CURVE_POWER) * CURVE_SCALE

        // // COUNT UP FROM 0
        // // Compute final step of performance, but with an incremental state
        // let played_notes = state.top_values.Count + state.floor_count
        // let unplayed_notes = state.total_notes - played_notes
        //     // If there exist unplayed notes, take the last one's weight?
        // let mutable weight = if unplayed_notes <= 0 then 0.0f else state.unplayed_weight.[unplayed_notes - 1]
        // let mutable total = 0.0f

        // if played_notes <= int MOST_IMPORTANT_NOTES then    // Compute the running weights for less than 2500 notes
        //     for j = 0 to played_notes - 1 do
        //         let x = max 0.0f (float32 (j + int MOST_IMPORTANT_NOTES - played_notes) / float32 MOST_IMPORTANT_NOTES)
        //         let w = WEIGHTING_CURVE x 
        //         weight <- weight + w
        //         total <- total + state.top_values.[j] * w
        // else    // Compute weights for all 2500 notes + all the 0.002-weighted ones
        //     weight <- weight + 0.002f * float32 state.floor_count
        //     total <- total + 0.002f * state.floor_sum
        //     for j=0 to int MOST_IMPORTANT_NOTES - 1 do
        //         let w = top_weight_table.[j]
        //         weight <- weight + w
        //         total <- total + state.top_values.[j] * w
        
        // MathF.Pow(total / weight, CURVE_POWER) * CURVE_SCALE