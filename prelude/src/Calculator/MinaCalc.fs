namespace Prelude.Calculator

open System.Runtime.InteropServices
open Prelude
open Prelude.Charts

[<StructLayout(LayoutKind.Sequential)>]
type NoteInfo =
    struct
        val mutable notes: uint32
        val mutable rowTime: float32
    end

[<StructLayout(LayoutKind.Sequential)>]
type Ssr =
    struct
        val mutable overall: float32
        val mutable stream: float32
        val mutable jumpstream: float32
        val mutable handstream: float32
        val mutable stamina: float32
        val mutable jackspeed: float32
        val mutable chordjack: float32
        val mutable technical: float32
    end

[<StructLayout(LayoutKind.Sequential)>]
type MsdForAllRates =
    struct
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 14)>]
        val mutable msds: Ssr[]
    end

module MinaCalcNative =
    [<DllImport("minacalc", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint create_calc()

    [<DllImport("minacalc", CallingConvention = CallingConvention.Cdecl)>]
    extern void destroy_calc(nativeint calc)

    [<DllImport("minacalc", CallingConvention = CallingConvention.Cdecl)>]
    extern MsdForAllRates calc_msd(nativeint calc, NoteInfo[] rows, unativeint num_rows)

    [<DllImport("minacalc", CallingConvention = CallingConvention.Cdecl)>]
    extern void calc_msd_into(nativeint calc, NoteInfo[] rows, unativeint num_rows, nativeint out_result)

module MinaCalc =

    let private handle : nativeint = MinaCalcNative.create_calc()

    let to_note_info (note_data: NoteData) : NoteInfo[] =
        note_data.Notes
        |> Array.choose (fun { Time = time; Data = row } ->
            let mutable bitmask = 0u
            for col = 0 to row.Length - 1 do
                match row.[col] with
                | NoteType.NORMAL
                | NoteType.HOLDHEAD -> bitmask <- bitmask ||| (1u <<< col)
                | _ -> ()
            if bitmask = 0u then
                None
            else
                let mutable ni = NoteInfo()
                ni.notes <- bitmask
                ni.rowTime <- float32 time / 1000.0f
                Some ni
        )

    let calculate_all_rates (note_data: NoteData) : MsdForAllRates option =

        if note_data.Keys <> 4 && note_data.Keys <> 6 && note_data.Keys <> 7 then None
        else
            let rows = to_note_info note_data
            if rows.Length < 10 then None
            else
                Some (MinaCalcNative.calc_msd(handle, rows, unativeint rows.Length))

    let msd_at_rate (rate: float32, all_rates: MsdForAllRates) : Ssr option =
        if rate < 0.7f || rate > 2.0f then None
        else
            let index = System.Math.Clamp(System.MathF.Round((rate - 0.7f) * 10.0f) |> int, 0, 13) 
            Some all_rates.msds.[index]