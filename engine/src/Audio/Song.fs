namespace Percyqaz.Flux.Audio

open System.Diagnostics
open ManagedBass
open ManagedBass.Fx
open Percyqaz.Common

type Song =
    {
        mutable ID: int
        Frequency: int // in hz
        Duration: Time
        PreviewPoint: Time
        LastNote: Time
        mutable LocalOffset: Time
        mutable LowPassFilterEffect: int
        mutable Playing: bool
    }

    static member CreatePlaceholder(preview_point: Time, last_note: Time, local_offset: Time) : Song =
        {
            ID = 0
            Frequency = 1
            Duration = 1000.0f<ms>
            PreviewPoint = preview_point
            LastNote = last_note
            LocalOffset = local_offset
            LowPassFilterEffect = 0
            Playing = false
        }

    static member CreateFromFile(file: string, preview_point: Time, last_note: Time, local_offset: Time) : Song =
        let handle = Bass.CreateStream(file, 0L, 0L, BassFlags.Prescan ||| BassFlags.Decode)

        if handle = 0 then
            let error =
                match Bass.LastError with
                | Errors.FileOpen -> "File couldn't be found/opened OR contains an audio format not supported"
                | other -> other.ToString()

            Logging.Error "Couldn't load audio track from '%s': %s" file error
            Song.CreatePlaceholder(preview_point, last_note, local_offset)
        else
            let duration_seconds =
                Bass.ChannelBytes2Seconds(handle, Bass.ChannelGetLength(handle))

            let duration = float32 duration_seconds * 1000.0f<ms>
            let channel_info = Bass.ChannelGetInfo(handle)
            let frequency = channel_info.Frequency

            let fx_handle = BassFx.TempoCreate(handle, BassFlags.FxFreeSource)
            Bass.ChannelSetAttribute(fx_handle, ChannelAttribute.NoBuffer, 1f).LogBassError()
            Bass.ChannelSetAttribute(fx_handle, ChannelAttribute.TempoUseQuickAlgorithm, 1).LogBassError()
            Bass.ChannelSetAttribute(fx_handle, ChannelAttribute.TempoOverlapMilliseconds, 4).LogBassError()
            Bass.ChannelSetAttribute(fx_handle, ChannelAttribute.TempoSequenceMilliseconds, 30).LogBassError()

            {
                ID = fx_handle
                Frequency = frequency
                Duration = duration
                PreviewPoint =
                    if preview_point >= duration - 3000.0f<ms> || preview_point < 0.0f<ms> then
                        duration * 0.5f
                    else
                        preview_point
                LastNote = last_note
                LocalOffset = local_offset
                LowPassFilterEffect = 0
                Playing = false
            }

    member this.ChangeDevice(device_id: int) : unit =
        if this.ID <> 0 then
            Bass.ChannelSetDevice(this.ID, device_id).LogBassError()

    member this.ChangeToCurrentDevice() : unit = this.ChangeDevice(Bass.CurrentDevice)

    member this.TimeToBytes(time: Time) : int64 =
        Bass.ChannelSeconds2Bytes(this.ID, time / 1000.0f<ms> |> float)

    member this.CurrentPosition() : Time =
        float32(Bass.ChannelBytes2Seconds(this.ID, Bass.ChannelGetPosition(this.ID))) * 1000.0f<ms>

    member this.ShouldPlayAt(time: Time) : bool =
        this.ID <> 0 && time >= 0.0f<ms> && time < this.Duration

    member this.ChangeRate(rate: float32<rate>, change_pitch: bool) : unit =

        let set_frequency_multiplier (rate: float32<rate>) =
            let scaled_frequency = float32 this.Frequency * float32 rate
            Bass.ChannelSetAttribute(this.ID, ChannelAttribute.Frequency, scaled_frequency).LogBassError()
    let mutable private _pitch_offset = 0.0f
    let mutable private _local_offset = 0.0f<ms>
    let mutable private _global_offset = 0.0f<ms / rate>

        let set_tempo (rate: float32<rate>) =
            let tempo_adjustment = (rate - 1.0f<rate>) * 100.0f< / rate>
            Bass.ChannelSetAttribute(this.ID, ChannelAttribute.Tempo, tempo_adjustment).LogBassError()

        if this.ID <> 0 then
            if change_pitch then
                set_tempo(1.0f<rate>)
                set_frequency_multiplier(rate)
            else
                set_tempo(rate)
                set_frequency_multiplier(1.0f<rate>)

    member this.SetLowPassFilter(amount: float32) : unit =

        let apply_effect (handle: int) =
            let center = lerp amount 22049f 800f
            let parameters = BQFParameters(lFilter = BQFType.LowPass, fQ = 0.7f)
            parameters.fCenter <- center
            parameters.fBandwidth <- 0f
            Bass.FXSetParameters(handle, parameters).LogBassError()

        match this.LowPassFilterEffect with
        | _ when this.ID = 0 -> ()
        | 0 ->
            if amount > 0.0f then
                let effect = Bass.ChannelSetFX(this.ID, EffectType.BQF, 1)
                apply_effect(effect)
                this.LowPassFilterEffect <- effect

        | existing_handle when amount = 0.0f ->
            Bass.ChannelRemoveFX(this.ID, existing_handle).LogBassError()
            this.LowPassFilterEffect <- 0

        | existing_handle -> apply_effect(existing_handle)

    member this.Pause() : Time =
        if this.ID <> 0 && this.Playing then
            this.Playing <- false

            if Bass.ChannelIsActive(this.ID) = PlaybackState.Playing then
                Bass.ChannelPause(this.ID).LogBassError()

        this.CurrentPosition()

    member this.Play(delay_callback: float32<ms / rate> -> unit) : unit =
        if this.ID <> 0 && not this.Playing then
            this.Playing <- true
            let before = Stopwatch.GetTimestamp()
            let sync_flags = SyncFlags.Position ||| SyncFlags.Onetime

            let callback =
                SyncProcedure(fun _ _ _ _ ->
                    delay_callback(float32(Stopwatch.GetElapsedTime(before).TotalMilliseconds) * 1.0f<ms / rate>)
                )

            Bass.ChannelSetSync(this.ID, sync_flags, Bass.ChannelGetPosition(this.ID) + 1L, callback).LogBassError()
            Bass.ChannelPlay(this.ID).LogBassError()

    member inline this.PlayFrom(time: Time, delay_callback: float32<ms / rate> -> unit) : unit =
        ignore(this.Pause())
        this.Seek(time)
        this.Play(delay_callback)

    member this.Seek(time: Time) : unit =
        if this.Playing then
            failwithf "Must not seek while song is playing"

        if this.ID <> 0 then
            Bass.ChannelSetPosition(this.ID, this.TimeToBytes(time)).LogBassError()

    member this.Free() : unit =
        if this.ID <> 0 then
            Bass.StreamFree(this.ID).LogBassError()
            this.ID <- 0

    member this.FadeIn(duration_ms: int) : unit =
        if this.ID <> 0 then
            Bass.ChannelSetAttribute(this.ID, ChannelAttribute.Volume, 0.0f).LogBassError()
            Bass.ChannelSlideAttribute(this.ID, ChannelAttribute.Volume, 1.0f, duration_ms).LogBassError()

    member this.FadeOutThenFree(duration_ms: int) : unit =
        if this.ID <> 0 then
            let slide_completed _ _ _ _ = this.Free()
            let callback = SyncProcedure(slide_completed)
            Bass.ChannelSetSync(this.ID, SyncFlags.Slided ||| SyncFlags.Onetime, 0, callback).LogBassError()
            Bass.ChannelSetSync(this.ID, SyncFlags.End ||| SyncFlags.Onetime, 0, callback).LogBassError()
            Bass.ChannelSlideAttribute(this.ID, ChannelAttribute.Volume, 0.0f, duration_ms).LogBassError()
        timer.Start()
        paused <- false

    let change_rate (new_rate: float32<rate>) : unit =
        let rate_changed = rate <> new_rate
        let time = time ()
        rate <- new_rate

        let pitch_enabled_for_direction =
            if rate = 1.0f<rate> then true
            elif rate > 1.0f<rate> then enable_pitch_rate_up
            else enable_pitch_rate_down

        if pitch_enabled_for_direction then
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Tempo, 0.0f) |> display_bass_error
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Frequency, float32 now_playing.Frequency * float32 rate) |> display_bass_error
        else
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Tempo, (rate - 1.0f<rate>) * 100.0f</rate>) |> display_bass_error
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Frequency, float32 now_playing.Frequency) |> display_bass_error

        if rate_changed then
            seek time

    let playback_rate () : float32<rate> = rate

    let set_pitch_rate_down_enabled (enabled: bool) : unit =
        enable_pitch_rate_down <- enabled
        change_rate rate

    let set_pitch_rate_up_enabled (enabled: bool) : unit =
        enable_pitch_rate_up <- enabled
        change_rate rate

    let set_low_pass (amount: float32) : unit =
        low_pass_target <- amount
        now_playing.SetLowPass low_pass_amount

    let private apply_pitch () : unit =
        Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Pitch, _pitch_offset) |> display_bass_error

    let set_pitch_offset (semitones: float32) : unit =
        _pitch_offset <- semitones
        apply_pitch ()
    let adjust_pitch_offset (semitonedelta: float32) : unit =
        _pitch_offset <- (_pitch_offset + semitonedelta)
        apply_pitch ()
    let set_local_offset (offset: Time): unit = _local_offset <- offset
    let set_global_offset (offset: float32<ms / rate>) : unit = _global_offset <- offset

    let private song_loader =
        { new Async.CancelQueue<Time * Time * string option * SongLoadAction, Song * SongLoadAction>() with
            override this.Process((preview, last_note, path, after_load)) =
                async {
                    return
                        match path with
                        | Some p -> Song.FromFile (preview, last_note, p), after_load
                        | None -> Song.Default, after_load
                }

            override this.Handle((song, after_load: SongLoadAction)) =
                loading <- false
                now_playing <- song
                change_rate rate
                apply_pitch ()

                song.SetLowPass low_pass_amount

                match after_load with
                | SongLoadAction.PlayFromPreview ->
                    (if paused then seek else play_from) song.PreviewPoint
                    Bass.ChannelSetAttribute(song.ID, ChannelAttribute.Volume, 0.0f) |> display_bass_error
                    Bass.ChannelSlideAttribute(song.ID, ChannelAttribute.Volume, 1.0f, CROSSFADE_DURATION_MS) |> display_bass_error
                | SongLoadAction.PlayFromBeginning ->
                    (if paused then seek else play_from) 0.0f<ms>
                | SongLoadAction.Wait ->
                    play_from (LEADIN_TIME * rate)
                    pause()

                on_loaded_ev.Trigger()
        }

    let change (path: string option, offset: Time, new_rate: float32<rate>, (preview: Time, chart_last_note: Time), after_load: SongLoadAction) : unit =
        let path_changed = path <> load_path
        load_path <- path
        set_local_offset offset
        change_rate new_rate
        apply_pitch ()

        if path_changed then

            let current = now_playing
            let slide_completed _ _ _ _ = current.Free()

            if current.ID <> 0 then
                Bass.ChannelSetSync(current.ID, SyncFlags.Slided ||| SyncFlags.Onetime, 0, SyncProcedure(slide_completed)) |> display_bass_error
                Bass.ChannelSetSync(current.ID, SyncFlags.End ||| SyncFlags.Onetime, 0, SyncProcedure(slide_completed)) |> display_bass_error
                Bass.ChannelSlideAttribute(current.ID, ChannelAttribute.Volume, 0.0f, CROSSFADE_DURATION_MS) |> display_bass_error

            channel_playing <- false
            loading <- true
            song_loader.Request(preview, chart_last_note, path, after_load)

    let update (elapsed_ms: float) : unit =
        let time_last_update = _time
        update_time()

        if low_pass_target <> low_pass_amount then
            low_pass_amount <- lerp (float32 <| Math.Pow(0.994, elapsed_ms)) low_pass_target low_pass_amount
            if abs (low_pass_amount - low_pass_target) < 0.01f then low_pass_amount <- low_pass_target
            now_playing.SetLowPass low_pass_amount

        song_loader.Join()

        if playing () && _time >= 0.0f<ms> && _time < now_playing.Duration && not channel_playing then

            channel_playing <- true

            Bass.ChannelSetPosition(now_playing.ID, Bass.ChannelSeconds2Bytes(now_playing.ID, float <| _time / 1000.0f<ms>))
            |> display_bass_error

            Bass.ChannelPlay now_playing.ID |> display_bass_error

        elif _time > now_playing.Duration then

            match on_finish with
            | SongFinishAction.LoopFromPreview ->
                if _time >= now_playing.LastNote then
                    play_from now_playing.PreviewPoint
            | SongFinishAction.LoopFromBeginning ->
                if _time >= now_playing.LastNote then
                    play_from 0.0f<ms>
            | SongFinishAction.Wait -> ()
            | SongFinishAction.Custom action ->
                if channel_playing then
                    action ()

            channel_playing <- false

        elif channel_playing && playing () && timer.ElapsedMilliseconds > 80L && timer.ElapsedMilliseconds < 2000L then
            let audio_position = Bass.ChannelBytes2Seconds(now_playing.ID, Bass.ChannelGetPosition(now_playing.ID, PositionFlags.Bytes)) |> float32

            let clock_elapsed = _time - time_last_update

            let off_by = time_compensated () - audio_position * 1000.0f<ms>
            let adjustment = abs off_by |> min (clock_elapsed * 0.8f)

            if adjustment > 0.5f<ms> && abs off_by < 200.0f<ms> then
                seek_inaccuracy_compensation <- seek_inaccuracy_compensation + if off_by > 0.0f<ms> then adjustment else -adjustment
