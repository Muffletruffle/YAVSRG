namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type ColumnErrorBarsWindowPage() =
    inherit Page()

    let config = Content.HUD

    let windows_opacity =
        config.ColumnErrorBarsWindowsOpacity |> Setting.bounded(0.0f, 0.6f)
    let window_scale = config.ColumnErrorBarsWindowScale |> Setting.bounded (0.2f, 1.0f)
    let log_scale_sensitivity =
        config.ColumnErrorBarsLogSensitivity |> Setting.bounded(0.0f, 1.0f)

    member this.SaveChanges() : unit =
        Skins.save_hud_config
            { Content.HUD with
                ColumnErrorBarsWindowsOpacity = windows_opacity.Value
                ColumnErrorBarsWindowScale = window_scale.Value
                ColumnErrorBarsLogSensitivity = log_scale_sensitivity.Value
            }

    override this.Content() : Widget =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.column_error_bars.timing_windows_opacity", Slider.Percent(windows_opacity))
                    .Help(Help.Info("hud.column_error_bars.timing_windows_opacity"))
                    .Pos(0),
                PageSetting(%"hud.column_error_bars.window_scale", Slider.Percent(window_scale))
                    .Help(Help.Info("hud.column_error_bars.window_scale"))
                    .Pos(2),
                PageSetting(
                    %"hud.column_error_bars.logsensitivity",
                    Slider.Percent(log_scale_sensitivity, Step = 0.01f)
                )
                    .Help(Help.Info("hud.column_error_bars.logsensitivity"))
                    .Pos(4)
            )

    override this.Title = %"hud.column_error_bars.timing_windows_settings"
