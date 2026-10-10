namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Skins.HudLayouts
open Prelude
open Interlude.Content
open Interlude.UI

type PerformancePointPage() =
    inherit Page()

    let config = Content.HUD

    let text_format = Setting.simple config.PerformancePointTextType
    let update_frequency = config.PerformancePointUpdateInterval |> Setting.bounded (0.5f, 5.0f)

    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                PerformancePointTextType = text_format.Value
                PerformancePointUpdateInterval = update_frequency.Value
            }

    override this.Content() = 
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.performance_point.text",
                SelectDropdown(
                    [|
                        PerformancePointText.None, %"hud.performance_point.text.none"
                        PerformancePointText.Suffix, %"hud.performance_point.text.suffix"
                        PerformancePointText.SuffixSpace, %"hud.performance_point.text.suffixspace"
                        PerformancePointText.Prefix, %"hud.performance_point.text.prefix"
                    |],
                    text_format
                )
                )
                    .Help(Help.Info("hud.performance_point.text"))
                    .Pos(0),
                PageSetting(%"hud.performance_point.updateinterval", Slider(update_frequency, Step = 0.1f))
                    .Help(Help.Info("hud.performance_point.updateinterval"))
                    .Pos(2)
            )

    override this.Title = %"hud.performance_point"