namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI

type SDMeanPage() =
    inherit Page()

    let config = Content.HUD

    let show_mean = Setting.simple config.SDMeanShowMean
    let show_sd = Setting.simple config.SDMeanShowSD

    member this.SaveChanges() = 
        Skins.save_hud_config
            { Content.HUD with
                SDMeanShowMean = show_mean.Value
                SDMeanShowSD = show_sd.Value
            }

    override this.Content() = 
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.sdmean.showmean", Checkbox show_mean)
                    .Help(Help.Info("hud.sdmean.showmean"))
                    .Pos(0),
                PageSetting(%"hud.sdmean.showsd", Checkbox show_sd)
                    .Help(Help.Info("hud.sdmean.showsd"))
                    .Pos(2)
            )

    override this.Title = %"hud.sdmean"