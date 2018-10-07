StatHurricane <- ggproto("StatHurricane", Stat,
                    compute_group = ,
                    default_aes = ,
                    required_aes = )


GeomHurricane <- ggproto("GeomHurricane", Geom,
                    required_aes = c("x", "y"),
                    default_aes = ,
                    draw_key = ,
                    draw_panel = function(data, panel_scales, coord){})
