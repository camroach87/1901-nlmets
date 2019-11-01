library(ggplot2)

ba_palette <- c("#3d6b78", "#60bb6b", "#d52f59", "#f5b835", "#2dbbd6",
                "#816b93", "#b84f80", "#f08c3e", "#c1b97d", "#7e450a",
                "#d4d700", "#00978f")

ba_palette_ramp <- colorRampPalette(ba_palette)

theme_set(theme_bw() +
            theme(strip.background = element_blank()))

ggplot <- function(...) {
  ggplot2::ggplot(...) + 
    scale_colour_manual(values = ba_palette) +
    scale_fill_manual(values = ba_palette)
}
