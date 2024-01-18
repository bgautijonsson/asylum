#'@export
plot_data <- function(data) {
  data |> 
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth),
      land = glue("<i style='color:{colour}'>{land}</i>"),
      land = fct_reorder(land, per_pop)
    ) |> 
    ggplot(aes(per_pop, land, col = colour, size = size)) +
    geom_point() +
    geom_segment(aes(yend = land, xend = 0, linewidth = linewidth), lty = 2, alpha = 0.5) +
    scale_x_continuous(
      expand = expansion(c(0, 0.1)),
      breaks = breaks_extended(),
      limits = c(0, NA)
    ) +
    scale_colour_identity() +
    scale_size_manual(values = c(1, 3)) +
    scale_linewidth(
      range = c(0.2, 0.4)
    ) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 5, r = 25, b = 5, l = 5),
      axis.text.y = element_markdown(size = 10, family = "Lato"),
      legend.position = "none"
    ) +
    labs(
      x = NULL, 
      y = NULL
    )
}
