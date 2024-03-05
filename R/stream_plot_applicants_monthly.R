# library(tidyverse)
# library(metill)
# library(scales)
# library(geomtextpath)
# library(ggtext)
# library(janitor)
# library(patchwork)
# library(glue)
# library(gt)
# library(gtExtras)
# library(ggstream)
# library(hagstofa)
# library(ggh4x)
# library(eurostat)
# theme_set(theme_metill())
# Sys.setlocale("LC_ALL", "is_IS.UTF-8")
# 
# d <- get_eurostat(
#   "migr_asyappctzm",
#   filters = list(
#     sex = "T",
#     age = "TOTAL",
#     asyl_app = "ASY_APP",
#     geo = "IS"
#   )
# ) |>
#   select(-sex, -age, -freq, -unit, -asyl_app, -geo) |>
#   drop_na() |>
#   label_eurostat() |>
#   rename(value = values) |>
#   janitor::clean_names() |>
#   filter(
#     !citizen %in% c(
#       "Total",
#       "Extra-EU27 (from 2020)",
#       "European Union - 27 countries (from 2020)"
#       )
#   )
# 
# countries <- tribble(
#   ~rikisfang, ~citizen, ~flag,
#   "Afganistan", "Afghanistan", "#000000",
#   "Albanía", "Albania", "#a50f15",
#   "Georgía", "Georgia", "#ef3b2c",
#   "Írak", "Iraq", "#67000d",
#   "Íran", "Iran", "#239F40",
#   "Nígería", "Nigeria", "#238b45",
#   "Norður Makedónía", "North Macedonia", "#ffeda0",
#   "Palestína", "Palestine*", "grey30",
#   "Sómalía", "Somalia", "#6baed6",
#   "Venesúela", "Venezuela", "#fd8d3c",
# )
# 
# 
plot_dat <- d |>
  filter(citizen != "Ukraine") |>
  mutate(
    flag = if_else(citizen == "Venezuela", "#fd8d3c", "#000000")
  ) |>
  summarise(
    value = sum(value),
    .by = c(flag, time)
  ) |>
  mutate(
    total = sum(value),
    .by = flag
  ) |>
  mutate(
    flag = fct_reorder(flag, total)
  )


p1 <- plot_dat |> 
  ggplot(aes(time, value, fill = flag)) +
  geom_stream(col = "black", bw = 0.5) +
  annotate(
    geom = "richtext",
    x = clock::date_build(2022),
    y = 75,
    hjust = 1,
    label = str_c(
      "Umsóknum frá <b style='color:#fd8d3c;'>venesúelskum</b> ríkisborgurum fjölgaði ört út árið 2022<br>",
      "en þeim hefur fækkað verulega frá upphafi árs 2023"
    ),
    family = "Lato",
    colour = "#525252",
    label.colour = NA,
    fill = NA
  ) +
  scale_x_date(
    breaks = breaks_width("year", offset = "year"),
    labels = label_date_short(),
    guide = guide_axis_truncated(),
    expand = expansion(0.01)
  ) +
  scale_y_continuous(
    breaks = breaks_extended(7),
    labels = \(x) number(abs(x)),
    guide = guide_axis_truncated()
  ) +
  scale_fill_identity() +
  # theme(
  #   axis.text.y = element_blank(),
  #   axis.line.y = element_blank(),
  #   axis.ticks.y = element_blank()
  # ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  )

p2 <- plot_dat |> 
  ggplot(aes(time, value)) +
  geom_col(aes(fill = factor(flag), col = factor(flag)), position = "stack", width = 31) +
  geom_text(
    data = ~filter(
      .x, 
      time %in% clock::date_build(c(2022, 2016, 2024, 2023, 2013, 2020, 2017, 2022), c(12, 11, 1, 10, 1, 7, 8, 2))
    ) |> 
      summarise(value = sum(value), .by = time) |> 
      mutate(
        time = if_else(time == max(time), time + 20, time)
      ),
    aes(label = value),
    nudge_y = 30,
    size = 3
  ) +
  annotate(
    geom = "text",
    label = str_c(
      "Súlurnar sýna heildarfjölda þeirra sem sóttu um hæli viðeigandi mánuð\n",
      "Heildarfjöldi mánaðarlegra umsókna er sýndur fyrir ofan nokkrar súlur"
    ),
    x = clock::date_build(2008), y = 300,
    hjust = 0, 
    size = 3.5,
    family = "Lato",
    colour = "#000000"
  ) +
  scale_x_date(
    breaks = breaks_width("year", offset = "year"),
    labels = label_date_short(),
    guide = guide_axis_truncated(),
    expand = expansion(0.01)
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  theme(
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) 


p <- p1 + p2 +
  plot_layout(
    ncol = 1, heights = c(1, 0.2)
  ) +
  plot_annotation(
    title = "Umsækjendum um hæli hefur fækkað mikið undanfarið ár",
    subtitle = str_c(
      "Myndin sýnir mánaðarlegan fjölda umsókna um hæli eða vernd frá <b style='color:#fd8d3c;'>Venesúela</b> ",
      "og <b style='color:#000000;'>öðrum löndum</b> ",
      "(að Úkraínu undanskilinni)"
    ),
    caption = str_c(
      "Byggt á gögnum Eurostat um hælisleitendur\n",
      "Gögn og kóði:  https://github.com/bgautijonsson/asylum\n",
      "www.metill.is"
    ),
    theme = theme(
      plot.subtitle = element_markdown()
    )
  )

p

ggsave(
  plot = p,
  filename = "Figures/stream_plot_applicants_monthly.png",
  width = 8, height = 0.621 * 8, scale = 1.8
)

ggsave(
  plot = p & theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank()
  ),
  filename = "Figures/stream_plot_applicants_monthly_fp.png",
  width = 8, height = 0.621 * 8, scale = 1.8
)
