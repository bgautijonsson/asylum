library(tidyverse)
library(metill)
library(scales)
library(geomtextpath)
library(ggtext)
library(janitor)
library(patchwork)
library(glue)
library(gt)
library(gtExtras)
library(ggstream)
library(hagstofa)
library(ggh4x)
library(eurostat)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

d <- get_eurostat(
  "migr_asyappctzm",
  filters = list(
    sex = "T",
    age = "TOTAL",
    asyl_app = "ASY_APP",
    geo = "IS"
  )
) |>
  select(-sex, -age, -freq, -unit, -asyl_app, -geo) |>
  drop_na() |>
  label_eurostat() |>
  rename(value = values) |>
  janitor::clean_names() |>
  filter(
    !citizen %in% c(
      "Total",
      "Extra-EU27 (from 2020)",
      "European Union - 27 countries (from 2020)"
      )
  )

countries <- tribble(
  ~rikisfang, ~citizen, ~flag,
  "Afganistan", "Afghanistan", "#000000",
  "Albanía", "Albania", "#a50f15",
  "Kólumbía", "Colombia", "#FFCD00",
  "Georgía", "Georgia", "#DA291C",
  "Írak", "Iraq", "#67000d",
  "Íran", "Iran", "#239F40",
  "Nígería", "Nigeria", "#238b45",
  "Norður Makedónía", "North Macedonia", "#ffeda0",
  "Palestína", "Palestine*", "grey30",
  "Sómalía", "Somalia", "#6baed6",
  "Sýrland", "Syria", "grey10",
  "Úkraína", "Ukraine", "yellow",
  "Venesúela", "Venezuela", "#fd8d3c"
)

# d |> 
#   anti_join(countries) |> 
#   filter(year(time) >= 2021) |> 
#   count(citizen, wt = value) |> 
#   View()


plot_dat <- d |>
  left_join(
    countries,
    by = join_by(citizen)
  ) |> 
  mutate(
    rikisfang = if_else(is.na(flag), "Annað", rikisfang),
  ) |>
  summarise(
    value = sum(value),
    .by = c(flag, time, rikisfang)
  ) |>
  mutate(
    total = sum(value),
    .by = flag
  ) |>
  mutate(
    flag = coalesce(flag, "grey80"),
    flag = fct_reorder(flag, total)
  )

p1 <- plot_dat |> 
  mutate(
    rikisfang = -1 * (rikisfang == "Venesúela") - 2 * (rikisfang == "Úkraína")
  ) |> 
  count(rikisfang, time, wt = value, name = "value") |>
  ggplot(aes(time, value)) +
  geom_col(
    aes(fill = factor(rikisfang)), 
    col = "black",
    linewidth = 0.02,
    position = "stack", 
    width = 31
  ) +
  geom_text(
    data = ~filter(
      .x, 
      time %in% clock::date_build(
        c(2013, 2016, 2017, 2020, 2021, 2022, 2022, 2023, 2024, 2024, 2024), 
        c(1, 11, 8, 7, 11, 12, 3, 10, 2, 5, 8)
      )
    ) |> 
      summarise(value = sum(value), .by = time) |> 
      mutate(
        time = if_else(time == max(time), time + 60, time)
      ),
    aes(label = value),
    nudge_y = 18,
    size = 3.5,
    fontface = "bold"
  ) +
  annotate(
    geom = "richtext",
    label = str_c(
      "<b>Súlurnar sýna heildarfjölda</b> þeirra sem sóttu um hæli viðkomandi mánuð<br>",
      "Nokkur gildi eru sýnd fyrir ofan viðkomandi súlur"
    ),
    x = clock::date_build(2022), 
    y = 400,
    hjust = 1, 
    size = 3.5,
    family = "Lato",
    colour = "#000000",
    fill = NA,
    label.colour = NA
  ) +
  scale_x_date(
    breaks = breaks_width("year", offset = "year"),
    labels = label_date_short(),
    guide = guide_axis_truncated(),
    expand = expansion(0.01)
  ) +
  scale_y_continuous(
    expand = expansion(),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_fill_manual(
    values = c("#0057B7", "#fe9929", "grey30"),
    labels = c(
      "Úkraína",
      "Venesúela",
      "Annað"
    ) |> 
      str_pad(width = 29, side = "right"),
    guide = guide_legend(
      keyheight = unit(0.6, "cm"),
      override.aes = list(linewidth = 0.6)
    )
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 550),
    xlim = clock::date_build(c(2008, 2024), c(1, 8))
  ) +
  theme(
    plot.subtitle = element_textbox()
  ) +
  labs(
    x = NULL,
    y = NULL,
    col = NULL,
    fill = NULL,
    subtitle = "<b>Heildarfjöldi umsókna</b>"
  ) 


plot_dat1 <- plot_dat |>
  filter(
    !rikisfang %in% c("Venesúela", "Úkraína")
  ) |>
  mutate(
    rikisfang = fct_collapse(
      rikisfang,
      "Albanía" = "Albanía",
      "Palestína" = "Palestína",
      "Norður Makedónía" = "Norður Makedónía",
      "Nígería" = "Nígería",
      "Sómalía" = "Sómalía",
      "Georgía" = "Georgía",
      "Sýrland" = "Sýrland",
      other_level = "Önnur lönd"
    ),
    flag = if_else(
      rikisfang == "Önnur lönd",
      "grey90",
      flag
    )
  ) |> 
  count(rikisfang, flag, time, wt = value, name = "value") |> 
  mutate(
    total = sum(value),
    .by = rikisfang
  ) |> 
  mutate(
    rikisfang = fct_reorder(rikisfang, -total),
    flag = fct_reorder(flag, -total)
  )

p2 <- plot_dat1 |> 
  ggplot(aes(time, value, fill = flag, col = flag)) +
  geom_col(
    position = "stack",
    col = "black",
    width = 31,
    linewidth = 0.02
  ) +
  geom_text(
    data = ~filter(
      .x, 
      time %in% clock::date_build(
        c(2016, 2017, 2020, 2021, 2022, 2023, 2024), 
        c(11, 8, 7, 11, 12, 9, 8)
      )
    ) |> 
      summarise(value = sum(value), .by = time) |> 
      mutate(
        time = if_else(time == max(time), time + 0, time)
      ),
    aes(x = time, y = value, label = value),
    nudge_y = 6,
    size = 3.5,
    fontface = "bold",
    inherit.aes = FALSE,
    col = "grey20"
  ) +
  annotate( #
    geom = "richtext",
    x = clock::date_build(2015),
    y = 55,
    hjust = 1,
    label = str_c(
      "Fram til ársins <b>2015</b> barst Íslenskum yfirvöldum<br>",
      "<b>nær engar</b> umsóknir um hæli"
    ),
    family = "Lato",
    colour = "#525252",
    fill = NA,
    label.colour = NA
  ) +
  annotate(
    geom = "richtext",
    label = str_c(
      "Löndin að neðan rúmast öll innan<br>flokksins <b>Annað</b> í efri mynd"
    ),
    x = clock::date_build(2023, 8, 15),
    y = 230,
    hjust = 0,
    family = "Lato",
    colour = "#525252",
    size = 3.6
  ) +
  annotate(
    geom = "segment",
    x = clock::date_build(2025, 4, 25),
    xend = clock::date_build(2025, 4, 25),
    y = 260,
    yend = 340,
    arrow = arrow(type = "closed", length = unit(0.3, "cm")),
    colour = "#525252",
    linewidth = 1
  ) +
  scale_x_date(
    breaks = breaks_width("year", offset = "year"),
    labels = label_date_short(),
    guide = guide_axis_truncated(),
    expand = expansion(0.01)
  ) +
  scale_y_continuous(
    expand = expansion(),
    guide = ggh4x::guide_axis_truncated(),
    breaks = seq(0, 250, by = 50)
  ) +
  scale_fill_identity(
    labels = plot_dat1 |> 
      distinct(rikisfang, flag) |> 
      arrange(rikisfang) |> 
      pull(rikisfang),
    guide = guide_legend(
      keyheight = unit(0.6, "cm"),
      override.aes = list(col = "black", linewidth = 0.6)
    )
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(NA, 250),
    xlim = clock::date_build(c(2008, 2024), c(1, 8))
    ) +
  theme(
    plot.subtitle = element_textbox()
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    subtitle = str_c(
      "<b>Umsóknir frá öðrum en</b> <b style='color:#0057B7'>Úkraínu</b> <b>og</b> <b style='color:#fe9929'>Venesúela</b>"
    )
  )




p <- p1 + p2 +
  plot_layout(
    ncol = 1, heights = c(0.7, 1)
  ) +
  plot_annotation(
    title = "Hvaðan koma umsækjendur um hæli og vernd til Íslands?",
    subtitle = str_c(
      "Myndirnar sýna fjölda umsókna um hæli eða vernd á Íslandi eftir ríkisfangi umsækjenda frá árinu 2008"
    ),
    caption = str_c(
      "Byggt á gögnum Eurostat um hælisleitendur\n",
      "Gögn og kóði:  https://github.com/bgautijonsson/asylum\n",
      "www.metill.is"
    )
  )

p

scale <- 1.6
aspect <- 0.6
ggsave(
  plot = p,
  filename = "Figures/col_plot_grants_monthly.png",
  width = 8, 
  height = aspect * 8, 
  scale = scale
)

ggsave(
  plot = p & theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank()
  ),
  filename = "Figures/col_plot_grants_monthly_fp.png",
  width = 8,
  height = aspect * 8, 
  scale = scale
)

