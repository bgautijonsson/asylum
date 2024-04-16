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
  "Georgía", "Georgia", "#ef3b2c",
  "Írak", "Iraq", "#67000d",
  "Íran", "Iran", "#239F40",
  "Nígería", "Nigeria", "#238b45",
  "Norður Makedónía", "North Macedonia", "#ffeda0",
  "Palestína", "Palestine*", "grey30",
  "Sómalía", "Somalia", "#6baed6",
  "Venesúela", "Venezuela", "#fd8d3c"
)


plot_dat <- d |>
  filter(
    !citizen %in% c("Ukraine")
    ) |>
  left_join(
    countries,
    by = join_by(citizen)
  ) |> 
  mutate(
    rikisfang = if_else(is.na(flag), "Annað", rikisfang)
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
    flag = fct_reorder(flag, total)
  )

plot_dat1 <- plot_dat |>
  filter(
    !rikisfang %in% c("Annað", "Venesúela")
  ) |>
  mutate(
    rikisfang = fct_reorder(rikisfang, total)
  )

p1 <- plot_dat1 |> 
  ggplot(aes(time, value, fill = flag)) +
  geom_stream(col = "black", bw = 0.5) +
  annotate(
    geom = "text",
    x = clock::date_build(2014),
    y = 100 / 12,
    hjust = 1,
    label = str_c(
      "Fram til ársins 2016 barst Íslenskum yfirvöldum\n",
      "nær engar umsóknir um hæli"
    ),
    family = "Lato",
    colour = "#525252"
  ) +
  annotate(
    geom = "text",
    label = str_c(
      "Þykkt straumanna er í hlutfalli við\nfjölda umsókna frá viðeigandi landi"
    ),
    x = clock::date_build(2016, 3), y = -300/12,
    hjust = 1, 
    family = "Lato",
    colour = "#525252"
  ) +
  scale_x_date(
    breaks = breaks_width("year", offset = "year"),
    labels = label_date_short(),
    guide = guide_axis_truncated(),
    expand = expansion(0.01)
  ) +
  scale_fill_identity(
    labels = plot_dat1 |> distinct(rikisfang, flag) |> arrange(rikisfang) |> pull(rikisfang),
    guide = guide_legend(
      keyheight = unit(0.8, "cm")
    )
  ) +
  # scale_fill_manual(
  #   values = plot_dat1 |> distinct(rikisfang, flag) |> arrange(rikisfang) |>  pull(flag),
  #   guide = guide_legend(
  #     keyheight = unit(1.45, "cm")
  #   )
  # ) +
  theme(
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  )
p1

p2 <- plot_dat |> 
  mutate(
    rikisfang = -1 * (rikisfang == "Annað") - 2 * (rikisfang == "Venesúela")
  ) |> 
  count(rikisfang, time, wt = value, name = "value") |>
  ggplot(aes(time, value)) +
  geom_col(aes(fill = factor(rikisfang), col = factor(rikisfang)), position = "stack", width = 31) +
  geom_text(
    data = ~filter(
      .x, 
      time %in% clock::date_build(c(2013, 2016, 2017, 2020, 2022, 2022, 2023, 2024), c(1, 11, 8, 7, 2, 12, 10, 2))
    ) |> 
      summarise(value = sum(value), .by = time) |> 
      mutate(
        time = if_else(time == max(time), time + 0, time)
      ),
    aes(label = value),
    nudge_y = 30,
    size = 3
  ) +
  annotate(
    geom = "text",
    label = str_c(
      "Súlurnar sýna heildarfjölda þeirra sem sóttu um hæli viðeigandi ár\n",
      "Ljósari súlurnar tákna fjölda frá löndum sem eru ekki sýnd á myndinni að Úkraínu undanskilinni\n",
      "Appelsínugular súlur tákna umsækjendur frá Venesúela"
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
  scale_colour_manual(
    values = c("#fe9929", "grey90", "grey30")
  ) +
  scale_fill_manual(
    values = c("#fe9929", "grey90", "grey30")
  ) +
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
    title = "Hvaðan koma umsækjendur um hæli og vernd til Íslands?",
    subtitle = str_c(
      "Myndin sýnir fjölda umsókna um hæli eða vernd á Íslandi eftir ríkisfangi umsækjenda frá árinu 2008 að Úkraínu undanskilinni | ",
      "Ekki eru öll ríkisföng sýnd |\n",
      "Umsækjendur frá Venesúela eru ekki sýndir í efri mynd | Fjöldi frá Venesúela er sýndur sér á súluriti að neðan"
    ),
    caption = str_c(
      "Byggt á gögnum Eurostat um hælisleitendur\n",
      "Gögn og kóði:  https://github.com/bgautijonsson/asylum\n",
      "www.metill.is"
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

