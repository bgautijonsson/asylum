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
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")


decisions <- read_csv("data/decisions.csv") |>
  rename(time = TIME_PERIOD)

decisions <- decisions |>
  filter(
    decision == "Total positive decisions",
    geo == "Iceland",
    age == "Total",
    sex == "Total",
    !citizen %in% c(
      "Total",
      "Extra-EU27 (from 2020)",
      "European Union - 27 countries (from 2020)"
    )
  ) |>
  select(
    -freq, -unit,  -age, -sex, -geo, -decision
  ) |>
  rename(
    decisions = values
  )



url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/3_bakgrunnur/Vernd_dvalarleyfi/MAN45002.px"

d <- hg_data(
  url
) |>
  filter(
    Aldur == "Alls",
    Kyn == "Alls",
    `Tegund verndar` == "Alls"
  ) |>
  collect()

d <- d |>
  janitor::clean_names() |>
  rename(value = 6) |>
  select(-aldur, -kyn) |>
  filter(rikisfang != "Alls") |>
  mutate(ar = parse_number(ar)) |>
  mutate(
    value = coalesce(value, 0)
  )


countries <- tribble(
  ~rikisfang, ~citizen, ~flag,
  "Afganistan", "Afghanistan", "#000000",
  "Hondúras", "Honduras", "#00bce4",
  "Írak", "Iraq", "#67000d",
  "Íran", "Iran", "#239F40",
  "Nígería", "Nigeria", "#238b45",
  "Palestína", "Palestine*", "grey30",
  "Sómalía", "Somalia", "#6baed6",
  "Sýrland", "Syria", "#CE1126",
  "Venesúela", "Venezuela", "#fd8d3c",
)


plot_dat <- decisions |>
  filter(citizen != "Ukraine") |>
  left_join(
    countries,
    by = join_by(citizen)
  ) |>
  mutate(
    ar = year(time)
  ) |>
  select(-time, -citizen) |>
  bind_rows(
    d |>
      filter(
        ar < 2008
      ) |>
      rename(decisions = value) |>
      left_join(
        countries,
        by = join_by(rikisfang)
      )
  ) |>
  mutate(
    rikisfang = if_else(is.na(flag), "Annað", rikisfang)
  ) |>
  mutate(
    total = sum(decisions),
    .by = rikisfang
  )

decisions |> count(citizen, wt = decisions, sort = T)

plot_dat1 <- plot_dat |>
  filter(
    !rikisfang %in% c("Annað", "Venesúela")
  ) |>
  mutate(
    rikisfang = fct_reorder(rikisfang, total)
  )

p1 <- plot_dat1 |> 
  ggplot(aes(ar, decisions, fill = rikisfang)) +
  geom_stream(col = "black", bw = 0.65) +
  scale_x_continuous(
    breaks = seq(1997, 2023, by = 2),
    guide = guide_axis_truncated(
      trunc_lower = 1997, trunc_upper = 2023
    ),
    expand = expansion(0.01)
  ) +
  scale_y_continuous(
    breaks = breaks_extended(7),
    labels = \(x) number(abs(x)),
    guide = guide_axis_truncated()
  ) +
  scale_fill_manual(
    values = plot_dat1 |> distinct(rikisfang, flag) |> arrange(rikisfang) |>  pull(flag),
    guide = guide_legend(
      keyheight = unit(0.075, "npc")
    )
  ) +
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
  mutate(
    rikisfang = -1 * (rikisfang == "Annað") - 2 * (rikisfang == "Venesúela")
  ) |> 
  count(rikisfang, ar, wt = decisions) |> 
  ggplot(aes(ar, n)) +
  geom_col(aes(fill = factor(rikisfang)), position = "stack") +
  geom_text(
    data = ~filter(.x, rikisfang != -4) |> summarise(n = sum(n), .by = ar),
    aes(label = n),
    nudge_y = 400,
    size = 3
  ) +
  annotate(
    geom = "text",
    label = str_c(
      "Súlurnar sýna heildarfjölda þeirra sem sóttu um hæli viðeigandi ár\n",
      "Ljósari súlurnar tákna fjölda frá löndum sem eru ekki sýnd á myndinni að Úkraínu undanskilinni\n",
      "Appelsínugular súlur tákna umsækjendur frá Venesúela"
    ),
    x = 1997, y = 2200,
    hjust = 0, 
    size = 3.5,
    family = "Lato",
    colour = "#525252"
  ) +
  scale_x_continuous(
    breaks = seq(1997, 2023, by = 2),
    guide = guide_axis_truncated(
      trunc_lower = 1997, trunc_upper = 2023
    ),
    expand = expansion(0.01)
  ) +
  scale_fill_manual(
    values = c("#fe9929", "grey90", "grey30")
  ) +
  coord_cartesian(clip = "off", xlim = c(1997, 2023)) +
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
      "Myndin sýnir fjölda umsókna um hæli eða vernd á Íslandi eftir ríkisfangi umsækjenda frá árinu 1997 að Úkraínu undanskilinni | ",
      "Ekki eru öll ríkisföng sýnd |\n",
      "Umsækjendur frá Venesúela eru ekki sýndir í efri mynd | Fjöldi frá Venesúela er sýndur sér á súluriti að neðan"
    ),
    caption = str_c(
      "Byggt á gögnum Hagstofu (1997 - 2007) og Eurostat (2008 - 2023) um hælisleitendur\n",
      "Gögn og kóði:  https://github.com/bgautijonsson/asylum\n",
      "www.metill.is"
    )
  )

p

ggsave(
  plot = p,
  filename = "Figures/stream_plot_grants.png",
  width = 8, height = 0.621 * 8, scale = 1.8
)

ggsave(
  plot = p & theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank()
  ),
  filename = "Figures/stream_plot_grants_fp.png",
  width = 8, height = 0.621 * 8, scale = 1.8
)
