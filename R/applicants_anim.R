library(tidyverse)
library(metill)
library(scales)
library(geomtextpath)
library(ggtext)
library(janitor)
library(patchwork)
library(glue)
library(gganimate)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

source("R/plot_utils.R")

update_cache <- TRUE


caption <- str_c(
  "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum Eurostat um fólksflutninga.",
  "\nGögn og kóði: https://github.com/bgautijonsson/asylum"
)

litur_island <- "#08306b"
litur_danmork <- "#e41a1c"
litur_finnland <- "#3690c0"
litur_noregur <- "#7f0000"
litur_svithjod <- "#fd8d3c"
litur_annad <- "#737373"

applicants <- read_csv("data/applicants.csv") |>
  rename(time = TIME_PERIOD) |>
  filter(
    sex == "Total",
    age == "Total",
    asyl_app == "Asylum applicant",
    geo != "European Union - 27 countries (from 2020)"
  ) |>
  select(
    -freq, -sex, -unit, -age, -asyl_app
  ) |>
  rename(
    applicants = values
  )

pop <- read_csv("data/pop.csv") |>
  rename(time = TIME_PERIOD) |>
  filter(
    sex == "Total",
    age == "Total",
  ) |>
  select(
    -freq, -sex, -unit, -age
  ) |>
  rename(pop = values)


d <- applicants |>
  inner_join(
    pop,
    by = join_by(geo, time)
  ) |>
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |>
  select(land, citizen, applicants, pop, time) |>
  filter(
    citizen %in% c(
      "Ukraine",
      "Palestine*",
      "Venezuela",
      "Total"
    )
  ) |>
  mutate(
    citizen = str_replace(citizen, "\\*", "") |>
      as_factor() |>
      fct_recode(
        "Palestína" = "Palestine",
        "Úkraína" = "Ukraine",
        "Venesúela" = "Venezuela",
        "Samtals" = "Total"
      )
  ) |>
  mutate(
    per_pop = applicants / pop * 1e5
  ) |>
  arrange(time) |>
  mutate(
    per_pop = cumsum(per_pop),
    .by = c(citizen, land)
  ) |>
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
    size = as_factor(linewidth)
  )


#### Samtals ####

p <- d |>
  mutate(
    land = fct_reorder(land, per_pop, .fun = \(x) x[length(x)]),
    ar = year(time)
  ) |>
  filter(
    citizen == "Samtals"
  ) |>
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
    land = fct_reorder(land, per_pop, .fun = \(x) x[length(x)])
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
  ) +
  labs(
    title = "Þróun heildarfjölda umsókna um alþjóðlega vernd frá 2008 til {closest_state}",
    subtitle = str_c(
      "<em style='font-size:16px; font-weight:100;'>Sýnt sem heildarfjöldi á 100.000 íbúa komulands</em>"
    ),
    caption = caption
  ) +
  theme(
    plot.subtitle = element_markdown()
  ) +
  transition_states(
    ar,
    wrap = FALSE, 
    transition_length = 1, 
    state_length = 0.01
  ) +
  ease_aes("cubic-in-out")

fps <- 20

p_anim <- animate(
  p,
  duration = 10,
  fps = fps,
  renderer = ffmpeg_renderer(format = "mp4"),
  width = 1200,
  height = 746,
  res = 110
)

anim_save(
  animation = p_anim,
  filename = "Figures/applicants_anim.mp4"
)

p_anim <- animate(
  p,
  duration = 13,
  fps = fps,
  renderer = gifski_renderer(),
  width = 1200,
  height = 746,
  res = 110,
  end_pause = 3 * fps
)

anim_save(
  animation = p_anim,
  filename = "Figures/applicants_anim.gif"
)

