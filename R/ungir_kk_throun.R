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
library(ggh4x)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

update_cache <- TRUE


caption <- str_c(
  "Tafla eftir @bggjonsson hjá metill.is byggð á gögnum Eurostat um fólksflutninga.",
  "\nGögn og kóði: https://github.com/bgautijonsson/asylum"
)

litur_island <- "#08306b"
litur_danmork <- "#e41a1c"
litur_finnland <- "#3690c0"
litur_noregur <- "#7f0000"
litur_svithjod <- "#fd8d3c"
litur_annad <- "#737373"
litur_total <- "#005824"

applicants <- read_csv("data/applicants.csv") |>
  rename(time = TIME_PERIOD) |>
  filter( 
    asyl_app == "Asylum applicant",
    geo != "European Union - 27 countries (from 2020)"
  ) |>
  select(
    -freq, -unit,  -asyl_app
  ) |>
  rename(
    applicants = values
  )


plot_dat <- applicants |> 
  filter(
    geo %in% c(
      "Iceland", 
      "Norway",
      "Sweden",
      "Denmark",
      "Finland"
    ),
    citizen %in% c(
      "Total",
      "Ukraine"
    ),
    age %in% c(
      "Less than 14 years",
      "From 14 to 17 years", 
      "From 18 to 34 years",
      "From 35 to 64 years",
      "65 years or over"
    ),
    sex %in% c("Males", "Females")
  ) |>
  mutate(
    age = as_factor(age) |> 
      fct_recode(
        "Undir 14 ára" = "Less than 14 years",
        "14 - 17 ára" = "From 14 to 17 years",
        "18 - 34 ára" = "From 18 to 34 years",
        "35 - 64 ára" = "From 35 to 64 years",
        "65+ ára" = "65 years or over"
      ) |> 
      fct_relevel(
        "Undir 14 ára"
      ),
    sex = as_factor(sex) |> 
      fct_recode(
        "KVK" = "Females",
        "KK" = "Males"
      )
  ) |> 
  count(
    geo, age, sex, time, citizen, wt = applicants
  ) |> 
  pivot_wider(names_from = citizen, values_from = n) |> 
  mutate(
    Ukraine = Ukraine * (year(time) >= 2022),
    n = Total - Ukraine
  ) |> 
  mutate(
    p = n / sum(n),
    .by = c(geo, time)
  ) |> 
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |> 
  select(-geo) |> 
  mutate(
    colour = case_when(
      land == "Ísland" ~ litur_island,
      land == "Danmörk" ~ litur_danmork,
      land == "Finnland" ~ litur_finnland,
      land == "Noregur" ~ litur_noregur,
      land == "Svíþjóð" ~ litur_svithjod,
      land == "Samtals" ~ litur_total,
      TRUE ~ litur_annad
    ),
    linewidth = 1 * (land == "Ísland"),
    size = as_factor(linewidth)
  ) |> 
  filter(
    sex == "KK",
    age == "18 - 34 ára"
  ) |> 
  mutate(
    year = year(time)
  ) |> 
  group_by(
    land, colour
  ) |> 
  group_modify(
    \(x, ...) {
      m <- loess(p ~ year, data = x, span = 0.5)
      out <- tibble(
        time = seq.Date(from = min(x$time), to = max(x$time), by = "day")
      ) 
      
      model_matrix <- year(out$time) + (yday(out$time) - 1)/365.25
      
      out |> 
        mutate(
          value = predict(m, newdata = model_matrix)
        )
    }
  )


p <- plot_dat |> 
  mutate(
    text_value = case_when(
      land == "Finnland" ~ value * 1.0,
      land == "Noregur" ~ value * 1,
      land == "Danmörk" ~ value * 1,
      land == "Svíþjóð" ~ value * 0.975,
      land == "Ísland" ~ value * 1.025,
      TRUE ~ value
    ),
    angle = case_when(
      land == "Danmörk" ~ 7,
      TRUE ~ 0
    )
  ) |> 
  ggplot(aes(time, value)) +
  geom_line(
    data = ~filter(.x, land != "Ísland"),
    aes(group = land, col = colour),
    linewidth = 0.7,
    arrow = arrow(type = "closed", length = unit(0.2, "cm")),
    alpha = 0.8
  ) +
  geom_line(
    data = ~filter(.x, land == "Ísland"),
    aes(group = land, col = colour),
    linewidth = 1.1,
    arrow = arrow(type = "closed", length = unit(0.3, "cm"))
  ) +
  geom_text(
    data = ~ group_by(.x, land) |> filter(time == max(time)),
    aes(y = text_value, label = land, col = colour, angle = angle),
    hjust = 0,
    nudge_x = 30,
    size = 4
  ) +
  scale_x_date(
    breaks = clock::date_build(2008:2023),
    labels = label_date_short(),
    guide = guide_axis_truncated(),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  scale_y_continuous(
    limits = c(0.2, 0.7),
    breaks = breaks_extended(6),
    labels = label_hlutf(),
    guide = guide_axis_truncated()
  ) +
  scale_colour_identity() +
  labs(
    x = NULL,
    y = NULL,
    title = "Ungum karlmönnum hefur fækkað hlutfallslega á meðal hælisleitenda á Íslandi",
    subtitle = "Hlutfall 18 - 34 ára karlmanna af öllum hælisleitendum að undanskildum þeim sem komu frá Úkraínu í kjölfar innrásar Rússlands",
    caption = caption
  )



ggsave(
  plot = p,
  filename = "Figures/ungir_karlmenn.png",
  width = 8, height = 0.5 * 8, scale = 1.5
)
