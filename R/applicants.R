library(tidyverse)
library(metill)
library(scales)
library(geomtextpath)
library(ggtext)
library(janitor)
library(patchwork)
library(glue)
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
    year(time) == 2022,
    asyl_app == "Asylum applicant",
    geo != "European Union - 27 countries (from 2020)"
  ) |>
  select(
    -freq, -sex, -unit, -age, -asyl_app, -time
  ) |>
  rename(
    applicants = values
  )

pop <- read_csv("data/pop.csv") |>
  rename(time = TIME_PERIOD) |>
  filter(
    sex == "Total",
    age == "Total",
    year(time) == 2022
  ) |>
  select(
    -freq, -sex, -unit, -age, -time
  ) |>
  rename(pop = values)


d <- applicants |>
  inner_join(
    pop,
    by = join_by(geo)
  ) |>
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |>
  select(land, citizen, applicants, pop) |>
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

p_tot1 <- d |> 
  filter(
    citizen == "Samtals"
  ) |> 
  plot_data() +
  labs(
    subtitle = "Samtals"
  )

#### Úkraína ####


p_ukraine <- d |> 
  filter(
    citizen == "Úkraína"
  ) |> 
  plot_data() +
  labs(
    subtitle = "Frá Úkraínu"
  )

#### Samtals - Úkraína ####

p_tot2 <- d |> 
  select(-pop, -applicants) |> 
  pivot_wider(names_from = citizen, values_from = per_pop) |> 
  mutate(
    per_pop = Samtals - Úkraína
  ) |> 
  plot_data() +
  labs(
    subtitle = "Samtals án Úkraínu"
  )

#### Venesúela ####

p_venesuela <- d |> 
  filter(
    citizen == "Venesúela"
  ) |> 
  plot_data() +
  labs(
    subtitle = "Frá Venesúela"
  )


#### Samtals - Úkraína - Venesúela ####

p_tot3 <- d |> 
  select(-pop, -applicants) |> 
  pivot_wider(names_from = citizen, values_from = per_pop) |> 
  mutate(
    per_pop = Samtals - Úkraína - Venesúela
  ) |> 
  plot_data() +
  labs(
    subtitle = "Líka án Venesúela"
  )


#### Palestína ####

p_palestina <- d |> 
  filter(
    citizen == "Palestína"
  ) |> 
  plot_data() +
  labs(
    subtitle = "Umsækjendur frá Palestínu"
  )


#### Samtals - Úkraína - Venesúela - Palestína

p_tot4 <- d |> 
  select(-pop, -applicants) |> 
  pivot_wider(names_from = citizen, values_from = per_pop) |> 
  mutate(
    per_pop = Samtals - Úkraína - Venesúela - Palestína
  ) |> 
  plot_data() +
  labs(
    subtitle = "Líka án Pálestínu"
  )

p_tot1

p_countries <- p_ukraine +
  p_venesuela +
  p_palestina


p_countries


p_tot <- p_tot1 +
  p_tot2 +
  p_tot3 +
  p_tot4 +
  plot_layout(
    nrow = 1
  ) 

subtitle <- str_c(
  "Ísland hefur tekið við mun fleiri umsóknum um alþjóðlega vernd en aðrar þjóðir. ",
  "Frá hvaða löndum koma þessar umsóknir helst? Fáum við fleiri umsóknir frá öllum<br>löndum eða bara fáum? ",
  "Í neðri röðinni sjáum við fjölda umsókna frá þremur löndum: Úkraínu, Venesúela og Palestínu. ",
  "Efri röðin sýnir svo fjölda umsókna alls auk<br>þess að sýna fjölda umsókna að frádregnum löndunum þremur. ",
  "\n\n<em style='font-size:14px;font-weight:300;'>Tölur eru sýndar sem fjöldi á 100.000 íbúa komulands.</em>"
)

p <- p_tot /
  p_countries +
  plot_annotation(
    title = "Umsækjendur um tímabundna vernd eftir upprunalandi (2022)",
    subtitle = subtitle,
    caption = caption, 
    theme = theme(
      plot.subtitle = element_markdown(
        family = "Lato",
        size = 14
      )
    )
  )


ggsave(
  plot = p,
  filename = "Figures/umsaekjendur.png",
  width = 8, height = 0.55 * 8, scale = 1.8
)

