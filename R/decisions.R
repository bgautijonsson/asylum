# library(tidyverse)
# library(metill)
# library(scales)
# library(geomtextpath)
# library(ggtext)
# library(janitor)
# library(patchwork)
# library(glue)
# theme_set(theme_metill())
# Sys.setlocale("LC_ALL", "is_IS.UTF-8")
# 
# source("R/plot_utils.R")
# 
# update_cache <- TRUE
# 
# 
# caption <- str_c(
#   "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum Eurostat um fólksflutninga.",
#   "\nGögn og kóði: https://github.com/bgautijonsson/asylum"
# )
# 
# litur_island <- "#08306b"
# litur_danmork <- "#e41a1c"
# litur_finnland <- "#3690c0"
# litur_noregur <- "#7f0000"
# litur_svithjod <- "#fd8d3c"
# litur_annad <- "#737373"
# 
# decisions <- read_csv("data/decisions.csv") |>
#   rename(time = TIME_PERIOD) |> 
#   filter(
#     sex == "Total",
#     age == "Total",
#     year(time) == 2022,
#     geo != "European Union - 27 countries (from 2020)",
#     decision == "Total positive decisions"
#   ) |> 
#   select(
#     -freq, -sex, -unit, -age, -decision, -time
#   ) |>
#   rename(
#     decisions = values
#   )
# 
# pop <- read_csv("data/pop.csv") |>
#   rename(time = TIME_PERIOD) |>
#   filter(
#     sex == "Total",
#     age == "Total",
#     year(time) == 2022
#   ) |>
#   select(
#     -freq, -sex, -unit, -age, -time
#   ) |>
#   rename(pop = values)
# 
# 
# d <- decisions |>
#   inner_join(
#     pop,
#     by = join_by(geo)
#   ) |>
#   inner_join(
#     metill::country_names(),
#     by = join_by(geo == country)
#   ) |>
#   select(land, citizen, decisions, pop) |>
#   filter(
#     citizen %in% c(
#       "Ukraine",
#       "Palestine*",
#       "Venezuela",
#       "Total"
#     )
#   ) |>
#   mutate(
#     citizen = str_replace(citizen, "\\*", "") |>
#       as_factor() |>
#       fct_recode(
#         "Palestína" = "Palestine",
#         "Úkraína" = "Ukraine",
#         "Venesúela" = "Venezuela",
#         "Samtals" = "Total"
#       )
#   ) |>
#   mutate(
#     per_pop = decisions / pop * 1e5
#   ) |>
#   mutate(
#     colour = case_when(
#       land == "Ísland" ~ litur_island,
#       land == "Danmörk" ~ litur_danmork,
#       land == "Finnland" ~ litur_finnland,
#       land == "Noregur" ~ litur_noregur,
#       land == "Svíþjóð" ~ litur_svithjod,
#       TRUE ~ litur_annad
#     ),
#     linewidth = 1 * (land == "Ísland"),
#     size = as_factor(linewidth)
#   )


#### Samtals ####

p_tot1 <- d |> 
  filter(
    citizen == "Samtals"
  ) |> 
  plot_data() +
  labs(
    subtitle = "Samtals <em style='font-size:16px; font-weight:100;'>(veitt vernd)</em>"
  ) +
  theme(
    plot.subtitle = element_markdown()
  )

#### Úkraína ####


p_ukraine <- d |> 
  filter(
    citizen == "Úkraína"
  ) |> 
  plot_data() +
  labs(
    subtitle = "Frá Úkraínu <em style='font-size:16px; font-weight:100;'>(veitt vernd)</em>"
  ) +
  theme(
    plot.subtitle = element_markdown()
  )

#### Samtals - Úkraína ####

p_tot2 <- d |> 
  select(-pop, -decisions) |> 
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
  select(-pop, -decisions) |> 
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
    subtitle = "Frá Palestínu"
  )


#### Samtals - Úkraína - Venesúela - Palestína

p_tot4 <- d |> 
  select(-pop, -decisions) |> 
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
  "Miðað við höfðatölu hefur Ísland veitt fleiri einstaklingum tímabundna vernd en aðrar þjóðir. ",
  "Frá hvaða löndum koma þessir einstaklingar helst? Er staðan sú sama<br>í öðrum löndum? ",
  "Í neðri röðinni sjáum við fjölda verndarveitinga til einstaklinga frá þremur löndum: Úkraínu, Venesúela og Palestínu. ",
  "Efri röðin sýnir svo fjölda<br>einstaklinga alls auk þess að sýna fjölda einstaklinga að frádregnum löndunum þremur. ",
  "\n\n<em style='font-size:14px;font-weight:300;'>Tölur eru sýndar sem fjöldi á 100.000 íbúa komulands.</em>"
)

p <- p_tot /
  p_countries +
  plot_annotation(
    title = "Einstaklingar sem fá tímabundna vernd eftir upprunalandi og komulandi (2022)",
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
  filename = "Figures/veitingar.png",
  width = 8, height = 0.55 * 8, scale = 1.8
)

