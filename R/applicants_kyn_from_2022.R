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

applicants <- read_csv("data/applicants.csv") |>
  rename(time = TIME_PERIOD) |>
  filter(
    year(time) >= 2022,
    asyl_app == "Asylum applicant",
    geo != "European Union - 27 countries (from 2020)"
  ) |>
  select(
    -freq, -unit,  -asyl_app, -time
  ) |>
  rename(
    applicants = values
  )


table_dat <- applicants |> 
  filter(
    geo == "Iceland",
    citizen %in% c(
      "Total",
      "Ukraine",
      "Venezuela"
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
    citizen = str_replace(citizen, "\\*", "") |>
      as_factor() |>
      fct_recode(
        "Samtals" = "Total",
        "Úkraína" = "Ukraine",
        "Venesúela" = "Venezuela"
      ),
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
    citizen, age, sex, wt = applicants
  ) |> 
  pivot_wider(names_from = citizen, values_from = n) |> 
  mutate(
    `Samtals (án Úkraínu)` = Samtals - Úkraína
  ) |> 
  pivot_longer(-c(age:sex), names_to = "citizen", values_to = "n") |> 
  mutate(
    citizen = fct_relevel(
      citizen,
      "Úkraína",
      "Venesúela",
      "Samtals (án Úkraínu)",
      "Samtals"
    )
  ) |> 
  mutate(
    p = n / sum(n),
    n_tot = sum(n) |> number(big.mark = ".", decimal.mark = ","),
    .by = c(citizen)
  )

sex_dat <- table_dat |> 
  count(citizen, sex, wt = n) |> 
  mutate(
    total_p = n / sum(n),
    .by = citizen
  ) |> 
  select(-n)

table_dat |> 
  arrange(citizen, age, sex) |> 
  select(-n) |> 
  pivot_wider(names_from = age, values_from = c(p)) |> 
  inner_join(
    sex_dat,
    by = join_by(citizen, sex)
  ) |> 
  mutate(
    citizen_n = md(glue("**{citizen}**<br>Heildarfjöldi: {n_tot}"))
  ) |> 
  select(-citizen, -n_tot) |> 
  group_by(citizen_n) |> 
  gt(
    locale = "is",process_md = TRUE
  ) |> 
  tab_header(
    title = md("**Aldurs- og kynjadreifing einstaklinga sem sóttu um alþjóðlega vernd eða hæli á Íslandi frá 2022**"),
    subtitle = md("Samkvæmt nýjustu tölum Eurostat")
  ) |> 
  tab_footnote(
    caption
  ) |> 
  cols_label(
    sex = "",
    total_p = "Samtals"
  ) |> 
  fmt_percent(
    columns = -c(sex),
    decimals = 0
  ) |> 
  gt_color_rows(
    c(-sex, -total_p), 
    domain = c(0, 1),
    pal_type = "continuous",
    palette = "Blues"
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = "total_p"
    )
  ) |> 
  gtsave(
    filename = "Figures/aldur_kyn_dreifing_2022.png",
    zoom = 1,
    delay = 0.1,
    expand = 50
  )

