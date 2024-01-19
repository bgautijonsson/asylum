library(tidyverse)
library(metill)
library(scales)
library(geomtextpath)
library(ggtext)
library(janitor)
library(patchwork)
library(glue)
library(gt)
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

decisions <- read_csv("data/decisions.csv") |>
  rename(time = TIME_PERIOD) |>
  filter(
    year(time) == 2022,
    geo != "European Union - 27 countries (from 2020)",
    decision == "Total positive decisions"
  ) |>
  select(
    -freq, -unit,  -decision, -time
  ) |>
  rename(
    decisions = values
  )


decisions |> 
  filter(
    geo == "Iceland",
    
    citizen %in% c(
      "Ukraine",
      "Palestine*",
      "Venezuela",
      "Total"
    ),
    age == "Total",
    sex != "Total"
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
  count(
    citizen, sex, geo, wt = decisions
  ) |> 
  mutate(
    p = n / sum(n),
    .by = citizen
  )


table_dat <- decisions |> 
  filter(
    geo == "Iceland",
    
    citizen %in% c(
      "Ukraine",
      "Palestine*",
      "Venezuela",
      "Total"
    ),
    age %in% c(
      "From 14 to 17 years", 
      "From 18 to 34 years",
      "From 35 to 64 years",
      "65 years or over"
    ),
    sex %in% c("Males", "Females", "Total")
  ) |>
  mutate(
    citizen = str_replace(citizen, "\\*", "") |>
      as_factor() |>
      fct_recode(
        "Palestína" = "Palestine",
        "Úkraína" = "Ukraine",
        "Venesúela" = "Venezuela",
        "Samtals" = "Total"
      ) |> 
      fct_relevel(
        "Úkraína", 
        "Palestína",
        "Venesúela",
        "Samtals"
      ),
    age = as_factor(age) |> 
      fct_recode(
        "14 - 17 ára" = "From 14 to 17 years",
        "18 - 34 ára" = "From 18 to 34 years",
        "35 - 64 ára" = "From 35 to 64 years",
        "65+ ára" = "65 years or over"
      ),
    sex = as_factor(sex) |> 
      fct_recode(
        "KVK" = "Females",
        "KK" = "Males",
        "Samtals" = "Total"
      )
  ) |> 
  count(
    citizen, age, sex, geo, wt = decisions
  ) |> 
  mutate(
    p = n / sum(n),
    .by = c(citizen, sex)
  ) |> 
  select(-geo)

sex_dat <- table_dat  |> 
  filter(sex != "Samtals") |> 
  mutate(
    age = "Samtals"
  ) |> 
  summarise(
    n = sum(n),
    .by = c(citizen, age, sex)
  ) |> 
  mutate(
    p = n / sum(n),
    .by = c(citizen)
  )

samtals_dat <- table_dat  |> 
  filter(
    sex == "Samtals"
    ) |> 
  mutate(
    age = "Samtals"
  ) |> 
  summarise(
    n = sum(n),
    .by = c(citizen, age, sex)
  ) |> 
  mutate(
    p = n / sum(n),
    .by = c(citizen)
  )

table_dat |> 
  bind_rows(
    sex_dat,
    samtals_dat
  ) |> 
  arrange(citizen, age, sex) |> 
  pivot_wider(names_from = sex, values_from = c(n, p)) |> 
  select(citizen, age, n_KVK, p_KVK, n_KK, p_KK, n_Samtals, p_Samtals) |> 
  group_by(citizen) |> 
  gt(
    locale = "is",process_md = TRUE
  ) |> 
  tab_header(
    title = md("**Aldur og kyn einstaklinga sem fá veitta alþjóðlega vernd á Íslandi**"),
    subtitle = md("Samkvæmt nýjustu tölum Eurostat")
  ) |> 
  cols_label(
    age = "",
    n_KVK = "Fjöldi",
    p_KVK = "%",
    n_KK = "Fjöldi",
    p_KK = "%",
    n_Samtals = "Fjöldi",
    p_Samtals = "%"
  ) |> 
  tab_spanner(
    label = "Konur / Stúlkur",
    columns = contains("KVK")
  ) |> 
  tab_spanner(
    label = "Karlar / Strákar",
    columns = contains("KK")
  ) |> 
  tab_spanner(
    label = "Samtals",
    columns = contains("Samtals")
  ) |> 
  fmt_percent(
    columns = starts_with("p_"),
    decimals = 0
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = age == "Samtals"
    )
  )

