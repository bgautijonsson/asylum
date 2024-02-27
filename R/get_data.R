library(eurostat)
library(tidyverse)

#### Applicants ####

applicants <- get_eurostat(
  "migr_asyappctza",
  cache = TRUE,
  update_cache = TRUE,
  cache_dir = "data-raw"
)  

d <- applicants |> 
  label_eurostat()

d |> 
  write_csv("data/applicants.csv")


#### Population ####

pop <- get_eurostat(
  "demo_pjan",
  cache = TRUE,
  update_cache = TRUE,
  cache_dir = "data-raw"
)

d <- pop |> 
  label_eurostat()

d |> 
  write_csv(
    "data/pop.csv"
  )


#### Decisions ####

decisions <- get_eurostat(
  "migr_asydcfsta",
  cache = TRUE,
  update_cache = TRUE,
  cache_dir = "data-raw"
)

d <- decisions |> 
  label_eurostat()

d |> 
  write_csv("data/decisions.csv")




#### Temporary Protection ####

temporary_protection <- get_eurostat(
  "migr_asytpfa",
  filters = list(
    citizen = "TOTAL",
    age = "TOTAL",
    sex = "T"
  )
)  |> 
  select(-citizen, -age, -sex, -freq, -unit) |> 
  label_eurostat() |> 
  rename(
    granted_temporary_protection = values
  )


#### Final Decisions ####


final_decisions <- get_eurostat(
  "migr_asydcfina",
  filters = list(
    citizen = "TOTAL",
    age = "TOTAL",
    sex = "T"
  )
)  |> 
  select(-citizen, -age, -sex, -freq, -unit) |> 
  label_eurostat() |> 
  rename(
    final_decision = values
  )
