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
