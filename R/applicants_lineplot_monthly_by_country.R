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

litur_samtals <- "#000000"
litur_ukr <- "#525252"
litur_ukr_ven <- "#737373"

# d <- get_eurostat(
#   "migr_asyappctzm",
#   filters = list(
#     sex = "T",
#     age = "TOTAL",
#     asyl_app = "ASY_APP",
#     geo = "IS"
#   )
# ) |>
#   select(-sex, -age, -freq, -unit, -asyl_app, -geo) |>
#   drop_na() |>
#   label_eurostat() |>
#   rename(value = values) |>
#   janitor::clean_names() |>
#   filter(
#     !citizen %in% c(
#       "Total",
#       "Extra-EU27 (from 2020)",
#       "European Union - 27 countries (from 2020)"
#     )
#   )

top_countries <- d |> 
  summarise(
    total = sum(value),
    .by = citizen
  ) |> 
  slice_max(order_by = total, n = 2)

plot_dat <- d |> 
  mutate(
    citizen = if_else(
      citizen %in% top_countries$citizen,
      citizen,
      "Other"
    )
  ) |> 
  summarise(
    value = sum(value),
    .by = c(citizen, time)
  ) |> 
  mutate(
    citizen = as_factor(citizen) |> 
      fct_recode(
        "Annað"  = "Other",
        "Úkraína" = "Ukraine",
        "Venesúela" = "Venezuela"
      )
  )


p <- plot_dat |> 
  # filter(year(time) >= 2015) |> 
  ggplot(aes(time, value, group = citizen, label = citizen, col = citizen, hjust = citizen)) +
  # geom_point(alpha = 0.1) +
  geom_textsmooth(
    span = 0.15, 
    se = 0,  
    n = 600,
    linewidth = 1,
    arrow = arrow(
      length = unit(0.3, "cm"),
      type = "closed"
    ),
    size = 5
  ) +
  scale_x_date(
    breaks = breaks_width("year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = seq(0, 200, by = 50),
    guide = guide_axis_truncated(),
    expand = expansion(c(0, 0.1)),
    limits = c(0, NA)
  ) +
  scale_color_manual(
    values = c("black", "#084594", "#fd8d3c")
  ) +
  scale_hjust_manual(
    values = c(0.94, 0.82, 0.92)
  ) +
  coord_cartesian(
    ylim = c(0, 210)
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_markdown()
  ) +
  labs(
    x = NULL,
    y = "Mánaðarlegur fjöldi umsókna",
    title = "Fjöldi umsækjenda frá Úkraínu, Venesúela og öðrum löndum",
    subtitle = "Eftir hápunkt um áramótin 2023 hefur fjöldi mánaðarlegra umsókna fækkað hratt",
    caption = str_c(
      "Byggt á gögnum Eurostat um hælisleitendur\n",
      "Gögn og kóði:  https://github.com/bgautijonsson/asylum\n",
      "www.metill.is"
    )
  )



ggsave(
  plot = p,
  filename = "Figures/monthly_applicants_lineplot_by_country.png",
  width = 8, height = 0.5 * 8, scale = 1.9
)