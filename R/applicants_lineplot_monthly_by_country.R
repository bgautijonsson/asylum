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

litur_ukr <- "grey20"
litur_ven <- "grey40"
litur_annad <- "grey80"

# New parameter for linewidth
linewidth_param <- 0.1

text_size_param <- 4
text_linewidth_param <- 0.2

text_list <- list(
  list(
    date = clock::date_build(2018,1 , 1),
    y = 250
  )
)

d <- get_eurostat(
  "migr_asyappctzm",
  filters = list(
    sex = "T",
    age = "TOTAL",
    asyl_app = "ASY_APP",
    geo = "IS"
  )
) |>
  select(-sex, -age, -freq, -unit, -asyl_app, -geo) |>
  drop_na() |>
  label_eurostat() |>
  rename(value = values) |>
  janitor::clean_names() |>
  filter(
    !citizen %in% c(
      "Total",
      "Extra-EU27 (from 2020)",
      "European Union - 27 countries (from 2020)"
    )
  )

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
        "Annað" = "Other",
        "Úkraína" = "Ukraine",
        "Venesúela" = "Venezuela"
      )
  ) |>
  bind_rows(
    tibble(
      citizen = c("Annað", "Úkraína", "Venesúela"),
      time = clock::date_build(2024, 09, 01),
      value = c(45, 69, 15)
    )
  )

p <- plot_dat |>
  mutate(
    pred = loess(value ~ row_number(), span = 0.13) |>
      predict(),
    .by = citizen
  ) |>
  mutate(
    col = case_when(
      citizen == "Annað" ~ litur_annad,
      citizen == "Úkraína" ~ litur_ukr,
      TRUE ~ litur_ven
    ),
    citizen = fct_relevel(
      citizen,
      "Úkraína", "Venesúela", "Annað"
    )
  ) |>
  filter(year(time) >= 2015) |>
  ggplot(aes(time, value, group = citizen, label = citizen, fill = citizen, col = col, hjust = citizen)) +
  # geom_point(alpha = 0.1) +
  # geom_area(
  #   position = "stack",
  #   col = "black",
  #   linewidth = 0.3
  # ) +
  stat_smooth(
    span = 0.17,
    geom = "area",
    position = "stack",
    n = 3000,
    alpha = 0.5,
    col = "black",
    linewidth = linewidth_param
  ) +
  geom_textsegment(
    data = tibble(
      y = 6000 / 24, yend = 6000 / 24,
      x = clock::date_build(2014, 11),
      xend = max(plot_dat$time)
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    label = "Í greinargerð sem fylgir frumvarpi Dómsmálaráðherra eru áætlaðar samtals 6.000 umsóknir árin 2025 - 2026 (uþb 250 á mánuði)",
    lty = 1,
    col = "#662506",
    size = 4,
    inherit.aes = FALSE,
    linewidth = linewidth_param,
    hjust = 0
  ) +
  geom_textsegment(
    data = tibble(
      y = 350, yend = 350,
      x = clock::date_build(2018, 2),
      xend = max(plot_dat$time)
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    label = "Árin 2022 og 2023 fengum við samtals 8.500 umsóknir (uþb 350 á mánuði)",
    lty = 1,
    col = "grey0",
    size = 4,
    inherit.aes = FALSE,
    linewidth = linewidth_param,
    hjust = 0
  ) +
  geom_textsegment(
    data = tibble(
      y = 190, yend = 190,
      x = clock::date_build(2016, 6),
      xend = max(plot_dat$time)
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    label = "Að Úkraínu undanskilinni fengum við samtals 4500 umsóknir árin 2022 - 2023 (uþb 190 á mánuði)",
    lty = 1,
    col = "grey10",
    size = 4,
    inherit.aes = FALSE,
    linewidth = linewidth_param,
    hjust = 0
  ) +
  geom_textsegment(
    data = tibble(
      y = 70, yend = 70,
      x = clock::date_build(2015, 4),
      xend = max(plot_dat$time)
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    label = "Að Úkraínu og Venesúela undanskildum fengum við samtals 1700 umsóknir árin 2022 - 2023 (uþb 70 á mánuði)",
    lty = 1,
    col = "grey10",
    size = 4,
    inherit.aes = FALSE,
    linewidth = linewidth_param,
    hjust = 0
  ) +
  geom_textvline(
    xintercept = clock::date_build(2023, 3, 15),
    label = "Fyrri útlendingalög samþykkt",
    hjust = 0.95,
    lty = 2,
    col = "grey0",
    size = text_size_param,
    inherit.aes = FALSE,
    linewidth = text_linewidth_param,
    hjust = 0
  ) +
  geom_textvline(
    xintercept = clock::date_build(2024, 6, 14),
    label = "Seinni útlendingalög samþykkt",
    hjust = 0.95,
    lty = 2,
    col = "grey0",
    size = text_size_param,
    inherit.aes = FALSE,
    linewidth = text_linewidth_param,
    hjust = 0
  ) +
  scale_x_date(
    breaks = breaks_width("year"),
    labels = label_date_short(),
    guide = guide_axis_truncated(),
    expand = expansion(c(0.05, 0.06))
  ) +
  scale_y_continuous(
    breaks = breaks_width(50),
    guide = guide_axis_truncated(),
    expand = expansion(c(0, 0.1)),
    limits = c(0, NA)
  ) +
  scale_color_identity() +
  scale_fill_manual(
    values = c(litur_ukr, litur_ven, litur_annad),
    labels = c("Úkraína", "Venesúela", "Annað"),
    guide = guide_legend(
      reverse = FALSE,
      override.aes = list(col = NA)
    )
  ) +
  scale_hjust_manual(
    values = c(0.94, 0.82, 0.92)
  ) +
  coord_cartesian() +
  theme(
    legend.position = "right",
    plot.subtitle = element_markdown(),
    legend.key.height = unit(0.1, "npc")
  ) +
  labs(
    x = NULL,
    y = "Samanlagður fjöldi umsókna",
    title = "Eftir hámark um áramótin 2022/2023 hefur umsóknum um hæli farið ört fækkandi",
    subtitle = "Myndin sýnir mánaðarlegan fjölda hælisumsókna eftir ríkisfangi umsækjenda",
    fill = NULL,
    caption = str_c(
      "Byggt á gögnum Eurostat um hælisleitendur\n",
      "Gögn og kóði:  https://github.com/bgautijonsson/asylum\n",
      "www.metill.is"
    )
  )

p

ggsave(
  plot = p,
  filename = "Figures/monthly_applicants_lineplot_by_country.png",
  width = 8, height = 0.5 * 8, scale = 1.9
)
