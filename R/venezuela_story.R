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
library(ggtext)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

litur_ukr <- "grey20"
litur_ven <- "grey40"
litur_annad <- "grey80"

# New parameter for linewidth
linewidth_param <- 0.1

text_size_param <- 4
text_linewidth_param <- 0.3

col_ven <- "#fd8d3c"
col_utlendingastofnun <- "#034e7b"

utlendingastofnun <- "<b style='color:#034e7b'>Útlendingastofnun</b>"
utlendingastofnunar <- "<b style='color:#034e7b'>útlendingastofnunar</b>"


text_list <- list(
  list(
    date = clock::date_build(2018,1 , 1),
    y = 200,
    text = "{utlendingastofnun} veitir ríkisborgurum<br><b style='color:{col_ven}'>Venesúelabúa</b> viðbótarvernd"
  ),
  list(
    date = clock::date_build(2021,1 , 1),
    y = 100,
    text = "{utlendingastofnun} byrjar að synja<br>umsóknum <b style='color:{col_ven}'>Venesúelabúa</b>"
  ),
  list(
    date = clock::date_build(2021, 9, 1),
    y = 150,
    text = "<b>Kærunefnd útlendingamála</b> fellur synjanir<br>{utlendingastofnunar} úr gildi"
  ),
  list(
    date = clock::date_build(2022, 1, 1),
    y = 200,
    text = "{utlendingastofnun} byrjar aftur að synja umsóknum"
  ),
  list(
    date = clock::date_build(2022, 11, 1),
    y = 340,
    text = "{utlendingastofnun} stöðvar afgreiðslu umsókna <b style='color:{col_ven}'>Venesúelabúa</b>"
  ),
  list(
    date = clock::date_build(2023, 3, 1),
    y = 370,
    text = "{utlendingastofnun} byrjar aftur að afgreiða umsóknir <b style='color:{col_ven}'>Venesúelabúa</b>"
  ),
  list(
    date = clock::date_build(2023, 3, 15),
    y = 470,
    text = "Fyrri útlendingalög samþykkt"
  ),
  list(
    date = clock::date_build(2024, 6, 1),
    y = 500,
    text = "Seinni útlendingalög samþykkt"
  )
)

plot_text <- function(textlist) {
  list(
    annotate(
      geom = "segment",
      x = textlist$date,
      xend = textlist$date,
      y = textlist$y,
      yend = 0,
      lty = 2,
      col = "grey0",
      size = text_size_param,
      inherit.aes = FALSE,
      linewidth = text_linewidth_param
    ),
    annotate(
      geom = "richtext",
      x = textlist$date,
      y = textlist$y,
      label = glue(textlist$text),
      hjust = 1,
      vjust = 0,
      lty = 2,
      col = "grey0",
      size = text_size_param,
      inherit.aes = FALSE,
      alpha = 0.8
    ) 
  )
}

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
  stat_smooth(
    span = 0.17,
    geom = "area",
    position = "stack",
    n = 3000,
    alpha = 0.5,
    col = "black",
    linewidth = linewidth_param
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

for (sub_text in text_list) {
  p <- p + plot_text(sub_text)
}

p

ggsave(
  plot = p,
  filename = "Figures/venezuela_story.png",
  width = 8, height = 0.5 * 8, scale = 1.9
)
