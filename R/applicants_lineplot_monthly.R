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


plot_dat <- d |>
  summarise(
    Samtals = sum(value),
    `Án ÚKR` = sum(value[citizen != "Ukraine"], na.rm = T),
    `Án ÚKR og VEN` = sum(value[!citizen %in% c("Ukraine", "Venezuela")], na.rm = T),
    .by = time
  ) |>
  pivot_longer(c(-time))

plot_dat |> filter(year(time) %in% 2022:2023) |> mutate(ar = year(time)) |>  count(name, wt = value)

p <- plot_dat |> 
  ggplot(aes(time, value, col = name, label = name, hjust = name)) +
  geom_texthline(
    yintercept = 6000 / 24,
    label = "Í greinargerð sem fylgir frumvarpi Dómsmálaráðherra eru áætlaðar samtals 6.000 umsóknir árin 2025 - 2026 (uþb 250 á mánuði)",
    lty = 2,
    col = "#662506",
    size = 4,
    inherit.aes = FALSE,
    linewidth = 0.3
  ) +
  geom_texthline(
    yintercept = 350,
    label = "Árin 2022 og 2023 fengum við samtals 8.500 umsóknir (uþb 350 á mánuði)",
    lty = 2,
    col = "grey0",
    size = 4,
    inherit.aes = FALSE,
    linewidth = 0.3
  ) +
  geom_texthline(
    yintercept = 190,
    label = "Að Úkraínu undanskilinni fengum við samtals 4500 umsóknir árin 2022 - 2023 (uþb 190 á mánuði)",
    lty = 2,
    col = "grey30",
    size = 4,
    inherit.aes = FALSE,
    linewidth = 0.3
  ) +
  geom_texthline(
    yintercept = 70,
    label = "Að Úkraínu og Venesúela undanskildum fengum við samtals 1700 umsóknir árin 2022 - 2023 (uþb 70 á mánuði)",
    lty = 2,
    col = "grey30", 
    size = 4,
    inherit.aes = FALSE,
    hjust = 0.05,
    linewidth = 0.3
  ) +
  geom_textsmooth(
    span = 0.2, 
    se = 0,  
    n = 600,
    linewidth = 1,
    arrow = arrow(
      length = unit(0.4, "cm"),
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
    breaks = c(100 * 0:4, 250, 350, 190, 70),
    guide = guide_axis_truncated(),
    expand = expansion(c(0, 0.1)),
    limits = c(0, NA)
  ) +
  scale_colour_manual(
    values = c(litur_ukr, litur_ukr_ven, litur_samtals)
  ) +
  scale_hjust_manual(
    values = c(0.95, 0.97, 0.9)
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_markdown()
  ) +
  labs(
    x = NULL,
    y = "Mánaðarlegur fjöldi umsókna",
    title = "Þrátt fyrir metfjölda umsókna um hæli/vernd árin 2022-2023 hefur þeim fækkað verulega síðastliðið ár",
    subtitle = glue(str_c(
      "Þótt 2023 hafi verið metár í fjölda hælisumsókna á Íslandi ",
      "hefur mánaðarlegum fjölda þeirra farið ört lækkandi frá upphafi árs | ",
      "Myndin sýnir fjölda umsókna á mánuði<br>fyrir þrjá hópa umsækjenda: ",
      "<span style='color:{litur_samtals};'>1) samtals umsækjendur</span> ",
      "<span style='color:{litur_ukr};'>2) að úkraínskum undanskildum</span> ",
      "<span style='color:{litur_ukr_ven};'>3) að úkraínskum og venesúelskum undanskildum</span>"
    )),
    caption = str_c(
      "Byggt á gögnum Eurostat um hælisleitendur\n",
      "Gögn og kóði:  https://github.com/bgautijonsson/asylum\n",
      "www.metill.is"
    )
  )


ggsave(
  plot = p,
  filename = "Figures/monthly_applicants_lineplot.png",
  width = 8, height = 0.5 * 8, scale = 1.9
)


ggsave(
  plot = p & theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank()
  ),
  filename = "Figures/monthly_applicants_lineplot_fp.png",
  width = 8, height = 0.621 * 8, scale = 1.8
)