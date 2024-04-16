d |> 
  count(time, wt = value) |> 
  mutate(
    value = slider::slide_dbl(n, mean, .before = 0) * 12
  ) |> 
  ggplot(aes(time, value)) +
  # geom_line() +
  stat_smooth(
    geom = "line",
    span = 0.2, 
    se = 0,  
    n = 200,
    linewidth = 1,
    arrow = arrow(
      length = unit(0.4, "cm"),
      type = "closed"
    )
  ) +
  scale_x_date(
    breaks = breaks_width("year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    guide = guide_axis_truncated(),
    expand = expansion(c(0, 0.1)),
    limits = c(0, NA)
  ) +
  labs(
    x = NULL,
    y = "Mánaðarlegar umsóknir\n(sinnum 12 til að setja á árlegan kvarða)",
    title = "Hver verður fjöldi umsókna um hæli/vernd á næsta ári?",
    subtitle = str_c(
      "Miðað við fækkun umsókna síðastliðið ár er líklegra að umsóknir ",
      "verða færri en 2.000 á næsta ári | ",
      "Myndin sýnir\nvæntan fjölda árlegra umsókna miðað við fjölda ",
      "hvers mánaðar"
    )
  )

library(mgcv)
library(broom)
model_d <- d |> 
  mutate(
    manudur = month(time),
    dagur = as.numeric(as.factor(time))
  ) |> 
  # filter(time < clock::date_build(2023, 10)) |>
  count(time, dagur, manudur, wt = value) 


m <- gam(n ~ s(manudur, bs = "cs") + s(dagur), data = model_d, family = gaussian(link = "log"))




augment(
  m,
  newdata = bind_rows(
    tibble(
      dagur = max(model_d$dagur) + seq(1, 25)
    ) |> 
      mutate(
        manudur = dagur %% 12
      )
  ), 
  conf.int = TRUE
) |> 
  mutate(
    time = map_vec(dagur, \(x) max(d$time) + months(x - max(model_d$dagur)), .ptype = Date()),
    pred = exp(.fitted)
  ) |> 
  add_row(
    time = max(model_d$time),
    pred = model_d |> filter(time == max(time)) |> pull(n)
  ) |> 
  ggplot(aes(time, pred)) +
  geom_line(
    data = model_d,
    aes(y = n)
  ) +
  geom_line(
    col = "red"
  ) +
  scale_colour_manual(
    values = c("grey50", "red")
  ) +
  scale_x_date(
    breaks = breaks_width("year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    guide = guide_axis_truncated(),
    expand = expansion(c(0, 0.1)),
    limits = c(0, NA)
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Hver verður fjöldi umsókna um hæli/vernd á næsta ári?",
    subtitle = str_c(
      "Miðað við fækkun umsókna síðastliðið ár er líklegra að umsóknir ",
      "verða færri en 2.000 á næsta ári | ",
      "Myndin sýnir\nvæntan mánaðarlegan fjölda umsókna og einfalda spá út árið 2024"
    )
  )



