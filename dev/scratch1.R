# develop the EEVEE maskset information data frame!

# Capability
# 1. It has all the info and can just be joined to the data.
# 2. It takes the barID and dieID as input. That's it.

# setup ----

# make sure to 'Load All'
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
wafer_diameter_mm = 75
circ_dat = generate_circle(diameter = wafer_diameter_mm * 1e3)

# generate eevee die coords
df_eevee =
  generate_eevee_ids() |>
  add_eevee_die_coords() |>
  mutate(r = sqrt(die_x_coord^2 + die_y_coord^2)) |>
  add_eevee_grating_info()

# wafer map checks ----

# cell ID
df_eevee |>
  filter(r <= wafer_diameter_mm*1e3/2) |>
  ggplot(aes(xmin = die_x_coord, xmax = die_x_coord + DIE_WIDTH, ymin = die_y_coord, ymax = die_y_coord + DIE_LENGTH)) +
  geom_rect(aes(fill = as.factor(cell_id)), color = "white", linewidth = 0, na.rm = FALSE) +
  geom_path(
    data = circ_dat,
    mapping = aes(x = x, y = y),
    inherit.aes = FALSE,
    color = "gray"
  ) +
  coord_equal(xlim = c(-80000/2, 80000/2)) +
  # scale_fill_viridis_d(direction = 1, option = "plasma") +
  # scale_fill_viridis_d() +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    title = "EEVEE Cell ID Map",
    fill = "Cell ID"
  ) +
  guides(fill=guide_legend(ncol=2))

# grating no
df_eevee |>
  filter(r <= wafer_diameter_mm*1e3/2) |>
  ggplot(aes(xmin = die_x_coord, xmax = die_x_coord + DIE_WIDTH, ymin = die_y_coord, ymax = die_y_coord + DIE_LENGTH)) +
  geom_rect(aes(fill = grt_no), color = "white", linewidth = 0, na.rm = FALSE) +
  coord_equal(xlim = c(-80000/2, 80000/2)) +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    title = "EEVEE Grating No. Map",
  ) +
  guides(fill=guide_legend(ncol=1))


# grating design statistics ----
df_eevee |>
  # drop_na(grt_no) |>
  filter(r <= wafer_diameter_mm*1e3/2) |>
  count(grt_no) |>
  mutate(
    pct = n / sum(n) * 100
  ) |>
  janitor::adorn_totals()

