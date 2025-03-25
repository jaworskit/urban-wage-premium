# %%
library(tidyverse)
library(glue)
library(here)
library(sf)
# remotes::install_github("kylebutts/kfbmisc")
library(kfbmisc)

dropbox <- "~/Dropbox/UrbanWagePremium"

states <- tigris::states(cb = TRUE) |>
  rmapshaper::ms_simplify(keep = 0.01) |>
  st_transform(st_crs(msa)) |>
  filter(!(STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")))

counties <- tigris::states(cb = TRUE) |>
  rmapshaper::ms_simplify(keep = 0.01) |>
  st_transform(st_crs(msa)) |>
  filter(!(STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")))

# %%
msa <- read_sf(glue("{dropbox}/data/shapefiles/MSA_2000")) |>
  rmapshaper::ms_simplify(keep = 0.01) |>
  filter(
    !(MSACMSA %in% c("0380", "3320"))
  )

# Create indicator for being a 1950 MSA
msa_1950 <- read_sf(glue("{dropbox}/data/shapefiles/MSA_1950")) |>
  select(GISJOIN, geometry) |>
  st_transform(st_crs(msa))

list_of_1950_msas <- msa |>
  mutate(
    area_total = st_area(geometry) |>
      units::set_units("acres") |>
      units::drop_units()
  ) |>
  st_intersection(msa_1950 |> st_buffer(dist = -100)) |>
  mutate(
    area_intersection = st_area(geometry) |>
      units::set_units("acres") |>
      units::drop_units()
  ) |>
  filter(area_intersection / area_total > 0.001) |>
  pull(GISJOIN)

msa <- msa |>
  mutate(is_1950_msa = GISJOIN %in% list_of_1950_msas)

# %%
(map_msa <- ggplot() +
  geom_sf(
    data = counties,
    fill = "white",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_sf(
    aes(fill = "MSAs"),
    data = msa,
    color = NA
  ) +
  geom_sf(
    aes(color = "MSAs"),
    data = msa,
    fill = NA,
  ) +
  scale_fill_manual(
    values = colorspace::lighten(
      kfbmisc::kyle_color("blue"),
      amount = 0.3,
      space = "HCL"
    )
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color("blue")
  ) +
  labs(fill = NULL, color = NULL) +
  kfbmisc::theme_kyle() +
  kfbmisc::theme_map() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.18, 0.16),
    plot.margin = margin(),
  ))

(map_msa_indicate_1950 <- ggplot() +
  geom_sf(
    data = counties,
    fill = "white",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_sf(
    aes(fill = is_1950_msa),
    data = msa,
    color = NA
  ) +
  geom_sf(
    data = msa,
    fill = NA,
    color = kfbmisc::kyle_color("blue")
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = colorspace::lighten(
        kfbmisc::kyle_color("blue"),
        amount = 0.2,
        space = "HCL"
      ),
      "FALSE" = colorspace::lighten(
        kfbmisc::kyle_color("blue"),
        amount = 0.45,
        space = "HCL"
      )
    ),
    labels = c("TRUE" = "1950s MSA", "FALSE" = "New MSA")
  ) +
  scale_color_manual(
    values = kfbmisc::kyle_color("blue")
  ) +
  labs(fill = NULL, color = NULL) +
  kfbmisc::theme_kyle() +
  kfbmisc::theme_map() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.18, 0.16),
    plot.margin = margin(),
  ))

kfbmisc::tikzsave(
  here("out/figures/slides/map_msa.pdf"),
  map_msa,
  width = 6,
  height = 4.2
)
