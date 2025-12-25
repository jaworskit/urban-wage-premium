# %%
library(tidyverse)
library(here)
library(glue)
library(haven)

# Local
dropbox <- "~/Dropbox/Projects/UrbanWagePremium"
gh <- "~/Documents/Projects/urban-wage-premium"

# https://data.nber.org/data/census_popest.html
pop_1970_2010_raw <- read_csv(
  "https://data.nber.org/census/popest/county_population.csv"
)
# pop_1970_2010_raw <- read_csv("~/Downloads/county_population.csv")

pop_1970_2010 <- pop_1970_2010_raw |>
  filter(county_fips != "0") |>
  select(
    fips,
    county_name,
    pop1970_a = pop1970,
    pop1980_a = pop1980,
    pop1990_a = pop1990,
    pop2000,
    pop2010
  ) |>
  summarize(
    .by = fips,
    county_name = paste(county_name, collapse = "; "),
    pop1970_a = first(pop1970_a, na_rm = TRUE),
    pop1980_a = first(pop1980_a, na_rm = TRUE),
    pop1990_a = first(pop1990_a, na_rm = TRUE),
    pop2000 = first(pop2000, na_rm = TRUE),
    pop2010 = first(pop2010, na_rm = TRUE),
  )


# https://www.nber.org/research/data/census-us-decennial-county-population-data-1900-1990
pop_1900_1990_raw <- read_csv(
  "https://data.nber.org/census/population/cencounts/cencounts.csv"
)
# pop_1900_1990_raw <- read_csv("~/Downloads/cencounts.csv")

pop_1900_1990 <- pop_1900_1990_raw |>
  filter(!str_ends(fips, "0")) |>
  mutate(
    .by = fips,
    across(starts_with("pop"), function(x) {
      x |> na_if(".") |> parse_number()
    })
  )

county_pop <- tidylog::full_join(pop_1900_1990, pop_1970_2010, "fips")

## Confirming the two data sources have same population (in overlaping year)
plot(county_pop$pop1970_a, county_pop$pop1970)
plot(county_pop$pop1980_a, county_pop$pop1980)
plot(county_pop$pop1990_a, county_pop$pop1990)

county_pop <- county_pop |> select(-ends_with("_a"))
county_pop_long <- county_pop |>
  select(-name, -county_name) |>
  pivot_longer(
    !fips,
    names_to = "year",
    names_pattern = "pop(.*)",
    values_to = "pop"
  ) |>
  mutate(year = parse_number(year))

# %%
## Merge in MSA classifications to get MSA population
msas <- glue("{dropbox}/data/urbanareas/metarea_final.dta") |>
  haven::read_dta() |>
  mutate(fips = sprintf("%05d", fips))

cat("Joining county population with county to MSA crosswalk")
county_pop_long <- county_pop_long |>
  tidylog::left_join(msas, y = _, by = c("fips", "year"))

## Each fip is in at most one MSA
county_pop_long |>
  summarize(.by = c(fips, year), n = n()) |>
  with(max(n))

msa_pop_long <- county_pop_long |>
  summarize(.by = c(code, metarea, year), pop = sum(pop, na.rm = TRUE))

msa_pop_long |> filter(year == 1940) |> with(hist(log(pop)))
msa_pop_long |> filter(year == 2010) |> with(hist(log(pop)))

msa_pop_long |> filter(year == 1940) |> arrange(desc(pop)) |> slice(1:20)

## Checking if 1940 and 2010 top 20 cities line up
top20_codes <- msa_pop_long |>
  filter(year == 1940) |>
  arrange(desc(pop)) |>
  slice(1:20) |>
  pull(code)

msa_pop_long |>
  filter(year == 2010) |>
  mutate(
    pop_rank = row_number(desc(pop))
  ) |>
  filter(code %in% top20_codes)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
msa_pop <- msa_pop_long |>
  arrange(year) |>
  pivot_wider(
    id_cols = c(metarea, code),
    names_from = year,
    values_from = pop,
    names_prefix = "pop_"
  ) |>
  arrange(code)

pop_rankings <- msa_pop |>
  mutate(
    rank_1940 = row_number(desc(pop_1940)),
    pct_rank_1940 = percent_rank(desc(pop_1940)),
    rank_1950 = row_number(desc(pop_1950)),
    pct_rank_1950 = percent_rank(desc(pop_1950)),
    rank_1960 = row_number(desc(pop_1960)),
    pct_rank_1960 = percent_rank(desc(pop_1960)),
    rank_1970 = row_number(desc(pop_1970)),
    pct_rank_1970 = percent_rank(desc(pop_1970)),
    rank_1980 = row_number(desc(pop_1980)),
    pct_rank_1980 = percent_rank(desc(pop_1980)),
    rank_1990 = row_number(desc(pop_1990)),
    pct_rank_1990 = percent_rank(desc(pop_1990)),
    rank_2000 = row_number(desc(pop_2000)),
    pct_rank_2000 = percent_rank(desc(pop_2000)),
    rank_2010 = row_number(desc(pop_2010)),
    pct_rank_2010 = percent_rank(desc(pop_2010))
  ) |>
  arrange(rank_1940)

ggplot() +
  geom_point(
    aes(x = pct_rank_1940, y = pct_rank_2010),
    data = pop_rankings
  ) +
  # geom_smooth(
  #   aes(x = rank_1940, y = rank_2010),
  #   data = pop_rankings,
  #   method = "lm",
  #   formula = y ~ x
  # ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  # scale_x_continuous(limits = c(1, NA)) +
  # scale_y_continuous(limits = c(1, NA)) +
  coord_equal() +
  labs(
    x = "Population ranking in 1940 (%)",
    y = "Population ranking in 2010 (%)"
  ) +
  kfbmisc::theme_kyle()


pop_rankings |> filter(rank_1940 <= 20) |> select(metarea, rank_1940, rank_2010)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_csv(
  msa_pop_long,
  glue("{dropbox}/data/urbanareas/metarea_population_1940_2010.csv")
)
