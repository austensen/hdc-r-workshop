# R for NYC Housing Data Analysis
# A workshop for Housing Data Coalition on using R to analyze NYC housing data 
# Maxwell Austensen
# "2018-12-08"


# Load Tidyverse Packages -------------------------------------------------

library(dplyr) # manipulate dataframes
library(readr) # read/write dataframes
library(tidyr) # reshapeing dataframes
library(stringr) # string manipulation
library(forcats) # factor manipulation
library(purrr) # iteration (instead of loops)
library(ggplot2) # making plots


# Import Watchlist Data ---------------------------------------------------

watchlist_bldgs <- read_csv("data/landlord-watchlist-buildings_2018-12-08.csv")

watchlist_bldgs <- read_csv(
  file = "data/landlord-watchlist-buildings_2018-12-08.csv",
  col_types = cols(
    .default = col_character(),
    units = col_integer(),
    violations = col_integer()
  )
)

watchlist_bldgs

glimpse(watchlist_bldgs)


# filter() ----------------------------------------------------------------

bk_bldgs <- filter(watchlist_bldgs, borough == "BROOKLYN")

bk_bldgs


bk_big_bldgs <- filter(watchlist_bldgs, units > 10, borough == "QUEENS")

bk_big_bldgs


# select() ----------------------------------------------------------------

select(watchlist_bldgs, landlord, borough, units)

select(watchlist_bldgs, -landlord)


select(watchlist_bldgs, borough_name = borough)

rename(watchlist_bldgs, landlord_name = landlord)


# mutate() ----------------------------------------------------------------

mutate(watchlist_bldgs, landlord_lower = str_to_lower(landlord))

transmute(watchlist_bldgs, violations_per_unit = violations / units)


# arrange() ---------------------------------------------------------------

arrange(watchlist_bldgs, landlord, desc(units))

summarise(watchlist_bldgs, total_units = sum(units))


# group_by() --------------------------------------------------------------

boro_bldgs <- group_by(watchlist_bldgs, borough)
boro_bldgs <- summarise(boro_bldgs, total_units = sum(units))
boro_bldgs <- ungroup(boro_bldgs)

boro_bldgs


# Data manipulation pipelines with `%>%` ("pipe") -------------------------

str_c("a", "b")

"a" %>% str_c("b")

bk_landlords <- watchlist_bldgs %>% 
  filter(borough == "BROOKLYN") %>% 
  mutate(landlord_name = str_to_title(landlord)) %>% 
  select(landlord_name, units, violations) %>% 
  group_by(landlord_name) %>% 
  summarize(
    buildings = n(),
    total_units = sum(units),
    avg_bldg_size = mean(units),
    total_viol = sum(violations),
    avg_bldg_viol = mean(violations)
  ) %>% 
  ungroup() %>% 
  arrange(desc(buildings))

bk_landlords


# Making graphs with ggplot2 ----------------------------------------------

ggplot(data = bk_landlords) +
  aes(x = landlord_name, y = total_viol) +
  geom_col() +
  labs(
    title = '"Worst" Landlords in Brooklyn',
    subtitle = "Total HPD Violations in All Buildings for 2017",
    x = NULL,
    y = "Number of Violations",
    caption = "Source: NYC Public Advocate's Landlord Watchlist"
  )

landlord_bk_10_worst <- bk_landlords %>% 
  arrange(desc(total_viol)) %>% 
  filter(row_number() <= 10) %>% 
  mutate(
    landlord_name = str_remove(landlord_name, " Properties"),
    landlord_name = fct_reorder(landlord_name, total_viol)
  )

ggplot(data = landlord_bk_10_worst) +
  aes(x = landlord_name, y = total_viol) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(margin = margin(r = -15)),
    plot.caption = element_text(face = "italic", color = "darkgrey", margin = margin(t = 10))
  ) +
  labs(
    title = '10 "Worst" Landlords in Brooklyn',
    subtitle = "Total HPD Violations in All Buildings for 2017",
    x = NULL,
    y = "Number of Violations",
    caption = "Source: NYC Public Advocate's Landlord Watchlist"
  )


# Geocoding addresses with Geoclient API ----------------------------------

library(geoclient) # remotes::install_github("austensen/geoclient")

geoclient_api_keys(
  id = getPass::getPass("geoclient ID:"),
  key = getPass::getPass("geoclient KEY:"),
  install = TRUE,
  overwrite = TRUE
)

readRenviron("~/.Renviron")


geo_info <- watchlist_bldgs %>% 
  geo_address_data(number, street, borough, zip) %>% 
  select(
    bbl, 
    bin = buildingIdentificationNumber, 
    tract_2010 = censusTract2010,
    cd = communityDistrict,
    nta,
    lat = latitudeInternalLabel,
    lng = longitudeInternalLabel
  )

geo_info


write_rds(geo_info, "data/geo_info.rds")

# geo_info <- read_rds("data/geo_info.rds")


watchlist_bldgs_geos <- bind_cols(watchlist_bldgs, geo_info)

watchlist_bldgs_geos


# Querying the NYC-DB SQL Database ----------------------------------------

library(DBI)

con <- dbConnect(
  RPostgres::Postgres(),
  user = getPass::getPass("NYC-DB USER:"),
  password = getPass::getPass("NYC-DB PASSWORD:"),
  host = getPass::getPass("NYC-DB HOST:"),
  port = 5432,
  dbname = "nycdb"
)

dbListTables(con)

hpd_viol <- tbl(con, "hpd_violations")

hpd_viol

glimpse(hpd_viol)

watchlist_bbls <- pull(watchlist_bldgs_geos, bbl)

violation_bbls <- hpd_viol %>% 
  filter(
    bbl %in% watchlist_bbls, # only get the BBLs we need
    sql("novdescription ~ '27-20(2[8-9]|3[0-3])'"), # heat violations
    sql("novissueddate between '2017-10-01' and '2018-05-31'") # 2017-2018 heat season
  ) %>% 
  group_by(bbl) %>% 
  summarise(heat_viols = n())

violation_bbls


heat_violations <- collect(violation_bbls)

watchlist_bldgs_geos_heat <- inner_join(watchlist_bldgs_geos, heat_violations, by = "bbl")


# Spatial Data in R -------------------------------------------------------

library(sf)

watchlist_bldgs_spatial <- st_as_sf(watchlist_bldgs_geos_heat, coords = c("lng", "lat"), crs = 4326)

watchlist_bldgs_spatial


library(mapview)

mapview(watchlist_bldgs_spatial)
