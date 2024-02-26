library(tidyverse)
library(censusapi)
library(tigris)
library(sf)
library(mapview)
Sys.setenv(CENSUS_KEY="a3eb4302132d764b239a08bcd847a97847e05c88")

income_sf <- 
  getCensus(
    name = "acs/acs5",
    vintage = 2019,
    region = "block group:*",
    regionin = "state:06+county:075",
    vars = "group(B19001)" 
  )

income2_sf <- income_sf %>%
  transmute(
    GEOID =
      paste0(state,county,tract,block_group),
    pop = B19001_001E,
    high_income = (B19001_016E + B19001_017E)/pop 
  )

sfc_cbgs <- block_groups(
  state = "CA", 
  county = "San Francisco"
)

income_map_sf <- sfc_cbgs %>%
  left_join(income2_sf)
mapview(income_map_sf, zcol = "high_income")
