library(tidyverse)
library(censusapi)
library(tigris)
library(sf)
library(mapview)
Sys.setenv(CENSUS_KEY="a3eb4302132d764b239a08bcd847a97847e05c88")

income <- 
  getCensus(
    name = "acs/acs5",
    vintage = 2019,
    region = "block group:*",
    regionin = "state:06+county:081",
    vars = "group(B19001)" 
  )

income2 <- income %>%
  transmute(
    GEOID =
      paste0(state,county,tract,block_group),
    pop = B19001_001E,
    high_income = (B19001_016E + B19001_017E)/pop 
  )

smc_cbgs <- block_groups(
  state = "CA", 
  county = "San Mateo"
)

income_map <- smc_cbgs %>%
  left_join(income2)
mapview(income_map, zcol = "high_income")

income_education <- 
  getCensus(
    name = "acs/acs5",
    vintage = 2019,
    region = "tract:*",
    regionin = "state:06+county:081,085",
    vars = c("B19001_001E","B19001_016E","B19001_017E",
             "B06009_001E","B06009_005E","B06009_006E")
  ) %>%
  mutate(
    GEOID = paste0(state,county,tract),
    high_income = (B19001_016E+B19001_017E)/B19001_001E,
    college = (B06009_005E+B06009_006E)/B06009_001E
  )

hist(income_education$high_income)
hist(income_education$college)

ggplot(income_education,aes(
      x = high_income,
      y = college
  )) +
  geom_point() + 
  geom_smooth(method="lm")

smc_tracts <- tracts("CA", "San Mateo")
scc_tracts <- tracts("CA", "Santa Clara")

income_map <- 
  rbind(smc_tracts,scc_tracts) %>%
  left_join(income_education)