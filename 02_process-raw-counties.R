library(tidyverse)
library(tidycensus)

r70 = read_csv('data/nhgis-raw/tables/nhgis0019_csv/nhgis0019_ds94_1970_county.csv')

c70 = r70 %>% 
  transmute(
    year = 1970,
    GISJOIN,
    STATEA, COUNTYA,
    STATE, COUNTY,
    total = CCO001 + CCO002 + CCO003,
    crowded = CCO002 + CCO003
  )

r80 = read_csv('data/nhgis-raw/tables/nhgis0019_csv/nhgis0019_ds104_1980_county.csv')

c80 = r80 %>% 
  transmute(
    year = 1980,
    GISJOIN,
    STATEA, COUNTYA,
    STATE, COUNTY,
    total = C8H001 + C8H002 + C8H003 + C8H004 + C8H005 + C8H006,
    crowded = C8H002 + C8H003 + C8H005 + C8H006
  )

r90 = read_csv('data/nhgis-raw/tables/nhgis0019_csv/nhgis0019_ds120_1990_county.csv')

c90 = r90 %>% 
  transmute(
    year = 1990,
    GISJOIN,
    STATEA, COUNTYA,
    STATE, COUNTY,
    total = ESP001 + ESP002 + ESP003 + ESP004 + ESP005,
    crowded = ESP003 + ESP004 + ESP005
  )

r00 = read_csv('data/nhgis-raw/tables/nhgis0019_csv/nhgis0019_ds151_2000_county.csv')

c00 = r00 %>% 
  transmute(
    year = 2000,
    GISJOIN,
    STATEA, COUNTYA,
    STATE, COUNTY,
    total = F90001 + F90002 + F90003 + F90004 + F90005 + F90006 + F90007 + F90008 + F90009 + F90010,
    crowded = F90003 + F90004 + F90005 + F90008 + F90009 + F90010,
  )

r10 = read_csv('data/nhgis-raw/tables/nhgis0019_csv/nhgis0019_ds176_20105_2010_county.csv')

c10 = r10 %>% 
  transmute(
    year = 2010,
    GISJOIN,
    STATEA, COUNTYA,
    STATE, COUNTY,
    total = JR0E001,
    crowded = JR0E005 + JR0E006 + JR0E007 + JR0E011 + JR0E012 + JR0E013,
  )

r19 = get_acs(
  geography = 'county',
  variables = c(
    # https://www.socialexplorer.com/data/ACS2018_5yr/metadata/?ds=ACS18_5yr&table=B25014
    "total" = 'B25014_001',
    "crowded" = 'B25014_005',
    "crowded" = 'B25014_006',
    "crowded" = 'B25014_007',
    "crowded" = 'B25014_011',
    "crowded" = 'B25014_012',
    "crowded" = 'B25014_013'
  ),
  year = 2019,
  cache_table = TRUE
)

c19 = r19 %>% 
  group_by(GEOID, NAME, variable) %>% 
  summarise(estimate = sum(estimate), .groups = 'drop') %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  separate(NAME, sep = ', ', into = c('COUNTY', 'STATE')) %>% 
  mutate(
    year = 2019,
    STATEA = str_sub(GEOID, end = 2),
    COUNTYA = str_sub(GEOID, start = 3),
    GISJOIN = glue::glue('G{STATEA}0{COUNTYA}0'),
    STATE, COUNTY,
    total, crowded
  ) %>% 
  select(
    year,
    GISJOIN,
    STATEA, COUNTYA,
    STATE, COUNTY,
    total, crowded
  )

counties = bind_rows(c70, c80, c90, c00, c10, c19)

counties

counties.crowding = counties %>% 
  group_by(year) %>% 
  mutate(
    pct.crowded = crowded / total,
    usa.pct.crowded = sum(crowded, na.rm = TRUE) / sum(total, na.rm = TRUE),
    z = (pct.crowded - usa.pct.crowded) / sqrt(usa.pct.crowded * (1 - usa.pct.crowded) / total),
  ) %>% 
  ungroup() %>% 
  arrange(-z)

counties.crowding

counties.crowding %>% write_csv('data/processed/crowding-counties-history.csv')

top10.yearly = counties.crowding %>% 
  arrange(year, -z) %>% 
  group_by(year) %>% 
  top_n(10) %>%
  select(-usa.pct.crowded, -GISJOIN)

top10.yearly

top10.yearly %>% write_csv('data/processed/crowding-counties-top-10.csv')

counties.crowding %>% 
  group_by(year) %>% 
  summarise(total = sum(total), crowded = sum(crowded))
