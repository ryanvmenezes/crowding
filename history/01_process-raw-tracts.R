library(tidyverse)
library(tidycensus)

# 1960 --------------------------------------------------------------------

t60raw = read_csv('data/nhgis-raw/tables/nhgis0016_csv/nhgis0016_ds92_1960_tract.csv')

t60raw

# states missing
fips_codes %>% 
  as_tibble() %>% 
  distinct(state, state_code, state_name) %>% 
  left_join(t60raw %>% count(STATEA), by = c('state_code' = 'STATEA')) %>% 
  filter(is.na(n))

t60clean = t60raw %>%
  transmute(
    year = 1960,
    GISJOIN,
    COUNTY,
    total = B76001 + B76002 + B76003 + B76004,
    crowded = B76004,
  )

t60clean

t60clean %>% 
  count(county = str_sub(GISJOIN, end = 8)) %>% 
  filter(county %in% c('G0600370', 'G0600590'))

# 1970 --------------------------------------------------------------------

t70raw = read_csv('data/nhgis-raw/tables/nhgis0015_csv/nhgis0015_ds96_1970_tract.csv')

t70raw

# no states missing
fips_codes %>% 
  as_tibble() %>% 
  distinct(state, state_code, state_name) %>% 
  left_join(t70raw %>% count(STATEA), by = c('state_code' = 'STATEA')) %>% 
  filter(is.na(n))

t70clean = t70raw %>%
  transmute(
    year = 1970,
    GISJOIN,
    COUNTY,
    total = CLH001 + CLH002 + CLH003,
    crowded = CLH002 + CLH003,
  )

t70clean

t70clean %>% 
  count(county = str_sub(GISJOIN, end = 8)) %>% 
  filter(county %in% c('G0600370', 'G0600590'))

# 1980 --------------------------------------------------------------------

t80raw = read_csv('data/nhgis-raw/tables/nhgis0014_csv/nhgis0014_ds104_1980_tract.csv', guess_max = 50000)

t80raw

t80clean = t80raw %>%
  transmute(
    year = 1980,
    GISJOIN,
    COUNTY,
    total = C8G001 + C8G002 + C8G003,
    crowded = C8G002 + C8G003,
  )

t80clean

t80clean %>% 
  count(county = str_sub(GISJOIN, end = 8)) %>% 
  filter(county %in% c('G0600370', 'G0600590'))

# 1990 --------------------------------------------------------------------

t90raw = read_csv('data/nhgis-raw/tables/nhgis0012_csv/nhgis0012_ds120_1990_tract.csv', guess_max = 1e5)

t90raw

t90clean = t90raw %>%
  transmute(
    year = 1990,
    GISJOIN,
    COUNTY,
    total = ESP001 + ESP002 + ESP003 + ESP004 + ESP005,
    crowded = ESP003 + ESP004 + ESP005,
  )

t90clean

t90clean %>% 
  count(county = str_sub(GISJOIN, end = 8)) %>% 
  filter(county %in% c('G0600370', 'G0600590'))

# 2000 --------------------------------------------------------------------

t00raw = read_csv('data/nhgis-raw/tables/nhgis0013_csv/nhgis0013_ds151_2000_tract.csv', guess_max = 1e5)

t00raw

t00clean = t00raw %>% 
  transmute(
    year = 2000,
    GISJOIN,
    COUNTY,
    total = F90001 + F90002 + F90003 + F90004 + F90005 + F90006 + F90007 + F90008 + F90009 + F90010,
    crowded = F90003 + F90004 + F90005 + F90008 + F90009 + F90010,
  )

t00clean

t00clean %>% 
  count(county = str_sub(GISJOIN, end = 8)) %>% 
  filter(county %in% c('G0600370', 'G0600590'))

# 2000 / 2019 -------------------------------------------------------------

get.table = function(s, y) {
  get_acs(
    geography = 'tract',
    state = s,
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
    year = y,
    cache_table = TRUE
  )
}

api.get = fips_codes %>% 
  as_tibble() %>% 
  select(contains('state')) %>% 
  distinct() %>% 
  # take out these territories, the get_acs function call breaks on them
  filter(!(state %in% c('AS','GU','MP','UM','VI'))) %>% 
  mutate(years = map(state, ~tibble(year = c(2010, 2019)))) %>% 
  unnest(c(years)) %>% 
  mutate(api.data = map2(state_code, year, get.table))

api.get

t1019clean = api.get %>% 
  unnest(c(api.data)) %>% 
  mutate(
    state = str_sub(GEOID, end = 2),
    county = str_sub(GEOID, start = 3, end = 5),
    tract = str_sub(GEOID, start = 6),
  ) %>% 
  left_join(
    fips_codes %>% 
      select(state = state_code, county = county_code, county.name = county)
  ) %>% 
  transmute(
    year,
    GISJOIN = glue::glue('G{state}0{county}0{tract}'),
    COUNTY = county.name,
    variable,
    estimate
  ) %>% 
  group_by(year, GISJOIN, COUNTY, variable) %>% 
  summarise(estimate = sum(estimate), .groups = 'drop') %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  select(-crowded, crowded)

t1019clean

t1019clean %>% 
  count(year, county = str_sub(GISJOIN, end = 8)) %>% 
  filter(county %in% c('G0600370', 'G0600590'))

# combine -----------------------------------------------------------------

clean = bind_rows(
  t60clean,
  t70clean,
  t80clean,
  t90clean,
  t00clean,
  t1019clean
)

clean %>% write_csv('data/processed/crowding-tracts-history.csv')
