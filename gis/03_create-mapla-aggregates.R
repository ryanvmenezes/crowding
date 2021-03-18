library(tidyverse)

tractdata = read_csv('data/processed/crowding-tracts-history.csv')

tractdata

xwalk = read_csv('data/processed/tract-mapla-xwalk.csv')

xwalk

laoc = tractdata %>% 
  filter(str_sub(GISJOIN, end = 8) %in% c('G0600370', 'G0600590'))

laoc

laoc %>% 
  group_by(year, COUNTY) %>% 
  summarise(total = sum(total, na.rm = TRUE))

laoc %>% 
  anti_join(xwalk) %>%
  count(year)

# number of tracts per year after apportionment is taken into account
laoc %>%
  left_join(xwalk) %>%
  group_by(year) %>%
  summarise(sum(pct.tract, na.rm = TRUE))

# laoc %>% 
#   left_join(xwalk) %>% 
#   arrange(year, GISJOIN)

mapla.aggregates = laoc %>%
  left_join(xwalk) %>%
  mutate(
    total.p = pct.tract * total,
    crowded.p = pct.tract * crowded
  ) %>% 
  group_by(year, hood.name, county, region) %>% 
  summarise(
    total = sum(total.p, na.rm = TRUE),
    crowded = sum(crowded.p, na.rm = TRUE),
    pct.crowded = crowded / total,
    .groups = 'drop'
  )

mapla.aggregates

mapla.aggregates %>% 
  group_by(year, county) %>% 
  summarise(total = sum(total)) %>% 
  drop_na(county)

mapla.aggregates %>% 
  count(year, county) %>% 
  head(20)

mapla.aggregates %>% write_csv('data/processed/mapla-crowding-history.csv', na = '')

