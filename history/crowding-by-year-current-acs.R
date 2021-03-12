library(tidyverse)
library(tidycensus)

get.table.for.year = function(y) {
  get_acs(
    geography = 'county',
    variables = c(
      # https://www.socialexplorer.com/data/ACS2018_5yr/metadata/?ds=ACS18_5yr&table=B25014
      "total" = 'B25014_001',
      "owner_occ" = 'B25014_002',
      "owner_occ_0.5_or_less_per_room" = 'B25014_003',
      "owner_occ_0.51_to_1.0_per_room" = 'B25014_004',
      "owner_occ_1.01_to_1.5_per_room" = 'B25014_005',
      "owner_occ_1.51_to_2.0_per_room" = 'B25014_006',
      "owner_occ_2.01_or_more_per_room" = 'B25014_007',
      "renter_occ" = 'B25014_008',
      "renter_occ_0.5_or_less_per_room" = 'B25014_009',
      "renter_occ_0.51_to_1.0_per_room" = 'B25014_010',
      "renter_occ_1.01_to_1.5_per_room" = 'B25014_011',
      "renter_occ_1.51_to_2.0_per_room" = 'B25014_012',
      "renter_occ_2.01_or_more_per_room" = 'B25014_013'
    ),
    year = y,
    cache_table = TRUE
  )
}

data = tibble(year = 2010:2019) %>% 
  mutate(data = map(year, get.table.for.year))

data

counties.by.year = data %>% 
  unnest(c(data)) %>% 
  mutate(
    crowdtag = case_when(
      variable == "owner_occ_0.5_or_less_per_room" ~ 'notcrowded',
      variable == "owner_occ_0.51_to_1.0_per_room" ~ 'notcrowded',
      variable == "owner_occ_1.01_to_1.5_per_room" ~ 'crowded',
      variable == "owner_occ_1.51_to_2.0_per_room" ~ 'crowded',
      variable == "owner_occ_2.01_or_more_per_room" ~ 'crowded',
      variable == "renter_occ_0.5_or_less_per_room" ~ 'notcrowded',
      variable == "renter_occ_0.51_to_1.0_per_room" ~ 'notcrowded',
      variable == "renter_occ_1.01_to_1.5_per_room" ~ 'crowded',
      variable == "renter_occ_1.51_to_2.0_per_room" ~ 'crowded',
      variable == "renter_occ_2.01_or_more_per_room" ~ 'crowded',
    )
  ) %>% 
  drop_na(crowdtag) %>% 
  group_by(year, GEOID, NAME, crowdtag) %>% 
  summarise(total = sum(estimate), .groups = 'drop') %>% 
  pivot_wider(names_from = 'crowdtag', values_from = 'total') %>% 
  mutate(
    total = crowded + notcrowded,
    pct.crowded = crowded / total
  ) %>%
  group_by(year) %>% 
  mutate(us.avg = sum(crowded) / sum(total)) %>% 
  mutate(
    zval = (pct.crowded - us.avg) / sqrt(us.avg * (1 - us.avg) / total),
    pct.rank = rank(pct.crowded),
    zval.rank = rank(desc(zval))
  )

counties.by.year

counties.by.year %>% 
  arrange(zval.rank) %>% 
  filter(zval.rank <= 10) %>% 
  select(NAME, year, z = zval.rank, pct = pct.crowded) %>% 
  pivot_wider(names_from = year, values_from = c(z, pct))
  