library(tidyverse)

tracts = read_csv('data/processed/crowding-tracts-history.csv')

tracts

usa = tracts %>% 
  group_by(year) %>% 
  summarise(
    total = sum(total, na.rm = TRUE),
    crowded = sum(crowded, na.rm = TRUE),
    pct.crowded = crowded / total
  )

usa

usa %>% 
  ggplot(aes(year, pct.crowded)) +
  geom_line() + 
  geom_point()

counties = tracts %>% 
  group_by(year, state, county, county.name) %>% 
  summarise(
    total = sum(total, na.rm = TRUE),
    crowded = sum(crowded, na.rm = TRUE),
    pct.crowded = crowded / total,
    .groups = 'drop'
  ) %>% 
  left_join(
    usa %>% 
      select(year, usa.crowded = pct.crowded)
  ) %>% 
  mutate(
    z = (pct.crowded - usa.crowded) / sqrt(usa.crowded * (1 - usa.crowded) / total),
    # county.name = str_remove(county.name, ' County')
  )

counties

counties %>% write_csv('data/processed/crowding-counties-history.csv')

top10.by.year = counties %>%
  arrange(year, -z) %>% 
  group_by(year) %>% 
  top_n(10) %>% 
  mutate(
    zrank = rank(-z),
    # county = case_when(
    #   state == '12' & county == '025' ~ '086',
    #   TRUE ~ county
    # )
  ) %>%
  # select(-county.name) %>%
  left_join(
    tidycensus::fips_codes %>% 
      as_tibble() %>% 
      # select(state = state_code, county = county_code, county.name = county, state.name = state_name)
      distinct(state = state_code, state.name = state_name)
  ) %>% 
  select(year, zrank, county.name, state.name, year, pct = pct.crowded) # %>%
  # pivot_wider(
  #   id_cols = zrank,
  #   names_from = year,
  #   values_from = c(county.name, state.name, pct)
  # ) %>% 
  # select(zrank, ends_with('60'), ends_with('70'), ends_with('80'), ends_with('90'), ends_with('00'), ends_with('10'), ends_with('19'))

top10.by.year

top10.by.year %>% write_csv('data/processed/crowded-counties-top-10.csv', na = '')

top5.2019.history = counties %>% 
  filter(
    (state == '06' & county %in% c('037', '059')) |
      (state == '36' & county %in% c('047', '005', '081'))
  ) %>% 
  select(year, county.name, pct.crowded) %>% 
  mutate(county.name = str_remove(county.name, ' County')) %>% 
  bind_rows(
    usa %>% 
      transmute(
        year,
        county.name = 'USA',
        pct.crowded
      )
  )

top5.2019.history

top5.2019.history %>% 
  pivot_wider(names_from = county.name, values_from = pct.crowded)

top5.2019.history %>% 
  ggplot(aes(year, pct.crowded, color = county.name)) +
  geom_line() +
  geom_point() +
  geom_text(
    data = . %>% 
      filter(year == max(year)),
    aes(label = county.name),
    hjust = 'left',
    nudge_x = .5
  ) +
  scale_x_continuous(limits = c(1960, 2025)) +
  theme_minimal() +
  theme(
    legend.position = 'none'
  )
