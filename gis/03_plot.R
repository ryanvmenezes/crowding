library(tidyverse)

tractdata = read_csv('data/processed/crowding-tracts-history.csv')

tractdata

xwalk = read_csv('data/processed/tract-mapla-xwalk.csv')

xwalk

laoc = tractdata %>% 
  filter(state == '06', county %in% c('037', '059')) %>% 
  unite('tract', state, county, tract, sep = '') %>% 
  select(-county.name)

laoc %>% 
  anti_join(xwalk) %>% 
  count(year)

mapla.aggregates = laoc %>% 
  left_join(xwalk) %>% 
  mutate(
    total.p = pct.mapla * total,
    crowded.p = pct.mapla * crowded
  ) %>% 
  group_by(year, hood.name, county, region) %>% 
  summarise(
    total = sum(total.p),
    crowded = sum(crowded.p),
    pct.crowded = crowded / total,
    .groups = 'drop'
  )

mapla.aggregates

mapla.aggregates %>% 
  count(year, county) %>% 
  head(20)

mapla.shp = read_sf('data/processed/mapping-la-neighborhoods.geojson') %>% 
  filter(hood.name != 'Avalon', !str_detect(hood.name, 'Catalina'))

mapla.shp

mapla.shp %>% ggplot() + geom_sf()

mapla.aggregates %>% 
  filter(county == 'orange')

plt.by.year = mapla.shp %>% 
  left_join(mapla.aggregates) %>% 
  ggplot() +
  geom_sf(aes(fill = pct.crowded), color = NA) +
  facet_wrap(. ~ year, nrow = 1) +
  scale_fill_continuous(high = '#00441b', low = '#a1d99b') +
  theme_minimal()

ggsave(
  plt.by.year,
  filename = 'plots/la-oc-by-year.pdf',
  width = 40,
  height = 10
)


plots = mapla.aggregates %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    plot = map2(
      data, 
      year,
      ~.x %>% 
        left_join(mapla.shp) %>% 
        st_as_sf() %>% 
        ggplot() +
        geom_sf(aes(fill = pct.crowded), color = NA) +
        scale_fill_continuous(high = '#00441b', low = '#a1d99b') +
        theme_minimal() +
        labs(
          title = glue::glue('LA/OC crowding in {.y}')
        )
    )
  )

plots

walk2(
  plots$plot,
  plots$year,
  ~ggsave(filename = glue::glue('plot-{.y}.pdf'), plot = .x, width = 10, height = 15)
)
