library(tidyverse)

tractdata = read_csv('data/processed/crowding-tracts-history.csv')

tractdata

xwalk = read_csv('data/processed/tract-mapla-xwalk.csv')

xwalk

laoc = tractdata %>% 
  filter(str_sub(GISJOIN, end = 8) %in% c('G0600370', 'G0600590'))

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
    total = sum(total.p, na.rm = TRUE),
    crowded = sum(crowded.p, na.rm = TRUE),
    pct.crowded = crowded / total,
    .groups = 'drop'
  )

mapla.aggregates

mapla.aggregates %>% 
  count(year, county) %>% 
  head(20)

mapla.aggregates %>% write_csv('data/processed/mapla-crowding-history.csv', na = '')

mapla.shp = read_sf('data/processed/mapping-la-neighborhoods.geojson') %>% 
  filter(hood.name != 'Avalon', !str_detect(hood.name, 'Catalina'))

mapla.shp

# mapla.shp %>% ggplot() + geom_sf()

mapla.aggregates %>%
  filter(county == 'orange')

places.of.interest = mapla.shp %>% 
  filter(
    hood.name %in% c(
      'Pico-Union',
      'Santa Ana',
      'Pacoima',
      # 'Historic South-Central',
      'Bell Gardens',
      'Lennox'
    )
  ) %>% 
  st_centroid() %>% 
  mutate(
    x = map_dbl(geometry, ~.x[1]),
    y = map_dbl(geometry, ~.x[2])
  )

places.of.interest

plt.by.year = mapla.shp %>% 
  left_join(mapla.aggregates) %>% 
  ggplot() +
  geom_sf(aes(fill = pct.crowded), color = NA) +
  geom_sf(
    data = places.of.interest,
    size = 0.1,
    color = 'white'
  ) +
  geom_text(
    data = places.of.interest,
    aes(x = x, y = y, label = hood.name),
    color = 'white',
    hjust = 'left',
    nudge_x = 0.01,
    size = 1
  ) +
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
        geom_sf(
          data = places.of.interest,
          size = 0.1,
          color = 'white'
        ) +
        geom_text(
          data = places.of.interest,
          aes(x = x, y = y, label = hood.name),
          color = 'white',
          hjust = 'left',
          nudge_x = 0.01,
          size = 1
        ) +
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
  ~ggsave(filename = glue::glue('plots/plot-{.y}.pdf'), plot = .x, width = 10, height = 15)
)