library(tidyverse)
library(tidycensus)
library(srvyr)
library(glue)

md = read_rds('data/raw/california-pums.rds')

md

allvars = pums_variables %>% 
  filter(year == 2019, survey == 'acs5')

allvars

vars = allvars %>% 
  filter(var_code %in% names(md))

vars

md %>% 
  filter(FINCP < 0) %>% 
  count(
    race = case_when(
      HISP != '01' ~ 'latino',
      HISP == '01' & RAC1P == 1 ~ 'white',
      HISP == '01' & RAC1P == 2 ~ 'black',
      HISP == '01' & RAC1P == 6 ~ 'asian',
      TRUE ~ 'other'
    ),
    wt = PWGTP
  )
  

# tag data set with correct attributes

md %>% 
  count(RAC2P, HISP, wt = PWGTP) %>% 
  arrange(-n) %>% 
  left_join(
    vars %>% 
      filter(var_code == 'HISP') %>% 
      select(HISP = val_max, HISP_label = val_label)
  ) %>% 
  left_join(
    vars %>% 
      filter(var_code == 'RAC2P') %>% 
      select(RAC2P = val_max, RAC2P_label = val_label)
  ) %>% 
  mutate(
    RAC2P = case_when(
      HISP != '01' ~ '',
      TRUE ~ RAC2P
    ),
    RAC2P_label = case_when(
      HISP != '01' ~ '',
      TRUE ~ RAC2P_label
    )
  ) %>% 
  group_by(RAC2P, HISP, RAC2P_label, HISP_label) %>% 
  summarise(n = sum(n), .groups = 'drop') %>% 
  arrange(-n)
  # distinct(SERIALNO, .keep_all = TRUE) %>% 
  # count(RAC1P, hisp = if_else(HISP == '01', 'nothispanic', 'hispanic'), wt = PWGTP) %>% 
  
vars %>% filter(var_code == 'YBL')

coded = md %>% 
  mutate(
    SERIALNO,
    PUMA,
    people = NP,
    rooms = RMSP,
    crowded = if_else(
      NP > RMSP,
      'crowded',
      'not'
    ),
    age.bracket = cut(
      AGEP,
      breaks = c(-Inf, 14, 24, 34, 44, 54, 64, 74, Inf),
      labels = c('lt15', 'gte15_lte24', 'gte25_lte34', 'gte35_lte44', 'gte45_lte54', 'gte55_lte64', 'gte65_lte74', 'gte75')
    ),
    household.type = case_when(
      FES %in% c('1','2','3','4') ~ 'married',
      FES %in% c('5','6') ~ 'male-headed',
      FES %in% c('7','8') ~ 'female-headed',
      TRUE ~ 'other'
    ),
    immigration.year = case_when(
      CIT == '1' ~ 'native-born',
      CIT != '1' & YOEP <= '1969' ~ '1960s_before',
      CIT != '1' & YOEP <= '1979' ~ '1970s',
      CIT != '1' & YOEP <= '1989' ~ '1980s',
      CIT != '1' & YOEP <= '1999' ~ '1990s',
      CIT != '1' & YOEP <= '2009' ~ '2000s',
      CIT != '1' & YOEP <= '2019' ~ '2010s',
    ),
    race = case_when(
      HISP != '01' ~ 'latino',
      HISP == '01' & RAC1P == 1 ~ 'white',
      HISP == '01' & RAC1P == 2 ~ 'black',
      HISP == '01' & RAC1P == 6 ~ 'asian',
      TRUE ~ 'other'
    ),
    income = case_when(
      FINCP >= 0 ~ FINCP
    ),
    negative.income = FINCP < 0,
    # income.poverty.ratio = if_else(FINCP <= 0, 0, FINCP) / 75235,
    income.poverty.ratio = income / 75235,
    relative.income = cut(
      income.poverty.ratio,
      breaks = c(-Inf, 0.35, 0.5, 0.8, 1.2, 2, Inf),
      labels = c('very-low', 'low', 'moderate', 'middle', 'upper-middle', 'higher')
    ),
    tenure = case_when(
      TEN %in% c('1', '2') ~ 'owned',
      TEN %in% c('3', '4') ~ 'rented',
    ),
    when.moved = case_when(
      MV == '1' ~ 'a_12mos_less',
      MV == '2' ~ 'b_12to23months',
      MV == '3' ~ 'c_2to4years',
      MV == '4' ~ 'd_5to9years',
      MV == '5' ~ 'e_10to19years',
      MV == '6' ~ 'f_20to29years',
      MV == '7' ~ 'g_30yearsmore',
    ),
    multigen = case_when(
      MULTG == '1' ~ 'no',
      MULTG == '2' ~ 'yes',
    ),
    structure.built = case_when(
      YBL == '01' ~ '1930s_earlier',
      YBL == '02' ~ '1940s',
      YBL == '03' ~ '1950s',
      YBL == '04' ~ '1960s',
      YBL == '05' ~ '1970s',
      YBL == '06' ~ '1980s',
      YBL == '07' ~ '1990s',
      YBL %in% c('08','09','10','11','12','13') ~ '2000s',
      YBL %in% c('14','15','16','17','18','19','20','21','22','23') ~ '2010s',
    ),
    race.detailed = case_when(
      HISP == '02' ~ 'mexican',
      HISP == '11' ~ 'salvadoran',
      HISP == '07' ~ 'guatemalan',
      HISP == '03' ~ 'puerto-rican',
      RAC2P == '01' & HISP == '01' ~ 'white',
      RAC2P == '02' & HISP == '01' ~ 'black',
      RAC2P == '43' & HISP == '01' ~ 'chinese',
      RAC2P == '45' & HISP == '01' ~ 'filipino',
      RAC2P == '38' & HISP == '01' ~ 'asian-indian',
      RAC2P == '57' & HISP == '01' ~ 'vietnamese',
      RAC2P == '49' & HISP == '01' ~ 'korean',
      RAC2P == '48' & HISP == '01' ~ 'japanese',
      TRUE ~ 'all-others',
    ),
    latino.type = case_when(
      HISP != '01' & CIT == '1' ~ 'native-born-latino',
      HISP != '01' & CIT != '1' ~ 'foriegn-born-latino',
      TRUE ~ 'other'
    )
  )

coded

survey.obj = coded %>% 
  distinct(SERIALNO, .keep_all = TRUE) %>%
  # filter(str_starts(PUMA, '059')) %>% # oc only
  # filter(str_starts(PUMA, '037')) %>% # la only
  to_survey(type = 'housing')

survey.obj

survey.obj %>% 
  survey_count(crowded) %>% 
  mutate(pct = n / sum(n))

survey.obj %>% 
  survey_count(age.bracket, crowded) %>%
  group_by(age.bracket) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>% 
  filter(crowded == 'crowded')

survey.obj %>% 
  survey_count(household.type, crowded) %>%
  group_by(household.type) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>% 
  filter(crowded == 'crowded')

survey.obj %>% 
  survey_count(immigration.year, crowded) %>%
  group_by(immigration.year) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>% 
  filter(crowded == 'crowded')

survey.obj %>% 
  survey_count(race, crowded) %>%
  group_by(race) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>% 
  filter(crowded == 'crowded')

survey.obj %>% 
  survey_count(negative.income) %>%
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  )

survey.obj %>% 
  survey_count(relative.income, crowded) %>%
  group_by(relative.income) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>% 
  filter(crowded == 'crowded')

survey.obj %>% 
  survey_count(multigen, crowded) %>%
  group_by(multigen) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>% 
  filter(crowded == 'crowded')

survey.obj %>% 
  survey_count(when.moved, crowded) %>%
  group_by(when.moved) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>% 
  filter(crowded == 'crowded')

survey.obj %>% 
  survey_count(structure.built, crowded) %>%
  group_by(structure.built) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>% 
  filter(crowded == 'crowded')

survey.obj %>% 
  survey_count(race.detailed, crowded) %>%
  group_by(race.detailed) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>% 
  filter(crowded == 'crowded') %>% 
  arrange(-n)

survey.obj %>% 
  survey_count(latino.type, crowded) %>%
  group_by(latino.type) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>%
  filter(crowded == 'crowded') %>% 
  arrange(-n)

survey.obj %>% 
  survey_count(negative.income, race) %>% 
  filter(negative.income) %>% 
  mutate(n / sum(n))

race.income.breakdown = survey.obj %>% 
  survey_count(race, relative.income, crowded) %>% 
  group_by(race, relative.income) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>%
  filter(crowded == 'crowded') %>% 
  arrange(-n)

race.income.breakdown

plot.crowding.by.income = race.income.breakdown %>% 
  filter(race != 'other') %>% 
  drop_na(relative.income) %>%
  ungroup() %>% 
  ggplot(aes(relative.income, pct, group = race, color = race)) +
  geom_line() +
  labs(
    title = 'Crowding by relative income',
    x = 'Relative income bracket',
    y = 'Percent of housing units crowded',
    caption = 'California data'
  ) +
  theme_minimal()

plot.crowding.by.income

ggsave(filename = 'plots/crowding-by-income.png', plot = plot.crowding.by.income, width = 12, height = 8)

plot.unit.size.by.race = survey.obj %>% 
  survey_count(people, race) %>% 
  group_by(race) %>% 
  mutate(
    pct = n / sum(n),
    wtmean = sum(n * people) / sum(n)
  ) %>% 
  ungroup() %>% 
  filter(race != 'other') %>% 
  ggplot(aes(people, pct * 100)) +
  geom_bar(stat = 'identity') +
  geom_vline(aes(xintercept = wtmean), color = 'red') +
  facet_wrap(. ~ race, ncol = 1) +
  scale_x_continuous(limits = c(0, 10), breaks = 1:10) +
  labs(
    x = 'Number of people in unit',
    y = 'Percent of race total',
    title = 'Distribution of size of unit by race',
    subtitle = 'Average number of people for race',
    caption = 'California only'
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(color = 'red')
  )

plot.unit.size.by.race

ggsave(filename = 'plots/unit-size-by-race.png', plot = plot.unit.size.by.race, width = 12, height = 8)

plot.rooms.by.four.person.unit = survey.obj %>% 
  filter(people == 4) %>% 
  filter(race != 'other') %>% 
  survey_count(race, rooms) %>% 
  group_by(race) %>% 
  mutate(
    pct = n / sum(n),
    wtmean = sum(n * rooms) / sum(n),
    crowd = sum(pct[rooms < 4]),
    race = glue('{race}: {round(crowd * 100, 1)}% crowded')
  ) %>% 
  filter(rooms < 10) %>% 
  ggplot(aes(rooms, pct * 100, fill = if_else(rooms < 4, 'crowded', 'notcrowded'))) +
  geom_bar(stat = 'identity') +
  geom_text(
    data = . %>% filter(rooms < 4),
    aes(label = glue('{round(pct * 100, 1)}%')),
    nudge_y = 3
  ) +
  geom_vline(aes(xintercept = wtmean), color = 'red') +
  facet_wrap(. ~ race, ncol = 1) +
  scale_x_continuous(breaks = 1:9) +
  labs(
    fill = '',
    title = 'Number of rooms for a housing unit with four people',
    subtitle = 'Average number of rooms for race group',
    y = 'Percent of race group total',
    x = 'Rooms',
    caption = 'Data for all of California'
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(color = 'red')
  )

plot.rooms.by.four.person.unit

ggsave(filename = 'plots/rooms-by-four-person-unit.png', plot = plot.rooms.by.four.person.unit, width = 12, height = 8)

plot.crowding.by.income.four.person.unit = survey.obj %>% 
  filter(people == 4) %>% 
  filter(race != 'other') %>% 
  survey_count(race, relative.income, crowded) %>% 
  group_by(race, relative.income) %>% 
  mutate(
    se.pct = n_se / n * 100,
    pct = n / sum(n) * 100
  ) %>%
  filter(crowded == 'crowded') %>% 
  ungroup() %>% 
  drop_na(relative.income) %>% 
  ggplot(aes(relative.income, pct, group = race, color = race)) +
  geom_line() +
  labs(
    title = 'Crowding by relative income',
    subtitle = '**Four person household units only**',
    x = 'Relative income bracket',
    y = 'Percent of housing units crowded',
    caption = 'Data for all of California'
  ) +
  theme_minimal()

plot.crowding.by.income.four.person.unit

ggsave(filename = 'plots/crowding-by-income-four-person-unit.png', plot = plot.crowding.by.income.four.person.unit, width = 12, height = 8)
