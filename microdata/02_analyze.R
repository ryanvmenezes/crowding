library(tidyverse)
library(tidycensus)
library(srvyr)

md = read_rds('data/raw/california-pums.rds')

md

allvars = pums_variables %>% 
  filter(year == 2019, survey == 'acs5')

allvars

vars = allvars %>% 
  filter(var_code %in% names(md))

vars

# tag data set with correct attributes

md %>% 
  count(YBL, wt = PWGTP)
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
    income.poverty.ratio = if_else(FINCP <= 0, 0, FINCP) / 75235,
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
    )
  )

coded

# coded %>% 
#   distinct(SERIALNO, .keep_all = TRUE) %>% 
#   count(structure.built, wt = PWGTP) %>% 
#   mutate(total = sum(n), pct = n / sum(n))

survey.obj = coded %>% 
  distinct(SERIALNO, .keep_all = TRUE) %>%
  filter(str_starts(PUMA, '059')) %>% # one county only
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
