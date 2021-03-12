library(tidyverse)
library(tidycensus)

pums_variables

pums_variables %>% count(level)

pums_variables %>% count(year)

pums_variables %>% 
  filter(year == 2019) %>% 
  filter(level == 'person') %>% 
  filter(survey == 'acs5') %>% 
  # distinct(var_code, var_label) %>%
  view()

# RMSP number of rooms
# NP number of people

ut_ten <- get_pums(
  variables = "TEN",
  state = "Utah",
  survey = "acs1",
  year = 2018,
  rep_weights = "housing"
)

ut_ten

homes = ut_ten %>%
  # count(SERIALNO) %>% 
  # filter(n > 1) %>% 
  # left_join(ut_ten) %>% 
  group_by(SERIALNO) %>% 
  filter(SPORDER == 1) %>% 
  ungroup()

homes

homes2 = ut_ten %>% 
  distinct(SERIALNO, .keep_all = TRUE)

homes2

homes %>% 
  count(TEN, wt = PWGTP)

homes2 %>% 
  to_survey(type = 'housing') %>% 
  srvyr::survey_count(TEN)


homes2 %>% 
  distinct(SERIALNO, SPORDER) %>% 
  anti_join(
    homes %>% 
      distinct(SERIALNO, SPORDER)    
  )



