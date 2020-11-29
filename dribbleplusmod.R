library(tidyverse)
library(lme4)

rawdata = read.csv('../rawdata/tf_events_2020.csv', na.strings="", encoding = 'UTF-8')


procd=rawdata %>% 
  #add info about next action
  mutate(action.lead = lead(goals_added_action_type),
    value.lead = lead(goals_added_dp_weighted)) %>%
  #filter for dribbles, next action is same player
  filter(!is.na(value.lead), 
         lead(player_name) == player_name,
         action.lead != 'Interrupting',
         action.lead != 'Dribbling') %>%
  filter(goals_added_action_type =='Dribbling')

totalmod=lmer(value.lead ~ (1|player_name), data = procd)
totalcoefs=coef(totalmod)$player_name %>%
  rownames_to_column("player") %>%
  rename(value = `(Intercept)`) %>% arrange(-value)

shotmod=lmer(value.lead ~ (1|player_name), data = procd %>%
               filter(action.lead=='Shooting'))
shotcoefs=coef(shotmod)$player_name %>%
  rownames_to_column("player") %>%
  rename(value = `(Intercept)`) %>% arrange(-value)

passmod=lmer(value.lead ~ (1|player_name), data = procd %>%
               filter(action.lead=='Passing'))
passcoefs=coef(passmod)$player_name %>%
  rownames_to_column("player") %>%
  rename(value = `(Intercept)`) %>% arrange(-value)


passmod=lmer(value.lead ~ (1|player_name), data = procd %>%
               filter(action.lead=='Passing'))
passcoefs=coef(passmod)$player_name %>%
  rownames_to_column("player") %>%
  rename(value = `(Intercept)`) %>% arrange(-value)


foulmod=lmer(value.lead ~ (1|player_name), data = procd %>%
               filter(action.lead=='Fouling'))
foulcoefs=coef(foulmod)$player_name %>%
  rownames_to_column("player") %>%
  rename(value = `(Intercept)`) %>% arrange(-value)

dribbleboost = left_join(totalcoefs %>% rename(totalboost=value), 
                        shotcoefs %>% rename(shooting=value)) %>%
  left_join(passcoefs %>% rename(passing=value)) %>%
  left_join(foulcoefs %>% rename(fouling=value))

head(dribbleboost, 10)

dribblevals = rawdata %>% filter(goals_added_action_type=='Dribbling') %>%
  group_by(player_name) %>% summarise(total=sum(goals_added_dp_weighted),
                                      avg = mean(goals_added_dp_weighted)) %>%
  rename(player=player_name)

alldribblestats = left_join(dribbleboost, dribblevals)

cor.test(alldribblestats$total, alldribblestats$avg)
