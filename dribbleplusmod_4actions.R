library(dplyr)

rawdata = read.csv('../rawdata/tf_events_2020.csv', na.strings="", encoding = 'UTF-8')


procd=rawdata %>% 
  #add info about next four actions
  mutate(
    action.lead = lead(goals_added_action_type),
    action.lead2 = lead(goals_added_action_type,2),
    action.lead3 = lead(goals_added_action_type,3),
    action.lead4 = lead(goals_added_action_type,4),
    
    value.lead = lead(goals_added_dp_weighted),
    value.lead2 = lead(goals_added_dp_weighted,2),
    value.lead3 = lead(goals_added_dp_weighted,3),
    value.lead4 = lead(goals_added_dp_weighted,4),
    
    player.lead = lead(player_name),
    player.lead2 = lead(player_name,2),
    player.lead3 = lead(player_name,3),
    player.lead4 = lead(player_name,4),
    
    team.lead = lead(team_abbreviation),
    team.lead2 = lead(team_abbreviation,2),
    team.lead3 = lead(team_abbreviation,3),
    team.lead4 = lead(team_abbreviation,4)
    ) %>%
  #account for times in which possession switched during those 4 actions
  mutate(value.lead1 = case_when(team.lead1 != team_abbreviation ~ NA,
                                 TRUE ~ value.lead1),
         value.lead2 = case_when(team.lead != team_abbreviation ~ NA,
                                 team.lead2 != team_abbreviation ~ NA,
                                 TRUE ~ value.lead2),
         value.lead3 = case_when(team.lead != team_abbreviation ~ NA,
                                 team.lead2 != team_abbreviation ~ NA,
                                 team.lead3 != team_abbreviation ~ NA,
                                 TRUE ~ value.lead3),
         value.lead4 = case_when(team.lead != team_abbreviation ~ NA,
                                 team.lead2 != team_abbreviation ~ NA,
                                 team.lead4 != team_abbreviation ~ NA,
                                 team.lead4 != team_abbreviation ~ NA,
                                 TRUE ~ value.lead4),
         )
           
  #add next action value for next non receiving action of same team player
  mutate(nextactionval = case_when(
    player.lead1 != player_name &
      team.lead1 == team_abbreviation ~ value.lead1,
    player.lead2 != player_name &
      team.lead2 == team_abbreviation ~ value.lead2,
    player.lead3 != player_name &
      team.lead3 == team_abbreviation ~ value.lead3,
    player.lead4 != player_name &
      team.lead4 == team_abbreviation ~ value.lead4
  ))
  