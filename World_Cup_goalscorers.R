# Filename: "World_Cup_goals.R"
# Description: Analysis of data for goals in FIFA World Cup finals

# Match results & goals data sourced from planetworldcup.com


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries, directories & parameters

# load packages that are or may be used
library(tidyverse)    # includes ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(lubridate)
library(scales)
library(rvest)      # reading tables from a wikipedia page

#rm(list=ls())

# Set directory paths
dir_main = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
dir_input = paste(dir_main, "/Input", sep = "")
dir_output = paste(dir_main, "/R_output", sep = "")

setwd(dir_main)

# Initialise variables & parameters
years = c(seq(1930, 1938, 4), seq(1950, 2018, 4))
years_pens = c(seq(1978, 2018, 4))
no_teams = c(13, 16, 15, 13, rep(16, 7), rep(24, 4), rep(32, 6))

pos_level_order = c("GK", "DF", "MF","FW")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files

# Obtain player_database output from previously run World_Cup_players.R
setwd(dir_output)
load(file="World_Cup_player_database.Rdata")
load(file="World_Cup_match_results_database.Rdata")

setwd(dir_input)
goals_raw = read_csv("World_Cup_goals_list.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations

# Includes data cleansing for fields which have values that are not consistent throughout
# & add additional variables
colnames(goals_raw) = gsub(" ","_",colnames(goals_raw))
colnames(goals_raw) = gsub("\\.","",colnames(goals_raw))

goals = goals_raw %>%
  arrange(Game_ID, minute) %>%
  mutate(wc_goal_id = row_number(),
         date_game_text = paste(Date_mmm_dd, Year, sep = " "),
         date_game = as.Date(date_game_text, format = "%b-%d %Y"),
         team = ifelse(Team_scored == Team_1_name, Team_1_name, Team_2_name),
         opponent = ifelse(Team_scored == Team_2_name, Team_1_name, Team_2_name),
         team_current_name = case_when(
           team == "West Germany" ~ "Germany",
           team == "Soviet Union" ~ "USSR",
           TRUE ~ team),
         opponent_current_name = case_when(
           opponent == "West Germany" ~ "Germany",
           opponent == "Soviet Union" ~ "USSR",
           TRUE ~ opponent))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Data validation  ####

# Check on names in goals list vs player database
player_info = player_database %>%
  select(years:player)

goals_info = goals %>%
  select(Year:Game_ID, Player_name_in_Rdata:Player_number)

check_join = goals_info %>%
  left_join(player_info, by = c("Year" = "years",
                                "Player_name_wiki" = "player",
                                "Team_played_for" = "squad"))

error_join_name = check_join %>%
  filter(is.na(pos))

error_join_name = goals_info %>%
  anti_join(player_info, by = c("Year" = "years",
                                "Player_name_wiki" = "player",
                                "Team_played_for" = "squad"))

error_join_number = check_join %>%
  filter(Year >= 1954) %>%
  filter(!is.na(pos)) %>%
  mutate(diff_no = Player_number - no) %>%
  filter(!(diff_no == 0))


# Internal check of goals list
goals_per_team_per_game_team_1 = goals %>%
  group_by(Game_ID, Team_1_name) %>%
  summarise(sum_goals = mean(Team_1_final_score)) %>%
  rename("Team_name" = "Team_1_name")

goals_per_team_per_game_team_2 = goals %>%
  group_by(Game_ID, Team_2_name) %>%
  summarise(sum_goals = mean(Team_2_final_score)) %>%
  rename("Team_name" = "Team_2_name")

goals_per_team_per_game = goals_per_team_per_game_team_1 %>%
  bind_rows(goals_per_team_per_game_team_2)

goals_per_team_per_game_count = goals %>%
  group_by(Game_ID, Team_scored) %>%
  summarise(count_goals = n())

diff_goals_game = goals_per_team_per_game %>%
  left_join(goals_per_team_per_game_count, by = c("Game_ID" = "Game_ID",
                                                  "Team_name" = "Team_scored")) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(diff_goals = sum_goals - count_goals)

error_goals = diff_goals_game %>%
  filter(!(diff_goals == 0))

# Check goals in match results vs goals
goals_team1 = goals %>%
  select(Game_ID, Team_1_name, Team_1_final_score)
goals_team2 = goals %>%
  select(Game_ID, Team_2_name, Team_2_final_score) 
colnames(goals_team1) = gsub("Team_1", "Team", colnames(goals_team1))
colnames(goals_team2) = gsub("Team_2", "Team", colnames(goals_team2))
goals_teams_all = goals_team1 %>%
  bind_rows(goals_team2) %>%
  unique() %>%
  arrange(Game_ID, desc(Team_final_score))

goals_matches = goals_teams_all %>%
  left_join(match_results_database, by = c("Game_ID" = "Game_ID",
                          "Team_name" = "team_name")) %>%
  mutate(diff_team_score = Team_final_score - team_final_score)

error_goals_matches = goals_matches %>%
  filter(!(diff_team_score == 0))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations
goals_full_data = goals %>%
  left_join(player_database, by = c("Year" = "years",
                                "Player_name_wiki" = "player",
                                "Team_played_for" = "squad")) %>%
  mutate(year_adj = ifelse(month(dob_date) * 100 + day(dob_date) > month(date_game) * 100 + day(date_game), -1, 0),
         day_adj = ifelse(month(dob_date) == 2 & day(dob_date) == 29, -1, 0),
         date_last_bday = make_date(year(date_game) + year_adj, month(dob_date), day(dob_date) + day_adj),
         age_last_bday = year(date_last_bday) - year(dob_date),
         days_since_last_bday = as.numeric(date_game - date_last_bday),
         age_at_date_game = age_last_bday + days_since_last_bday/365.25) %>%
  rename("player" = "Player_name_wiki",
         "squad" = "Team_played_for") %>%
  select(-Player_number) 

goals_full_data_excl_og = goals_full_data %>%
  filter(is.na(extra_info) | !(extra_info == "og"))

goals_full_data_og = goals_full_data %>%
  filter(extra_info == "og")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Analysis  ####
# Summary statistics
# by player
summary_stats_goals_history = goals_full_data_excl_og %>%
  group_by(player, player_url, nat_team_current_name) %>%
  summarise(count_goals = n(),
            pens = sum(extra_info == "pen", na.rm = TRUE),
            min_year = min(Year),
            max_year = max(Year),
            first_goal_id = min(wc_goal_id),
            min_age_scored = min(age_at_date_game),
            max_age_scored = max(age_at_date_game)) %>%
  arrange(first_goal_id) %>%
  ungroup() %>%
  mutate(years_range = ifelse(min_year == max_year, min_year, paste(min_year,"-", max_year, sep="")),
         years_range_scored = max_age_scored - min_age_scored)

summary_stats_goals_squadno = goals_full_data_excl_og %>%
  group_by(player, player_url, nat_team_current_name, no) %>%
  summarise(count_goals = n(),
            pens = sum(extra_info == "pen", na.rm = TRUE),
            min_year = min(Year),
            max_year = max(Year),
            first_goal_id = min(wc_goal_id)) %>%
  arrange(first_goal_id) %>%
  ungroup() %>%
  mutate(years_range = ifelse(min_year == max_year, min_year, paste(min_year,"-", max_year, sep="")))

summary_stats_goals_match = goals_full_data_excl_og %>%
  group_by(player, player_url, team, opponent, Game_ID, Year, date_game) %>%
  summarise(count_goals = n(),
            pens = sum(extra_info == "pen", na.rm = TRUE),
            first_goal_id = min(wc_goal_id)) %>%
  arrange(Game_ID, first_goal_id) %>%
  ungroup() 

summary_stats_goals_tournament = goals_full_data_excl_og %>%
  group_by(player, player_url, squad, Year) %>%
  summarise(count_goals = n(),
            pens = sum(extra_info == "pen", na.rm = TRUE),
            first_goal_id = min(wc_goal_id)) %>%
  arrange(first_goal_id) %>%
  ungroup() 

# by team
summary_stats_goals_team = goals_full_data %>%
  group_by(team_current_name) %>%
  summarise(count_goals = n(),
            pens = sum(extra_info == "pen", na.rm = TRUE),
            min_year = min(Year),
            max_year = max(Year),
            first_goal_id = min(wc_goal_id)) %>%
  arrange(first_goal_id) %>%
  ungroup() %>%
  mutate(years_range = ifelse(min_year == max_year, min_year, paste(min_year,"-", max_year, sep="")))

summary_stats_goals_match_team = goals_full_data_excl_og %>%
  group_by(team, opponent, Game_ID, Year) %>%
  summarise(count_goals = n(),
            pens = sum(extra_info == "pen", na.rm = TRUE),
            first_goal_id = min(wc_goal_id)) %>%
  arrange(Game_ID, first_goal_id) %>%
  ungroup() 

summary_stats_goals_tournament_team = goals_full_data %>%
  group_by(team, Year) %>%
  summarise(count_goals = n(),
            pens = sum(extra_info == "pen", na.rm = TRUE),
            first_goal_id = min(wc_goal_id)) %>%
  arrange(first_goal_id) %>%
  ungroup() 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Records ####

# 1. Players
# Records for goals by player is from full data excluding own goals

# Most goals ever
most_goals_history = summary_stats_goals_history %>%
  select(-c(player_url, pens, first_goal_id, min_year, max_year)) %>%
  arrange(desc(count_goals))
head(most_goals_history, 20)

most_pens_history = summary_stats_goals_history %>%
  select(-c(player_url, count_goals, first_goal_id, min_year, max_year)) %>%
  arrange(desc(pens))
head(most_pens_history, 10)

# Most goals for each country
most_goals_country = summary_stats_goals_history %>%
  select(-c(min_year, max_year)) %>%
  group_by(nat_team_current_name) %>%
  top_n(1, count_goals) %>%
  select(-c(player_url, pens, first_goal_id)) %>%
  arrange(nat_team_current_name)
most_goals_country

new_most_goals_country_latest_tournament = most_goals_country %>%
  filter(years_range == max(years) | substr(years_range,6,9) == max(years))
new_most_goals_country_latest_tournament

# Most goals in a tournament
most_goals_tournament = summary_stats_goals_tournament %>%
  select(-c(player_url, pens, first_goal_id)) %>%
  arrange(desc(count_goals))
head(most_goals_tournament, 10)

most_goals_each_tournament = summary_stats_goals_tournament %>%
  group_by(Year) %>%
  top_n(1, count_goals) %>%
  select(-c(player_url, pens, first_goal_id))
most_goals_each_tournament

# Most goals for each shirt number
most_goals_each_squadno = summary_stats_goals_squadno %>%
  filter(!is.na(no)) %>%
  group_by(no) %>%
  top_n(1, count_goals) %>%
  select(-c(player_url, pens, first_goal_id)) %>%
  select(no, player:nat_team_current_name, count_goals, years_range) %>%
  arrange(no)
most_goals_each_squadno

# youngest & oldest goalscorers
youngest_goalscorer = goals_full_data_excl_og %>%
  arrange(age_at_date_game) %>%
  select(player, team:opponent, Year, age_last_bday, days_since_last_bday)
head(youngest_goalscorer, 10)

oldest_goalscorer = goals_full_data_excl_og %>%
  arrange(desc(age_at_date_game)) %>%
  select(player, team:opponent, Year, age_last_bday, days_since_last_bday)
head(oldest_goalscorer, 10)

youngest_goalscorer_by_tournament = goals_full_data_excl_og %>%
  group_by(Year) %>%
  top_n(-1, age_at_date_game) %>%
  select(player, team:opponent, Year, age_last_bday, days_since_last_bday)
youngest_goalscorer_by_tournament

oldest_goalscorer_by_tournament = goals_full_data_excl_og %>%
  group_by(Year) %>%
  top_n(1, age_at_date_game) %>%
  select(player, team:opponent, Year, age_last_bday, days_since_last_bday)
oldest_goalscorer_by_tournament

youngest_goalscorer_by_team = goals_full_data_excl_og %>%
  group_by(nat_team_current_name) %>%
  top_n(-1, age_at_date_game) %>%
  select(player, team:opponent, Year, age_last_bday, days_since_last_bday) %>%
  arrange(nat_team_current_name)
youngest_goalscorer_by_team

oldest_goalscorer_by_team = goals_full_data_excl_og %>%
  group_by(nat_team_current_name) %>%
  top_n(1, age_at_date_game) %>%
  select(player, team:opponent, Year, age_last_bday, days_since_last_bday) %>%
  arrange(nat_team_current_name)
oldest_goalscorer_by_team

youngest_and_oldest_goalscorer_by_team = youngest_goalscorer_by_team %>%
  left_join(oldest_goalscorer_by_team, by = c("team" = "team")) %>%
  filter(player.x == player.y) %>%
  # exclude those where country has only scored one goal
  filter(!((age_last_bday.x == age_last_bday.y) & (days_since_last_bday.x == days_since_last_bday.y)))
colnames(youngest_and_oldest_goalscorer_by_team) = gsub("\\.x", "_youngest", colnames(youngest_and_oldest_goalscorer_by_team))
colnames(youngest_and_oldest_goalscorer_by_team) = gsub("\\.y", "_oldest", colnames(youngest_and_oldest_goalscorer_by_team))
youngest_and_oldest_goalscorer_by_team

goals_birthday = goals_full_data_excl_og %>%
  filter(days_since_last_bday == 0) %>%
  select(player, extra_info, team:opponent, Year, age_last_bday)
goals_birthday

# Years range goals scored
range_goals_scored = summary_stats_goals_history %>%
  top_n(10, years_range_scored) %>%
  select(-c(player_url, pens, first_goal_id)) %>%
  arrange(desc(years_range_scored))
range_goals_scored


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Teams
# Records for goals by team is from full data (including own goals)

# Most goals ever
most_goals_history_team = summary_stats_goals_team %>%
  select(-c(pens, first_goal_id, min_year, max_year)) %>%
  arrange(desc(count_goals))
head(most_goals_history_team, 20)

most_pens_history_team = summary_stats_goals_team %>%
  select(-c(count_goals, first_goal_id, min_year, max_year)) %>%
  arrange(desc(pens))
head(most_pens_history_team, 20)

# Most goals in a tournament
most_goals_tournament_team = summary_stats_goals_tournament_team %>%
  select(-c(pens, first_goal_id)) %>%
  arrange(desc(count_goals))
head(most_goals_tournament_team, 10)

most_goals_each_tournament_team = summary_stats_goals_tournament_team %>%
  group_by(Year) %>%
  top_n(1, count_goals) %>%
  select(-c(pens, first_goal_id))
most_goals_each_tournament_team


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Matches

most_goals_game = summary_stats_goals_match %>%
  select(-c(player_url, pens, first_goal_id)) %>%
  top_n(5, count_goals) %>%
  arrange(desc(count_goals))
most_goals_game

most_goalscorers_game = summary_stats_goals_match %>%
  group_by(date_game, team, opponent) %>%
  summarise(count_goals = n()) %>%
  top_n(10, count_goals) %>%
  arrange(desc(count_goals))
most_goalscorers_game


hattricks = summary_stats_goals_match %>%
  select(-c(player_url, pens, first_goal_id)) %>%
  filter(count_goals >= 3) %>%
  arrange(Game_ID)
# Add to this - minutes
hattricks

multiple_hattricks_in_a_game = hattricks %>%
  group_by(Game_ID) %>%
  mutate(count_hattrick = n()) %>%
  filter(count_hattrick > 1)
multiple_hattricks_in_a_game


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Graphs ####



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Output ####
# export output to csv format
setwd(dir_output)
write.csv(error_join_name, file = "World_Cup_error_join_name.csv")
write.csv(error_join_number, file = "World_Cup_error_join_number.csv")

write.csv(goals_full_data, file = "World_Cup_goals_full_data.csv")
write.csv(most_goals_each_squadno, file = "World_Cup_goals_most_goals_each_squadno.csv")
setwd(dir_input)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Temporary (working) code




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# To do:

