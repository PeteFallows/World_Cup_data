# Filename: "World_Cup_matches.R"
# Description: Analysis of data for match results in FIFA World Cup finals

# Match results & goals data sourced from planetworldcup.com


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries, directories & parameters

# load packages that are or may be used
library(tidyverse)    # includes ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(lubridate)
library(scales)
library(rvest)      # reading tables from a wikipedia page

# Set directory paths
dir_main = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
dir_input = paste(dir_main, "/Input", sep = "")
dir_output = paste(dir_main, "/R_output", sep = "")

setwd(dir_main)

# Initialise variables & parameters
years = c(seq(1930, 1938, 4), seq(1950, 2018, 4))
years_pens = c(seq(1978, 2018, 4))
no_teams = c(13, 16, 15, 13, rep(16, 7), rep(24, 4), rep(32, 6))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files

setwd(dir_input)
match_results_raw = read_csv("World_Cup_match_results.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations

# Includes data cleansing for fields which have values that are not consistent throughout
# & add additional variables
colnames(match_results_raw) = gsub(" ","_",colnames(match_results_raw))
colnames(match_results_raw) = gsub("\\.","",colnames(match_results_raw))

match_results = match_results_raw %>%
  arrange(Game_ID) %>%
  mutate(date_match_text = paste(Date_mmm_dd, Year, sep = " "),
         date_match = as.Date(date_match_text, format = "%b %d %Y"),
         Team_1_goals_pens = Team_1_final_score + ifelse(is.na(Team_1_pens), 0,  Team_1_pens/ 100),
         Team_2_goals_pens = Team_2_final_score + ifelse(is.na(Team_2_pens), 0,  Team_2_pens/ 100) / 100,
         score_diff_goals_pens = Team_1_goals_pens - Team_2_goals_pens,
         Team_1_current_name = case_when(
           Team_1_name == "West Germany" ~ "Germany",
           Team_1_name == "Soviet Union" ~ "USSR",
           TRUE ~ Team_1_name),
         Team_2_current_name = case_when(
           Team_2_name == "West Germany" ~ "Germany",
           Team_2_name == "Soviet Union" ~ "USSR",
           TRUE ~ Team_2_name),
         winner = ifelse(score_diff_goals_pens > 0, Team_1_name, ifelse(score_diff_goals_pens == 0, "Draw", Team_2_name)),
         loser = ifelse(score_diff_goals_pens < 0, Team_1_name, ifelse(score_diff_goals_pens == 0, "Draw", Team_2_name)),
         winner_goals = ifelse(score_diff_goals_pens > 0, Team_1_final_score, Team_2_final_score),
         loser_goals = ifelse(score_diff_goals_pens < 0, Team_1_final_score, Team_2_final_score),
         margin_goals = abs(Team_1_final_score - Team_2_final_score),
         game_total_goals = Team_1_final_score + Team_2_final_score)

temp_matches = match_results %>%
  filter(loser == "Australia")

# Show 2 rows for every match, separate row for each team in a separate row
match_results_team_1 = match_results
colnames(match_results_team_1) = gsub("Team_1", "team", colnames(match_results_team_1))
colnames(match_results_team_1) = gsub("Team_2", "opponent", colnames(match_results_team_1))

match_results_team_2 = match_results
colnames(match_results_team_2) = gsub("Team_2", "team", colnames(match_results_team_2))
colnames(match_results_team_2) = gsub("Team_1", "opponent", colnames(match_results_team_2))

match_results_database = match_results_team_1 %>%
  bind_rows(match_results_team_2) %>%
  arrange(Game_ID) %>%
  mutate(score_diff_goals_pens = team_goals_pens - opponent_goals_pens,
         margin_goals = team_final_score - opponent_final_score,
         match_result = ifelse(team_final_score > opponent_final_score, "W", 
                               ifelse(team_final_score < opponent_final_score, "L", "D")),
         pens_result = ifelse(is.na(team_pens), "NA",
                              ifelse(team_goals_pens > opponent_goals_pens, "WP", "LP")),
         overall_result = ifelse(pens_result == "NA", match_result, paste(match_result, pens_result, sep = "")),
         points_match = case_when(
           Match_stage == "Group A" ~ 1,
           Match_stage == "Group B" ~ 1,
           Match_stage == "Group C" ~ 1,
           Match_stage == "Group D" ~ 1,
           Match_stage == "Group E" ~ 1,
           Match_stage == "Group F" ~ 1,
           Match_stage == "Group G" ~ 1,
           Match_stage == "Group H" ~ 1,
           Year == 1950 & Match_stage == "Final Pool" ~ 1,
           Year == 1950 & Match_stage == "Final" ~ 1,
           Year == 1974 & substr(Match_stage, 1, 12) == "Second Round" ~ 1,
           Year == 1978 & substr(Match_stage, 1, 12) == "Second Round" ~ 1,
           Year == 1982 & substr(Match_stage, 1, 12) == "Second Round" ~ 1,
           TRUE ~ 0),
         knockout_match = 1 - points_match,
         points_actual = ifelse(match_result == "W", 
                                ifelse(Year >= 1994, 3, 2), 
                                ifelse(match_result == "D", 1, 0)) * points_match,
         points_nominal_3win = ifelse(match_result == "W", 3,
                                ifelse(substr(match_result, 1, 1) == "D", 1, 0)))

temp_matches_database = match_results_database %>%
  filter(team_name == "Australia")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Data validation  ####



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Analysis  ####

# Summary statistics
# by team
summary_stats_goals_team_matches_year = match_results_database %>%
  group_by(team_current_name, Year) %>%
  summarise(played = n(),
            count_goals_scored = sum(team_final_score),
            count_goals_conceded = sum(opponent_final_score))

summary_stats_goals_team_matches = summary_stats_goals_team_matches_year %>%
  group_by(team_current_name) %>%
  summarise(tournaments = n(),
            count_goals_scored = sum(count_goals_scored),
            count_goals_conceded = sum(count_goals_conceded))


temp = summary_stats_goals_team_matches %>%
  filter(team_current_name == "Australia") 
temp

# Group stages / pools tables
groups_points_tables = match_results_database %>%
  filter(points_match == 1) %>%
  mutate(Match_stage = ifelse(Year == 1950 & Match_stage == "Final", "Final Pool", Match_stage)) %>%
  group_by(Year, Match_stage, team_name) %>%
  summarise(P = n(),
            W = sum(match_result == "W"),
            D = sum(match_result == "D"),
            L = sum(match_result == "L"),
            For = sum(team_final_score),
            Against = sum(opponent_final_score),
            Points = sum(points_actual)) %>%
  arrange(Year, Match_stage, desc(Points), desc(For - Against), desc(For))
  


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Records ####

# 1. Players


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Teams
# Records for goals by team is from full data (including own goals)

# Most goals ever
most_goals_scored_each_tournament_team = summary_stats_goals_team_matches_year %>%
  group_by(Year) %>%
  top_n(1, count_goals_scored) %>%
  arrange(Year)
most_goals_scored_each_tournament_team

most_goals_conceded_each_tournament_team = summary_stats_goals_team_matches_year %>%
  group_by(Year) %>%
  top_n(1, count_goals_conceded) %>%
  arrange(Year)
most_goals_conceded_each_tournament_team

most_goals_scored_team = summary_stats_goals_team_matches %>%
  arrange(desc(count_goals_scored)) %>%
  select(-count_goals_conceded)
head(most_goals_scored_team,20)

most_goals_conceded_team = summary_stats_goals_team_matches %>%
  arrange(desc(count_goals_conceded)) %>%
  select(-count_goals_scored)
head(most_goals_conceded_team,20)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Matches
# Most goals by one team in a game
most_goals_game = match_results %>%
  select(date_match, Match_stage, Team_1_name:Team_2_name, winner, winner_goals:loser_goals, game_total_goals) %>%
  arrange(desc(winner_goals)) 
head(most_goals_game, 10)

# ... per tournament
most_goals_game_tournament = match_results %>%
  select(Year, date_match, Match_stage, Team_1_name:Team_2_name, winner, winner_goals:loser_goals, game_total_goals) %>%
  group_by(Year) %>%
  top_n(1, winner_goals) %>%
  arrange(Year) 
most_goals_game_tournament

# Most total goals in a game
most_total_goals_game = match_results %>%
  select(date_match, Match_stage, Team_1_name:Team_2_name, winner, winner_goals:loser_goals, game_total_goals) %>%
  arrange(desc(game_total_goals)) 
head(most_total_goals_game, 20)

# ... per tournament
most_total_goals_game_tournament = match_results %>%
  select(Year, date_match, Match_stage, Team_1_name:Team_2_name, winner, winner_goals:loser_goals, game_total_goals) %>%
  group_by(Year) %>%
  top_n(1, game_total_goals) %>%
  arrange(Year) 
most_total_goals_game_tournament

# Highest winning margin in a game
highest_margin_game = match_results %>%
  select(date_match, Match_stage, winner:loser_goals, margin_goals) %>%
  arrange(desc(margin_goals)) 
head(highest_margin_game, 10)

# ... per tournament
highest_margin_game_tournament = match_results %>%
  select(Year, Match_stage, winner:loser_goals, margin_goals) %>%
  group_by(Year) %>%
  top_n(1, margin_goals) %>%
  arrange(Year) 
highest_margin_game_tournament

# ... per team in their history
highest_winning_margin_game_team = match_results %>%
  select(Year, Match_stage, winner:loser_goals, margin_goals) %>%
  group_by(winner) %>%
  top_n(1, margin_goals) %>%
  arrange(winner) 
highest_winning_margin_game_team

highest_losing_margin_game_team = match_results %>%
  select(Year, Match_stage, winner:loser_goals, margin_goals) %>%
  group_by(loser) %>%
  top_n(1, margin_goals) %>%
  arrange(loser) 
highest_losing_margin_game_team

# most goals in a game, by team in their history
most_goals_scored_game_team = match_results_database %>%
  select(date_match, Match_stage, team_name:opponent_name, winner, team_final_score:opponent_final_score) %>%
  group_by(team_name) %>%
  top_n(1, team_final_score) %>%
  arrange(team_name) 
most_goals_scored_game_team

most_goals_conceded_game_team = match_results_database %>%
  select(date_match, Match_stage, team_name:opponent_name, winner, team_final_score:opponent_final_score) %>%
  group_by(team_name) %>%
  top_n(1, opponent_final_score) %>%
  arrange(team_name) 
most_goals_conceded_game_team

most_goals_scored_game_and_lost = most_goals_scored_game_team %>%
  filter(!(team_name == winner)) %>%
  arrange(date_match)
most_goals_scored_game_and_lost

most_goals_conceded_game_and_won = most_goals_conceded_game_team %>%
  filter(team_name == winner) %>%
  arrange(date_match)
most_goals_conceded_game_and_won


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Tables
colnames(groups_points_tables)

# Most goals scored in a pool
most_goals_pool = groups_points_tables %>%
  arrange(desc(For))
head(most_goals_pool, 10)

most_goals_conceded_pool = groups_points_tables %>%
  arrange(desc(Against))
head(most_goals_conceded_pool, 10)

exact_same_points = groups_points_tables %>%
  group_by(Year, Match_stage, Points, For, Against) %>%
  mutate(count = n()) %>%
  filter(count > 1) %>%
  select(-count)
exact_same_points


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Graphs ####



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#### Output ####
# export output to csv format
setwd(dir_output)
save(match_results_database, file = "World_Cup_match_results_database.Rdata")
write.csv(match_results_database, file = "World_Cup_match_results_database.csv")

write.csv(groups_points_tables, file = "World_Cup_groups_points_tables.csv")
setwd(dir_input)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Temporary (working) code




# - - - - - - - - -