# Filename: "World_Cup_records.R"
# Description: Analysis of data for FIFA World Cup


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries, directories & parameters

# load packages that are or may be used
library(tidyverse)    # includes ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(lubridate)
library(scales)
library(rvest)      # reading tables from a wikipedia page


# Set directory paths
# Read in data files - example directories
dir_main = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
dir_input = "C:/Users/fallo/OneDrive/Documents/Pete/R-files/Input"
dir_output = "C:/Users/fallo/OneDrive/Documents/Pete/R-files/R_output"

setwd(dir_main)

# Initialise variables & parameters
years = c(seq(1930, 1938, 4), seq(1950, 2018, 4))
no_teams = c(13, 16, 15, 13, rep(16, 7), rep(24, 4), rep(32, 6))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 

# Note: When inside function, press Ctrl+Shift+Alt+R to get skeleton to document the function.


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files

# get from World_Cup_players.R
# player_data_full

setwd(dir_input)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations

# Includes data cleansing for fields which have values that are not consistent throughout
# & add additional variables




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis - Statistics by team

summary_stats_teams_by_year = player_database %>%
  group_by(years, nat_team, nat_team_alpha3) %>%
  summarise(count_players = n(),
            count_captains = sum(captain),
            host_flag = min(sum(host_flag),1),
            winner_flag = min(sum(winner_flag),1),
            runner_up_flag = min(sum(runner_up_flag),1),
            count_age = sum(!(is.na(age_int))),
            sum_age_int = sum(age_int, na.rm = TRUE),
            sum_age_days = sum(days_since_bday, na.rm = TRUE),
            ave_age_years = mean(age_int, na.rm = TRUE),
            ave_age_days = mean(days_since_bday, na.rm = TRUE),
            ave_age = ave_age_years + ave_age_days/365.25,
            count_teenagers = sum(age_int < 20, na.rm = TRUE),
            min_dob = min(dob_date, na.rm = TRUE),
            max_dob = max(dob_date, na.rm = TRUE),
            age_diff = max_dob - min_dob,
            age_diff_dec = as.numeric(age_diff / 365.25),
            sum_caps = sum(caps, na.rm = TRUE),
            count_caps_na = sum(is.na(caps), na.rm = TRUE),
            ave_caps = sum_caps / (count_players - count_caps_na),
            count_club_league_same = sum(club_league_same),
            count_club_country_same = sum(club_country_same)) %>%
  mutate(count_other_leagues = count_players - count_club_league_same)
summary_stats_teams_by_year

summary_stats_teams_by_pos = player_database %>%
  group_by(pos) %>%
  # same summary as above
  summarise(count_players = n(),
            count_captains = sum(captain),
            host_flag = min(sum(host_flag),1),
            winner_flag = min(sum(winner_flag),1),
            runner_up_flag = min(sum(runner_up_flag),1),
            count_age = sum(!(is.na(age_int))),
            sum_age_int = sum(age_int, na.rm = TRUE),
            sum_age_days = sum(days_since_bday, na.rm = TRUE),
            ave_age_years = mean(age_int, na.rm = TRUE),
            ave_age_days = mean(days_since_bday, na.rm = TRUE),
            ave_age = ave_age_years + ave_age_days/365.25,
            count_teenagers = sum(age_int < 20, na.rm = TRUE),
            min_dob = min(dob_date, na.rm = TRUE),
            max_dob = max(dob_date, na.rm = TRUE),
            age_diff = max_dob - min_dob,
            age_diff_dec = as.numeric(age_diff / 365.25),
            sum_caps = sum(caps, na.rm = TRUE),
            count_caps_na = sum(is.na(caps), na.rm = TRUE),
            ave_caps = sum_caps / (count_players - count_caps_na),
            count_club_league_same = sum(club_league_same),
            count_club_country_same = sum(club_country_same)) %>%
  mutate(count_other_leagues = count_players - count_club_league_same)
summary_stats_teams_by_pos

summary_stats_leagues = player_database %>%
  group_by(years, club_league_country, club_league_alpha3) %>%
  summarise(count_players = n(),
            count_captains = sum(captain),
            host_flag = min(sum(host_flag),1),
            winner_flag = min(sum(winner_flag),1),
            runner_up_flag = min(sum(runner_up_flag),1),
            count_club_league_same = sum(club_league_same),
            count_club_country_same = sum(club_country_same)) %>%
  mutate(count_imported_players = count_players - count_club_league_same) %>%
  group_by(years) %>%
  mutate(count_players_tournament = sum(count_players),
         propn_from_league = count_players / count_players_tournament)
summary_stats_leagues

summary_stats_team_league = player_database %>% 
  group_by(years, nat_team, club_league_country, club_league_same) %>%  
  summarize(count_players = n()) 

summary_stats_team_club = player_database %>% 
  group_by(years, nat_team, club, club_league_country) %>%  
  summarize(count_players = n()) %>%
  group_by(years, nat_team) %>%
  mutate(count_clubs = n())

summary_stats_team_club_year = summary_stats_team_club %>%
  group_by(years, nat_team) %>%
  summarise(count_clubs = n())
summary_stats_team_club_year

summary_stats_team_league_year = summary_stats_team_club %>%
  select(years:nat_team, club_league_country:count_players) %>%
  group_by(years, nat_team, club_league_country) %>%
  summarise(count_players = sum(count_players)) %>%
  group_by(years, nat_team) %>%
  summarise(count_leagues = n())
summary_stats_team_league_year

summary_stats_clubs = player_database %>% 
  group_by(years, club, club_league_country) %>%  
  summarize(count_players = n())

summary_stats_team_confed = player_database %>% 
  group_by(years, nat_team_confederation) %>%  
  summarize(count_players = n()) 

summary_stats_club_confed = player_database %>% 
  group_by(years, club_confederation) %>%  
  summarize(count_players = n())

summary_stats_players = player_database %>%
  group_by(player, player_url) %>%
  summarise(count_appearances = n(),
            first_tournament = min(years),
            last_tournament = max(years),
            years_range = last_tournament - first_tournament,
            count_captains = sum(captain),
            count_hosts = sum(host_flag),
            count_winners = sum(winner_flag),
            count_runners_up = sum(runner_up_flag),
            count_finals = count_winners + count_runners_up,
            min_age = min(age_int, na.rm = TRUE),
            max_age = max(age_int, na.rm = TRUE))
#summary_stats_players

summary_stats_teams = summary_stats_teams_by_year %>%
  mutate(nat_team = str_replace(nat_team, "West Germany", "Germany")) %>%
  group_by(nat_team, nat_team_alpha3) %>%
  summarise(count_appearances = n(),
            count_hosts = sum(host_flag),
            count_winners = sum(winner_flag),
            count_runners_up = sum(runner_up_flag),
            count_finals = count_winners + count_runners_up,
            first_appearance = min(years),
            last_appearance = max(years),
            average_age = mean(ave_age),
            count_teenagers = sum(count_teenagers),
            count_club_league_same = sum(count_club_league_same),
            count_club_country_same = sum(count_club_country_same))   
summary_stats_teams

summary_stats_tournaments = summary_stats_teams_by_year %>%
  group_by(years) %>%
  summarise(count_teams = n(),
            tot_players = sum(count_players),
            max_players_in_squad = max(count_players),
            tot_captains = sum(count_captains),
            max_captain_in_squad = max(count_captains),
            count_age = sum(count_age),
            sum_age_int = sum(sum_age_int),
            sum_age_days = sum(sum_age_days),
            count_teenagers = sum(count_teenagers),
            sum_caps = sum(sum_caps),
            count_caps_na = sum(count_caps_na),
            count_club_league_same = sum(count_club_league_same),
            count_club_country_same = sum(count_club_country_same)) %>%
  mutate(tot_possible_payers = count_teams * max_players_in_squad,
         shortfall_players = tot_possible_payers - tot_players,
         shortfall_captains = count_teams - tot_captains,
         ave_age_years = sum_age_int / count_age,
         ave_age_days = sum_age_days / count_age,
         ave_age = ave_age_years + ave_age_days/365.25,
         ave_caps = sum_caps / (tot_players - count_caps_na),
         propn_club_league_same = count_club_league_same / tot_players,
         propn_count_club_country_same = count_club_country_same / tot_players)
summary_stats_tournaments


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis - 1. Tournament Records

# 1.1 Teams
# Most appearances by team, winners, runners-up, finalists, hosts
most_tournaments = summary_stats_teams %>%
  arrange(desc(count_appearances)) %>%
  select(nat_team:count_appearances, first_appearance:last_appearance)
head(most_tournaments,5)

multiple_hosts = summary_stats_teams %>%
  filter(count_hosts > 1) %>%
  select(nat_team:nat_team_alpha3, count_hosts)
multiple_hosts

winners = summary_stats_teams %>%
  filter(count_winners >= 1) %>%
  select(nat_team:nat_team_alpha3, count_winners, count_runners_up) %>%
  arrange(desc(count_winners), desc(count_runners_up))
winners

finalists = summary_stats_teams %>%
  filter(count_finals >= 1) %>%
  select(nat_team:nat_team_alpha3, count_winners:count_finals) %>%
  arrange(desc(count_finals), desc(count_winners))
finalists

teams_no_captains = summary_stats_teams_by_year %>%
  filter(count_captains == 0)

host_winners = summary_stats_teams_by_year %>%
  filter(host_flag == 1 & winner_flag == 1) %>%
  select(years:nat_team_alpha3)
host_winners

host_runners_up = summary_stats_teams_by_year %>%
  filter(host_flag == 1 & runner_up_flag == 1) %>%
  select(years:nat_team_alpha3)
host_runners_up

table_hosts_winners = summary_stats_teams_by_year %>% 
  group_by(host_flag, winner_flag, runner_up_flag) %>%  
  summarize(count_teams = n_distinct(nat_team, years)) %>%  
  arrange(desc(host_flag), desc(winner_flag), desc(runner_up_flag))
#  spread(host_flag, count_teams)
table_hosts_winners

prob_host_winning = nrow(host_winners) / sum(summary_stats_teams_by_year$host_flag)
prob_host_runner_up = nrow(host_winners) / sum(summary_stats_teams_by_year$runner_up_flag)

prob_host = sum(summary_stats_teams_by_year$host_flag) / nrow(summary_stats_teams_by_year)

# 1.2 Players
# longest World Cup career span by players
longest_span_player = summary_stats_players %>%
  arrange(desc(years_range), first_tournament) %>%
  filter(years_range >= max(summary_stats_players$years_range) - 4) %>%
  select(player, count_players:years_range, min_age:max_age)
longest_span_player

most_tournaments_player = summary_stats_players %>%
  arrange(desc(count_players), first_tournament) %>%
  filter(count_players == max(summary_stats_players$count_players)) %>%
  select(player, count_players:years_range, min_age:max_age)
most_tournaments_player

most_winners_player = summary_stats_players %>%
  filter(count_winners >= 2) %>%
  select(player, count_winners, count_runners_up) %>%
  arrange(desc(count_winners), desc(count_runners_up))
most_winners_player

most_finalists_player = summary_stats_players %>%
  filter(count_finals >= 3) %>%
  select(player, count_winners:count_finals) %>%
  arrange(desc(count_finals), desc(count_winners))
most_finalists_player

# 1.3 Leagues
# Most players from leagues
leagues_most_players_by_tournament = summary_stats_leagues %>%
  group_by(years) %>%
  top_n(1, count_players) %>%
  select(years:club_league_country, count_players, count_club_league_same, count_imported_players, propn_from_league) %>%
  filter(count_imported_players > 0)
leagues_most_players_by_tournament

highest_propn_players = summary_stats_leagues %>%
  select(years:club_league_country, count_players, count_club_league_same, count_imported_players, propn_from_league) %>%
  arrange(desc(propn_from_league))
head(highest_propn_players,5)

leagues_more_than_20_players_by_tournament = summary_stats_leagues %>%
  filter(count_players >= 20) %>%
  select(years:club_league_country, count_players, count_club_league_same, count_imported_players) %>%
  arrange(desc(count_players))
leagues_more_than_20_players_by_tournament

teams_entirely_from_own_league = summary_stats_teams_by_year %>%
  filter(count_other_leagues == 0) %>%
  select(years:count_players)
teams_entirely_from_own_league
  
teams_most_players_foreign_leagues_by_tournament = summary_stats_team_league %>%
  filter(club_league_same == 0) %>%
  group_by(years) %>%
  top_n(1, count_players) %>%
  select(years:club_league_country, count_players, club_league_same)
teams_most_players_foreign_leagues_by_tournament

teams_entirely_from_other_leagues = summary_stats_teams_by_year %>%
  filter(count_other_leagues == count_players) %>%
  select(years:count_players)
teams_entirely_from_other_leagues

league_with_most_players_from_league_team_not_represented_by_tournament = summary_stats_leagues %>%
  filter(count_club_league_same == 0) %>%
  group_by(years) %>%
  top_n(1, count_players) %>%
  select(years:club_league_country, count_players, count_club_league_same)
league_with_most_players_from_league_team_not_represented_by_tournament

teams_most_players_foreign_leagues_by_tournament = summary_stats_teams_by_year %>%
#  filter(count_club_league_same == 0) %>%
  group_by(years) %>%
  top_n(1, count_other_leagues) %>%
  select(years:nat_team, count_other_leagues, count_club_league_same)
teams_most_players_foreign_leagues_by_tournament

teams_most_leagues_by_tournament = summary_stats_team_league_year %>%
  group_by(years) %>%
  top_n(1, count_leagues)
teams_most_leagues_by_tournament

teams_least_leagues_by_tournament = summary_stats_team_league_year %>%
  group_by(years) %>%
  top_n(-1, count_leagues)
teams_least_leagues_by_tournament

teams_most_leagues = summary_stats_team_league_year %>%
  arrange(desc(count_leagues))
head(teams_most_leagues,5)

teams_least_leagues = summary_stats_team_league_year %>%
  arrange(count_leagues) %>%
  filter(count_leagues == 1)
teams_least_leagues

# 1.4 Clubs
clubs_most_players_by_tournament = summary_stats_clubs %>%
  group_by(years) %>%
  top_n(1, count_players)
clubs_most_players_by_tournament

clubs_more_than_10_players_by_tournament = summary_stats_clubs %>%
  filter(count_players >= 10) %>%
  select(years:club_league_country, count_players) %>%
  arrange(years, desc(count_players))
clubs_more_than_10_players_by_tournament

teams_most_players_same_club_by_tournament = summary_stats_team_club %>%
  group_by(years) %>%
  top_n(1, count_players)
teams_most_players_same_club_by_tournament

teams_most_players_same_club = summary_stats_team_club %>%
  arrange(desc(count_players))
head(teams_most_players_same_club,5)

teams_most_clubs_by_tournament = summary_stats_team_club_year %>%
  group_by(years) %>%
  top_n(1, count_clubs)
teams_most_clubs_by_tournament

teams_least_clubs_by_tournament = summary_stats_team_club_year %>%
  group_by(years) %>%
  top_n(-1, count_clubs)
teams_least_clubs_by_tournament

teams_most_clubs = summary_stats_team_club_year %>%
  arrange(desc(count_clubs))
head(teams_most_clubs,5)

teams_least_clubs = summary_stats_team_club_year %>%
  arrange(count_clubs)
head(teams_least_clubs,10)


# 1.5 Confederations

summary_stats_club_confed %>% 
  filter(years == 2018) %>%
  arrange(desc(count_players))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis - 2. Age Records

# 2.1 Players
# oldest players ever
oldest_players = arrange(player_database, desc(age_years.days)) %>%
  select(years:player, age_int, days_since_bday)
head(oldest_players, 5)

oldest_winners = player_database %>%
  filter(winner_flag == 1) %>%
  arrange(desc(age_years.days)) %>%
  select(years:player, age_int, days_since_bday)
head(oldest_winners, 5)

oldest_captain = player_database %>%
  filter(captain == 1) %>%
  arrange(desc(age_years.days)) %>%
  select(years:player, age_int, days_since_bday)
head(oldest_captain, 5)

oldest_players_by_tournament = player_database %>%
  group_by(years) %>%
  top_n(1, age_years.days) %>%
  select(years:player, dob_date:age_int, days_since_bday)
oldest_players_by_tournament

oldest_captain_by_tournament = player_database %>%
  filter(captain == 1) %>%
  group_by(years) %>%
  top_n(1, age_years.days) %>%
  select(years:player, dob_date:age_int, days_since_bday)
oldest_captain_by_tournament

oldest_goalkeepers_by_tournament = player_database %>%
  filter(pos == "GK") %>%
  group_by(years) %>%
  top_n(1, age_years.days) %>%
  select(years:player, dob_date:age_int, days_since_bday)
oldest_goalkeepers_by_tournament

oldest_outfield_player_by_tournament = player_database %>%
  filter(!(pos == "GK")) %>%
  group_by(years) %>%
  top_n(1, age_years.days) %>%
  select(years:player, dob_date:age_int, days_since_bday)
oldest_outfield_player_by_tournament

# youngest players ever
youngest_players = arrange(player_database, age_years.days) %>%
  select(years:player, age_int, days_since_bday)
head(youngest_players, 5)

youngest_winners = player_database %>%
  filter(winner_flag == 1) %>%
  arrange(age_years.days) %>%
  select(years:player, age_int, days_since_bday)
head(youngest_winners, 5)

youngest_captain = player_database %>%
  filter(captain == 1) %>%
  arrange(age_years.days) %>%
  select(years:player, age_int, days_since_bday)
head(youngest_captain, 5)

youngest_players_by_tournament = player_database %>%
  group_by(years) %>%
  top_n(-1, age_years.days) %>%
  select(years:player, dob_date:age_int, days_since_bday)
youngest_players_by_tournament

youngest_captain_by_tournament = player_database %>%
  filter(captain == 1) %>%
  group_by(years) %>%
  top_n(-1, age_years.days) %>%
  select(years:player, dob_date:age_int, days_since_bday)
youngest_captain_by_tournament

youngest_goalkeepers_by_tournament = player_database %>%
  filter(pos == "GK") %>%
  group_by(years) %>%
  top_n(-1, age_years.days) %>%
  select(years:player, dob_date:age_int, days_since_bday)
youngest_goalkeepers_by_tournament


# 2.2 Teams
# oldest teams ever
oldest_teams = summary_stats_teams_by_year %>%
  arrange(desc(ave_age)) %>%
  select(years:nat_team_alpha3, ave_age)
head(oldest_teams,5)

oldest_winners_teams = summary_stats_teams_by_year %>%
  filter(winner_flag == 1) %>%
  arrange(desc(ave_age)) %>%
  select(years:nat_team_alpha3, ave_age)
head(oldest_winners_teams,3)

oldest_tournament = summary_stats_tournaments %>%
  arrange(desc(ave_age)) %>%
  select(years:count_teams, ave_age)
head(oldest_tournament, 3)

oldest_teams_by_tournament = summary_stats_teams_by_year %>%
  group_by(years) %>%
  top_n(1, ave_age) %>%
  select(years:nat_team_alpha3, ave_age)
oldest_teams_by_tournament

# youngest teams ever
youngest_teams = summary_stats_teams_by_year %>%
  arrange(ave_age) %>%
  select(years:nat_team_alpha3, ave_age)
head(youngest_teams,5)

youngest_winners_teams = summary_stats_teams_by_year %>%
  filter(winner_flag == 1) %>%
  arrange(ave_age) %>%
  select(years:nat_team_alpha3, ave_age)
head(youngest_winners_teams,3)

youngest_tournament = summary_stats_tournaments %>%
  arrange(ave_age) %>%
  select(years:count_teams, ave_age)
head(youngest_tournament, 3)

youngest_teams_by_tournament = summary_stats_teams_by_year %>%
  group_by(years) %>%
  top_n(-1, ave_age) %>%
  select(years:nat_team_alpha3, ave_age)
youngest_teams_by_tournament


# age difference within teams
largest_age_diff = summary_stats_teams_by_year %>%
  arrange(desc(age_diff)) %>%
  select(years:nat_team, age_diff:age_diff_dec)
head(largest_age_diff, 5)

largest_age_diff_winners = summary_stats_teams_by_year %>%
  filter(winner_flag == 1) %>%
  arrange(desc(age_diff)) %>%
  select(years:nat_team, age_diff:age_diff_dec)
head(largest_age_diff_winners, 5)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis - 3. Caps Records

# 3.1 Players
# most capped players ever
most_caps_players = arrange(player_database, desc(caps)) %>%
  select(years:player, caps)
head(most_caps_players, 5)

most_caps_winners = player_database %>%
  filter(winner_flag == 1) %>%
  arrange(desc(caps)) %>%
  select(years:player, caps)
head(most_caps_winners, 5)

most_caps_captain = player_database %>%
  filter(captain == 1) %>%
  arrange(desc(caps)) %>%
  select(years:player, caps)
head(most_caps_captain, 5)

most_caps_players_by_tournament = player_database %>%
  group_by(years) %>%
  top_n(1, caps) %>%
  select(years:player, caps)
most_caps_players_by_tournament

most_caps_captain_by_tournament = player_database %>%
  filter(captain == 1) %>%
  group_by(years) %>%
  top_n(1, caps) %>%
  select(years:player, caps)
most_caps_captain_by_tournament

# least capped players ever
least_caps_winners = player_database %>%
  filter(winner_flag == 1 & caps == 0) %>%
  select(years:player, caps)
least_caps_winners

least_caps_captain = player_database %>%
  filter(captain == 1 & caps == 0) %>%
  select(years:player, caps)
least_caps_captain


# 3.2 Teams
# most capped teams ever
most_caps_teams = summary_stats_teams_by_year %>%
  arrange(desc(sum_caps)) %>%
  select(years:nat_team_alpha3, sum_caps)
head(most_caps_teams,5)

most_caps_winners_teams = summary_stats_teams_by_year %>%
  filter(winner_flag == 1) %>%
  arrange(desc(sum_caps)) %>%
  select(years:nat_team_alpha3, sum_caps)
head(most_caps_winners_teams,3)

most_caps_teams_by_tournament = summary_stats_teams_by_year %>%
  group_by(years) %>%
  top_n(1, sum_caps) %>%
  select(years:nat_team_alpha3, sum_caps, host_flag:winner_flag)
most_caps_teams_by_tournament

# least capped teams ever
least_caps_teams = summary_stats_teams_by_year %>%
  filter(sum_caps == 0) %>%
  select(years:nat_team_alpha3, sum_caps)
least_caps_teams

least_caps_winners_teams = summary_stats_teams_by_year %>%
  filter(winner_flag == 1) %>%
  arrange(sum_caps) %>%
  select(years:nat_team_alpha3, sum_caps)
head(least_caps_winners_teams,3)

least_caps_teams_by_tournament = summary_stats_teams_by_year %>%
  group_by(years) %>%
  top_n(-1, sum_caps) %>%
  select(years:nat_team_alpha3, sum_caps)
least_caps_teams_by_tournament


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Graphs

# initialise variables for graph
start_yr = min(years)
end_yr = max(years)
pos_level_order = c("GK", "DF", "MF","FW")

winners_ages = summary_stats_teams_by_year %>%
  filter(winner_flag == 1) %>%
  select(years, ave_age, ave_caps) %>%
  rename("ave_age_winner" = "ave_age")

data_for_graph_01 = summary_stats_tournaments %>%
  select(years, ave_age) %>%
  cbind(winners_ages$ave_age_winner)

graph_01 = ggplot(data_for_graph_01) +
  geom_line(aes(x = years, y = ave_age), size=1.08) +
  geom_point(aes(x = years, y = ave_age)) +
  ggtitle(paste("Average Age of World Cup squads from", start_yr, "to", end_yr)) + 
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
  labs(x="Year", y="Average age (years)") + 
  # add points for winners age
  geom_point(aes(x = years, y = winners_ages$ave_age_winner), shape=8, fill = "red", colour = "red") 
graph_01

graph_02 = ggplot(player_database, aes(x = age_int)) +
  geom_histogram(binwidth = 0.5, colour = "black", fill = "white") +
  ggtitle(paste("Age Distribution of World Cup players from", start_yr, "to", end_yr)) + 
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
  labs(x="Age", y="Count") +
  scale_x_continuous(breaks = seq(20, 45, 5)) +
  geom_vline(aes(xintercept = mean(age_int, na.rm = T)),   
             colour = "red", linetype = "dashed", size = 1)
graph_02

graph_03 = ggplot(player_database, aes(x = age_int)) +
  geom_density() + 
#  facet_grid(pos ~ .) +
  facet_grid(fct_relevel(pos, pos_level_order) ~ .) +
  ggtitle(paste("Age Distribution of World Cup players by position from", start_yr, "to", end_yr)) + 
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
  labs(x="Age", y="Count") +
  scale_x_continuous(breaks = seq(20, 45, 5)) +
  geom_vline(data = summary_stats_teams_by_pos, aes(xintercept = ave_age),   
           colour = "red", linetype = "dashed", size = 1) 
graph_03

data_for_graph_04 = summary_stats_tournaments %>%
  select(years, sum_caps, ave_caps) %>%
  cbind(winners_ages$ave_caps)

graph_04 = ggplot(data_for_graph_04) +
  geom_line(aes(x = years, y = ave_caps), size=1.08) +
  geom_point(aes(x = years, y = ave_caps)) +
  ggtitle(paste("Average No. of Caps of World Cup squads from", start_yr, "to", end_yr)) + 
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
  labs(x="Year", y="Average caps") + 
  # add points for winners average caps
  geom_point(aes(x = years, y = winners_ages$ave_caps), shape=8, fill = "red", colour = "red") 
graph_04

data_for_graph_05 = summary_stats_tournaments %>%
  select(years, propn_count_club_country_same)

graph_05 = ggplot(data_for_graph_05) +
  geom_line(aes(x = years, y = propn_count_club_country_same), size=1) +
  geom_point(aes(x = years, y = propn_count_club_country_same)) +
  ggtitle(paste("Proportion of players at World Cups from", start_yr, "to", end_yr, "from clubs outside same country")) + 
  theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
  labs(x="Year", y="Proportion") +
  scale_y_continuous(labels = percent)
graph_05


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export output to csv format
setwd(dir_output)
write.csv(summary_stats_teams_by_year, file = "World_Cup_summary_stats_teams_by_year.csv")
write.csv(summary_stats_leagues, file = "World_Cup_summary_stats_leagues.csv")
write.csv(summary_stats_players, file = "World_Cup_summary_stats_players.csv")
write.csv(summary_stats_teams, file = "World_Cup_summary_stats_teams.csv")
write.csv(summary_stats_tournaments, file = "World_Cup_summary_stats_tournaments.csv")
setwd(dir_input)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Temporary (working) code




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# To do:

# Make a function that does the summaries, but just varies by the fields input
#  - summary_stats_teams_by_year is the same as summary_stats_teams_by_pos
# (do this for all summaries that are off the same source player_database)

# summary_stats_players - include nat_team(s) in the table

# Make animated graphs for:
# - graph_02: Age Distribution of players, by tournament
# - graph_03: Age Distribution of players by position, by tournament
