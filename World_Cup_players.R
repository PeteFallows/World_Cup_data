# Filename: "World_Cup_players.R"
# Description: Listings of all players at FIFA men's world cups from 1930 to current

# R code based off Guy Abel's work for Euro squads - https://github.com/guyabel/uefa-ec
# & World Cup - https://github.com/guyabel/fifa-wc
# where he scraped data from Wikipedia
## scrape_players: players in squads
## scrape_flags: national team flags (not yet used)
## scrape_colours: national team kits (not used here)
## scrape_comp: competition teams and logos (not yet used)

# Structure is:
# control_data: control table
# player_data_list: R list of players from Wikipedia pages
# player_data_unnested: unnested R list of players, with additional data for players
# player_data_full: player_data_unnested, with additional data for country codes


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries, directories & parameters

# load packages that are or may be used
library(tidyverse)    # includes ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(lubridate)
library(rvest)       # reading tables from a wikipedia page
library(countrycode) # used for consistency of country names, abbreviations
library(janitor)


# Set directory paths
# Read in data files - example directories
dir_main = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
dir_input = "C:/Users/fallo/OneDrive/Documents/Pete/R-files/Input"
dir_output = "C:/Users/fallo/OneDrive/Documents/Pete/R-files/R_output"

setwd(dir_main)


# Initialise variables & parameters
years = c(seq(1930, 1938, 4), seq(1950, 2018, 4))
no_teams = c(13, 16, 15, 13, rep(16, 7), rep(24, 4), rep(32, 6))

control_data <- tibble(
  years, 
  no_teams,
  url = paste0("https://en.wikipedia.org/wiki/", years, "_FIFA_World_Cup_squads")
) 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 

get_players <- function(u, n){
  # u = d$url[i];u
  # n = d$teams[i]
  h <- read_html(u)
  
  hh <- h %>%
    html_nodes("table.sortable")
  
  p <- tibble(
    squads = hh %>%
      html_table()
  ) %>%
    mutate(nn = map(.x = squads, .f = ~nrow(x = .x)), 
           nn = unlist(nn),
           squad_list = nn >= 11, 
           table_no = cumsum(squad_list), 
           squad_list = ifelse(table_no > n, FALSE, squad_list))
  
  
  s <- h %>%
    html_nodes(".mw-headline") %>%
    html_text() %>%
    str_subset(pattern = "Group", negate = TRUE) %>%
    .[1:n]
  
  not_line <- hh %>%
    .[p$squad_list] %>%
    html_nodes("td:nth-last-child(1)") %>%
    html_attr("colspan") %>%
    is.na()
  
  tibble(
    squad = s,
    player = p$squads[p$squad_list]
  ) %>%
    mutate(
      player = map(
        .x = player, 
        .f = ~mutate(.x, across(everything(), as.character))
      )) %>%
    unnest(cols = player) %>%
    filter(Player != "") %>%
    mutate(player_url = hh %>%
             .[p$squad_list] %>%
             html_nodes(".nat-fs-player th") %>%
             html_node("a") %>%
             html_attr("href"),
           club_fa_url = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node("a") %>%
             html_attr("href"),
           club_fa = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node("a") %>%
             html_attr("title"),
           club_url = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_attr("href"),
           club_league_country_original = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node(".flagicon") %>%
             html_node(".thumbborder") %>%
             html_attr("alt"),
           club_league_country_flag = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node(".flagicon") %>%
             html_node("img") %>%
             html_attr("src")
    )
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files

setwd(dir_input)

# Note: contains multiple hosts for 2002
tournament_details = read_csv("World_Cup_tournament_details.csv")
tournament_details$tournament_start_date = as.Date(tournament_details$tournament_start_date, format = "%d/%m/%Y")

# only contains countries that have teams for clubs represented
confederations = read_csv("Football_confederations.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations


# Includes data cleansing for fields which have values that are not consistent throughout
# & add additional variables
player_data_list <- control_data %>%
  mutate(players = map2(.x = url, .y = no_teams, 
                        .f = ~get_players(u = .x, n = .y)))


player_data_unnested <- player_data_list %>%
  unnest(cols = c("players")) %>%
  clean_names() %>%
  select(-url, -no_teams) %>%
  mutate(pos = str_sub(string = pos, start = 2),
         captain = str_detect(string = player, pattern = "captain|Captain|\\(c\\)"),
         player_original = player,
         player = str_remove(string = player, pattern = " \\s*\\([^\\)]+\\)"),
         player = str_replace(player, "\\[.*\\]", ""),
         date_of_birth_age = str_replace(date_of_birth_age, "\\( ", "\\("),
         date_of_birth_age = str_replace(date_of_birth_age, "age ", "aged "),
         date_of_birth_age = ifelse(date_of_birth_age == "", NA, date_of_birth_age),
         caps = str_replace(caps, "\\/", "\\"),
         caps = str_replace(caps, "\\?", "NA"),
         caps = str_replace(caps, "0-", "NA"),
         caps = str_replace(caps, "-", "NA"),
         caps = str_replace(caps, "\\[.*\\]", "NA"),
         caps = str_replace(caps, "\\*", ""),
         club = str_replace(club, "\\[.*\\]", ""),
         dob_age_format = str_count(date_of_birth_age, "\\("),
         dob_length = nchar(date_of_birth_age),
         dob_1 = ifelse(dob_age_format == 2, substr(date_of_birth_age, 2, 11),
                        ifelse(dob_age_format == 0, paste0(substr(date_of_birth_age, 1, 4), "-01-01", sep = ""), NA)),
         dob_2 = ifelse(dob_age_format == 2, substr(date_of_birth_age, 13, dob_length - 10),
                        ifelse(dob_age_format == 1, substr(date_of_birth_age, 1, dob_length - 10), NA)),
         dob_1_date = as.Date(dob_1, format = "%Y-%m-%d"),
         dob_2_date = as.Date(dob_2, format = "%d %B %Y"),
         dob_to_use = ifelse(dob_age_format >= 1, dob_2_date, dob_1_date), 
         dob_date = as_date(days(as.numeric(dob_to_use))),
         age_int = ifelse(dob_age_format >= 1, as.numeric(substr(date_of_birth_age, dob_length - 2, dob_length - 1)), 
                          years - as.numeric(date_of_birth_age))) %>%
  # remove helper variables
  select(c(years:player_original, dob_date:age_int))
player_data_unnested$caps = as.numeric(player_data_unnested$caps)

country_custom_match <- c("CIS" = "CIS", 
        "CSSR" = "CSK",
        "Czechoslovakia" = "CSK",
        "England" = "GB-ENG", 
        "Northern Ireland" = "GB-NIR",
        "Scotland" = "GB-SCT",
        "Wales" = "GB-WLS", 
        "Serbia and Montenegro" = "SCG",
        "FR Yugoslavia" = "SCG",
        "Yugoslavia" = "YUG",
        "Kingdom of Yugoslavia" = "YUG",
        "Federal Republic of Yugoslavia" = "YUG",
        "USSR" = "SUN",
        "Soviet Union" = "SUN",
        "East Germany" = "DDR",
        "Dutch East Indies" = "DEI")


player_data_full <- player_data_unnested %>%
  mutate(nat_team = case_when(
    squad == "Soviet Union" ~ "USSR",
    squad == "China PR" ~ "China",
    TRUE ~ squad),
    club_league_country = case_when(
      club_league_country_original == "Wales" ~ "England",
      club_league_country_original == "Soviet Union" ~ "USSR",
      club_league_country_original == "Socialist Federal Republic of Yugoslavia" ~ "Yugoslavia",
      club_league_country_original == "Kingdom of Yugoslavia" ~ "Yugoslavia",
      club_league_country_original == "Federal Republic of Yugoslavia"  ~ "FR Yugoslavia",
      TRUE ~ club_league_country_original),
    club_country = case_when(
      # clubs which play in a league in a different country, Does not include NASL, MLS.
      club == "Wellington Phoenix" ~ "New Zealand",
      club == "Cardiff City" ~ "Wales",
      club == "Swansea Town" ~ "Wales",
      club == "Swansea City" ~ "Wales",
      club == "Wrexham" ~ "Wales",
      club == "AS Monaco" ~ "Monaco",
      club == "Monaco" ~ "Monaco",
      TRUE ~ club_league_country),   
     nat_team_alpha3 = countrycode(
      sourcevar = nat_team, origin = "country.name", 
      destination = "iso3c", custom_match = country_custom_match),
    club_league_alpha3 = countrycode(
      sourcevar = club_league_country, origin = "country.name", 
      destination = "iso3c", custom_match = country_custom_match),
    club_country_alpha3 = countrycode(
      sourcevar = club_country, origin = "country.name", 
      destination = "iso3c", custom_match = country_custom_match),
    club_league_country_flag = str_remove(string = club_league_country_flag, pattern = "thumb"),
    club_league_country_flag = str_remove(string = club_league_country_flag, pattern = "/[^/]+$"),
    club_league_same = replace_na(ifelse(nat_team_alpha3 == club_league_alpha3, 1, 0), 0),
    club_country_same = replace_na(ifelse(nat_team_alpha3 == club_country_alpha3, 1, 0), 0),
    club_league_country_same = replace_na(ifelse(club_league_alpha3 == club_country_alpha3, 1, 0), 0)) %>%
  # get additional information from input tables
  left_join(confederations, by = c("nat_team" = "Country")) %>%
  rename("nat_team_confederation" = "Confederation") %>%
  left_join(confederations, by = c("club_league_country" = "Country")) %>%
  rename("club_confederation" = "Confederation") %>%
  mutate(nat_team_confederation = case_when(
    nat_team == "Australia" & years <= 2006 ~ "OFC",
    nat_team == "Israel" & years <= 1974 ~ "OFC",
    TRUE ~ nat_team_confederation),
    club_confederation = case_when(
      club_league_country == "Australia" & years <= 2006 ~ "OFC",
      club_league_country == "Israel" & years <= 1974 ~ "OFC",
      club_league_country == "Réunion" ~ "CAF",
      TRUE ~ club_confederation))

player_database = player_data_full %>%
  left_join(tournament_details , by = c("years" = "Year")) %>%
  mutate(last_bday = dob_date + ifelse(month(dob_date) == 2 & day(dob_date) == 29, -1, 0) + years(age_int),
         days_since_bday = as.numeric(tournament_start_date - last_bday),
         age_years.days = age_int + days_since_bday / 365.25,
         dob_age_mismatch = ifelse(last_bday > tournament_start_date, 1, 0), # omit any further data cleansing for these cases
         host_flag = ifelse(nat_team == Host_country, 1, 0),
         winner_flag = ifelse(nat_team == Winner, 1, 0),
         runner_up_flag = ifelse(nat_team == Runner_up, 1, 0)) %>%
  select(-c(Host_country:Runner_up)) %>%
  distinct() %>%
  filter(!(host_flag == 0 & years == 2002 & nat_team %in% c("Japan", "South Korea"))) %>%
  group_by(player, player_url, nat_team_alpha3) %>%
  mutate(count_appearances_by_team = n()) %>%
  ungroup %>%
  group_by(player, player_url) %>%
  mutate(count_teams = n(),
         first_appearance = min(years)) %>%
  ungroup


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis - 

# checks on nat_team and club_league_country match
unatached_players = player_data_full %>%
  # filter(is.na(nat_team))
  # filter(is.na(nat_team_alpha3))
  # filter(is.na(club_league_country))
  filter(is.na(club_league_alpha3))

# list of countries represented in data - either player or club
countries_represented = player_data_full %>%
  select(nat_team, club_league_country) %>%
  pivot_longer(cols = 1:2, names_to = "type", values_to = "label") %>%
  select(-type) %>%
  distinct() %>%
  arrange(label)

# players from Australian clubs
aust = player_data_full %>%
  filter(club_league_country == "Australia")

# players with multiple countries
multiple_teams = player_database %>%
  filter(!(count_appearances_by_team == count_teams)) %>%
  arrange(first_appearance, player) %>%
  select(-first_appearance)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export output to csv format
setwd(dir_output)
write.csv(player_database, file = "World_Cup_player_database.csv")
write.csv(countries_represented, file = "World_Cup_countries_represented.csv")
setwd(dir_input) 



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Graphs



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Temporary (working) code

# Sample scrape to get players from one wiki page
 i = 16
 get_players(u = d$url[i], n = d$teams[i]) %>%
   group_by(squad) %>%
   summarise(n = n()) %>%
   arrange(desc(n)) %>%
   print(n = 32)
 for(i in 1:nrow(d))
   get_players(u = d$url[i], n = d$teams[i])


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# To do:
 
 
# Review output - World_Cup_player_data.csv
# club_url is NA. Amend function to have club_url showing.
 
# Review Guy Abel's other scripts 
 ## scrape_flag: national team flags (not yet used)
 ## scrape_comp: competition teams and logos (not yet used)