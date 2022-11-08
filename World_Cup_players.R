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
# control_data: table of parameters
# player_data_list: R list of players from Wikipedia pages
# player_data_unnested: unnested R list of players, with additional data for players
# player_data_with_countries: with additional data for countries
# player_data_with_correct_names: with data cleansing for player names
# player_data_with_correct_clubnames: with data cleansing for player's club's names
# player_data_with_ages: with additional data for dates & ages
# player_database: full database of player data, after additional info & data cleansing


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

pos_level_order = c("GK", "DF", "MF","FW")

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
             html_node("a:nth-child(2)") %>%
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

# sort letters in a string in alphabetical order
strSort <- function(x)
  sapply(lapply(strsplit(x, NULL), sort), paste, collapse="")

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
         no_original = no,
         no = ifelse(years <= 1950, NA, as.numeric(no)),
         player_original = player,
         player = str_remove(string = player, pattern = " \\s*\\([^\\)]+\\)"),
         player = str_replace(player, "\\[.*\\]", ""),
         player = str_replace(player, "\\*", ""),
         player = str_to_title(player),
         player_url = tolower(player_url),
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
         club = str_replace(club, "\\*", ""),
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
        "FR Yugoslavia" = "YUG",
        "Yugoslavia" = "YUG",
        "Kingdom of Yugoslavia" = "YUG",
        "Federal Republic of Yugoslavia" = "YUG",
        "USSR" = "SUN",
        "Soviet Union" = "SUN",
        "East Germany" = "DDR",
        "Dutch East Indies" = "DEI")


player_data_with_countries <- player_data_unnested %>%
  mutate(nat_team = case_when(
    squad == "Soviet Union" ~ "USSR",
    squad == "China PR" ~ "China",
    TRUE ~ squad),
    club_league_country = case_when(
      club_league_country_original == "Wales" ~ "England",
      club_league_country_original == "Soviet Union" ~ "USSR",
      club_league_country_original == "Socialist Federal Republic of Yugoslavia" ~ "Yugoslavia",
      club_league_country_original == "Kingdom of Yugoslavia" ~ "Yugoslavia",
      club_league_country_original == "Federal Republic of Yugoslavia"  ~ "Yugoslavia",
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
    club_league_country_same = replace_na(ifelse(club_league_alpha3 == club_country_alpha3, 1, 0), 0),
    # adjust for Serbia and Montenegro compared to Serbia
    club_league_same = ifelse(nat_team_alpha3 == "SCG" &  club_league_alpha3 == "SRB", 1, club_league_same),
    club_country_same = ifelse(nat_team_alpha3 == "SCG" &  club_country_alpha3 == "SRB", 1, club_country_same)) %>%
  # get additional information from input tables
  left_join(confederations, by = c("nat_team" = "Country")) %>%
  rename("team_confederation_current" = "Confederation") %>%
  left_join(confederations, by = c("club_league_country" = "Country")) %>%
  rename("club_confederation" = "Confederation") %>%
  mutate(nat_team_confederation = team_confederation_current,
         nat_team_confederation = case_when(
           nat_team == "Australia" & years <= 2006 ~ "OFC",
           nat_team == "Israel" & years <= 1974 ~ "OFC",
           TRUE ~ nat_team_confederation),
    club_confederation = case_when(
      club_league_country == "Australia" & years <= 2006 ~ "OFC",
      club_league_country == "Israel" & years <= 1974 ~ "OFC",
      club_league_country == "Réunion" ~ "CAF",
      TRUE ~ club_confederation),
    nat_team_current_name = ifelse(nat_team_alpha3 == "DEU", "Germany", nat_team),
    club_country_current_name = case_when(
      club_country_alpha3 == "DEU" ~ "Germany",
      club_country_alpha3 == "DDR" ~ "Germany",
      club_country_alpha3 == "SUN" ~ "Russia",
      club_country_alpha3 == "CSK" ~ "Czech Republic",
      TRUE ~ club_country)) %>%
    mutate(club_country_current_name = case_when(
      # German Third Reich
      club == "Admira Wien" & years == 1938 ~ "Austria",
      club == "SC Ostmark Wien" & years == 1938 ~ "Austria",
      club == "First Vienna" & years == 1938 ~ "Austria",
      club == "Rapid Wien" & years == 1938 ~ "Austria",
      
      # Former USSR (not to Russia)
      club == "Dynamo Kyiv" ~ "Ukraine",
      club == "Chornomorets Odesa" ~ "Ukraine",
      club == "Dnipro Dnipropetrovsk" ~ "Ukraine",
      club == "Ararat Yerevan" ~ "Armenia",
      club == "Dinamo Tbilisi" ~ "Georgia",
      club == "Dynamo Minsk" ~ "Belarus",
      club == "Dinamo Minsk" ~ "Belarus",
      club == "Neftyanik Baku" ~ "Azerbaijan",
      
      # Yugoslavia
      club == "Dinamo Zagreb" ~ "Croatia",
      club == "Hajduk Split" ~ "Croatia",
      club == "Rijeka" ~ "Croatia",
      club == "Lokomotiva Zagreb" ~ "Croatia",
      club == "Olimpija Ljubljana" ~ "Slovenia",
      club == "Partizan" ~ "Serbia",
      club == "Partizan Belgrade" ~ "Serbia",
      club == "Red Star Belgrade" ~ "Serbia",
      club == "OFK Belgrade" ~ "Serbia",
      club == "BSK Beograd" ~ "Serbia",
      club == "Jugoslavija Beograd" ~ "Serbia",
      club == "SK Jugoslavija" ~ "Serbia",
      club == "Nasa Krila Zemun" ~ "Serbia",
      club == "SK Soko" ~ "Serbia",
      club == "Spartak Subotica" ~ "Serbia",
      club == "Vojvodina Novi Sad" ~ "Serbia",
      club == "SK Vojvodina" ~ "Serbia",
      club == "Sarajevo" ~ "Bosnia and Herzegovina",
      club == "Sloboda Tuzla" ~ "Bosnia and Herzegovina",
      club == "Velez Mostar" ~ "Bosnia and Herzegovina",
      # the following did not map properly just by the characters in the club name
      club_url == "/wiki/Radni%C4%8Dki_Belgrade" ~ "Serbia",
      club_url == "/wiki/FK_%C5%BDeljezni%C4%8Dar_Sarajevo" ~ "Bosnia and Herzegovina",
      club_url == "/wiki/FK_Budu%C4%87nost_Podgorica" ~ "Montenegro",
      
      # Czechoslovakia (not to Czech Republic)
      club == "Slovan Bratislava" ~ "Slovakia",
      club == "SK Slovan Bratislava" ~ "Slovakia",
      club == "TJ Slovan CHZJD" ~ "Slovakia",
      club == "Inter Bratislava" ~ "Slovakia",
      club == "CH Bratislava" ~ "Slovakia",
      club == "TJ Internacionál Slovnaft" ~ "Slovakia",
      club == "TJ Slovnaft Bratislava" ~ "Slovakia",
      club == "CsSK Bratislava" ~ "Slovakia",
      club == "Baník Handlová" ~ "Slovakia",
      club == "Dunajská Streda" ~ "Slovakia",
      club == "FC Spartak Trnava" ~ "Slovakia",
      club == "Iskra Zilina" ~ "Slovakia",
      club == "Jednota Trencín" ~ "Slovakia",
      club == "Lokomotiva Kosice" ~ "Slovakia",
      club == "VSS Kosice" ~ "Slovakia",
      club == "Spartak TAZ Trnava" ~ "Slovakia",
      club == "Spartak Trnava" ~ "Slovakia",
      club == "Plastika Nitra" ~ "Slovakia",
      club == "Tatran Presov" ~ "Slovakia",
      club == "Trnava" ~ "Slovakia",
      
      TRUE ~ club_country_current_name)) 

# Correct the player name for different names for same wiki page url. Assume can only have one wiki page.
# Use player_url later combined with player for unique player ID.
player_urls_multiple_names = player_data_with_countries %>%
  select(player, player_url) %>%
  distinct() %>%
  group_by(player_url) %>%
  summarise(count_players_for_url = n()) %>%
  filter(count_players_for_url > 1)

player_names_correct = player_data_with_countries %>%
  left_join(player_urls_multiple_names, by = c("player_url" = "player_url")) %>%
  filter(count_players_for_url > 1) %>%
  select(years, player, player_url) %>%
  group_by(player_url) %>%
  mutate(max_years = max(years)) %>%
  ungroup() %>%
  filter(years == max_years) %>%
  select(player:player_url) %>%
  rename("player_correct_name" = 'player')

player_data_with_correct_names = player_data_with_countries %>%
  left_join(player_names_correct , by = c("player_url" = "player_url")) %>%
  mutate(player = ifelse(is.na(player_correct_name), player, player_correct_name))



# Correct the club name for different names for same wiki page url. Assume can only have one wiki page.
# Unlike player_url, which is used combined with player for unique player ID, club_url will not be used later.
# The unique listing for club will be club_current_name & country_current_name.
club_urls_multiple_names = player_data_with_countries %>%
  select(club, club_country_current_name, club_url) %>%
  distinct() %>%
  group_by(club_url, club_country_current_name) %>%
  summarise(count_clubs_for_url = n()) %>%
  filter(count_clubs_for_url > 1)

club_names_correct = player_data_with_countries %>%
  mutate(row_num = row_number()) %>%
  left_join(club_urls_multiple_names , by = c("club_url" = "club_url",
                                          "club_country_current_name" = "club_country_current_name")) %>%
  select(row_num, club, club_country_current_name, club_url) %>%
  group_by(club_url, club_country_current_name) %>%
  mutate(max_rownum = max(row_num)) %>%
  ungroup() %>%
  filter(row_num == max_rownum) %>%
  select(club:club_url) %>%
  rename("club_correct_name_from_url" = 'club') %>%
  distinct()

player_data_with_correct_clubnames = player_data_with_correct_names %>%
  left_join(club_names_correct, by = c("club_url" = "club_url",
                                        "club_country_current_name" = "club_country_current_name")) %>%
  mutate(club_original = club,
         club_current_name = ifelse(is.na(club_correct_name_from_url), club, club_correct_name_from_url)) %>%
  # change names of clubs manually for cases where clubs have different urls & names but are same club
  mutate(club_current_name = case_when(
      # Algeria
      club == "RS Kouba" ~ "RC Kouba",
      # Argentina
      club == "Colón de Santa Fe" ~ "Colón",
      # Austria
      club == "Wacker Wien" ~ "Admira Wacker",
      club == "Wiener SC" ~ "Wiener Sport-Club",
      club == "Wiener Sportclub" ~ "Wiener Sport-Club",
      club == "Linzer ASK" ~ "LASK",
      # Belgium
      club == "Germinal Ekeren" ~ "Germinal Beerschot",
      club == "Royal FC Malinois" ~ "Mechelen",
      club == "Royal FC Brugeois" ~ "Club Brugge",
      club == "Royal Racing Club de Gand" ~ "Gent",
      club == "SC Anderlechtois" ~ "Anderlecht",
      club == "R. Union Saint-Gilloise" ~ "Union St. Gilloise",
      # Bolivia
      club == "Club Bolivar" ~ "Bolívar",
      # Brazil
      club == "America-RJ" ~ "América",
      # Cameroon
      club == "Canon Yaoundé" ~ "Canon Yaounde",
      club == "Cottonsport Garoua" ~ "Coton Sport",
      club == "Cotonsport Garoua" ~ "Coton Sport",
      club == "Prévoyance Yaoundé" ~ "Prevoyance Yaounde",
      club == "Tonnerre Yaounde" ~ "Tonnerre Yaoundé",
      club == "Dragon Douala" ~ "Dragon Douala",
      # Chile
      club == "Colo Colo" ~ "Colo-Colo",
      # China
      club == "Beijing Guo'an" ~ "Beijing Sinobo Guoan",
      club == "Beijing Guoan" ~ "Beijing Sinobo Guoan",
      club == "Tianjin Teda" ~ "Tianjin TEDA",
      # Croatia
      club == "Lokomotiva Zagreb" ~ "Lokomotiva",
      # Cuba
      club == "Centro Gallego" ~ "CD Centro Gallego",
      club == "Iberia Habana" ~ "Iberia Havana",
      club == "Juventud Aturiana" ~ "Juventud Asturiana",
      # Former Czechoslovakia
      club == "Zidenice" ~ "SK Zidenice",
      # Egypt
      club == "Zamalek Mokhtalat" ~ "Zamalek",
      # France
      club == "RC Strasbourg" ~ "Strasbourg",
      club == "FC Sète" ~ "Sète",
      club == "FC Sochaux" ~ "Sochaux",
      club == "RC Paris" ~ "Racing Paris",
      club == "Racing Club de France" ~ "Racing Paris",
      # Germany
      club == "SG Dynamo Dresden" ~ "Dynamo Dresden",
      # Hungary
      club == "Pécs" ~ "Pécsi Munkás",
      club == "Tatabányai Bányász SE" ~ "Tatabányai Bányász",
      club == "Budapesti Dózsa" ~ "Újpesti Dózsa",
      club == "MTK" ~ "MTK Hungária",
      # Italy
      club == "Internazionale Milan" ~ "Inter Milan",
      # Japan
      club == "JEF United Ichihara" ~ "JEF United",
      # Mexico
      club == "CD Zacatepec" ~ " Zacatepec",
      club == "CD Guadalajara" ~ "Guadalajara",
      club == "CF Atlas" ~ "Atlas",
      club == "CF Monterrey" ~ "Monterrey",
      club == "Léon" ~ "León",
      # Morocco
      club == "Chabab Mohammedia" ~ " Chabab Mohammédia",
      # Netherlands
      club == "Ajax Amsterdam" ~ "Ajax",
      club == "SC Feyenoord Rotterdam" ~ "Feyenoord",
      # North Korea
      club == "8 February" ~ "April 25",
      # Norway
      club == "Vålerengen" ~ "Vålerenga",
      # Paraguay
      club == "Guarani" ~ "Guaraní",
      # Portugal
      club == "Vitória de Setúbal" ~ "Vitória de Guimarães",
      # Russia
      club == "SKA Rostov" ~ "Rostov",
      club == "Zenit Leningrad" ~ "Zenit Saint Petersburg",
      # Serbia
      club == "OFK Belgrade" ~ "OFK Beograd",
      club == "Partizan Belgrade" ~ "Partizan",
      # South Korea
      club == "Daewoo Royals" ~ "Busan IPark",
      # Spain
      club == "Donostia" ~ "Real Sociedad",
      club == "FC Oviedo" ~ "Oviedo",
      # Switzerland
      club == "Lucerne" ~ "Luzern",
      club == "Young Boys Bern" ~ "Young Boys",
      # Turkey
      club == "Fenerbahçe SK" ~ "Fenerbahçe",
      club == "Galatasaray S.K." ~ "Galatasaray",
      
      # other
      club == "Free agent" ~ "Free agent",
      club == "Unattached" ~ "Unattached",

      # former Dutch East Indies
      squad == "Dutch East Indies" ~ club_original,

      # clubs with name from more than one country
      club == "Real España" & club_country == "Mexico" ~ "Real Club España",

      # no changes needed
      TRUE ~ club_current_name)) %>%
  mutate(club_current_name = case_when(
    club_correct_name_from_url == "TR Veracruz" ~ "Veracruz",
    club_correct_name_from_url == "Ferencvárosi TC" ~ "Ferencvárosi",
    club_url == "/wiki/R%C3%A1ba_ETO_Gy%C5%91r" ~ "Gyori",
    club_url == "/wiki/Gy%C5%91ri_ETO_FC" ~ "Gyori",
    club_correct_name_from_url == "Internazionale" ~ "Inter Milan",
    club_correct_name_from_url == "Internazionale Milano F.C." ~ "Inter Milan",
    club_correct_name_from_url == "SC Young Fellows" ~ "YF Juventus",

    club_url == "/wiki/Kas%C4%B1mpa%C5%9Fa_SK"  ~ "Kasimpasa",
    club_url == "/wiki/Kas%C4%B1mpa%C5%9Fa_S.K."  ~ "Kasimpasa",
    club_url == "/wiki/Chinezul_Timi%C8%99oara" ~ "Chinezul Timi??oara",
    TRUE ~ club_current_name))


# add in age information & other flags
player_data_with_ages = player_data_with_correct_clubnames %>%
  left_join(tournament_details , by = c("years" = "Year")) %>%
  mutate(last_bday = dob_date + ifelse(month(dob_date) == 2 & day(dob_date) == 29, -1, 0) + years(age_int),
         days_since_bday = as.numeric(tournament_start_date - last_bday),
         age_years.days = age_int + days_since_bday / 365.25,
         birth_month = month(dob_date),
         birthday = format(dob_date, "%d %b"),
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
  mutate(app_count = 1, 
         count_appearances = n(),
         first_appearance = min(years),
         appearance_no = cumsum(app_count),
         nat_team_1 = nat_team[1],
         nat_team_2 = nat_team[2],
         nat_team_3 = nat_team[3],
         nat_team_4 = nat_team[4],
         nat_team_5 = nat_team[5],
         concat_teams_1_2 = ifelse(is.na(nat_team_2) | nat_team_2 == nat_team_1, nat_team_1, paste(nat_team_1, ", ",nat_team_2, sep = "")),
         teams_represented = ifelse(is.na(nat_team_3) | nat_team_3 == nat_team_2, concat_teams_1_2, paste(concat_teams_1_2, ", ",nat_team_3, sep = "")),
         nat_team_confed_1 = nat_team_confederation[1],
         nat_team_confed_2 = nat_team_confederation[2],
         # only need to add to this if player changes confed in their third tournament
         confeds_represented = ifelse(is.na(nat_team_confed_2) | nat_team_confed_2 == nat_team_confed_1, nat_team_confed_1, paste(nat_team_confed_1, ", ",nat_team_confed_2, sep = "")),
         first_confed = nat_team_confed_1,
         name_characters = str_replace_all(player, "\"", "'"),
         name_characters = str_replace(name_characters, "\\'.*\\'", ""),
         name_characters = str_replace_all(name_characters, "\'", ""),
         name_characters = tolower(strSort(name_characters)), 
         name_characters = str_replace_all(name_characters, " ", ""),
         name_characters = str_replace_all(name_characters, "\\.", ""),
         name_characters = str_replace_all(name_characters, "\\-", ""),
         n_chars = nchar(name_characters)) %>%
  ungroup %>%
  # remove helper variables
  select(-c(app_count, nat_team_1:concat_teams_1_2, nat_team_confed_1:nat_team_confed_2))
player_data_with_ages$pos <- factor(player_data_with_ages$pos, ordered = TRUE, levels = pos_level_order)


player_database = player_data_with_ages


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis - 

# list of all clubs by country
list_unique_clubs = player_database %>%
  group_by(club_current_name, club_country_current_name) %>%
  summarise(count = n(),
            min_year = min(years),
            max_year = max(years)) %>%
  arrange(club_country_current_name, club_current_name, desc(max_year), count)   

new_clubs = list_unique_clubs %>%
  filter(min_year == max(years))

# checks on nat_team and club_league_country match
unattached_players = player_database %>%
  # filter(is.na(nat_team))
  # filter(is.na(nat_team_alpha3))
  # filter(is.na(club_league_country))
  filter(is.na(club_league_alpha3))

# list of countries represented in data - either player or club
countries_represented = player_database %>%
  select(years, nat_team, club_league_country) %>%
  pivot_longer(cols = 2:3, names_to = "type", values_to = "country") %>%
  select(-type) %>%
  group_by(country) %>%
  summarise(min_year = min(years),
            max_year = max(years)) %>%
  arrange(country)

new_countries = countries_represented %>%
  filter(min_year == max(years))

# players from Australian clubs
aust = player_database %>%
  filter(club_league_country == "Australia")

# players with multiple countries
multiple_teams = player_database %>%
  filter(!(count_appearances_by_team == count_appearances)) %>%
  arrange(first_appearance, player) %>%
  select(-first_appearance)

# winning captains
winning_captains = player_database %>%
  filter(winner_flag == 1 & captain == 1) %>%
  select(years:club, club_current_name, club_country_current_name, birthday, age_int,days_since_bday)
winning_captains


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export output to csv format
setwd(dir_output)
save(player_database, file = "World_Cup_player_database.Rdata")
write.csv(player_database, file = "World_Cup_player_database.csv")

write.csv(list_unique_clubs, file = "World_Cup_list_clubs.csv")
write.csv(countries_represented, file = "World_Cup_countries_represented.csv")
write.csv(multiple_teams, file = "World_Cup_multiple_teams.csv")
#write.csv(winning_captains, file = "World_Cup_winning_captains.csv")
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
 
# Small number of errors in club_current_name. (usually related to non-English alphabet characters)

# (review vignette for countrycode package) Update current country name for Dutch East Indies, Zaire, and any others?

# Review Guy Abel's other scripts 
 ## scrape_flag: national team flags (not yet used)
 ## scrape_comp: competition teams and logos (not yet used)