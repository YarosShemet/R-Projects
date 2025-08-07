
# Packages ----------------------------------------------------------------

library(devtools)
#devtools::install_github("JaseZiv/worldfootballR", ref = "main")
#devtools::install_github("statsbomb/StatsBombR")
#install.packages("worldfootballR")
#install.packages('StatsBombR')
#install.packages('plm')
library(worldfootballR)
library(stargazer)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(lubridate)
library(plm)
library(MASS)
library(car)
library(rpart.plot)
library(gplots)
library(lmtest)
library(tseries)
library(tidyr)
library(pastecs)
library(psych)
library(Hmisc)
library(corrplot)
library(sandwich)
library(strucchange)

mapped_players <- player_dictionary_mapping()
dplyr::glimpse(mapped_players)
time_dim <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

# Players data --------------------------------------------------------------
#bundes_team_players_raw <- data.frame()
#for (i in 1:8){ 
#  team_urls <- tm_league_team_urls(country_name = "Germany", start_year = time_dim[i]) #start_year = 
#  bundes_team_players_year <- cbind(tm_squad_stats(team_url = team_urls), year = time_dim[i])
#  bundes_team_players_year <- bundes_team_players_year[, c('year', 'player_name', 'team_name','player_pos', 'player_age', 'nationality',
#                                                           'goals', 'minutes_played')]
#  bundes_team_players_raw <- rbind(bundes_team_players_raw,  bundes_team_players_year)
#}

#team_urls <- tm_league_team_urls(country_name = "Germany", start_year = 2016)
#bundes_team_players_2016 <- tm_squad_stats(team_url = team_urls)

#bundes_team_players_2015 <- cbind(bundes_team_players_2015, year = 2015)
#bundes_team_players_2016 <- cbind(bundes_team_players_2016, year = 2016)

#bundes_team_players <- rbind(bundes_team_players_2020[, c('year', 'player_name', 'team_name','player_pos', 'player_age', 'nationality',
#                                                          'goals', 'minutes_played')], bundes_team_players_2016[, c('year', 'player_name', 'team_name','player_pos', 'player_age', 'nationality', 'goals', 'minutes_played')])
#player_url ???
#player_pos obtain statistics in each group (avg, quantile) as.factor
#nationality ElseIf region ??? 
#goals for strikers, clean sheets for goalkeepers/defenders, assists for midfildiers

#bundes_team_players$nationality <- as.factor(bundes_team_players$nationality) #transformacja

#write.csv(bundes_team_players, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_team_players.csv", row.names=FALSE)
bundes_team_players <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_team_players.csv")

bundes_team_players <- filter(bundes_team_players, minutes_played > 60) #work with only those, who play more than an hour througth season

bundes_team_players$player_pos <- as.factor(bundes_team_players$player_pos)
levels(bundes_team_players$player_pos)
bundes_team_players$player_pos <- ifelse(bundes_team_players$player_pos %in% c('Centre-Back', 'Defender', 'Left-Back', 'Right-Back'), 'Defence', 
                                          ifelse(bundes_team_players$player_pos %in% c('Attacking Midfield', 'Central Midfield', 'Defensive Midfield', 'Left Midfield', 'Right Midfield'), 'Midfield',
                                                 ifelse(bundes_team_players$player_pos %in% c('Centre-Forward', 'Left Winger', 'Right Winger', 'Second Striker'), 'Attack', 'Goalkeeper')))

bundes_team_players$nationality <- as.factor(bundes_team_players$nationality)
levels(bundes_team_players$nationality)
european_countries <- c("England", "Wales", "Ireland", "Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Vatican City")
african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Democratic Republic of the Congo", "Republic of the Congo", "Cote d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
asian_countries <- c("Afghanistan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar (Burma)", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Russia", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand", "Timor-Leste (East Timor)", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen")
american_countries <- c("Antigua and Barbuda", "Argentina", "Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "United States", "Uruguay", "Venezuela")
bundes_team_players$nationality_region <- ifelse(bundes_team_players$nationality %in% european_countries, 'Europe',
                                            ifelse(bundes_team_players$nationality %in% american_countries, 'America', 'Africa or Asia'))
nrow(filter(bundes_team_players, nationality_region=='Africa or Asia'))
nrow(filter(bundes_team_players, nationality_region=='America'))
nrow(filter(bundes_team_players, nationality_region=='Europe'))
bundes_team_players$player_pos <- as.factor(bundes_team_players$player_pos)
bundes_team_players$nationality_region <- as.factor(bundes_team_players$nationality_region)
bundes_team_players$player_age_2 <- bundes_team_players$player_age^2
#goals (no lag), minutes_played (no lag)
bundes_team_players <- dplyr::select(bundes_team_players, c(-6, -7, -8)) 
bundes_team_players <- bundes_team_players[, c(1, 2, 3, 4, 5, 7, 6)]
#start_year

# Valuations --------------------------------------------------------------

#bundes_valuations_raw <- data.frame()
#for (i in 1:8){ 
#  bundes_valuations_year <- tm_player_market_values(country_name = "Germany", start_year = time_dim[i]) #foot, contract_expiry, market_value
#  bundes_valuations_raw <- rbind(bundes_valuations_raw,  bundes_valuations_year)
#}
#write.csv(bundes_valuations_raw, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_valuations.csv", row.names=FALSE)
bundes_valuations_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_valuations.csv")
bundes_valuations_raw <- bundes_valuations_raw[, c(4, 7, 14, 18)] #17 - contract_expiry
bundes_valuations_raw <- na.omit(bundes_valuations_raw)
#str(bundes_valuations_raw$contract_expiry)
#bundes_valuations_raw$player_market_value_euro <- bundes_valuations_raw$player_market_value_euro/1000
#bundes_valuations_raw$contract_expiry <- ymd(bundes_valuations_raw$contract_expiry)
#class(bundes_valuations_raw$contract_expiry)
bundes_valuations_raw$year <- year(ymd(bundes_valuations_raw$season_start_year, truncated = 2L)-182)+1
bundes_valuations_raw$player_foot <- ifelse(bundes_valuations_raw$player_foot=='left', 0, 1) #prawa = 1
#bundes_valuations_raw$player_foot <- as.factor(bundes_valuations_raw$player_foot)
#nrow(filter(bundes_valuations, player_foot==1))
bundes_valuations <- dplyr::select(bundes_valuations_raw, -c(1))
rm(bundes_valuations_raw)


bundes_data <- inner_join(bundes_team_players, bundes_valuations, by=c('year', 'player_name'))
bundes_data <- bundes_data %>% distinct(year, player_name, .keep_all = TRUE)
#bundes_data <- dplyr::select(bundes_data, c(-9, -10, -11))
#start_year

#only cross-sectional data
#bundes_valuations_raw$contract_expiry <- year(ymd(bundes_valuations_raw$contract_expiry))
#bundes_valuations_raw$years_last <- bundes_valuations_raw$contract_expiry - bundes_valuations_raw$season_start_year

# current contract expirations
#bundes_expiring_2023 <- tm_expiring_contracts(country_name = 'Germany', contract_end_year = 2023) 


# Injuries ----------------------------------------------------------------

#bundes_injuries <- data.frame()
# # can make use of a tm helper function:
#for (j in 1:8){
#  team_urls <- tm_league_team_urls(country_name = "Germany", start_year = time_dim[j])
#  for (i in 1:18){
#    team_player_urls <- tm_team_player_urls(team_url = team_urls[i])
#    team_player_injuries <- tm_player_injury_history(player_urls = team_player_urls)
#    bundes_injuries <- rbind(bundes_injuries, team_player_injuries) 
#  }
#}
#write.csv(bundes_injuries, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_injuries.csv", row.names=FALSE)
bundes_injuries_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_injuries.csv")
bundes_injuries_raw <- bundes_injuries_raw %>% distinct(season_injured, player_name, injured_since,  .keep_all = TRUE)
bundes_injuries_raw$player_name <- trimws(bundes_injuries_raw$player_name, which='both', whitespace = "[ \t\r\n#0123456789]")
bundes_injuries_raw$duration <- trimws(bundes_injuries_raw$duration, which='both', whitespace = "[days]")
bundes_injuries_raw[, 7] <- sapply(bundes_injuries_raw[, 7], as.numeric)
bundes_injuries_tr <- bundes_injuries_raw[, c(1,3,7)]
bundes_injuries_tr <- na.omit(bundes_injuries_tr)
bundes_injuries_groupped <- bundes_injuries_tr%>% 
  group_by(player_name, season_injured) %>% 
  summarise(sum_days_injured=sum(duration))
bundes_injuries_groupped$season_injured <- as.numeric(paste0("20", substr(bundes_injuries_groupped$season_injured, 4, 5))) - 1

bundes_injuries <- bundes_injuries_groupped
bundes_injuries$season_days_injured <- bundes_injuries_groupped$sum_days_injured
bundes_injuries <- dplyr::select(bundes_injuries, -3)
bundes_injuries <- filter(bundes_injuries, season_injured > 2014) #tylko urazy po 2015
colnames(bundes_injuries)[2] ="year"

#bundes_data <- inner_join(bundes_data, bundes_injuries, by=c('year', 'player_name'))
#bundes_data <- bundes_data %>% distinct(year, player_name, .keep_all = TRUE)

bundes_injuries_groupped<-bundes_injuries_groupped[order(bundes_injuries_groupped$player_name, bundes_injuries_groupped$season_injured), ]
bundes_injuries_cum <- bundes_injuries_groupped %>%
  group_by(player_name) %>%
  mutate(cum_days_injured = cumsum(sum_days_injured))
bundes_injuries_cum <- filter(bundes_injuries_cum, season_injured > 2014)
colnames(bundes_injuries_cum)[2] ="year"
bundes_data <- inner_join(bundes_data, bundes_injuries_cum, by=c('year', 'player_name'))
rm(bundes_injuries_groupped)
rm(bundes_injuries_raw)
rm(bundes_injuries_tr)
rm(bundes_injuries_cum) 

# Club position -----------------------------------------------------------

#bundes_table <- data.frame()
#for (i in 1:8){
#  bundes_table_year <- tm_matchday_table(country_name="Germany", start_year=time_dim[i], matchday=34) #18 teams * 2
#  bundes_table <- rbind(bundes_table, bundes_table_year)
#}

#from this I can find net profit (expenditures)

#bundes_table <- fb_season_team_stats(country = "GER", gender = "M", season_end_year = c(2015:2022), tier='1st', stat_type = "league_table")
#write.csv(bundes_table, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_table.csv", row.names=FALSE)
#dplyr::glimpse(bundes_table)

bundes_table_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_table.csv")
bundes_table_raw <- bundes_table_raw[, c(4, 5, 7, 17, 20)]
#bundes_table_raw$Season_End_Year <- bundes_table_raw$Season_End_Year-1
#no need to do this, season_end_year = start_year in my dataset (if notes is in eurocups --> next season (start_year) will be)
colnames(bundes_table_raw)[1] <- 'year'
colnames(bundes_table_raw)[2] <- 'team_name'
colnames(bundes_table_raw)[3] <- 'position'
colnames(bundes_table_raw)[5] <- 'is_eurocups'
bundes_table_raw$is_eurocups <- trimws(bundes_table_raw$is_eurocups, which='both', whitespace = '›')
bundes_table <- bundes_table_raw
rm(bundes_table_raw)
bundes_table$is_eurocups <- ifelse(bundes_table$is_eurocups %in% c(' Champions League via league finish', ' Europa League via league finish', ' Europa Conference League via league finish', ' Europa League via league finish 1'), 1, 0)
bundes_table["team_name"][bundes_table["team_name"] == "Augsburg"] <- "FC Augsburg"
bundes_table["team_name"][bundes_table["team_name"] == "Dortmund"] <- "Borussia Dortmund"
bundes_table["team_name"][bundes_table["team_name"] == "Eint Frankfurt"] <- "Eintracht Frankfurt"
bundes_table["team_name"][bundes_table["team_name"] == "Freiburg"] <- "SC Freiburg"
bundes_table["team_name"][bundes_table["team_name"] == "Hoffenheim"] <- "TSG 1899 Hoffenheim"
bundes_table["team_name"][bundes_table["team_name"] == "Köln"] <- "1. FC Köln"
bundes_table["team_name"][bundes_table["team_name"] == "Leverkusen"] <- "Bayer 04 Leverkusen"
bundes_table["team_name"][bundes_table["team_name"] == "M'Gladbach"] <- "Borussia Mönchengladbach"
bundes_table["team_name"][bundes_table["team_name"] == "Mainz 05"] <- "1.FSV Mainz 05"
bundes_table["team_name"][bundes_table["team_name"] == "Paderborn 07"] <- "SC Paderborn 07"
bundes_table["team_name"][bundes_table["team_name"] == "Schalke 04"] <- "FC Schalke 04"
bundes_table["team_name"][bundes_table["team_name"] == "Stuttgart"] <- "VfB Stuttgart"
bundes_table["team_name"][bundes_table["team_name"] == "Werder Bremen"] <- "SV Werder Bremen"
bundes_table["team_name"][bundes_table["team_name"] == "Wolfsburg"] <- "VfL Wolfsburg"
bundes_table["team_name"][bundes_table["team_name"] == "Darmstadt 98"] <- "SV Darmstadt 98"
bundes_table["team_name"][bundes_table["team_name"] == "Ingolstadt 04"] <- "FC Ingolstadt 04"
bundes_table["team_name"][bundes_table["team_name"] == "Düsseldorf"] <- "Fortuna Düsseldorf"
bundes_table["team_name"][bundes_table["team_name"] == "Nürnberg"] <- "1.FC Nuremberg"
bundes_table["team_name"][bundes_table["team_name"] == "Union Berlin"] <- "1.FC Union Berlin"
bundes_table["team_name"][bundes_table["team_name"] == "Arminia"] <- "Arminia Bielefeld"
bundes_table["team_name"][bundes_table["team_name"] == "Bochum"] <- "VfL Bochum"
bundes_table["team_name"][bundes_table["team_name"] == "Greuther Fürth"] <- "SpVgg Greuther Fürth"

bundes_data <- inner_join(bundes_data, bundes_table, by=c('year', 'team_name'))
#bundes_data2 <- anti_join(bundes_data, bundes_table, by=c('year', 'player_name'))
#pilkarze z 2. bundesligi (pozycji z poprzedniego roku nie bylo) i 2022 jescze sie nie skonczyl nie wliczaja 


# Wages -------------------------------------------------------------------

#teams_urls_2021 <- fb_teams_urls(league_url = c('https://fbref.com/en/comps/20/2021-2022/2021-2022-Bundesliga-Stats'))
#teams_urls_2020 <- fb_teams_urls(league_url = c('https://fbref.com/en/comps/20/2020-2021/2020-2021-Bundesliga-Stats'))
#teams_urls_2019 <- fb_teams_urls(league_url = c('https://fbref.com/en/comps/20/2019-2020/2019-2020-Bundesliga-Stats'))
#teams_urls_2018 <- fb_teams_urls(league_url = c('https://fbref.com/en/comps/20/2018-2019/2018-2019-Bundesliga-Stats'))
#teams_urls_2017 <- fb_teams_urls(league_url = c('https://fbref.com/en/comps/20/2017-2018/2017-2018-Bundesliga-Stats'))
#teams_urls_2016 <- fb_teams_urls(league_url = c('https://fbref.com/en/comps/20/2016-2017/2016-2017-Bundesliga-Stats'))
#teams_urls_2015 <- fb_teams_urls(league_url = c('https://fbref.com/en/comps/20/2015-2016/2015-2016-Bundesliga-Stats'))
#teams_urls_2018 <- teams_urls_2018[1:18]
#teams_urls <- c(teams_urls_2015, teams_urls_2016, teams_urls_2017, teams_urls_2018, teams_urls_2019, teams_urls_2020, teams_urls_2021)
#bundes_wages <- data.frame()
#teams_urls <- teams_urls[teams_urls!='https://fbref.com/en/squads/60b5e41f/2015-2016/Hannover-96-Stats']
#teams_urls <- teams_urls[teams_urls!='https://fbref.com/en/squads/60b5e41f/2017-2018/Hannover-96-Stats']
#teams_urls <- teams_urls[teams_urls!='https://fbref.com/en/squads/60b5e41f/2018-2019/Hannover-96-Stats']
#for (i in 1:123){ #126-3(Hannover_96 2015-2016, 2017-2018, 2018-2019)
#  bundes_wages_team_year <- fb_squad_wages(team_urls = teams_urls[i])
#  bundes_wages <- rbind(bundes_wages, bundes_wages_team_year)
#}
#man_city_url <- "https://fbref.com/en/squads/60b5e41f/2015-2016/Hannover-96-Stats"
#man_city_wages <- fb_squad_wages(team_urls = man_city_url) #no data

#write.csv(bundes_wages, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_wages.csv", row.names=FALSE)
bundes_wages_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_wages.csv")
bundes_wages_raw <- dplyr::select(bundes_wages_raw, c(3, 4, 11))
bundes_wages_raw$Season <- substr(bundes_wages_raw$Season,1,4)
bundes_wages_raw[, 1] <- sapply(bundes_wages_raw[, 1], as.numeric)
colnames(bundes_wages_raw)[1] <- 'year'
colnames(bundes_wages_raw)[2] <- 'player_name'
colnames(bundes_wages_raw)[3] <- 'wage'
bundes_wages <- na.omit(bundes_wages_raw)
rm(bundes_wages_raw)
bundes_data <- inner_join(bundes_data, bundes_wages, by=c('year', 'player_name'))
#bundes_data2 <- left_join(bundes_data, bundes_wages, by=c('year', 'player_name'))
#anti_join - which data is not common for both
#many players' salary is not estimated


# National team -----------------------------------------------------------
#wc_urls <- fb_match_urls(country = "", gender = "M", season_end_year = 2012:2022, tier = "", non_dom_league_url = "https://fbref.com/en/comps/1/history/World-Cup-Seasons")
#wc_lineups <- fb_match_lineups(match_url = wc_urls)
#write.csv(wc_lineups, "D:/STUDIA/SGH/Seminarium licencjackie/wc_lineups.csv", row.names=FALSE)
wc_lineups_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/wc_lineups.csv")
wc_lineups_raw <- dplyr::select(wc_lineups_raw, c(1, 2, 6, 11))
wc_lineups <- na.omit(wc_lineups_raw)
wc_lineups$Matchday <- substr(wc_lineups$Matchday,1,4)
wc_lineups[, 1] <- sapply(wc_lineups[, 1], as.numeric)

wc_players_minutes <- wc_lineups %>% 
  group_by(Matchday, Player_Name) %>% 
  summarise(sum_minutes=sum(Min))
colnames(wc_players_minutes)[1] <- 'year'
colnames(wc_players_minutes)[2] <- 'player_name'
wc_players_minutes$year[wc_players_minutes$year==2014] <- 2015

wc_players_min_cum <- wc_players_minutes %>%
  group_by(player_name) %>%
  mutate(wc_cum_minutes = cumsum(sum_minutes))
wc_players_min_cum <- dplyr::select(wc_players_min_cum, -sum_minutes)
rm(wc_lineups_raw)
rm(wc_lineups)
rm(wc_players_minutes)


#friendly_urls <- fb_match_urls(country = "", gender = "M", season_end_year = 2015:2022, tier = "", non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")
#friendly_lineups <- fb_match_lineups(match_url = friendly_urls)
#write.csv(friendly_lineups, "D:/STUDIA/SGH/Seminarium licencjackie/friendly_lineups.csv", row.names=FALSE)
friendly_lineups_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/friendly_lineups.csv")
friendly_lineups_raw <- dplyr::select(friendly_lineups_raw, c(1, 2, 6, 11))
friendly_lineups <- na.omit(friendly_lineups_raw)
friendly_lineups$Matchday <- substr(friendly_lineups$Matchday,1,4)
friendly_lineups[, 1] <- sapply(friendly_lineups[, 1], as.numeric)

friendly_players_minutes <- friendly_lineups %>% 
  group_by(Matchday, Player_Name) %>% 
  summarise(sum_minutes=sum(Min))
colnames(friendly_players_minutes)[1] <- 'year'
colnames(friendly_players_minutes)[2] <- 'player_name'

friendly_players_min_cum <- friendly_players_minutes %>%
  group_by(player_name) %>%
  mutate(friendly_cum_minutes = cumsum(sum_minutes))
friendly_players_min_cum <- dplyr::select(friendly_players_min_cum, -sum_minutes)
rm(friendly_lineups_raw)
rm(friendly_lineups)
rm(friendly_players_minutes)


#euro_urls <- fb_match_urls(country = "", gender = "M", season_end_year = 2012:2022, tier = "", non_dom_league_url = "https://fbref.com/en/comps/676/history/European-Championship-Seasons")
#euro_lineups <- fb_match_lineups(match_url = euro_urls)
#write.csv(euro_lineups, "D:/STUDIA/SGH/Seminarium licencjackie/euro_lineups.csv", row.names=FALSE)
euro_lineups_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/euro_lineups.csv")
euro_lineups_raw <- dplyr::select(euro_lineups_raw, c(1, 2, 6, 11))
euro_lineups <- na.omit(euro_lineups_raw)
euro_lineups$Matchday <- substr(euro_lineups$Matchday,1,4)
euro_lineups[, 1] <- sapply(euro_lineups[, 1], as.numeric)

euro_players_minutes <- euro_lineups %>% 
  group_by(Matchday, Player_Name) %>% 
  summarise(sum_minutes=sum(Min))
colnames(euro_players_minutes)[1] <- 'year'
colnames(euro_players_minutes)[2] <- 'player_name'
euro_players_minutes$year[euro_players_minutes$year==2012] <- 2015

euro_players_min_cum <- euro_players_minutes %>%
  group_by(player_name) %>%
  mutate(euro_cum_minutes = cumsum(sum_minutes))
euro_players_min_cum <- dplyr::select(euro_players_min_cum, -sum_minutes)
rm(euro_lineups_raw)
rm(euro_lineups)
rm(euro_players_minutes)

#copa_urls <- fb_match_urls(country = "", gender = "M", season_end_year = 2015:2022, tier = "", non_dom_league_url = "https://fbref.com/en/comps/685/history/Copa-America-Seasons")
#copa_lineups <- fb_match_lineups(match_url = copa_urls)
#write.csv(copa_lineups, "D:/STUDIA/SGH/Seminarium licencjackie/copa_lineups.csv", row.names=FALSE)
copa_lineups_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/copa_lineups.csv")
copa_lineups_raw <- dplyr::select(copa_lineups_raw, c(1, 2, 6, 11))
copa_lineups <- na.omit(copa_lineups_raw)
copa_lineups$Matchday <- substr(copa_lineups$Matchday,1,4)
copa_lineups[, 1] <- sapply(copa_lineups[, 1], as.numeric)
copa_players_minutes <- copa_lineups %>% 
  group_by(Matchday, Player_Name) %>% 
  summarise(sum_minutes=sum(Min))
colnames(copa_players_minutes)[1] <- 'year'
colnames(copa_players_minutes)[2] <- 'player_name'

copa_players_min_cum <- copa_players_minutes %>%
  group_by(player_name) %>%
  mutate(copa_cum_minutes = cumsum(sum_minutes))
copa_players_min_cum <- dplyr::select(copa_players_min_cum, -sum_minutes)
rm(copa_lineups_raw)
rm(copa_lineups)
rm(copa_players_minutes)

#africa_urls <- fb_match_urls(country = "", gender = "M", season_end_year = 2015:2022, tier = "", non_dom_league_url = "https://fbref.com/en/comps/656/history/Africa-Cup-of-Nations-Seasons")
#africa_lineups <- fb_match_lineups(match_url = africa_urls)
#write.csv(africa_lineups, "D:/STUDIA/SGH/Seminarium licencjackie/africa_lineups.csv", row.names=FALSE)
africa_lineups_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/africa_lineups.csv")
africa_lineups_raw <- dplyr::select(africa_lineups_raw, c(1, 2, 6, 11))
africa_lineups <- na.omit(africa_lineups_raw)
africa_lineups$Matchday <- substr(africa_lineups$Matchday,1,4)
africa_lineups[, 1] <- sapply(africa_lineups[, 1], as.numeric)

africa_players_minutes <- africa_lineups %>% 
  group_by(Matchday, Player_Name) %>% 
  summarise(sum_minutes=sum(Min))
colnames(africa_players_minutes)[1] <- 'year'
colnames(africa_players_minutes)[2] <- 'player_name'

africa_players_min_cum <- africa_players_minutes %>%
  group_by(player_name) %>%
  mutate(africa_cum_minutes = cumsum(sum_minutes))
africa_players_min_cum <- dplyr::select(africa_players_min_cum, -sum_minutes)
rm(africa_lineups_raw)
rm(africa_lineups)
rm(africa_players_minutes)

#asia_urls <- fb_match_urls(country = "", gender = "M", season_end_year = 2015:2022, tier = "", non_dom_league_url = "https://fbref.com/en/comps/664/history/Asian-Cup-Seasons")
#asia_lineups <- fb_match_lineups(match_url = asia_urls)
#write.csv(asia_lineups, "D:/STUDIA/SGH/Seminarium licencjackie/asia_lineups.csv", row.names=FALSE)
asia_lineups_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/asia_lineups.csv")
asia_lineups_raw <- dplyr::select(asia_lineups_raw, c(1, 2, 6, 11))
asia_lineups <- na.omit(asia_lineups_raw)
asia_lineups$Matchday <- substr(asia_lineups$Matchday,1,4)
asia_lineups[, 1] <- sapply(asia_lineups[, 1], as.numeric)

asia_players_minutes <- asia_lineups %>% 
  group_by(Matchday, Player_Name) %>% 
  summarise(sum_minutes=sum(Min))
colnames(asia_players_minutes)[1] <- 'year'
colnames(asia_players_minutes)[2] <- 'player_name'

asia_players_min_cum <- asia_players_minutes %>%
  group_by(player_name) %>%
  mutate(asia_cum_minutes = cumsum(sum_minutes))
asia_players_min_cum <- dplyr::select(asia_players_min_cum, -sum_minutes)
rm(asia_lineups_raw)
rm(asia_lineups)
rm(asia_players_minutes)



#cumulative minutes
#bundes_data <- left_join(bundes_data, wc_players_min_cum, by=c('year', 'player_name'))
#bundes_data <- left_join(bundes_data, euro_players_min_cum, by=c('year', 'player_name'))
#bundes_data <- left_join(bundes_data, copa_players_min_cum, by=c('year', 'player_name'))
#bundes_data <- left_join(bundes_data, friendly_players_min_cum, by=c('year', 'player_name'))
#bundes_data <- left_join(bundes_data, asia_players_min_cum, by=c('year', 'player_name'))
#bundes_data <- left_join(bundes_data, africa_players_min_cum, by=c('year', 'player_name'))

bundes_data <- full_join(bundes_data, wc_players_min_cum, by=c('year', 'player_name'))
bundes_data <- full_join(bundes_data, euro_players_min_cum, by=c('year', 'player_name'))
bundes_data <- full_join(bundes_data, copa_players_min_cum, by=c('year', 'player_name'))
bundes_data <- full_join(bundes_data, friendly_players_min_cum, by=c('year', 'player_name'))
bundes_data <- full_join(bundes_data, asia_players_min_cum, by=c('year', 'player_name'))
bundes_data <- full_join(bundes_data, africa_players_min_cum, by=c('year', 'player_name'))

bundes_data <- bundes_data %>% 
  mutate(wc_cum_minutes = coalesce(wc_cum_minutes, 0),
         euro_cum_minutes = coalesce(euro_cum_minutes, 0),
         friendly_cum_minutes = coalesce(friendly_cum_minutes, 0),
         copa_cum_minutes = coalesce(copa_cum_minutes, 0),
         asia_cum_minutes = coalesce(asia_cum_minutes, 0),
         africa_cum_minutes = coalesce(africa_cum_minutes, 0)
         )
bundes_data <- bundes_data %>% 
  group_by(player_name) %>% 
  arrange(year) %>% 
  mutate(wc_cum_minutes = cumsum(wc_cum_minutes),
         euro_cum_minutes = cumsum(euro_cum_minutes),
         friendly_cum_minutes = cumsum(friendly_cum_minutes),
         copa_cum_minutes = cumsum(copa_cum_minutes),
         asia_cum_minutes = cumsum(asia_cum_minutes),
         africa_cum_minutes = cumsum(africa_cum_minutes))
bundes_data$total_nat_minutes <- bundes_data$wc_cum_minutes+bundes_data$euro_cum_minutes+bundes_data$friendly_cum_minutes+bundes_data$copa_cum_minutes+bundes_data$asia_cum_minutes+bundes_data$africa_cum_minutes
bundes_data <- na.omit(bundes_data, subset = c("team_name"))
#bundes_data <- bundes_data %>%
#  group_by(player_name) %>%
#  mutate(cum_total_nat_minutes = cumsum(total_nat_minutes))
bundes_data <- bundes_data %>% distinct(year, player_name, .keep_all = TRUE)
#bundes_data <- select(bundes_data, -c(wc_cum_minutes, friendly_cum_minutes, euro_cum_minutes, copa_cum_minutes, asia_cum_minutes, africa_cum_minutes, total_nat_minutes))
bundes_data <- dplyr::select(bundes_data, -c(sum_days_injured, wc_cum_minutes, friendly_cum_minutes, euro_cum_minutes, copa_cum_minutes, asia_cum_minutes, africa_cum_minutes))


#binary - not time-dependant
#bundes_data$is_in_nat_team <- ifelse(bundes_data$player_name %in% c(wc_players_min_cum$player_name, friendly_players_min_cum$player_name, euro_players_min_cum$player_name, copa_players_min_cum$player_name, africa_players_min_cum$player_name, asia_players_min_cum$player_name), 1, 0)

#binary - time-dependant from cumulative minutes
bundes_data$is_in_nat_team <- ifelse(bundes_data$total_nat_minutes>0, 1, 0)
nrow(filter(bundes_data, is_in_nat_team==1))
#filter(bundes_data, player_name=='Robert Lewandowski')[c('year', 'player_name', 'player_market_value_euro', 'total_nat_minutes', 'is_in_nat_team')]

#binary - time-dependant - checking
#bundes_data_test %>% 
#  mutate(is_in_nat_team2 = if_else((bundes_data$player_name == wc_players_min_cum$player_name & bundes_data$year == wc_players_min_cum$year & wc_players_min_cum$wc_cum_minutes > 0) | 
#                                     (bundes_data$player_name == euro_players_min_cum$player_name & bundes_data$year == euro_players_min_cum$year & euro_players_min_cum$euro_cum_minutes > 0) |
#                                     (bundes_data$player_name == friendly_players_min_cum$player_name & bundes_data$year == friendly_players_min_cum$year & friendly_players_min_cum$friendly_cum_minutes > 0) |
#                                    (bundes_data$player_name == copa_players_min_cum$player_name & bundes_data$year == copa_players_min_cum$year & copa_players_min_cum$copa_cum_minutes > 0) |
#                                     (bundes_data$player_name == asia_players_min_cum$player_name & bundes_data$year == asia_players_min_cum$year & asia_players_min_cum$asia_cum_minutes > 0) |
#                                     (bundes_data$player_name == africa_players_min_cum$player_name & bundes_data$year == africa_players_min_cum$year & africa_players_min_cum$africa_cum_minutes > 0), 1, 0))
#bundes_data_test <- bundes_data
#bundes_data_test$is_in_nat_team2 <- NA
#for (i in 1:8){
#  bundes_data_test$is_in_nat_team2[bundes_data_test$year==time_dim[i]] <- ifelse((bundes_data_test$player_name %in% c(filter(wc_players_min_cum, year==time_dim[i])$player_name, filter(euro_players_min_cum, year==time_dim[i])$player_name, filter(friendly_players_min_cum, year==time_dim[i])$player_name, filter(copa_players_min_cum, year==time_dim[i])$player_name, filter(asia_players_min_cum, year==time_dim[i])$player_name, filter(africa_players_min_cum, year==time_dim[i])$player_name)), 1, 0)
#  if (i != 1){
#    if (bundes_data_test$is_in_nat_team2[bundes_data_test$year==time_dim[i]]!=bundes_data_test$is_in_nat_team2[bundes_data_test$year==time_dim[i-1]] & bundes_data_test$is_in_nat_team2[bundes_data_test$year==time_dim[i]]==0){
#      bundes_data_test$is_in_nat_team2[bundes_data_test$year==time_dim[i]] <- 1
#    }
#  }
#}
#
#no team_name, no is_in_nat_team
bundes_data <- dplyr::select(bundes_data, c(-3, -16)) #- sum_days_injured

# Revenues and German inflation ------------------------------------------------
bundes_revenues <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_revenues.csv", sep=';', row.names=NULL)
colnames(bundes_revenues)[1] <- 'year'
bundes_revenues <- na.omit(bundes_revenues)
bundes_revenues[, 11] <- as.numeric(sub(",", ".", bundes_revenues[, 11], fixed = TRUE))
bundes_revenues$match_revenue <- bundes_revenues$match_revenue*1000
bundes_revenues$advertising <- bundes_revenues$advertising*1000
bundes_revenues$media_revenue <- bundes_revenues$media_revenue*1000
bundes_revenues$merch <- bundes_revenues$match_merch*1000
bundes_revenues$total_revenue <- bundes_revenues$total_revenue*1000
bundes_revenues$adm_staff_costs <- bundes_revenues$adm_staff_costs*1000
bundes_revenues$total_exp <- bundes_revenues$total_exp*1000
bundes_revenues$net_profit <- bundes_revenues$net_profit*1000
#te dane mocno skorelowane z pozycja --> pojawia sie ryzyko wspolliniowosci
bundes_data_basic <- inner_join(bundes_data, bundes_revenues[, c(1, 2, 11)], by=c('year', 'position'))

# Actual Fees and Inflation ----------------------------------------------------

all_transfer_data <- read.csv('https://raw.githubusercontent.com/d2ski/football-transfers-data/main/dataset/transfers.csv') #SOURCE!!!
all_transfer_data <- na.omit(all_transfer_data)
bundes_all_transfer_data <- filter(all_transfer_data, team_country=='Germany')
bundes_all_transfer_data <- aggregate(bundes_all_transfer_data$market_val_amnt, list(bundes_all_transfer_data$season), mean) #MARKET VALUE
percentage_change <- c(NA, diff(bundes_all_transfer_data$x) / bundes_all_transfer_data$x[-length(bundes_all_transfer_data$x)] * 100)
bundes_all_transfer_data$foot_inflation <- percentage_change
bundes_all_transfer_data <- filter(bundes_all_transfer_data, Group.1>2014)
bundes_all_transfer_data <- dplyr::select(bundes_all_transfer_data, c(1, 3))
colnames(bundes_all_transfer_data)[1] <- 'year'


bundes_data_basic <- inner_join(bundes_data_basic, bundes_all_transfer_data, by=c('year'))

#bundes_transfer_data <- read.csv('https://raw.githubusercontent.com/ewenme/transfers/master/data/1-bundesliga.csv')
#problem z UTF-8
# FBref stats with URL ----------------------------------------------------
#total in season
#all_player_urls <- c()
#for (i in 1:8){
#  league_url <- fb_league_urls(country='GER', gender='M', season_end_year = time_dim[i])
#  teams_url <- fb_teams_urls(league_url)
#  for (j in 1:length(teams_url)){
#    player_urls <- fb_player_urls(teams_url[j])
#    all_player_urls <- c(all_player_urls, player_urls)
#  }
#}
#player_urls <- filter(mapped_players, PlayerFBref %in% list(bundes_team_players$player_name))
#write.csv(all_player_urls, "D:/STUDIA/SGH/Seminarium licencjackie/all_players_urls.csv", row.names=F)
#bundes_players_urls <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/all_players_urls.csv")
#bundes_players_urls <- bundes_players_urls$x
#all_players_standard_stats <- c()
#for (i in 1060:1682){
#  players_season_standard_stats <- fb_player_season_stats(player_url = bundes_players_urls[i],
#                                    stat_type = c("standard")) #all vectors at once does not work
#  all_players_standard_stats <- dplyr::bind_rows(all_players_standard_stats, players_season_standard_stats)
#}
#write.csv(all_players_standard_stats, "D:/STUDIA/SGH/Seminarium licencjackie/all_players_standard_stats.csv", row.names = F)
#for (i in 1275:1682){
#  players_season_standard_stats <- fb_player_season_stats(player_url = bundes_players_urls[i],
#                                                          stat_type = c("standard")) #all vectors at once does not work
#  all_players_standard_stats2 <- dplyr::bind_rows(all_players_standard_stats2, players_season_standard_stats)
#}
#write.csv(all_players_standard_stats2, "D:/STUDIA/SGH/Seminarium licencjackie/all_players_standard_stats.csv", row.names = F)
all_players_standard_stats_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/all_players_standard_stats.csv")
all_players_standard_stats <- dplyr::select(all_players_standard_stats_raw, c(1, 3, 7, 10, 12, 13, 18, 19))
all_players_standard_stats$Season <- substr(all_players_standard_stats$Season,6,10)
all_players_standard_stats[, 2] <- sapply(all_players_standard_stats[, 2], as.numeric)
colnames(all_players_standard_stats)[2] <- 'year'
all_players_standard_stats <- filter(all_players_standard_stats, year>2014)
gr_players_standard_stats <- all_players_standard_stats %>% 
  group_by(player_name, year) %>% 
  summarise(season_minutes=sum(Min_Time),
            season_goals=sum(Gls),
            season_assists=sum(Ast),
            season_yellow_cards=sum(CrdY),
            season_red_cards=sum(CrdR))
player_standard_stats <- na.omit(gr_players_standard_stats)
player_standard_stats$season_disc <- player_standard_stats$season_yellow_cards*0.5+player_standard_stats$season_yellow_cards*1
player_standard_stats <- dplyr::select(player_standard_stats, c(-6, -7))
rm(all_players_standard_stats_raw)
rm(all_players_standard_stats)
rm(gr_players_standard_stats)

bundes_data_basic <- inner_join(bundes_data_basic, player_standard_stats, by=c('year', 'player_name'))

# EDA -------------------------------------------------------------
#1,2 - indexes for panel


#3 - position - uproszczona pozycja gracza (potrzebna do interakcji)
#4,5 - age, age_2 - wiek nieliniowa zaleznosc 
#6 - nationality_region - uproszczony faktor (zbadanie hipotezy rynkow wchodzacych)
#7 - noga - binarna (hipoteza, ze lewonodzy sa drozsze)
#8 - player_market_value_euro - zmienna objasniana
#9 - cum_days_injured - ile spedzono w szpitalu, na ile sklonny do urazow 
#10 - position - miejsce w poprzednim sezonie  
#11 - attendance - srednia frekwencja na stadionie klubu
#12 - is_eurocups - czy w poprzednim sezonie wystepowal w ligach miedzynarodowych
#13 - wage - estymowane wynagrodzenie w sezonie
#14 - total_nat_minutes - ile spedzono w barwach narodowych
#15 - german_inflation - inflacja Niemiec
#16 - foot_inflation - inflacja rynku pilkarskiego mierzona jako procentowa zmiana sredniej ceny rynkowej realizowanych transferow
#17,18,19,20 - season_... - statystyki w poprzednim sezonie

#team_name - nie weryfikuje zadnej hipotezy o statusie klubu, dodaje zamiast position
#is_in_nat_team - zamiana na total_nat_minutes
#advertising, merch, - brak jakosci danych
bundes_data_basic <- bundes_data_basic[, c(1, 2, 8, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)]
bundes_data_basic <- bundes_data_basic %>%
  mutate(player_pos = relevel(player_pos, ref = "Attack")) #gracze atakujace jako grupa referencyjna
bundes_data_basic <- bundes_data_basic %>%
  mutate(nationality_region = relevel(nationality_region, ref = "Europe")) #gracze europejskie jako grupa referencyjna

bundes_data_basic_num <- bundes_data_basic %>% 
  ungroup()
bundes_data_basic_num <- dplyr::select_if(bundes_data_basic_num, is.numeric)
#bundes_data_basic_num$player_foot <- bundes_data_basic$player_foot

#summary(bundes_data_basic_num[-c(1)])
#stat.desc(bundes_data_basic_num[-c(1)])
summary_stats <- as.data.frame(psych::describe(bundes_data_basic_num[-c(1, 4, 7)]))#year, player_age_2, position
summary_stats[, c('median','skew', 'kurtosis')]
#stargazer(mtcars, type='text')
stargazer(as.data.frame(bundes_data_basic_num[-c(1,4,7)]), type = "text", title="Statystyki opisowe", digits=1, out="Descriptives.html")


#market value
#adf.test(bundes_data_basic$player_market_value_euro, k=1)
bundes_data$log_player_market_value_euro <- log(bundes_data$player_market_value_euro)
par(mfrow=c(2,1))
hist(bundes_data$player_market_value_euro, xlab='wartość rynkowa', ylab='częstość', col='red', main='Histogram wartości rynkowych przed normalizacją')
hist(bundes_data$log_player_market_value_euro, xlab='wartość rynkowa', ylab='częstość', col='green', main='Histogram wartości rynkowych po normalizacji')

#wage
bundes_data$log_wage <- log(bundes_data$wage)
par(mfrow=c(2,1))
hist(bundes_data$wage, xlab='wynagrodzenie', ylab='częstość', col='red', main='Histogram wynagrodzeń przed normalizacją')
hist(bundes_data$log_wage, xlab='wynagrodzenie', ylab='częstość', col='green', main='Histogram wynagrodzeń po normalizacji')

#Wiek
plot(bundes_data$player_age, bundes_data$player_market_value_euro, main="Wiek vs cena rynkowa piłkarza",
     xlab="Wiek ", ylab="Cena", pch=19)
#nieliniowa zaleznosc - wyrazna parabola - dodanie age_2
age_lm <- lm(player_market_value_euro~player_age+player_age_2, data=bundes_data)
summary(age_lm)
age_values <- seq(16, 40, 1)
price_age_predict <- predict(age_lm, list(player_age=age_values, player_age_2=age_values^2))
plot(bundes_data$player_age, bundes_data$player_market_value_euro, main="Zależność między wiekiem a wartością rynkową piłkarza",
     xlab="Wiek ", ylab="Wartość rynkowa", pch=19)
lines(age_values, price_age_predict, col='red')

Q1 <- quantile(bundes_data$player_market_value_euro, .25)
Q3 <- quantile(bundes_data$player_market_value_euro, .75)
IQR <- IQR(bundes_data$player_market_value_euro)
no_outliers_bundes_data <- subset(bundes_data, bundes_data$player_market_value_euro> (Q1 - 1.5*IQR) & bundes_data$player_market_value_euro< (Q3 + 1.5*IQR))
age_lm <- lm(player_market_value_euro~player_age+player_age_2, data=no_outliers_bundes_data)
summary(age_lm)
age_values <- seq(16, 40, 1)
price_age_predict <- predict(age_lm, list(player_age=age_values, player_age_2=age_values^2))
plot(no_outliers_bundes_data$player_age, no_outliers_bundes_data$player_market_value_euro, main="Zależność między wiekiem a wartością rynkową piłkarza\n (bez wartości odstających)",
     xlab="Wiek ", ylab="Wartość rynkowa", pch=19)
lines(age_values, price_age_predict, col='red')

par(mfrow=c(1,3))
#player_pos
aggregate(bundes_data$player_market_value_euro, list(bundes_data$player_pos), FUN=length) #median
pos_box <- ggplot(bundes_data, aes(x=player_pos, y=player_market_value_euro)) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 50000000))+
  xlab('Pozycja piłkarza')+
  ylab('wartość rynkowa')
  #ggtitle('Zależność między pozycją a wartością rynkową')

#nationality_region
aggregate(bundes_data$player_market_value_euro, list(bundes_data$nationality_region), FUN=length) 
nat_box <- ggplot(bundes_data, aes(x=nationality_region, y=player_market_value_euro)) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 50000000))+
  xlab('Region pochodzenia')+
  ylab('wartość rynkowa')

#player_foot

aggregate(bundes_data$player_market_value_euro, list(as.factor(bundes_data$player_foot)), FUN=median)
foot_box <- ggplot(bundes_data, aes(x=as.factor(player_foot), y=player_market_value_euro)) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 50000000))+
  xlab('Noga wiodąca')+
  ylab('wartość rynkowa')
ggarrange(pos_box, nat_box, foot_box, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

#korelacja
cormat <- cor(bundes_data_basic_num[-c(4,5)]) #macierz korelacji dla całego zbioru
melted_cormat <- melt(round(cor(bundes_data_basic_num[-c(4,5)]), 2))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + #wizualizacja macierzy korelacji
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90))
#korelacja
res2 <- rcorr(as.matrix(bundes_data_basic_num[-c(4,5)]))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")



#covid
bundes_data_basic_indexed <- pdata.frame(bundes_data_basic, index=c('player_name', 'year'))
plotmedians(player_market_value_euro ~ year, data = bundes_data_basic_indexed, ylab='Wartość rynkowa', xlab='Rok') 

bundes_pandemic <- read.csv2("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/Bundesliga_reports_R.csv")
colnames(bundes_pandemic)[1] <- 'rok'
colnames(bundes_pandemic)[4] <- 'wydatki transferowe'
bundes_pandemic <- bundes_pandemic %>%
  gather(key = "wskaźnik", value = "stan", -rok)
ggplot(bundes_pandemic, aes(x = rok, y = stan)) + 
  geom_line(aes(color = wskaźnik), linewidth=1.3)+
  scale_color_manual(values=c('blue', 'green', 'red'))+
  theme_classic2()
#options(scipen=999) 

# Basic model -------------------------------------------------------------

#Pooled OLS
pool_model <- plm(log(player_market_value_euro)~player_pos+player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic, model='pooling', index=c('player_name', 'year'))
summary(pool_model)
#FE (uwzgledniam efekt stale) 
#fe_ind <- plm(log(player_market_value_euro)~player_pos+player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic, model='within', effect='individual', index=c('player_name', 'year'))#wymiar przekrojowy
#summary(fe_ind)
fe_time <- plm(log(player_market_value_euro)~player_pos+player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic, model='within', effect='time', index=c('player_name', 'year')) #czasowy
summary(fe_time)
fe_2 <- plm(log(player_market_value_euro)~player_pos+player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic, model='within', effect='twoways', index=c('player_name', 'year')) #oba
summary(fe_2)

#niektore zmienne sa stale w czasie
#fixef(fe_ind) #constanty dla poszczegolnych pilkarzami
fixef(fe_time)
fixef(fe_2)
#zmiana cen wzgledem czasu srednio miedzy pilkarzami

#FE > Two-Way (dodajemy indywidualny skladnik), R^2, R^2 kara za liczbe parametry (duzo duzo pilkarzy)
#stale cechy wzgledem 
#interpretacje dla FE_time
#pFtest(fe_ind, pool_model) #FE_ind > OLS
#odrzucamy H0, czyli sa istotne roznicy miedzy pilkarzami
#odrzucamy H0, czyli sa istotne roznicy miedzy latami
pFtest(fe_time, pool_model) #FE_time > OLS
#plmtest(fe_ind, c('individual'), type='bp')
plmtest(fe_time, c('time'), type='kw')
plmtest(fe_2, type='kw')


re_time <- plm(log(player_market_value_euro)~player_pos+player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic, effect='time', random.method='walhus', model='random', index=c('player_name', 'year')) #random.method
summary(re_time)
re_ind <- plm(log(player_market_value_euro)~player_pos+player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic, model='random', index=c('player_name', 'year')) 
summary(re_ind)
#re2 <- plm(log(player_market_value_euro)~player_age+player_age_2+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic, model='random', index=c('player_name', 'year')) #random.method
#summary(re2) #bez stalych w czasie
#re3 <- plm(log(player_market_value_euro)~player_age+player_age_2+position+log(wage)+total_nat_minutes+german_inflation+season_minutes+season_goals, data=bundes_data_basic, model='random', index=c('player_name', 'year'))
#summary(re3)
#stargazer(re, re2, re3, type='text', title='Modele Random Effects', out='re.html')

variable_fe <- pvcm(log(player_market_value_euro)~player_pos+player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic, model='within', effect='time', index=c('player_name', 'year'))
pooltest(fe_ind, variable_fe) #Chow test for poolability

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect, variances across entities is zero (i.e. OLS better).
plmtest(pool_model, effect='individual',type=c("kw")) 
#odrzucamy H0, czyli są istotne różnicy miedzy pilkarzami

#Test Hausmana. Null is that unique errors are not correlated with regressors (i.e. random effects is better)
#phtest(fe_ind, re) #FE > RE
#phtest(fe_time, re_time) 
#phtest(fe_ind, re_ind)
phtest(fe_time, re_ind)
#phtest(re_ind, fe_time)
#piest(log(player_market_value_euro)~player_pos+player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic)

pcdtest(fe_time, effect='time') #no cs dependence
pcdtest(fe_time, effect='time', test='lm') #there is cs dependence

bptest(fe_time, studentize = F)

pbgtest(fe_time) #there is serial correlation
#pdwtest(fe_time) no const 


#pcdtest(re_ind, test = c("lm")) #there is cs dependence
#pbgtest(re_ind) #there is serial correlation

# Model testing -----------------------------------------------------------
fe_time_order <- plm(player_market_value_euro~player_pos+player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+is_eurocups+log(wage)+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data=bundes_data_basic, model='within', effect='time', index=c('player_name', 'year')) #czasowy

order_time_testing <- data.frame(id = 1:length(resid(fe_time_order)),
                                 residuals = resid(fe_time_order), 
                                 fitted = fitted(fe_time_order))
ggplot(order_time_testing, aes(x = fitted, y = residuals)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Residuals", x = "Fitted")+
  geom_smooth()

# generate data
fe_time_testing <- data.frame(id = 1:length(resid(fe_time)),
                  residuals = resid(fe_time), 
                  fitted = fitted(fe_time))
# generate plots
ggplot(fe_time_testing, aes(x = id, y = residuals)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_line() +
  labs(y = "Residuals", x = "Index")

#normalnosc
qqPlot(fe_time_testing$residuals, ylab='reszty', xlab='kwantyle rozkładu normalnego') 

#autocorrelation
plot(fe_time$residuals)

#residuals vs fitted
ggplot(fe_time_testing, aes(x = fitted, y = residuals)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point() +
  labs(y = "Residuals", x = "Fitted")+
  geom_smooth() #istnieje trend, heteroskedastycznosc

coeftest(fe_time)
coeftest(fe_time, vcov=vcovHC(fe_time, method = 'arellano', type='HC3'))

t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fe_time, method='arellano', type = x)))))

coeftest(fe_time, vcov=vcovNW(fe_time))

stargazer(pool_model, fe_time, fe_2, re, type='text', 
          model.numbers=F, column.labels=c("Baseline pool model", "Fixed Effects", "Two way FE", "Random Effects"), 
          covariate.labels = c('Obrońca', 'Bramkarz', 'Pomocnik', 'Wiek', 'Kwadrat wieku', 'Afryka', 'Ameryka', 'Noga wiodąca', 'Liczba dni z kontuzją', 'Pozycja klubu', 'Udział w pucharach', 'Logarytm wynagrodzenia', 'Frekwencja', 'Minuty w reprezentacji', 'Inflacja piłkarska', 'Inflacja w Niemczech', 'Minuty w sezonie', 'Gole', 'Asysty', 'Kartki'),
          dep.var.caption  = "Porównanie modeli",
          dep.var.labels   = 'Logarytm wartości rynkowej',
          out='Modele')

# Interactions FBref -----------------------------------------------------------
#standard = goals, assists, yellow cards, red cards BUNDESLIGA (not all tourneys)
#bundes_stats_raw2 <- data.frame()
#for (i in 4:8){
#  league_url <- fb_league_urls(country = "GER", gender = "M", season_end_year = time_dim[i])
#  teams <- fb_teams_urls(league_url)
#  bundes_stats_year <- fb_team_player_stats(team_urls= teams, stat_type= "standard")
#  bundes_stats_raw2 <- rbind(bundes_stats_raw2, bundes_stats_year)
#}

#bundes_stats_raw <- dplyr::select(bundes_stats_raw, c(1, 4, 12, 13, 14))
#bundes_stats_raw2 <- dplyr::select(bundes_stats_raw2, c(1, 4, 12, 13, 14))
#bundes_stats_raw <- rbind(bundes_stats_raw, bundes_stats_raw2)
#write.csv(bundes_stats_raw, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_stats.csv", row.names=FALSE)

#bundes_stats_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/bundes_stats.csv")
#colnames(bundes_stats_raw)[1] <- 'year'
#colnames(bundes_stats_raw)[2] <- 'player_name'
#colnames(bundes_stats_raw)[3] <- 'bundes_goals'
#colnames(bundes_stats_raw)[4] <- 'bundes_assists'
#colnames(bundes_stats_raw)[5] <- 'bundes_gplusa'
#stats for previous year, so 2014-2015 --> 2015
#bundes_stats_raw$year <- substr(bundes_stats_raw$year,6,10) 
#bundes_stats_raw[, 1] <- sapply(bundes_stats_raw[, 1], as.numeric)
#bundes_stats <- bundes_stats_raw
#rm(bundes_stats_raw)

#bundes_data2 <- inner_join(bundes_data, bundes_stats, by=c('year', 'player_name')) #INNER - 1613 --> 1401

#keepers - for goalkeepers
#bundes_keepers <- data.frame()
#for (i in 1:8){
#  big5_player_keepers <- fb_big5_advanced_season_stats(season_end_year = time_dim[i], stat_type= "keepers", team_or_player= "player")
#  bundes_keepers_year <- filter(big5_player_keepers, Comp=='Bundesliga')
#  bundes_keepers <- rbind(bundes_keepers, bundes_keepers_year)
#}
#write.csv(bundes_keepers, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_keepers.csv", row.names=FALSE)
bundes_keepers_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_keepers.csv")
bundes_keepers_raw <- dplyr::select(bundes_keepers_raw, c(1, 4, 13, 17, 22))
colnames(bundes_keepers_raw)[1] <- 'year'
colnames(bundes_keepers_raw)[2] <- 'player_name'
colnames(bundes_keepers_raw)[3] <- 'goals_against_gk'
colnames(bundes_keepers_raw)[4] <- 'save_percent_gk'
colnames(bundes_keepers_raw)[5] <- 'clean_sheet_percent_gk'
bundes_keepers <- bundes_keepers_raw
rm(bundes_keepers_raw)

bundes_data_int <- left_join(bundes_data_basic, bundes_keepers, by=c('year', 'player_name'))

#defense - for defenders
#bundes_defense <- data.frame()
#for (i in 1:8){
#  big5_player_defense <- fb_big5_advanced_season_stats(season_end_year = time_dim[i], stat_type= "defense", team_or_player= "player")
#  bundes_defense_year <- filter(big5_player_defense, Comp=='Bundesliga')
#  bundes_defense <- rbind(bundes_defense, bundes_defense_year)
#}
#write.csv(bundes_defense, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_defense.csv", row.names=FALSE)
bundes_defense_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_defense.csv")
bundes_defense <- na.omit(bundes_defense_raw)
bundes_defense <- dplyr::select(bundes_defense_raw, c(1, 4, 11, 17, 19))
colnames(bundes_defense)[1] <- 'year'
colnames(bundes_defense)[2] <- 'player_name'
colnames(bundes_defense)[3] <- 'tackles_won_df'
colnames(bundes_defense)[4] <- 'dribble_stop_df'
colnames(bundes_defense)[5] <- 'blocks_df'
rm(bundes_defense_raw)
  
bundes_data_int <- left_join(bundes_data_int, bundes_defense, by=c('year', 'player_name'))

#passing - for midfielders
#from 2018 - no data for lower 
#bundes_midfield <- data.frame()
#for (i in 4:8){
#  big5_player_midfield <- fb_big5_advanced_season_stats(season_end_year = time_dim[i], stat_type= "passing", team_or_player= "player")
#  bundes_midfield_year <- filter(big5_player_midfield, Comp=='Bundesliga')
#  bundes_midfield <- rbind(bundes_midfield, bundes_midfield_year)
#}
#write.csv(bundes_midfield, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_midfield.csv", row.names=FALSE)
bundes_midfield_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_midfield.csv")
bundes_midfield <- na.omit(bundes_midfield_raw)
bundes_midfield <- dplyr::select(bundes_midfield_raw, c(1, 4, 12, 28, 32))
colnames(bundes_midfield)[1] <- 'year'
colnames(bundes_midfield)[2] <- 'player_name'
colnames(bundes_midfield)[3] <- 'passes_complete_md'
colnames(bundes_midfield)[4] <- 'key_passes_md'
colnames(bundes_midfield)[5] <- 'progressive_passes_md'
rm(bundes_midfield_raw)

bundes_data_int <- left_join(bundes_data_int, bundes_midfield, by=c('year', 'player_name'))

#goal and shot creation - for attackers
#from 2018 - no data for lower 
#bundes_attack <- data.frame()
#for (i in 4:8){
#  big5_player_attack <- fb_big5_advanced_season_stats(season_end_year = time_dim[i], stat_type= "gca", team_or_player= "player")
#  bundes_attack_year <- filter(big5_player_attack, Comp=='Bundesliga')
#  bundes_attack <- rbind(bundes_attack, bundes_attack_year)
#}
#write.csv(bundes_attack, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_attack.csv", row.names=FALSE)
bundes_attack_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/Dane_wyniki/bundes_attack.csv")
bundes_attack <- na.omit(bundes_attack_raw)
bundes_attack <- dplyr::select(bundes_attack_raw, c(1, 4, 10, 18))
colnames(bundes_attack)[1] <- 'year'
colnames(bundes_attack)[2] <- 'player_name'
colnames(bundes_attack)[3] <- 'shot_create_at'
colnames(bundes_attack)[4] <- 'goal_create_at'
rm(bundes_attack_raw)

bundes_data_int <- left_join(bundes_data_int, bundes_attack, by=c('year', 'player_name'))

#for all - discipline
#bundes_disc <- data.frame()
#for (i in 1:8){
#   big5_player_disc <- fb_big5_advanced_season_stats(season_end_year = time_dim[i], stat_type= "misc", team_or_player= "player")
#   bundes_disc_year <- filter(big5_player_disc, Comp=='Bundesliga')
#   bundes_disc <- rbind(bundes_disc, bundes_disc_year)
#}
#bundes_disc2 <- data.frame()
#for (i in 4:8){
#  big5_player_disc <- fb_big5_advanced_season_stats(season_end_year = time_dim[i], stat_type= "misc", team_or_player= "player")
#  bundes_disc_year <- filter(big5_player_disc, Comp=='Bundesliga')
#  bundes_disc2 <- rbind(bundes_disc2, bundes_disc_year)
#}

#bundes_disc <- dplyr::select(bundes_disc, c(1, 4, 10, 11))
#bundes_disc2 <- dplyr::select(bundes_disc2, c(1, 4, 10, 11))
#bundes_disc <- rbind(bundes_disc, bundes_disc2)

#write.csv(bundes_disc, "D:/STUDIA/SGH/Seminarium licencjackie/bundes_disc.csv", row.names=FALSE)
#bundes_disc_raw <- read.csv("D:/STUDIA/SGH/Seminarium licencjackie/bundes_disc.csv")
#bundes_disc <- na.omit(bundes_disc_raw)
#colnames(bundes_disc)[1] <- 'year'
#colnames(bundes_disc)[2] <- 'player_name'
#colnames(bundes_disc)[3] <- 'yellow_cards'
#colnames(bundes_disc)[4] <- 'red_cards'
bundes_data_int <- filter(bundes_data_int, year>2017)
bundes_data_int$is_gk <- ifelse(bundes_data_int$player_pos=='Goalkeeper', 1, 0)
bundes_data_int$is_df <- ifelse(bundes_data_int$player_pos=='Defence', 1, 0)
bundes_data_int$is_md <- ifelse(bundes_data_int$player_pos=='Midfield', 1, 0)
bundes_data_int$is_at <- ifelse(bundes_data_int$player_pos=='Attack', 1, 0)
bundes_data_int <- bundes_data_int %>% drop_na(tackles_won_df, dribble_stop_df, blocks_df, passes_complete_md, key_passes_md, progressive_passes_md, shot_create_at, goal_create_at)
#bundes_data_int <- na.omit(bundes_data_int, subset = c(35, 36, 37, 38, 39, 40, 41, 42))
bundes_data_int <- bundes_data_int %>% 
  mutate(goals_against_gk = coalesce(goals_against_gk, 0),
         save_percent_gk  = coalesce(save_percent_gk, 0),
         clean_sheet_percent_gk = coalesce(clean_sheet_percent_gk, 0))

bundes_data_int$is_gk_x_goals_against_gk <- bundes_data_int$is_gk * bundes_data_int$goals_against_gk
bundes_data_int$is_gk_x_save_percent_gk <- bundes_data_int$is_gk * bundes_data_int$save_percent_gk
bundes_data_int$is_gk_x_clean_sheet_percent_gk <- bundes_data_int$is_gk * bundes_data_int$clean_sheet_percent_gk

bundes_data_int$is_df_x_tackles_won_df<- bundes_data_int$is_df * bundes_data_int$tackles_won_df
bundes_data_int$is_df_x_dribble_stop_df<- bundes_data_int$is_df * bundes_data_int$dribble_stop_df
bundes_data_int$is_df_x_blocks_df<- bundes_data_int$is_df * bundes_data_int$blocks_df

bundes_data_int$is_md_x_passes_complete_md <- bundes_data_int$is_md * bundes_data_int$passes_complete_md
bundes_data_int$is_md_x_key_passes_md <- bundes_data_int$is_md * bundes_data_int$key_passes_md
bundes_data_int$is_md_x_progressive_passes_md <- bundes_data_int$is_md * bundes_data_int$progressive_passes_md

bundes_data_int$is_at_x_shot_create_at <- bundes_data_int$is_at * bundes_data_int$shot_create_at
bundes_data_int$is_at_x_goal_create_at <- bundes_data_int$is_at * bundes_data_int$goal_create_at


bundes_data_int <- dplyr::select(bundes_data_int, -c(4)) #delete player_pos
#bundes_data2 <- left_join(bundes_data2, bundes_disc, by=c('year', 'player_name'))
bundes_data_int <- bundes_data_int %>% distinct(year, player_name, .keep_all = TRUE)
#684 observations
#precyzyjne finanse


# Interaction model -------------------------------------------------------

pool_model_int <- plm(log(player_market_value_euro)~player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+Attendance+is_eurocups+log(wage)+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc+goals_against_gk+save_percent_gk+clean_sheet_percent_gk+tackles_won_df+dribble_stop_df+blocks_df+passes_complete_md+key_passes_md+progressive_passes_md+shot_create_at+goal_create_at+is_gk+is_df+is_md+is_at+is_gk_x_goals_against_gk+is_gk_x_save_percent_gk+is_gk_x_clean_sheet_percent_gk+is_df_x_tackles_won_df+is_df_x_dribble_stop_df+is_df_x_blocks_df+is_md_x_passes_complete_md+is_md_x_key_passes_md+is_md_x_progressive_passes_md+is_at_x_shot_create_at+is_at_x_goal_create_at, data=bundes_data_int, model='pooling', index=c('player_name', 'year'))
summary(pool_model_int)
#pool_model_int <- plm(log(player_market_value_euro)~player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+Attendance+is_eurocups+log(wage)+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc+goals_against_gk+save_percent_gk+clean_sheet_percent_gk+tackles_won_df+dribble_stop_df+blocks_df+passes_complete_md+key_passes_md+progressive_passes_md+shot_create_at+goal_create_at+is_gk+is_df+is_md+is_at+is_gk*goals_against_gk+is_gk*save_percent_gk+is_gk*clean_sheet_percent_gk+is_df*tackles_won_df+is_df*dribble_stop_df+is_df*blocks_df+is_md*passes_complete_md+is_md*key_passes_md+is_md*progressive_passes_md+is_at*shot_create_at+is_at*goal_create_at, data=bundes_data_int, model='pooling', index=c('player_name', 'year'))

#FE (uwzgledniam indywidualne roznice)
fe_int <- plm(log(player_market_value_euro)~player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+Attendance+is_eurocups+log(wage)+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc+goals_against_gk+save_percent_gk+clean_sheet_percent_gk+tackles_won_df+dribble_stop_df+blocks_df+passes_complete_md+key_passes_md+progressive_passes_md+shot_create_at+goal_create_at+is_gk+is_df+is_md+is_at+is_gk_x_goals_against_gk+is_gk_x_save_percent_gk+is_gk_x_clean_sheet_percent_gk+is_df_x_tackles_won_df+is_df_x_dribble_stop_df+is_df_x_blocks_df+is_md_x_passes_complete_md+is_md_x_key_passes_md+is_md_x_progressive_passes_md+is_at_x_shot_create_at+is_at_x_goal_create_at, data=bundes_data_int, effect='time', model='within', index=c('player_name', 'year'))
summary(fe_int)
re_int <- plm(log(player_market_value_euro)~player_age+player_age_2+nationality_region+player_foot+cum_days_injured+position+Attendance+is_eurocups+log(wage)+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc+goals_against_gk+save_percent_gk+clean_sheet_percent_gk+tackles_won_df+dribble_stop_df+blocks_df+passes_complete_md+key_passes_md+progressive_passes_md+shot_create_at+goal_create_at+is_gk+is_df+is_md+is_at+is_gk_x_goals_against_gk+is_gk_x_save_percent_gk+is_gk_x_clean_sheet_percent_gk+is_df_x_tackles_won_df+is_df_x_dribble_stop_df+is_df_x_blocks_df+is_md_x_passes_complete_md+is_md_x_key_passes_md+is_md_x_progressive_passes_md+is_at_x_shot_create_at+is_at_x_goal_create_at, data=bundes_data_int, model='random', random.method='nerlove', index=c('player_name', 'year')) #random.method
summary(re_int)
#RE (uwzgledniam indywidualne roznce+zmiany w czasie)

stargazer(pool_model, fe_time, fe_2, re, type='text')
#out='Modele.html'

# Drzewo [do usiniecia] ---------------------------------------------------
# Regression tree (machine learning)
set.seed(108887)
test_prop <- 0.25
test.set.index <- (runif(nrow(bundes_data_basic)) < test_prop)
tdf.test <- bundes_data_basic[test.set.index, ]
tdf.train <- bundes_data_basic[!test.set.index, ] 

regr_tree <- rpart(player_market_value_euro ~ year+player_pos+player_age+nationality_region+player_foot+cum_days_injured+position+is_eurocups+wage+Attendance+total_nat_minutes+foot_inflation+german_inflation+season_minutes+season_goals+season_assists+season_disc, data = tdf.train)
rpart.plot(regr_tree)

modele <- list("d.regr" = regr_tree)
OcenaModeli <- function(modele, dane, predicted_col_name) {
  
  print("R-kwadrat")
  print(sapply(modele, function(x) 1 - sum((dane[[predicted_col_name]] - predict(x, dane))^2)/sum((dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))^2) ))
  
  print("Średni błąd absolutny MAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/nrow(dane) ))
  
  
}
OcenaModeli(modele, tdf.test, 'player_market_value_euro')
