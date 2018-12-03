## In this file, we create the functions that will be called in our
## shiny document. They will take in a string corresponding to user
## input,  get and sort data from Google BigQuery NCAA, and finally
## output data tables (or draw visualizations) for our document

library(bigrquery)
library(dplyr)
library(ggplot2)
library(zoo)
library(reshape2)

# ---------------------- Setting up Google BigQuery ----------------------

# This file is seperate for each group member:
# It contains the project ID for the  group member's
# Google Cloud project for billing purposes
source('./../myProjectBilling.R')

# Basic call for Google BigQuery using the bigrquery package
# This simply points to the ncaa_basketball dataset
# And sets up the group member's billing for GCP
connection <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "ncaa_basketball",
  billing = myBilling # This object is created in myProjectBilling.R
)

# --------------------- Creating example data table -----------------------

# Example: Creating a table from mbb_players_games_sr
#          of games that were on 11/20/2017,
#          table has game_id, and scheduled_date columns

# Make sure that whatever you do, you first call:
# tbl(connection, <DATASET-NAME>)

# And after using dplyr to make your table, end with:
# collect()

# games <- tbl(connection, "mbb_players_games_sr") %>% 
#   select(game_id, scheduled_date) %>%
#   filter(scheduled_date == "2017-11-20") %>% 
#   collect()

# IMPORTANT NOTE 1:
# DO NOT test your table creation too frequently!
# The queries for Google BigQuery are expensive, and I don't want
# us to go over our limits (free limit is 1 TB = 1000 GB of queries)
# the amount used when you run your command is in the console as "Billed: X MB)

# IMPORTANT NOTE 2:
# This file will be sourced, so make sure that nothing below runs when sourced!
# This means that you should:
# Make sure that before commiting you place your table/visualization commands
# into a function. It should take in a string (player, team, or year)
# and output the table (or draw the visualization). All of the code below
# should be in function bodies.

# For reference, here is the NCAA Dataset so you can get the correct
# subdataset names and preview the datasets to figure out what you
# need to do witht the data columns to get your desired result:
# https://console.cloud.google.com/marketplace/details/ncaa-bb-public/ncaa-basketball


# ---------------- Creating data tables for shiny document ----------------


# This table filters the 'mbb_players_games_sr' dataset for SETH
get_player_data <- function (name) {
  player_data <- tbl(connection, "mbb_players_games_sr") %>% 
    filter(full_name == name) %>%
    filter(season == max(season)) %>% 
    select(full_name, season, scheduled_date, team_name, free_throws_pct, two_points_pct, three_points_pct) %>%
    collect()
}


# Rolling Averages Function (SETH)
create_percent_season_plot <- function (name) {
  player_data <- get_player_data(name)
  ordered_player_data <- player_data[order(as.Date(player_data$scheduled_date)),]
  ordered_player_data <- data.frame(ordered_player_data, "Average Free Throw" = rollmean(ordered_player_data$free_throws_pct, 4, fill = NA, align=c("right")),
                                    "Average Two Pointer" = rollmean(ordered_player_data$two_points_pct, 4, fill = NA, align=c("right")), 
                                    "Average Three Pointer" = rollmean(ordered_player_data$three_points_pct, 4, fill = NA, align=c("right")))
  melted_data <- melt(ordered_player_data, id = c("full_name", "season", "scheduled_date", "team_name", "free_throws_pct", "two_points_pct", 
                                                  "three_points_pct"))
  p <- ggplot(melted_data, aes(scheduled_date, value, colour = variable)) +
    geom_line(size = 1.2) +
    xlab("Date") +
    ylab("Percent Made") +
    ggtitle(paste0("Rolling Shot Averages, ", name, ", ", ordered_player_data[1,2], " Season")) +
    scale_colour_manual(values=c("green", "yellow", "red"))
  print(p)
}


# This table filters the 'mbb_players_games_sr' dataset for MICHELLE
get_player_foul_data <- function (name) {
  player_data <- tbl(connection, "mbb_players_games_sr") %>%
    filter(full_name == name) %>%
    filter(season == max(season)) %>%
    select(full_name, season, team_name, jersey_number, height, weight, 
      personal_fouls, tech_fouls, flagrant_fouls, minutes, played, 
        birth_place) %>%
    collect()
}

# This function will return a specific player's most recent season (MICHELLE)
# This functio will calculate two percentages:
# 1. What percentage of games a player played in, in their most recent season
# 2. In the given season, what percentage of games a player played in and was in foul trouble 
# NOTE: 'Foul trouble' is when a player is within one or two of fouling out 
# Fouling out is having 6 fouls in a game
collectPlayerData <- function(playerName) {
  
  # Grab the specific player's data
  player_data <- get_player_foul_data(playerName)
  
  # Replace NA values in the table to 0
  player_data[is.na(player_data)] <- 0
  
  # Calculate the total number of fouls a player has in a given game
  player_data$totalFouls <- player_data$personal_fouls + player_data$tech_fouls + player_data$flagrant_fouls
  
  # Calculate number of games in the given season
  gamesInSeason <- NROW(player_data)
  
  # Filter the data table to only the games this specific player played in
  player_data <- filter(player_data, played == TRUE) %>% 
    select(full_name, season, team_name, totalFouls)
  gamesPlayed <- NROW(player_data)
  
  # Count how many times a player was in foul trouble during a game
  inFoulTrouble <- 0
  for (i in 1:nrow(player_data)) {
    if(player_data$totalFouls[i] > 3 & player_data$totalFouls[i] < 6) {
      inFoulTrouble = inFoulTrouble + 1
    }
  }
  
  # Caluclate the percentage of games a player played in this season
  gamesPlayedPercent <- (gamesPlayed / gamesInSeason) * 100
  
  # Calculate of the the games a player played in, how often were they in foul trouble
  frequencyInFoulTrouble <- (inFoulTrouble / gamesPlayed) * 100
  
  # Return a statement with games played percentage and games spent in foul trouble percentage
  finalStatement <- paste0(playerName, " played in ", gamesPlayedPercent, "% of games in the ", player_data$season,
    " season. Of these games, they spent ", round(frequencyInFoulTrouble),
      " in foul trouble.")
  return(finalStatement[1])
}

## Given Year (IBRAR)
## Who was the best player in a single game for certian season?
getBestPlayerInSingleGameForYear <- function(year) {
  games <- tbl(connection, "mbb_players_games_sr") %>% 
    filter(season == as.integer(year)) %>% 
    filter(points == max(points)) %>% 
    select(full_name, scheduled_date, jersey_number, field_goals_made, field_goals_att, field_goals_pct, points) %>%
    collect()
  
  return(games[1,])
}

## Which player had the most avg points in a certain season?
getBestAvgForPlayerInYear <- function(year) {
  games <- tbl(connection, "mbb_players_games_sr") %>%
    filter(season == as.integer(year)) %>%
    select(full_name, points) %>% 
    group_by(full_name) %>% 
    mutate(pointsAvg = mean(points)) %>% 
    filter(pointsAvg == max(pointsAvg)) %>% 
    arrange(-pointsAvg) %>% 
    collect()
  
  return(games[1,])
}

## What team had the highest ammount of points scored in a game for certain season?
getHighestPointInGameForYear <- function(year) {
  games <- tbl(connection, "mbb_teams_games_sr") %>%
    filter(season == as.integer(year)) %>% 
    filter(points == max(points)) %>% 
    select(market, name, gametime, season, points) %>%
    collect()
  
  return(games[1,])
}

## What team had the highest number of
# Assists in a game 
getHighestAssistsInGameForYear <- function(year) {
  games <- tbl(connection, "mbb_teams_games_sr") %>%
    filter(season == as.integer(year)) %>% 
    filter(assists == max(assists)) %>% 
    select(market, name, gametime, season, assists) %>%
    collect()
  
  return(games[1,])
}

# Field goals made in a game
getHighestFieldGoalsMadeInGameForYear <- function(year) {
  games <- tbl(connection, "mbb_teams_games_sr") %>%
    filter(season == as.integer(year)) %>% 
    filter(field_goals_made == max(field_goals_made)) %>% 
    select(market, name, gametime, season, field_goals_made) %>%
    collect()
  
  return(games[1,])
}

# Field goals attempted in a game
getHighestFieldGoalsAttemptedInGameForYear <- function(year) {
  games <- tbl(connection, "mbb_teams_games_sr") %>%
    filter(season == as.integer(year)) %>% 
    filter(field_goals_att == max(field_goals_att)) %>% 
    select(market, name, gametime, season, field_goals_att) %>%
    collect()
  
  return(games[1,])
}

# Field goal percentage in a game
getHighestFieldGoalsPercentageInGameForYear <- function(year) {
  games <- tbl(connection, "mbb_teams_games_sr") %>%
    filter(season == as.integer(year)) %>% 
    filter(field_goals_pct == max(field_goals_pct)) %>% 
    select(market, name, gametime, season, field_goals_pct) %>%
    collect()
  
  return(games[1,])
}

# Three pointers made in a game
getHighestThreePointMadeInGameForYear <- function(year) {
  games <- tbl(connection, "mbb_teams_games_sr") %>%
    filter(season == as.integer(year)) %>% 
    filter(three_points_made == max(three_points_made)) %>% 
    select(market, name, gametime, season, three_points_made) %>%
    collect()
  
  return(games[1,])
}

# Fouls in a game
getHighestFoulsInGameForYear <- function(year) {
  games <- tbl(connection, "mbb_teams_games_sr") %>%
    filter(season == as.integer(year)) %>% 
    filter(personal_fouls == max(personal_fouls)) %>% 
    select(market, name, gametime, season, personal_fouls) %>%
    collect()
  
  return(games[1,])
}


