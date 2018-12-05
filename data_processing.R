# ----------------------- File Intro -------------------------------------

# In this file, we create the functions that will be called in our
# shiny document. They will take in a string corresponding to user
# input,  get and sort data from Google BigQuery NCAA, and finally
# output data tables (or draw visualizations) for our document

# ---------------------- Setting up Google BigQuery ----------------------
# Required libraries for Google BigQuery
library(bigrquery)
library(dplyr)

# This file is seperate for each group member:
# It contains the project ID for the  group member's
# Google Cloud project for billing purposes
source('./myProjectBilling.R')

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
# into a function. It should take in a string (player, or year)
# and output the table (or draw the visualization). All of the code below
# should be in function bodies.

# For reference, here is the NCAA Dataset so you can get the correct
# subdataset names and preview the datasets to figure out what you
# need to do witht the data columns to get your desired result:
# https://console.cloud.google.com/marketplace/details/ncaa-bb-public/ncaa-basketball


# ---------------- Setting up data visualizations for shiny document -------------------

# Required libraries for functions
library(ggplot2)
library(zoo)
library(reshape2)
library(spatstat.utils)

# Get player list for shiny error handling
# Only done once, so now commented out

# playerList <- tbl(connection, "mbb_players_games_sr") %>%
#   select(full_name) %>%
#   collect() %>%
#   unique
# saveRDS(playerList, './playerList.RDS')

# -------------------------- Player plots (SETH/MICHELLE) -------------------------

# DEPENDANCY FOR PLAYER PLOT 1
# This table filters the 'mbb_players_games_sr' dataset, pulling shot percent data for a player's
# most recent season (SETH)
get_player_data <- function (name) {
  player_data <- tbl(connection, "mbb_players_games_sr") %>%
    filter(full_name == name) %>%
    filter(season == max(season)) %>%
    select(
      full_name,
      season,
      scheduled_date,
      team_name,
      free_throws_pct,
      two_points_pct,
      three_points_pct
    ) %>%
    collect()
}

percentRange <- c(0, 1)
convertPctFromDecimal <- function(numberVector) {
  for (i in 1:length(numberVector)) {
    if (inside.range(numberVector[i], percentRange)) {
      numberVector[i] = numberVector[i] * 100
    }
  }
  return(numberVector)
}
# PLAYER PLOT 1
# This function creates a line graph which displays a player's rolling averages for their most recent
# season. This includes free throws, two pointers, and three pointers. (SETH)
create_percent_season_plot <- function (name) {
  player_data <- get_player_data(name)
  ordered_player_data <-
    player_data[order(as.Date(player_data$scheduled_date)),]
  ordered_player_data <- mutate(
    ordered_player_data,
    two_points_pct = convertPctFromDecimal(two_points_pct),
    free_throws_pct = convertPctFromDecimal(free_throws_pct),
    three_points_pct = convertPctFromDecimal(three_points_pct)
  )
  ordered_player_data <-
    data.frame(
      ordered_player_data,
      "Free Throw Average" = rollmean(
        ordered_player_data$free_throws_pct,
        4,
        fill = NA,
        align = c("right")
      ),
      "Two Pointer Average" = rollmean(
        ordered_player_data$two_points_pct,
        4,
        fill = NA,
        align = c("right")
      ),
      "Three Pointer Average" = rollmean(
        ordered_player_data$three_points_pct,
        4,
        fill = NA,
        align = c("right")
      )
    )
  melted_data <-
    melt(
      ordered_player_data,
      id = c(
        "full_name",
        "season",
        "scheduled_date",
        "team_name",
        "free_throws_pct",
        "two_points_pct",
        "three_points_pct"
      )
    )
  p <-
    ggplot(melted_data, aes(scheduled_date, value, colour = variable)) +
    geom_line(size = 1.2) +
    xlab("Date") +
    ylab("Percent Made") +
    scale_colour_manual(values = c("green", "yellow", "red")) +
    ggtitle(paste0(
      "Rolling Shot Averages by ",
      name,
      " in the ",
      ordered_player_data[1, 2],
      " Season"
    )) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  print(p)
}

# DEPENDANCY FOR PLAYER PLOT 2
# Pulls all shot data for a player for their most recent season (SETH)
get_player_shot_data <- function (name) {
  player_data <- tbl(connection, "mbb_pbp_sr") %>%
    filter(player_full_name == name) %>%
    filter(season == max(season), is.na(shot_type) == FALSE) %>%
    select(player_full_name, season, shot_type, shot_made) %>%
    collect()
}

# PLAYER PLOT 2
# This function creates a bar plot displaying the various shot types taken by a certain player in their most
# recent season. It displays a black bar of the total amount of that type of shot taken, overlayed with a
# green bar indicating the amount of those shots made (SETH)
shot_type_comparison <- function (name) {
  shots <- get_player_shot_data(name)
  grouped_shots <- shots %>%
    select(shot_type, shot_made) %>%
    group_by(shot_type, shot_made) %>%
    summarise(Freq = n())
  p <- ggplot(grouped_shots, aes(shot_type, Freq)) +
    stat_summary(fun.y = sum, geom = "bar") +
    geom_bar(stat = "identity", aes(fill = shot_made)) +
    scale_fill_manual(
      name = "Shots",
      values = c("black", "green"),
      labels = c("Total Shots", "Made Shots")
    ) +
    xlab("Shot Type") +
    ylab("Frequency") +
    ggtitle(paste0(
      "Shot Type Frequency and Success by ",
      name,
      " in the ",
      shots[1, 2],
      " Season"
    )) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  print(p)
}

# DEPENDANCY FOR PLAYER PLOT 3
# Pulls blocks, assists, points data for a selected player's most recent season (SETH)
get_player_point_block_assist_data <- function (name) {
  p_b_a_data <- tbl(connection, "mbb_players_games_sr") %>%
    filter(full_name == name) %>%
    filter(season == max(season)) %>%
    select(full_name, season, scheduled_date, points, blocks, assists) %>%
    collect()
}

# PLAYER PLOT 3
# This function display a line graph over a player's most recent season. It will display the amount of blocks,
# assists, and point over the season per game (SETH)
point_assist_block_comparison <- function (name) {
  p_b_a_data <- get_player_point_block_assist_data(name)
  melted_data <-
    melt(p_b_a_data, id = c("full_name", "season", "scheduled_date"))
  p <-
    ggplot(melted_data, aes(scheduled_date, value, colour = variable)) +
    geom_line(size = 1.2) +
    xlab("Date") +
    ylab("Count") +
    scale_colour_manual(values = c("green", "red", "cyan")) +
    ggtitle(paste0(
      "Points, Blocks, and Assists by ",
      name,
      " in the ",
      p_b_a_data[1, 2],
      " Season"
    )) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  print(p)
}

# PLAYER PLOT 4
# Shows the average minutes played for a particular player over the course of their most recent season (Michelle)
playerMinutesPlayedSeason <- function(name) {
  # Filters the dataset to the player for their most recent season
  playerMinutesData <- tbl(connection, "mbb_players_games_sr") %>%
    filter(full_name == name) %>%
    filter(season == max(season)) %>%
    select(minutes_int64, scheduled_date, season) %>%
    collect()
  playerMinutesData[is.na(playerMinutesData)] <- 0
  
  # Initialize variables
  avgTimeNov <- 0
  avgTimeDec <- 0
  avgTimeJan <- 0
  avgTimeFeb <- 0
  avgTimeMar <- 0
  gamesNov <- 0
  gamesDec <- 0
  gamesJan <- 0
  gamesFeb <- 0
  gamesMar <- 0
  
  # Convert scheduled date to just its month
  playerMinutesData$newdate <-
    format(as.Date(playerMinutesData$scheduled_date), "%m")
  for (i in 1:nrow(playerMinutesData)) {
    # Adds the player's game time on to November's average time if the game occurred in November
    if (playerMinutesData$newdate[i] == "11") {
      avgTimeNov <- avgTimeNov + (playerMinutesData$minutes_int64[i])
      gamesNov <- gamesNov + 1
    }
    
    # Adds the player's game time on to December's average time if the game occurred in December
    if (playerMinutesData$newdate[i] == "12") {
      avgTimeDec <- avgTimeDec + (playerMinutesData$minutes_int64[i])
      gamesDec <- gamesDec + 1
    }
    
    # Adds the player's game time on to January's average time if the game occurred in January
    if (playerMinutesData$newdate[i] == "01") {
      avgTimeJan <- avgTimeJan + (playerMinutesData$minutes_int64[i])
      gamesJan <- gamesJan + 1
    }
    
    # Adds the player's game time on to Febuary's average time if the game occurred in Febuary
    if (playerMinutesData$newdate[i] == "02") {
      avgTimeFeb <- avgTimeFeb + (playerMinutesData$minutes_int64[i])
      gamesFeb <- gamesFeb + 1
    }
    
    # Adds the player's game time on to March's average time if the game occurred in March
    if (playerMinutesData$newdate[i] == "03") {
      avgTimeMar <- avgTimeMar + (playerMinutesData$minutes_int64[i])
      gamesMar <- gamesMar + 1
    }
  }
  
  # Calculate the player's average play time during the given month in the given season
  avgTimeNov <- round((avgTimeNov / gamesNov), 0)
  avgTimeDec <- round((avgTimeDec / gamesDec), 0)
  avgTimeJan <- round((avgTimeJan / gamesJan), 0)
  avgTimeFeb <- round((avgTimeFeb / gamesFeb), 0)
  avgTimeMar <- round((avgTimeMar / gamesMar), 0)
  
  # Plots the bar graph with the information
  monthAvgCount <-
    c(avgTimeNov, avgTimeDec, avgTimeJan, avgTimeFeb, avgTimeMar)
  months <- c("November", "December", "January", "Febuary", "March")
  colors <- c("pink", "blue", "green", "gray", "black")
  averagePlayTimeBarPlot <-
    barplot(
      monthAvgCount,
      names.arg = months,
      col = colors,
      xlab = "Month",
      ylab = "Minutes",
      main = paste0(
        "Average minutes played for per month for ",
        name,
        " in the ",
        playerMinutesData$season[1],
        " season"
      )
    )
  print(averagePlayTimeBarPlot)
}

# PLAYER PLOT 5
# Creates a barplot showing how many steals the players made each month
playerStealsSeason <- function(name) {
  # Filters the dataset to the player for their most recent season
  playerStealsData <- tbl(connection, "mbb_players_games_sr") %>%
    filter(full_name == name) %>%
    filter(season == max(season)) %>%
    select(steals, scheduled_date, season) %>%
    collect()
  playerStealsData[is.na(playerStealsData)] <- 0
  
  # Initialize variables
  stealsNov <- 0
  stealsDec <- 0
  stealsJan <- 0
  stealsFeb <- 0
  stealsMar <- 0
  
  # Convert scheduled date to just its month
  playerStealsData$newdate <-
    format(as.Date(playerStealsData$scheduled_date), "%m")
  for (i in 1:nrow(playerStealsData)) {
    # Calculates the total steals a specific player made in November
    if (playerStealsData$newdate[i] == "11") {
      stealsNov <- stealsNov + (playerStealsData$steals[i])
    }
    
    # Calculates the total steals a specific player made in December
    if (playerStealsData$newdate[i] == "12") {
      stealsDec <- stealsDec + (playerStealsData$steals[i])
    }
    
    # Calculates the total steals a specific player made in January
    if (playerStealsData$newdate[i] == "01") {
      stealsJan <- stealsJan + (playerStealsData$steals[i])
    }
    
    # Calculates the total steals a specific player made in Febuary
    if (playerStealsData$newdate[i] == "02") {
      stealsFeb <- stealsFeb + (playerStealsData$steals[i])
    }
    
    # Calculates the total steals a specific player made in March
    if (playerStealsData$newdate[i] == "03") {
      stealsMar <- stealsMar + (playerStealsData$steals[i])
    }
  }
  
  # Plots the bar graph with the information
  stealsPerMonth <-
    c(stealsNov, stealsDec, stealsJan, stealsFeb, stealsMar)
  months <- c("November", "December", "January", "Febuary", "March")
  colors <- c("pink", "blue", "green", "gray", "black")
  stealsMadeBarPlot <-
    barplot(
      stealsPerMonth,
      names.arg = months,
      col = colors,
      xlab = "Month",
      ylab = "Steals",
      main = paste0(
        "Number of blocks ",
        name,
        " made in the ",
        playerStealsData$season[1],
        " season"
      )
    )
  print(stealsMadeBarPlot)
}

# -------------------------- Player sentences (MICHELLE) -------------------------


# PLAYER SENTENCE 1
# This function will return a specific player's most recent season (MICHELLE)
# This functio will calculate two percentages:
# 1. What percentage of games a player played in, in their most recent season
# 2. In the given season, what percentage of games a player played in and was in foul trouble
# NOTE: 'Foul trouble' is when a player is within one or two of fouling out
# Fouling out is having 6 fouls in a game
mostRecentSeasonFoulTrouble <- function(playerName) {
  # This table filters the 'mbb_players_games_sr'
  player_data <- tbl(connection, "mbb_players_games_sr") %>%
    filter(full_name == playerName) %>%
    filter(season == max(season)) %>%
    select(full_name,
           season,
           team_name,
           personal_fouls,
           tech_fouls,
           flagrant_fouls,
           played) %>%
    collect()
  
  
  # Replace NA values in the table to 0
  player_data[is.na(player_data)] <- 0
  
  # Calculate the total number of fouls a player has in a given game
  player_data$totalFouls <-
    player_data$personal_fouls + player_data$tech_fouls + player_data$flagrant_fouls
  
  # Calculate number of games in the given season
  gamesInSeason <- NROW(player_data)
  
  # Filter the data table to only the games this specific player played in
  player_data <- filter(player_data, played == TRUE) %>%
    select(full_name, season, team_name, totalFouls)
  gamesPlayed <- NROW(player_data)
  
  # Count how many times a player was in foul trouble during a game
  inFoulTrouble <- 0
  for (i in 1:nrow(player_data)) {
    if (player_data$totalFouls[i] > 3 & player_data$totalFouls[i] < 6) {
      inFoulTrouble = inFoulTrouble + 1
    }
  }
  
  # Caluclate the percentage of games a player played in this season
  gamesPlayedPercent <- (gamesPlayed / gamesInSeason) * 100
  
  # Calculate of the the games a player played in, how often were they in foul trouble
  frequencyInFoulTrouble <- (inFoulTrouble / gamesPlayed) * 100
  
  # Return a statement with games played percentage and games spent in foul trouble percentage
  finalStatement <-
    paste0(
      playerName,
      " played in ",
      round(gamesPlayedPercent),
      "% of games in the ",
      player_data$season,
      " season. Of these games, he spent ",
      round(frequencyInFoulTrouble),
      " in foul trouble."
    )
  return(finalStatement[1])
}

# PLAYER SENTENCE 2
# Returns a player's personal information (Michelle)
get_player_personal_data <- function(name) {
  player_personal_data <- tbl(connection, "mbb_players_games_sr") %>%
    filter(full_name == name) %>%
    filter(season == max(season)) %>%
    select(full_name,
           season,
           team_name,
           jersey_number,
           height,
           weight,
           birth_place) %>%
    collect()
  finalStatement <- paste0(
    name,
    "'s most recent season was the ",
    player_personal_data$season,
    " season. He played for the ",
    player_personal_data$team_name,
    ". His jersey number was ",
    player_personal_data$jersey_number,
    ". ",
    name,
    " was born in ",
    player_personal_data$birth_place,
    " and was ",
    player_personal_data$weight,
    " pounds in their most recent season."
  )
  return(finalStatement[1])
}

# -------------------------- Yearly sentences (IBRAR) ----------------------------------

# YEARLY PARAGRAPH DEPENDANCIES
getYearlyStatsPlayer <- function(year) {
  games <- tbl(connection, "mbb_players_games_sr") %>%
    filter(season == as.integer(year)) %>%
    select(
      full_name,
      scheduled_date,
      jersey_number,
      field_goals_made,
      field_goals_att,
      field_goals_pct,
      points
    ) %>%
    collect()
}

getYearlyStatsTeam <- function(year) {
  games <- tbl(connection, "mbb_teams_games_sr") %>%
    filter(season == as.integer(year)) %>%
    select(
      market,
      name,
      gametime,
      season,
      points,
      assists,
      field_goals_made,
      field_goals_att,
      field_goals_pct,
      three_points_made,
      personal_fouls
    ) %>%
    collect()
}

## Who was the best player in a single game for certian season?
getBestPlayerInSingleGameForYear <- function(yearlyPlayerStats) {
  games <- yearlyPlayerStats %>%
    na.omit(invert = TRUE) %>%
    filter(points == max(points)) %>%
    select(
      full_name,
      scheduled_date,
      jersey_number,
      field_goals_made,
      field_goals_att,
      field_goals_pct,
      points
    )
  
  return(games[1,])
}

## Which player had the most avg points in a certain season?
getBestAvgForPlayerInYear <- function(yearlyPlayerStats) {
  games <- yearlyPlayerStats %>%
    na.omit(invert = TRUE) %>%
    select(full_name, points) %>%
    group_by(full_name) %>%
    mutate(pointsAvg = mean(points)) %>%
    filter(pointsAvg == max(pointsAvg)) %>%
    arrange(-pointsAvg)
  
  return(games[1,])
}

## What team had the highest ammount of points scored in a game for certain season?
getHighestPointInGameForYear <- function(yearlyTeamStats) {
  games <- yearlyTeamStats %>%
    filter(points == max(points)) %>%
    select(market, name, gametime, season, points)
  
  return(games[1,])
}

## What team had the highest number of
# Assists in a game
getHighestAssistsInGameForYear <- function(yearlyTeamStats) {
  games <- yearlyTeamStats %>%
    filter(assists == max(assists)) %>%
    select(market, name, gametime, season, assists)
  
  return(games[1,])
}

# Field goals made in a game
getHighestFieldGoalsMadeInGameForYear <- function(yearlyTeamStats) {
  games <- yearlyTeamStats %>%
    filter(field_goals_made == max(field_goals_made)) %>%
    select(market, name, gametime, season, field_goals_made)
  
  return(games[1,])
}

# Field goals attempted in a game
getHighestFieldGoalsAttemptedInGameForYear <-
  function(yearlyTeamStats) {
    games <- yearlyTeamStats %>%
      filter(field_goals_att == max(field_goals_att)) %>%
      select(market, name, gametime, season, field_goals_att)
    
    return(games[1,])
  }

# Field goal percentage in a game
getHighestFieldGoalsPercentageInGameForYear <-
  function(yearlyTeamStats) {
    games <- yearlyTeamStats %>%
      filter(field_goals_pct == max(field_goals_pct)) %>%
      select(market, name, gametime, season, field_goals_pct)
    
    return(games[1,])
  }

# Three pointers made in a game
getHighestThreePointMadeInGameForYear <- function(yearlyTeamStats) {
  games <- yearlyTeamStats %>%
    filter(three_points_made == max(three_points_made)) %>%
    select(market, name, gametime, season, three_points_made)
  
  return(games[1,])
}

# Fouls in a game
getHighestFoulsInGameForYear <- function(yearlyTeamStats) {
  games <- yearlyTeamStats %>%
    filter(personal_fouls == max(personal_fouls)) %>%
    select(market, name, gametime, season, personal_fouls)
  
  return(games[1,])
}


# YEARLY PARAGRAPH
# Puts all yeary functions together
yearFunctions <- function(year) {
  playerYearStats <- getYearlyStatsPlayer(year)
  teamYearStats <- getYearlyStatsTeam(year)
  table1 <- getBestPlayerInSingleGameForYear(playerYearStats)
  table2 <- getBestAvgForPlayerInYear(playerYearStats)
  table3 <- getHighestPointInGameForYear(teamYearStats)
  table4 <- getHighestAssistsInGameForYear(teamYearStats)
  table5 <- getHighestFieldGoalsMadeInGameForYear(teamYearStats)
  table6 <- getHighestFieldGoalsAttemptedInGameForYear(teamYearStats)
  table7 <-
    getHighestFieldGoalsPercentageInGameForYear(teamYearStats)
  table8 <- getHighestThreePointMadeInGameForYear(teamYearStats)
  table9 <- getHighestFoulsInGameForYear(teamYearStats)
  sentence1 <-
    paste0(
      "The most points scored in one game by a single player in ",
      year,
      " was ",
      table1[1, "points"],
      " by ",
      table1[1, "full_name"]
    )
  sentence2 <-
    paste0(
      "The player with the most points per game on average in ",
      year,
      " was ",
      table2[1, "full_name"],
      " with ",
      table2[1, "pointsAvg"],
      " points per game on average"
    )
  sentence3 <-
    paste0(
      "The most points scored by a team in one game in ",
      year,
      " was ",
      table3[1, "points"],
      " points, by the ",
      table3[1, "market"],
      " ",
      table3[1, "name"]
    )
  sentence4 <-
    paste0(
      "The most assists by a team in one game in ",
      year,
      " was ",
      table4[1, "assists"],
      " assists, by the ",
      table4[1, "market"],
      " ",
      table4[1, "name"]
    )
  sentence5 <-
    paste0(
      "The most field goals made by a team in one game in ",
      year,
      " was ",
      table5[1, "field_goals_made"],
      " field goals, by the ",
      table5[1, "market"],
      " ",
      table5[1, "name"]
    )
  sentence6 <-
    paste0(
      "The most field goals attempted by a team in one game in ",
      year,
      " was ",
      table6[1, "field_goals_att"],
      " field goals, by the ",
      table6[1, "market"],
      " ",
      table6[1, "name"]
    )
  sentence7 <-
    paste0(
      "The highest field goal made percentage by a team in one game in ",
      year,
      " was ",
      table7[1, "field_goals_pct"],
      "%, by the ",
      table7[1, "market"],
      " ",
      table7[1, "name"]
    )
  sentence8 <-
    paste0(
      "The most three pointers made by a team in one game in ",
      year,
      " was ",
      table8[1, "three_points_made"],
      " three pointers, by the ",
      table8[1, "market"],
      " ",
      table8[1, "name"]
    )
  sentence9 <-
    paste0(
      "The highest number of personal fouls given to a team in one game in ",
      year,
      " was ",
      table9[1, "personal_fouls"],
      " personal fouls, by the ",
      table9[1, "market"],
      " ",
      table9[1, "name"]
    )
  
  paragraph <- paste(
    sentence1,
    sentence2,
    sentence3,
    sentence4,
    sentence5,
    sentence6,
    sentence7,
    sentence8,
    sentence9,
    sep = "<br/>"
  )
}
