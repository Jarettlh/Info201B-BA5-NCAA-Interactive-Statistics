## In this file, we create the functions that will be called in our
## shiny document. They will take in a string corresponding to user
## input,  get and sort data from Google BigQuery NCAA, and finally
## output data tables (or draw visualizations) for our document

library(bigrquery)
library(dplyr)

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

# ---------------- Creating data tables for shiny document ----------------

# IMPORTANT NOTE 2:
# This file will be sourced, so make sure that nothing below runs when sourced!
# This means that you should:
# Make sure that before commiting you place your table/visualization commands
# into a function. It should take in a string (player, team, or year)
# and output the table (or draw the visualization). All of the code below
# should be in function bodies.
get_player_data <- function (name) {
  player_data <- tbl(connection, "mbb_players_games_sr") %>% 
    filter(full_name == name) %>%
    filter(season == max(season)) %>% 
    select(full_name, season, scheduled_date, team_name, free_throws_pct, two_points_pct, three_points_pct) %>%
    collect()
}


# For reference, here is the NCAA Dataset so you can get the correct
# subdataset names and preview the datasets to figure out what you
# need to do witht the data columns to get your desired result:
# https://console.cloud.google.com/marketplace/details/ncaa-bb-public/ncaa-basketball

# Create table functions here
# player_data <- get_player_data("Lonzo Ball")
# ordered_player_data <- player_data[order(as.Date(player_data$scheduled_date)),]

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
