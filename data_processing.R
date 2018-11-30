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

games <- tbl(connection, "mbb_players_games_sr") %>% 
  select(game_id, scheduled_date) %>%
  filter(scheduled_date == "2017-11-20") %>% 
  collect()

# IMPORTANT NOTE 1:
# DO NOT test your table creation too frequently!
# The queries for Google BigQuery are expensive, and I don't want
# us to go over our limits (free limit is 1 TB = 1000 GB of queries)

# ---------------- Creating data tables for shiny document ----------------

# IMPORTANT NOTE 2:
# This file will be sourced, so make sure that nothing below runs when sourced!
# This means that you should:
# Make sure that before commiting you place your table/visualization commands
# into a function. It should take in a string (player, team, or year)
# and output the table (or draw the visualization). All of the code below
# should be in function bodies.


#Create table functions here

