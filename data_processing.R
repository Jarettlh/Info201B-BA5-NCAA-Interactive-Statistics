## This is a stub file for functions taking data from 
## Google BigQuery NCAA and outputting data tables we can use

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

# --------------------- Creating a data table ----------------------------

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