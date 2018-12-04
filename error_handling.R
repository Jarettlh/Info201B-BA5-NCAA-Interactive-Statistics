# ----------------------- File Intro ------------------------------------

# This file is a dependancy for the shiny document
# Its purpose is to check the user input string
# and check that it is a valid player, or year
# If it was, it returns the type, and if not,
# it returns an error message

# ------------------- Setup Error Handling --------------------------------

# Library used for year (season) range function
library(spatstat.utils)

# Variables used for year (season) range function
seasonRange <- c(2013, 2018)
validOtherYears <- c(1, 2012)

# Function to check if the integer input given is a
# valid year in the dataset
isInSeasonRange <- function(inputString) {
  if (inside.range(as.integer(inputString), seasonRange)) {
    "YEAR"
  } else if (inside.range(as.integer(inputString), validOtherYears)) {
    "That year is not present in the dataset used"
  } else {
    "That is not a valid year"
  }
}

# Load player list to check for valid player input
playerList <- readRDS('./playerList.RDS')

# Function to check if the input given was a player
isaD1Player <- function(inputString) {
  any(playerList == inputString)
}

# ------------------- Error Handling Execution -----------------------------

# Ties all functions together
# This is the function called by the shiny document
# that checks the user input type, and reports
# back the input type or an error message if it was invalid
getInputType <- function(inputString) {
  # At first we assume the input is empty, so we output
  outputString <- "Please enter a Division 1 player, or year"
  
  # If it is not empty, we check for each type
  if (!inputString == "") {
    
    # Year check
    if (!grepl("\\D", inputString)) {
      outputString <- isInSeasonRange(inputString)
      
      # Player check
    } else if (isaD1Player(inputString)) {
      outputString <- "PLAYER"
    }
    # If that didn't work, an invalid player was entered
    else if (!grepl("\\s", inputString)) {
      outputString <-
        "You must type a full name. Are you missing a last name?"
    } else {
      outputString <- "Invalid player name, or player is not in database"
    }
  }
  # Then we return the type/error string
  return(outputString)
}