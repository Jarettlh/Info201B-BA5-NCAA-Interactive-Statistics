# ----------------------- File Intro ------------------------------------

# This is the shiny document our app will run on

# ----------------------- Shiny Setup  ----------------------------------

library(shiny)

# Source both .R files for:
# Data processing and
# Error handling
source('./data_processing.R')
source('./error_handling.R')

# Now all the necessary functions for data visualizing are
# ready to be called

# ----------------------- Shiny UI --------------------------------------

# UI to display for users
my_ui <- fluidPage(
  h1("NCAA College Basketball Interactive Statistics"),
  h2("Using Google BigQuery"),
  h3("Data is from 2013-2108 seasons"),
  
  # Text box for input with button
  textInput('userInputString', label = "Enter a Division 1 player, or year", value = ""),
  submitButton("Show me statistics!"),
  
  # Output for all tables, plots, and statements
  htmlOutput("title"),
  htmlOutput("yearText1"),
  textOutput("playerText1"),
  textOutput("playerText2"),
  plotOutput("playerPlot1"),
  plotOutput("playerPlot2"),
  plotOutput("playerPlot3"),
  plotOutput("playerPlot4"),
  plotOutput("playerPlot5")
)

# ----------------------- Shiny R Backend --------------------------------

# The R backend that runs when input is given
my_server <- function(input, output) {
  # Store reactive values in rVals
  rVals <- reactiveValues()
  
  # Reactive function that:
  # Validates user input (displaying error message if needed) OR
  # Gets the type of user input (Player/Year) if input was valid
  inputValidateAndType <- reactive({
    
    # Convert input string to camel case (eg. "trae young" -> "Trae Young")
    s <- strsplit(input$userInputString, " ")[[1]]
    rVals$userInputStringConverted <- paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
      
    # This function (in error_handling.R) does most the work
    rVals$inputType <- getInputType(rVals$userInputStringConverted)
    
    # After that has been called, we now have an inputType
    # string that is either a correct type, or an error message
    
    # Display error message if inputType is an error message
    if (rVals$inputType != "PLAYER" &&
        rVals$inputType != "YEAR") {
      validate(rVals$inputType)
    }
  })
  
  
  checkPlayer <- reactive({
    if(rVals$inputType == "PLAYER"){TRUE}
    else{FALSE}
  })
  
  checkYear <- reactive({
    if(rVals$inputType == "YEAR"){TRUE}
    else{FALSE}
  })

  
  # Now, we are ready to display our data visualizations
  
  # First, call the inputValidateAndType function when input changes
  # the code below that call will run only if we have a valid inputType
  
  output$title <-  renderText({
    inputValidateAndType()
    paste0("<b>Data for ", rVals$userInputStringConverted, "</b>")
  })
  
  # Then, display data visualizations corresponding to inputType
  # if the input was valid
  

  output$playerPlot1 <- renderPlot({
    if(checkPlayer()){
      create_percent_season_plot(rVals$userInputStringConverted)
    }
  })
  
  output$playerPlot2 <- renderPlot({
    if(checkPlayer()){
      shot_type_comparison(rVals$userInputStringConverted)
    }
  })
  
  output$playerPlot3 <- renderPlot({
    if(checkPlayer()){
      point_assist_block_comparison(rVals$userInputStringConverted)
    }
  })
  
  output$playerPlot4 <- renderPlot({
    if(checkPlayer()){
      playerMinutesPlayedSeason(rVals$userInputStringConverted)
    }
  })
  
  output$playerPlot5 <- renderPlot({
    if(checkPlayer()){
      playerStealsSeason(rVals$userInputStringConverted)
    }
  })
  
  output$playerText1 <- renderText({
    if(checkPlayer()){
      mostRecentSeasonFoulTrouble(rVals$userInputStringConverted)
    }
  })
  
  output$playerText2 <- renderText({
    if(checkPlayer()){
      get_player_personal_data(rVals$userInputStringConverted)
    }
  })
  
  output$yearText1 <- renderText({
    if(checkYear()){
      HTML(yearFunctions(rVals$userInputStringConverted))
    }
  })
  
}


# --------------------- Ready Shiny App for Launch ------------------------

shinyApp(ui = my_ui, server = my_server)