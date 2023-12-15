## ui.R
library(shiny)

fluidPage(
  tags$body(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed');
      body {
        font-family: 'Roboto Condensed';
      }"))
  ),
  titlePanel("NFL Air Yards Leaderboard"),
  fluidRow(
    column(2, 
           sliderInput("weekInput", "Select Week:", 
                       min = 1, max = 17, value = c(1, 17)),
           numericInput("minAirYards", "Minimum Total Air Yards:", 
                        min = 0, max = 2000, value = 100, step = 10),
           checkboxGroupInput("positionToggle", "Position",
                              choices = c("WR", "TE", "RB"),
                              selected = c("WR", "TE"))
    ),
    column(10,
           div(style = "padding: 10px 0px;", 
               "Prayer Yards are the amount of yards a ball traveled in the air (air yards) on passes deemed uncatchable."),
           reactableOutput("table"),
           div(style = "padding: 10px 0px;", 
               "Data: FTN Data via nflverse")
    ),
  )
)