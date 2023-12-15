library(shiny)
library(reactable)
library(dplyr)
library(scales)

# Read in dataset
new_df <- readRDS(gzcon(url("https://github.com/thefalkon-1/Prayer-Yards-Project/raw/main/data/data.rds")))

create_bar_plot <- function(value, width = 100, height = 20) {
  # Check if the value is NA or non-numeric
  if (is.na(value) || !is.numeric(value)) {
    return("<svg width='100' height='20'></svg>")  # Return an empty plot for NA or non-numeric values
  }
  
  # Create a color gradient function
  color_gradient <- scales::col_numeric(
    palette = c("blue", "lightgrey", "red"), 
    domain = c(0, 100)
  )
  
  # Determine the color based on the value
  color <- color_gradient(value)
  
  # Calculate the width of the bar based on the value
  # Normal width is 100. Divided by 105 to make sure the bar is less than the space
  bar_width <- (value / 110) * width
  
  # Adjust the radius for the circle and font size for the text
  circle_radius <- height / 2.25  # Larger circle radius
  text_font_size <- height / 2  # Smaller text font size
  
  # Circle should be anchored to the end of the bar plot
  # so its center is at the end of the bar minus the radius
  circle_center_x <- bar_width 
  
  # Ensure the circle is fully visible when value is small
  if (circle_center_x < circle_radius) {
    circle_center_x <- circle_radius
  }
  
  # Gray background bar dimensions
  gray_bar_height <- height / 5
  gray_bar_y_position <- (height - gray_bar_height) / 2
  
  # Create an inline SVG for the bar plot
  svg_plot <- sprintf("<svg width='%f' height='%f' viewBox='0 0 %f %f'>
                        <rect x='0' y='%f' width='%f' height='%f' fill='lightgrey'/>
                        <rect x='0' y='0' width='%f' height='%f' fill='%s'/>
                        <circle cx='%f' cy='%f' r='%f' fill='%s' stroke='white' stroke-width='1.25'/>
                        <text x='%f' y='%f' font-size='%f' fill='white' font-weight=bold text-anchor='middle' alignment-baseline='middle'>%.0f</text>
                      </svg>", 
                      width, height, width, height, 
                      gray_bar_y_position, width, gray_bar_height,
                      bar_width, height, color,
                      circle_center_x, height / 2, circle_radius, color,
                      circle_center_x, height / 2 + 0.75, text_font_size, value)
  return(svg_plot)
}

ui <- fluidPage(
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

server <- function(input, output) {
  filtered_data <- reactive({
    # Filter and summarize your data here
    week_filtered_data <- new_df %>%
      filter(position_receiver %in% input$positionToggle) %>%
      filter(week >= input$weekInput[1], week <= input$weekInput[2])
    
    re_summed_data <- week_filtered_data %>%
      group_by(full_name_receiver, receiver_player_id, posteam) %>%
      summarise(
        ay_catchable = sum(ifelse(is_catchable_ball == "TRUE", air_yards, 0), na.rm = TRUE),
        ay_uncatchable = sum(ifelse(is_catchable_ball == "FALSE", air_yards, 0), na.rm = TRUE),
        total = ay_catchable + ay_uncatchable
      ) %>%
      filter(total >= input$minAirYards) %>%
      mutate(uncatchable_pct = case_when(ay_catchable < 0 & ay_uncatchable < 0 ~ 1,
                                         ay_catchable < 0 & ay_uncatchable >= 0 ~ 1,
                                         ay_catchable >= 0 & ay_uncatchable < 0 ~ 0,
                                         .default = ay_uncatchable / total)) %>%
      ungroup() %>%
      select(-receiver_player_id) %>%
      arrange(-total) %>%
      filter(total > 0) %>%
      mutate(percentile = (rank(-uncatchable_pct, ties.method = "first") / n() * 100))
      
    
    re_summed_data
  })
  
  output$table <- renderReactable({
    reactable(filtered_data(), 
              # Specify columns and formatting here
              columns = list(
                full_name_receiver = colDef(name = "Player",
                                            style = list(fontFamily = "Roboto Condensed"),
                                            minWidth = 100,
                                            maxWidth = 300),
                posteam = colDef(name = "Team",
                                 style = list(fontFamily = "Roboto Condensed"),
                                 maxWidth = 50),
                ay_catchable = colDef(name = "Catchable Air Yards",
                                      style = list(fontFamily = "Roboto Condensed"),
                                      maxWidth = 150),
                ay_uncatchable = colDef(name = "Prayer Yards",
                                        style = list(fontFamily = "Roboto Condensed"),
                                        maxWidth = 150),
                total = colDef(name = "Total Air Yards",
                               style = list(fontFamily = "Roboto Condensed"),
                               maxWidth = 150),
                uncatchable_pct = colDef(name = "Prayer Yards %", 
                                         format = colFormat(percent = TRUE, digits = 1),
                                         style = list(fontFamily = "Roboto Condensed"),
                                         maxWidth = 150),
                percentile = colDef(name = "Percentile",
                                    style = list(fontFamily = "sans-serif",
                                                 fontWeight = "bold"),
                                    maxWidth = 125,
                                    html = TRUE,  
                                    cell = function(value) { 
                                      create_bar_plot(value) 
                                    })
              ),
              # Additional reactable options
              pagination = TRUE,
              defaultPageSize = 15,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(15, 25, 50, 100),
              sortable = TRUE,
              highlight = TRUE,
              compact = TRUE,
              striped = TRUE,
              borderless = TRUE,
              defaultSorted = c("total"),
              defaultSortOrder = "desc",
              theme = reactableTheme(
                borderColor = "black",
                headerStyle = list(textAlign = "right",
                                   fontSize = 12),
                tableBodyStyle = list(textAlign = "right",
                  fontSize = 14)
              )
    )
  })
}

shinyApp(ui, server)