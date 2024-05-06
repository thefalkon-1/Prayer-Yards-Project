library(shiny)
library(reactable)
library(dplyr)
library(scales)
library(reactablefmtr)
library(MetBrewer)

# Read in dataset
new_df <- readRDS(gzcon(url("https://github.com/thefalkon-1/Prayer-Yards-Project/raw/main/data/2023_regular_season.rds")))

max_week <- max(new_df$week)

with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
            title = tooltip, value)
}

create_bar_plot <- function(value, width = 100, height = 20) {
  # Check if the value is NA or non-numeric
  if (is.na(value) || !is.numeric(value)) {
    return("<svg width='100' height='20'></svg>")  # Return an empty plot for NA or non-numeric values
  }
  
  # Create a color gradient function
  color_gradient <- scales::col_numeric(
    palette = c("#133e7e", "#225bb2", "#447fdd", "#7db0ea", "#bad6f9", "#D0D0D0", "#fbc2a9", "#ee956a",
                "#da6c42", "#973d21", "#6b200c"), 
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
  
  text_dy <- "0.3em" 
  
  # Create an inline SVG for the bar plot
  svg_plot <- sprintf("<svg width='%f' height='%f' viewBox='0 0 %f %f' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>
                        <defs>
                            <filter id='shadow' x='-10%%' y='-10%%' width='140%%' height='140%%'>
                                <feDropShadow dx='0' dy='0' stdDeviation='0.5' flood-color='#444444'/>
                            </filter>
                        </defs>
                        <rect x='0' y='%f' width='%f' height='%f' fill='lightgrey'/>
                        <rect x='0' y='0' width='%f' height='%f' fill='%s'/>
                        <circle cx='%f' cy='%f' r='%f' fill='%s' stroke='white' stroke-width='1.25'/>
                        <text x='%f' y='%f' font-size='%f' fill='white' font-weight='bold' text-anchor='middle' dominant-baseline='middle' filter='url(#shadow)'>%.0f</text>
                      </svg>", 
                      width, height, width, height,
                      gray_bar_y_position, width, gray_bar_height,
                      bar_width, height, color,
                      circle_center_x, height / 2, circle_radius, color,
                      circle_center_x, ((height / 2) + 1), text_font_size, value)
  return(svg_plot)
  
  
}

ui <- navbarPage(
  tags$body(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed');
      body {
        font-family: 'Roboto Condensed';
      }"))
  ),
  title = "NFL Air Yards Leaderboard",
  tabPanel("Leaderboard",
           fluidRow(
             column(2, 
                    sliderInput("weekInput", "Select Week:", 
                                min = 1, max = max_week, value = c(1, max_week), step = 1),
                    numericInput("minTargets", "Minimum Targets:", 
                                 min = 1, max = 2000, value = 10, step = 10),
                    numericInput("minAirYards", "Minimum Total Air Yards:", 
                                 min = 0, max = 2000, value = 250, step = 10),
                    checkboxGroupInput("positionToggle", "Position",
                                       choices = c("WR", "TE", "RB"),
                                       selected = c("WR", "TE")),
                    selectizeInput(
                      inputId = "teamInput",
                      label = "Team",
                      choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", 
                                  "DAL", "DEN", "DET", "GB",  "HOU", "IND", "JAX", "KC",  "LA",  "LAC",
                                  "LV",  "MIA", "MIN", "NE",  "NO",  "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                  "SF",  "TB",  "TEN", "WAS"),
                      multiple = TRUE
                    )
             ),
             column(10,
                    div(style = "padding: 10px 0px;", 
                        "Prayer Yards are the amount of yards a ball traveled in the air (air yards) on passes deemed uncatchable | Data: FTN Data via nflverse | ", 
                        a(href = "https://twitter.com/thefalkon", target = "_blank", "@thefalkon")),
                    reactableOutput("table"),
                    div(style = "padding: 10px 0px; display: flex; align-items: center;", 
                        p("Player's relative ranking in terms of catchable air yards percentage among qualified pass catchers, where a lower percentile signifies a higher prayer yard percentage. " 
                        )
                    )
                    
             )
           )
  ),
  tabPanel("Why Prayer Yards?",
           div(class = "text-container", style = "padding: 10px; width: 70%; margin-left: auto; margin-right: auto;",
               h3("Why should we care about air yards and prayer yards?"),
               p("Targets and receiving yards can describe so much about an NFL receiver’s performance."),
               p("Consider two wideouts who both finished a week with 12 targets each. Their receiving totals⁠—89 and 56 yards, respectively⁠—begin to tell a story but don’t illustrate the full picture. Where was a player targeted on the football field? How much of a receiver’s yardage came from the throw itself compared to yards after the catch? Were the deep shots where the receiver didn’t come down with the ball meaningful?"),
               p("Enter air yards, first introduced to the fantasy community by Josh Hermsmeyer in 2016. Air yards are the amount of yards a pass travels in the air, and the metric can help identify how a receiver is being utilized. Ratchet up a receiver’s average depth of target (aDOT) and widen the range of outcomes: more deep shots but decrease the likelihood of those targets being caught. Tone down a receiver’s aDOT and generate more consistency: more short and intermediate looks and increase the chances the receiver can make good on those targets."),
               p("Air yards can be further split into “prayer yards.” Prayer yards are just what they sound like⁠—air yards on passes that are deemed uncatchable. In other words, targets that the receiver doesn’t have a prayer of catching. Prayer yards add more detail than air yards alone, but it doesn't tell the full story. The metric can say whether a ball was catchable or not, but it doesn't immediately point out if the reason was an overthrow, a tipped ball, a poor route by the wide receiver or something else. Not all prayer yards are bad, either, as uncatchable deep balls could turn into splash plays over time."),
               p("During the NFL season, ", 
                 a("I write a weekly air yards breakdown", href = "https://www.si.com/author/dan-falkenheim", target = "_blank"),
                 " that digs into both air yards and prayer yards and looks at the film to understand what happened on the field. I hope you find this helpful!"
               )
           ))
)

server <- function(input, output) {
  filtered_data <- reactive({
    # Filter and summarize data here
    week_filtered_data <- new_df %>%
      filter(position_receiver %in% input$positionToggle) %>%
      filter(week >= input$weekInput[1], week <= input$weekInput[2])
    
    
    re_summed_data <- week_filtered_data %>%
      group_by(full_name_receiver, receiver_player_id, posteam, team_logo_espn) %>%
      summarise(
        targets = n(),
        receptions = sum(complete_pass, na.rm = T),
        ay_catchable = sum(ifelse(is_catchable_ball == "TRUE", air_yards, 0), na.rm = TRUE),
        ay_uncatchable = sum(ifelse(is_catchable_ball == "FALSE", air_yards, 0), na.rm = TRUE),
        total = ay_catchable + ay_uncatchable,
        aDOT = total / targets
      ) %>%
      filter(total >= input$minAirYards & targets >= input$minTargets) %>%
      mutate(catchable_pct = case_when(ay_catchable < 0 & ay_uncatchable < 0 ~ 0,
                                       ay_catchable < 0 & ay_uncatchable >= 0 ~ 0,
                                       ay_catchable >= 0 & ay_uncatchable < 0 ~ 1,
                                       .default = ay_catchable / total)) %>%
      ungroup() %>%
      select(-receiver_player_id) %>%
      arrange(-total) %>%
      filter(total > 0) %>%
      mutate(percentile = (rank(catchable_pct, ties.method = "first") / n() * 100))
    
    if (length(input$teamInput) >= 1) {
      re_summed_data <- re_summed_data %>%
        filter(posteam %in% input$teamInput)
    }
    
    
    re_summed_data
  })
  
  output$table <- renderReactable({
    reactable(filtered_data(),
              # Specify columns and formatting here
              columns = list(team_logo_espn = colDef(name = "Team", 
                                                     maxWidth = 100,
                                                     cell = function(value, index) {
                                                       image_url <- filtered_data()$team_logo_espn[index]
                                                       team_name <- paste0("  ", filtered_data()$posteam[index])
                                                       image <- img(src = image_url, style = "height: 25px; width: 25px; vertical-align: middle;")
                                                       text_span <- span(team_name, style = "vertical-align: middle; display: inline-block;")
                                                       tagList(
                                                         div(image, " ", text_span)
                                                       )}),
                             full_name_receiver = colDef(name = "Player",
                                                         minWidth = 160,
                                                         sticky = "left",
                                                         align = "left",
                                                         style = list(textAlign = "left")),
                             posteam = colDef(name = "Team",
                                              maxWidth = 50,
                                              show = FALSE),
                             ay_catchable = colDef(name = "Catchable Air Yards",
                                                   maxWidth = 130),
                             ay_uncatchable = colDef(name = "Prayer Yards",
                                                     maxWidth = 100),
                             total = colDef(name = "Total Air Yards",
                                            maxWidth = 100),
                             targets = colDef(name = "Targets",
                                              maxWidth = 100),
                             receptions = colDef(name = "Receptions",
                                                 maxWidth = 100),
                             aDOT = colDef(header = with_tooltip("aDOT", 
                                                                 "Average Depth of Target (Yards)"),
                                           #format = colFormat(digits = 1),
                                           cell = data_bars(
                                             data = filtered_data(),
                                             text_position = "center",
                                             #box_shadow = TRUE,
                                             #round_edges = TRUE,
                                             fill_color = rev(MetBrewer::met.brewer('OKeeffe1')),
                                             number_fmt = scales::comma_format(accuracy = 0.1),
                                             bias = 1.9,
                                             max_value = 22,
                                             bar_height = 20,
                                             border_style = "solid",
                                             border_color = "#000000",
                                             border_width = "0.1mm"
                                             #bar_height = 4
                                           ),
                                           minWidth = 100,
                                           maxWidth = 200
                             ),
                             catchable_pct = colDef(header = with_tooltip("Catchable %", 
                                                                          "Percentage of air yards that are catchable"),
                                                    format = colFormat(percent = TRUE, digits = 1),
                                                    style = list(borderLeft = "1px solid #808080"),
                                                    maxWidth = 100),
                             percentile = colDef(header = with_tooltip("Percentile", 
                                                                       "Player's relative ranking in terms of catchable air yards percentage among qualified pass catchers, where a lower percentile signifies a higher prayer yard percentage."),
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
              defaultColDef = colDef(style = list(fontFamily = "Roboto Condensed",
                                                  textAlign = "center"),
                                     align = "center",
                                     headerVAlign = "bottom"),
              defaultSorted = c("total"),
              defaultSortOrder = "desc",
              theme = reactableTheme(
                borderColor = "black",
                headerStyle = list(fontSize = 12),
                tableBodyStyle = list(fontSize = 14),
                cellStyle = list(textAlign = "center",
                                 paddingTop = 0,
                                 paddingBottom = 0),
                rowStyle = list(marginBottom = -4,
                                paddingTop = 0,
                                paddingBottom = 0))
    )
  })
}

shinyApp(ui, server)