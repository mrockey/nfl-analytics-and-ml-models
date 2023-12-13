# Load necessary libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)

# Read the CSV file
nfl_data <- read_csv("WL_Spread_Combined.csv")

# Select the required columns
selected_data <- nfl_data %>% 
  select(Week, Home_Team, Away_Team, win_or_loss, Prediction, home_line_open, spread_actual, Predicted_Win_Prob,
         Home_Team_Lost_Spread, Home_Team_Lose_Spread_probability, Home_Team_Beat_Spread_probability)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("2022 NFL Game Predictions"),
  mainPanel(
    tabsetPanel(
      tabPanel("Visualizations",
               sidebarLayout(
                 sidebarPanel(
                   # Add a select input for the week
                   selectInput("selected_week", "Select Week:",
                               choices = unique(selected_data$Week),
                               selected = 3)
                 ),
                 mainPanel(
                   plotOutput("nfl_plot")
                 )
               )),
      tabPanel("Beat Line Tables",
                 mainPanel(
                   DTOutput("nfl_table_beat")
                 )
               ),
      tabPanel("Loss Line Tables",
                 mainPanel(
                   DTOutput("nfl_table_loss")
                 )
               ),
      tabPanel("Win or Loss Tables",
                 mainPanel(
                   DTOutput("nfl_table")
                 )
               )
    )
  )
)


# Define the server logic
server <- function(input, output) {
  output$nfl_plot <- renderPlot({
    # Filter data for the selected week
    filtered_data <- selected_data %>% 
      filter(Week == input$selected_week)

    # Plot the filtered data
    ggplot(filtered_data, aes(x = Home_Team, y = home_line_open, color = Prediction,
                              size = Home_Team_Beat_Spread_probability)) +
      geom_point() +
      #geom_text(aes(label = Home_Team_Lost_Spread), size = 4, hjust = -0.7, vjust = -0.7) +
      labs(title = paste0("NFL Week ", input$selected_week, " Predictions"),
           x = "Home Team",
           y = "Home Line Open") +
      theme_minimal() +
      scale_size_continuous(range = c(3, 10), guide = guide_legend(title = "Probability")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  })
  output$nfl_table_beat <- renderDT({
    # Filter data based on the selected win/loss probability range
    filtered_data <- selected_data %>%
      select(Week, Home_Team, Away_Team, home_line_open, spread_actual,
         Home_Team_Lost_Spread, Home_Team_Beat_Spread_probability)

    # Render the filtered data as a DataTable
    datatable(filtered_data, options = list(pageLength = 25))
  })
  output$nfl_table_loss <- renderDT({
    # Filter data based on the selected win/loss probability range
    filtered_data <- selected_data %>%
      select(Week, Home_Team, Away_Team, home_line_open, spread_actual,
         Home_Team_Lost_Spread, Home_Team_Lose_Spread_probability)
      
    # Render the filtered data as a DataTable
    datatable(filtered_data, options = list(pageLength = 25))
  })
  output$nfl_table <- renderDT({
    # Filter data based on the selected win/loss probability range
    filtered_data <- selected_data %>%
      select(Week, Home_Team, Away_Team, win_or_loss, Prediction, Predicted_Win_Prob)

    # Render the filtered data as a DataTable
    datatable(filtered_data, options = list(pageLength = 25))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
