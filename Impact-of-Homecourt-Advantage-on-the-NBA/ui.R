library(shiny)
library(ggplot2)
library(plotly)
labels <- c('Home_Vs_Away_Point_Differential' = 'Home/Away Point Differential',
            'Elevation' = 'Elevation',
            "Avg_Distance_Traveled_Against" = "Opponent's Avg Distance Traveled",
            'Attendance_Pct' = 'Attendance %')
ui <- fluidPage(
  tabsetPanel(tabPanel("Homecourt Advantage by Team", plotOutput("ggplot")), 
              tabPanel("Factors that Influence Homecourt Advantage",
                       selectInput("factors", 'select a factor', c(
                         'Home/Away Point Differential' = 'Home_Vs_Away_Point_Differential',
                         'Elevation' = 'Elevation',
                         "Opponent's Avg Distance Traveled" = "Avg_Distance_Traveled_Against",
                         'Attendance %' = 'Attendance_Pct')
                         ),
                       plotlyOutput("lineplot1")))
  )

server <- function(input, output) {
  output$ggplot <- renderPlot({
    p <- ggplot(final_data, aes(x=Win_Pct_Differential, y=reorder(Team, Win_Pct_Differential), fill = Team)) +
      geom_bar(stat='identity') +
      scale_fill_manual(values = league_pal("nba")) +
      theme(legend.position = 'none') +
      labs(title = "Home Court Impact", x = "Win % Differential Home vs Away", y = "Teams")
    p
  })
  output$lineplot1 <- renderPlotly({
    p <- ggplot(final_data, aes(x = .data[[input$factors]], y = Win_Pct_Differential)) + 
      geom_point(aes(color = Team)) +
      geom_smooth(method='lm') + 
      scale_fill_manual(values = league_pal("nba")) +
      ggtitle("Win % Differential vs Home/Away Point Differential") +
      xlab(labels[input$factors]) + ylab("Win % Differential") +
      theme(legend.position = "none")
    p = ggplotly(p)
    p
  })
}

shinyApp(ui = ui, server = server, options = list(height = 1080))

