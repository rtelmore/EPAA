#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(cowplot)

df <- readRDS("all-pts-above-avg-13-21.rds")
#df <- readRDS("app/NBA-apaa/all-pts-above-avg-18-21.rds")
df_summary <- readRDS("summary-shots-for-app.rds")
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "NBA Shots Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Density Plot", tabName = "density_plot", icon = icon("image")),
      menuItem("Table", tabName = "stats_table", icon = icon("table")),
      menuItem("Data", tabName = "download", icon = icon("download")),
      menuItem("Shots Summary", tabName = "shots_summary_plot", icon = icon("image")),
      menuItem("EPAA over Time", tabName = "epaa_time", icon = icon("image")),
      menuItem("About", tabName = "about", icon = icon("circle-info"))
    )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "density_plot",
              fluidRow(
                box(
                  title = "Select Players and Season",
                  selectInput("selector_season", h4("Season"),
                              choices = c(2013:2021), 
                              selected = "2021", width = '190px'),
                  selectInput("selector_p1", h4("Player One"),
                              choices = sort(unique(df$player)),
                              selected = "LeBron James",
                              width = '190px'),
                  selectInput("selector_p2", h4("Player Two"),
                              choices = sort(unique(df$player)),
                              selected = "James Harden",
                              width = '190px'),
                  selectInput("selector_p3", h4("Player Three"),
                              choices = sort(unique(df$player)),
                              selected = "Damian Lillard",
                              width = '190px'),
                  selectInput("selector_p4", h4("Player Four"),
                              choices = sort(unique(df$player)),
                              selected = "Russell Westbrook",
                              width = '190px'),
                  width = '3'
                ),
                box(plotOutput("density"), width = '9'),
              )
      ),
      
      tabItem(tabName = "stats_table",
              h2("Summarized Points Above Average Table"),
              dataTableOutput("table")
      ),
      tabItem(tabName = "download",
              h2("Raw Data"),
              "Note: The raw data file has all of the posterior draws from the points
              above average data. The file is big, so proceed with caution.",
              br(),
              downloadButton('download',"Download the data")
      ),
      
      tabItem(tabName = "shots_summary_plot",
              fluidRow(
                box(
                  title = "Select Team",
                  selectInput("selector_team", h4("Team"),
                              choices = sort(unique(df_summary$team)), 
                              selected = "Denver Nuggets", width = '190px'),
                  width = '3'
                ),
                box(plotOutput("shots_summary_plots"), width = '9'),
              )
      ),
      
      tabItem(tabName = "epaa_time",
              fluidRow(
                box(
                  title = "Select Players",
                  selectInput("selector_p5", h4("Player One"),
                              choices = sort(unique(df$player)),
                              selected = "LeBron James",
                              width = '190px'),
                  selectInput("selector_p6", h4("Player Two"),
                              choices = sort(unique(df$player)),
                              selected = "James Harden",
                              width = '190px'),
                  selectInput("selector_p7", h4("Player Three"),
                              choices = sort(unique(df$player)),
                              selected = "Damian Lillard",
                              width = '190px'),
                  selectInput("selector_p8", h4("Player Four"),
                              choices = sort(unique(df$player)),
                              selected = "Russell Westbrook",
                              width = '190px'),
                  width = '3'
                ),
                box(plotOutput("epaa_time"), width = '9'),
              )
      ),
      
      tabItem(tabName = "about",
              h2("About this App"),
              "This app is an online supplement for Williams, Schliep, Fosdick, &
              Elmore (2023). The app contains our points above average 
              summaries (densities and table) and raw data. In addition, we 
              provide the raw data for each team's shots and make probabilities 
              by region. "
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  players <- reactive({
    df |> dplyr::filter(season == input$selector_season)
  })
  
  # observeEvent(players(), {
  #   choices <- sort(unique(players()$player))
  #   updateSelectInput(inputId = "selector_p1", choices = choices, selected = "LeBron James")
  #   updateSelectInput(inputId = "selector_p2", choices = choices, selected = "James Harden")
  #   updateSelectInput(inputId = "selector_p3", choices = choices, selected = "Damian Lillard")
  #   updateSelectInput(inputId = "selector_p4", choices = choices, selected = "Russell Westbrook")
  # })  
  
  observeEvent(players(), {
    choices <- sort(unique(players()$player))
    updateSelectInput(inputId = "selector_p1", choices = choices, selected = "LeBron James")
    updateSelectInput(inputId = "selector_p2", choices = choices, selected = "James Harden")
    updateSelectInput(inputId = "selector_p3", choices = choices, selected = "Damian Lillard")
    updateSelectInput(inputId = "selector_p4", choices = choices, selected = "Russell Westbrook")
  })  
  
  # games <- reactive({
  #   ifelse(input$selector_season == 2021, 72, 82)
  # })
  
  df_input <- reactive({
    df |> 
      dplyr::filter(season == input$selector_season,
                    player %in% c(input$selector_p1, input$selector_p2, 
                                  input$selector_p3, input$selector_p4)) |> 
      dplyr::mutate(points = player_points - team_points,
                    sample = rep(1:10000, times = 4)) |> 
      dplyr::filter(sample >= 3000)
  })
  
  df_sum_input <- reactive({
    df_input() |> 
      dplyr::group_by(player) |> 
      dplyr::summarize(m = mean(points)/82)  
  })
  
  df_total_input <- reactive({
    df |> 
      dplyr::mutate(points = player_points - team_points) |> 
      dplyr::group_by(player, season) |>
      dplyr::summarize(mean_paa = mean(points)/82,
                       st_dev = sd(points)/82) |>
      dplyr::arrange(desc(mean_paa)) |>
      dplyr::ungroup()
  })
  
  df_summary_input <- reactive({
    df_summary |>
      dplyr::filter(type %in% c("Shots", "Percentage"),
                    team == input$selector_team)
  })
  
  df_rank_input <- reactive({
    df |> 
      dplyr::mutate(points = player_points - team_points,
                    sample = rep(1:10000, times = 895)) |> 
      dplyr::filter(sample >= 3001) |> 
      dplyr::group_by(player, season) |> 
      dplyr::summarize(avg = mean(points), med = median(points)) |> 
      dplyr::ungroup() |> 
      dplyr::group_by(season) |> 
      dplyr::mutate(rank_mean = rank(-avg), rank_med = rank(-med)) |> 
      dplyr::filter(player %in% c(input$selector_p5, input$selector_p6, 
                                  input$selector_p7, input$selector_p8))
    
  })
  
  output$table <- renderDataTable({
    datatable(df_total_input(), rownames = F) |> 
      formatRound(c(3:4), 3)
  })
  
  output$density <- renderPlot({
    p <- ggplot(data = df_input(),
                aes(x = points/82, fill = player, group = player))
    p + geom_density(alpha = .35) + 
      geom_vline(data = df_sum_input(), aes(xintercept = m), linetype = "dotted") + 
      scale_fill_brewer("Players", palette = "Set1") +
      labs(x = "points above average per game") +
      theme_bw() +
      guides(col = "none")
  })
  
  output$epaa_time <- renderPlot({
    p_ranks <- ggplot(data = df_rank_input(),
                      aes(x = season, y = rank_med, color = player))
    p_ranks + geom_point() +
      geom_line() +
      labs(x = "Season", y = "Rank of Median EPAA per Game") +
      scale_color_brewer("", palette = "Paired") +
      scale_x_continuous(breaks = 2013:2021, minor_breaks = NULL) +
      scale_y_reverse(breaks = seq(0, 100, by = 5)) +
      #  geom_label() +
      theme_bw()
  })
  
  output$shots_summary_plots <- renderPlot({
    p1 <- ggplot(data = df_summary_input() |> 
                   dplyr::filter(type %in% c("Shots")),
                 aes(x = season, y = value, col = name, group = name)) + 
      geom_point() +
      geom_line() +
      theme_bw() +
      scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
      labs(x = "Season",
           y = "Number of Shots Taken",
           col = "Region:") +
      scale_color_brewer(palette = "Paired")
    
    p2 <- ggplot(data = df_summary_input() |> 
                   dplyr::filter(type %in% c("Percentage")),
                 aes(x = season, y = value, col = name, group = name)) + 
      geom_point() +
      geom_line() +
      theme_bw() +
      scale_y_continuous(labels = scales::percent_format(accuracy=1), 
                         breaks = seq(.3, .9, by = .1),
                         limits = c(.25, .85)) +
      labs(x = "Season",
           y = "Percentage of Shots Made",
           col = "region") +
      scale_color_brewer(palette = "Paired") 
    
    prow <- cowplot::plot_grid(
      p1 + theme(legend.position="none"),
      p2 + theme(legend.position="none"),
      align = 'vh',
      nrow = 2
    )
    
    legend <- cowplot::get_legend(
      p1 +
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom", 
              legend.box.margin = margin(0, 0, 0, 0))
    )
    
    cowplot::plot_grid(prow, legend, nrow = 2, rel_heights = c(1, .1))
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("nba-paa", ".csv", sep="")
    },
    content = function(file) {
      write.csv(df, file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)