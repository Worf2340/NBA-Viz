library(shiny)
library(nbastatR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(reactable)
library(shinythemes)


# Load NBA data
nba_data_list = get_nba_data()

nba_data = nba_data_list$nba_data
league_averages = nba_data_list$league_averages
career_nba_data = nba_data_list$career_nba_data
head(career_nba_data)

# Load player list 
players = nba_players()
player_names = as.list(players)$namePlayer

# Load seasons list 
seasons_list = get_seasons_list()

# Load stat abbreviation list
get_stat_abbr = get_stat_abbr()


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"), 
  navbarPage(
    "NBA Data Visualization",
    tabPanel(
      "Scoring Efficiency (Compare Seasons)",
      sidebarLayout(
        sidebarPanel(style = "overflow-y:scroll",
          plot1Input("plot1", seasons_list, player_names)),
        mainPanel(plot1Output("plot1"))
        )
      ),
    tabPanel(
      "Scoing Efficiency (Compare Players)",
      sidebarLayout(
        sidebarPanel(plot2Input("plot2", player_names)),
        mainPanel(plot2Output("plot2"))
        )
      )
    )
  )

server = function(input, output, session){
  plot1Server("plot1", nba_data, league_averages)
  plot2Server("plot2", nba_data, career_nba_data)
}
# Run the application
shinyApp(ui = ui, server = server)
