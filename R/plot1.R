plot1Input = function(id, seasons_list, player_names) {
  
  ns = NS(id)
  
  tagList(
    helpText(
      "Displays the top 100 players by \"x-axis stat\" per season.",
    ),
    tabsetPanel(
      tabPanel(
        "Variable Selection",
        h2("Variable Selection"),
        
        xAxisVariableSelectionInput(id),
        yAxisVariableSelectionInput(id),    
        
        helpText("Season example: 2000 is the 1999-2000 season."),
        
        selectInput(
          ns("season"),
          "Season",
          choices = seasons_list,
          selected = "2022",
          selectize = FALSE
        ),
        
        checkboxInput(ns("playoffs"), "Playoffs"),
        
        h2("Player Filtering"),
        
        helpText("You can filter by either displaying only rookies or by
           selecting individual players. If both are selected, 'Show Rookies'
           has priority."),
        
        helpText(
          "Selecting players removes all players from the graph except those selected."
        ),
        
        playerSelectionInput(id, player_names),
        
        helpText("Note: The show rookies checkbox only works for rookies in the current NBA season."),
        checkboxInput(
          ns("rookies"), "Show only rookies", value = FALSE
        )
      ),
      tabPanel("Test", "Test")
    )
   
    
   
    
  )

}

plot1Output = function(id) {
  ns = NS(id)
  tagList(
    helpText("Hover over points for information. Click legend entries to hide that player. 
                   Double click legend entries to hide everyone except that player."),
    plotlyOutput(ns("plot1")),
    h4(textOutput(ns("league_average")))
  )
}


plot1Server = function(id, nba_data, league_averages) {
  moduleServer(id, function(input, output, session) {
    #Todo: figure out how reactives work and clean this up
    getSeasonData = reactive({
      nba_data %>% filter(yearSeason == (input$season[1]),
                          isPlayoffs == input$playoffs, minutesTotals >= 50)
    })
    
    getNameFilteredData = reactive({
      getSeasonData() %>% filter(namePlayer %in% (input$name_filter))
    })
    
    
    getRookieData = reactive({
      return (getSeasonData() %>% filter((isRookie) == TRUE))
    })
    
    getData = reactive({
      if ((input$rookies) == TRUE) return (getRookieData())
      if (length((input$name_filter)) == 0) {
        
        x =  (getSeasonData() %>%
                  slice_max(n = 100,
                            order_by = get(paste0(input$var_x, input$stat_type_x))))
        return(x)
      }
      getNameFilteredData()
    })
    
    getValidatedData = reactive({
      validate(need( !(input$season[1] < 1974 & input$stat_type_x == "PerPoss"), 
                     "Per 100 Possesion data only available from the 1973-74 season onward."))
      validate(need(nrow(getData()) != 0, "Players did not play in that season."))
      getData()
    })
    
    getLeagueAverage = reactive({
      (league_averages %>% filter(yearSeason == (input$season[1])) %>%
         select((paste0(input$var_y, input$stat_type_y))))[,1]
    })
    
    output$league_average = renderText({
      paste0("League Average ", get_stat_abbr()[[(paste0(input$var_y, input$stat_type_y))]], ": ",
             round(getLeagueAverage() * 100, 1))
    })
    
    output$plot1 = renderPlotly({
      ggplotly(
        generate_scatter_plot(
          getValidatedData(),
          input$var_x,
          input$var_y,
          input$stat_type_x,
          input$stat_type_y,
          getLeagueAverage()),
        tooltip = "text")
    })
    
  })
}
    
  