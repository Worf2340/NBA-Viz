plot2Input = function(id, player_names) {
  
  ns = NS(id)
  
  tagList(
    helpText(
      "Compare players scoring over every season.",
      tags$br()
    ),
    
    h2("Variable Selection"),
    xAxisVariableSelectionInput(id),
    
    helpText("Compare each season individually unless 'Display career stats' is selected."),
    checkboxInput(ns("career_stats"), "Display career stats"),
    
    yAxisVariableSelectionInput(id),   
    
    h2("Player Selection"),
    playerSelectionInput(id, player_names),
    checkboxInput(ns("playoffs"), "Playoffs"),
    
  )
}

plot2Output = function(id) {
  ns = NS(id)
  
  tagList(
    helpText("Hover over points for information. Click legend entries to hide that player. 
                   Double click legend entries to hide everyone except that player."),
    plotlyOutput(ns("plot2"))
    
  )
}

plot2Server = function(id, nba_data, career_nba_data) { 
  moduleServer(id, function(input, output, session) {
    
    minGames = reactive({
      if (input$playoffs) {
        return (0)
      }
      40 
    })
    
    
    getData = reactive({
      if (input$career_stats) {
        data = career_nba_data
      }
      else {
        data = nba_data
      }
      

      if (length(input$name_filter) == 0) {
        return (data %>%
                  filter(isPlayoffs == input$playoffs, ptsTotals >= 100, countGames >= minGames()) %>% 
                  slice_max(n = 100, order_by = get(paste0(input$var_x, input$stat_type_x))))
      } 
    
      nba_data %>% filter(namePlayer %in% input$name_filter,
                                    isPlayoffs == input$playoffs)
        
    })

    output$plot2 = renderPlotly({
      ggplotly(
        generate_scatter_plot(
          getData(),
          input$var_x,
          input$var_y,
          input$stat_type_x,
          input$stat_type_y),
        tooltip = "text")
    })

  })
}