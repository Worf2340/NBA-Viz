xAxisVariableSelectionInput = function(id) {
  ns = NS(id)
  
  tagList(
    selectInput(
      ns("var_x"),
      label = "Choose a variable to display on the x-axis.",
      choices = list(
        "Points" = "pts",
        "Field Goals Made" = "fgm",
        "Field Goals Attempted" = "fga",
        "3 Pointers Made" = "fg3m",
        "3 Pointers Attempted" = "fg3a",
        "Free Throws Attempted" = "fta",
        "Free Throws Made" = "ftm"
      ),
      selected = "ptsPerGame",
      selectize = FALSE
    ),
    radioButtons(
      ns("stat_type_x"),
      "Stat Type",
      choices = list("Per Game" = "PerGame",
                     "Per 100 Possesions" = "PerPoss",
                     "Totals" = "Totals")
    ),
  )
 
}

yAxisVariableSelectionInput = function(id) {
  ns = NS(id)
  
  tagList(
    selectInput(
      ns("var_y"),
      label = "Choose a variable to display on the y-axis.",
      choices = list(
        "Field Goal %" = "pctFG",
        "3 point %" = "pctFG3",
        "Free Throw %" = "pctFT",
        "True Shooting %" = "pctTrueShooting",
        "Effective Field Goal %" = "pctEFG"
      ),
      selected = "pctTrueShooting",
      selectize = FALSE
    ) ,
    radioButtons(
      ns("stat_type_y"),
      label = "Stat Type",
      choices = list(
        "Regular" = "",
        "League Adjusted" = "Plus"
      ),
      selected = ""
    ),
  )

}

playerSelectionInput = function (id, player_names) {
  ns = NS(id)
  
  tagList(
    selectizeInput(
      ns("name_filter"),
      "Players",
      choices = player_names,
      multiple = TRUE,
      options = list(placeholder = "All Players")
    )
  )

}