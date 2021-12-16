library(nbastatR)
library(tidyverse)

get_stat_abbr = function() {
  stat_abbr =  c(
    "PPG",
    "FGM/G",
    "FGA/G",
    "3PM/G",
    "3PA/G",
    "FTM/G",
    "FTA/G",
    
    "PTS/100",
    "FGM/100",
    "FGA/100",
    "3PM/100",
    "3PA/100",
    "FTM/100",
    "FTA/100",
    
    "PTS",
    "FGM",
    "FGA",
    "3PM",
    "3PA",
    "FTM",
    "FTA",
    
    "FG%",
    "3P%",
    "FT%",
    "TS%",
    "EFG%",
    
    "FG+",
    "3P+",
    "FT+",
    "TS+",
    "EFG+"
  )  
  names(stat_abbr) = c(
    "ptsPerGame",
    "fgmPerGame",
    "fgaPerGame",
    "fg3mPerGame",
    "fg3aPerGame",
    "ftmPerGame",
    "ftaPerGame",
  
    "ptsPerPoss",
    "fgmPerPoss",
    "fgaPerPoss",
    "fg3mPerPoss",
    "fg3aPerPoss",
    "ftmPerPoss",
    "ftaPerPoss",
    
    "ptsTotals",
    "fgmTotals",
    "fgaTotals",
    "fg3mTotals",
    "fg3aTotals",
    "ftmTotals",
    "ftaTotals",
    
    "pctFG",
    "pctFG3",
    "pctFT",
    "pctTrueShooting",
    "pctEFG",
    
    "pctFGPlus",
    "pctFG3Plus",
    "pctFTPlus",
    "pctTrueShootingPlus",
    "pctEFGPlus"
  )
  
  return (stat_abbr)
}

get_nba_data = function() {
  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  
  historical_nba_data = readRDS("./data/historical_nba_data.rds")
  
  current_nba_data = bref_players_stats(
    seasons = 2022,
    tables = c("totals", "per_game", "advanced", "per_minute", "per_poss")
  )
  
  nba_data = bind_rows(historical_nba_data, current_nba_data) %>% 
    left_join(nba_players()) %>%
    mutate(pctFT = case_when(pctFT == 0.01 ~ 1, TRUE ~ pctFT))
  
  league_averages = get_league_averages(nba_data)
  
  # Calculate adjusted true shooting 
  nba_data = nba_data %>% inner_join(league_averages, by=c("yearSeason"), suffix = c("",".avg"))
  
  # Calculate adjusted true shooting stats
  # Divide by 100 for formatting on graph 
  nba_data$pctFGPlus = round(100 * (nba_data$pctFG / nba_data$pctFG.avg), 0)/100
  nba_data$pctFG3Plus = round(100 * (nba_data$pctFG3 / nba_data$pctFG3.avg), 0)/100
  nba_data$pctFTPlus = round(100 * (nba_data$pctFT / nba_data$pctFT.avg), 0)/100
  nba_data$pctEFGPlus = round(100 * (nba_data$pctEFG / nba_data$pctEFG.avg), 0)/100
  nba_data$pctTrueShootingPlus = round(100 * (nba_data$pctTrueShooting / nba_data$pctTrueShooting.avg), 0)/100
  
  nba_data$tsaTotals = nba_data$fgaTotals + 0.44 * nba_data$ftaTotals 
  
  career_nba_data = get_career_nba_data(nba_data)
  
  return (list("nba_data" = nba_data, 
               "league_averages" = league_averages,
               "career_nba_data" = career_nba_data))
  }

get_league_averages = function(nba_data){
  
  totals = nba_data %>% group_by(yearSeason) %>% 
    summarise_at(vars(ptsTotals,
                      fgmTotals,
                      fgaTotals,
                      ftmTotals,
                      ftaTotals,
                      fg3mTotals,
                      fg3aTotals), list(sum))
  
  league_averages = data.frame(
    yearSeason = totals$yearSeason,
    pctFG = totals$fgmTotals/totals$fgaTotals,
    pctFG3 = totals$fg3mTotals/totals$fg3aTotals,
    pctFT = totals$ftmTotals/totals$ftaTotals,
    pctTrueShooting = (totals$ptsTotals)/(2*(totals$fgaTotals + 0.44 * totals$ftaTotals)),
    pctEFG = (totals$fgmTotals + 0.5 * totals$fg3mTotals)/totals$fgaTotals,
    
    pctFGPlus = 1,
    pctFG3Plus = 1,
    pctFTPlus = 1,
    pctTrueShootingPlus = 1,
    pctEFGPlus = 1
  )
  
  return (league_averages)
}

get_career_nba_data = function(nba_data) {
  
  career_totals = nba_data %>% 
    group_by(namePlayer, isPlayoffs) %>%
    summarise(
      across(
        c(
          ptsTotals,
          fgaTotals,
          fgmTotals,
          fg3mTotals,
          fg3aTotals,
          ftaTotals,
          ftmTotals,
          tsaTotals,
          countGames
        ),
        sum
      )
    ) %>%
    ungroup()
  

  
  career_per_poss =  nba_data %>%
    drop_na(ptsPerPoss) %>%
    group_by(namePlayer, isPlayoffs) %>%
    summarise(
      across(
        c(
          ptsPerPoss,
          fgaPerPoss,
          fgmPerPoss,
          fg3mPerPoss,
          fg3aPerPoss,
          ftaPerPoss,
          ftmPerPoss
        ),
        ~ round(weighted.mean(., minutesPerPoss, na.rm = TRUE), 1)
      )
    ) %>%
    ungroup()
  
  career_stats = inner_join(career_totals, career_per_poss)
  
  career_stats$ptsPerGame = round(career_stats$ptsTotals/career_stats$countGames, 1)
  career_stats$fgaPerGame = round(career_stats$fgaTotals/career_stats$countGames, 1)
  career_stats$fgmPerGame = round(career_stats$fgmTotals/career_stats$countGames, 1)
  career_stats$fg3mPerGame = round(career_stats$fg3mTotals/career_stats$countGames, 1)
  career_stats$fg3aPerGame = round(career_stats$fg3aTotals/career_stats$countGames, 1)
  career_stats$ftaPerGame = round(career_stats$ftaTotals/career_stats$countGames, 1)
  career_stats$ftmPerGame = round(career_stats$ftmTotals/career_stats$countGames, 1)

  career_stats$pctFG = round(career_stats$fgmTotals/career_stats$fgaTotals, 3)
  career_stats$pctFG3 = round(career_stats$fg3mTotals/career_stats$fg3aTotals, 3)
  career_stats$pctFT = round(career_stats$ftmTotals/career_stats$ftaTotals, 3)
  career_stats$pctTrueShooting = round(career_stats$ptsTotals/(2*career_stats$tsaTotals), 3)
  career_stats$pctEFG = round((career_stats$fgmTotals + 0.5 * career_stats$fg3mTotals)/
    (career_stats$fgaTotals), 3)
  
  career_stats = career_stats %>%
    inner_join(
      nba_data %>%
        group_by(namePlayer, isPlayoffs) %>%
        summarise(across(
          c(
            pctFGPlus,
            pctFG3Plus,
            pctFTPlus,
            pctEFGPlus,
            pctTrueShootingPlus
          ),
          ~ round(weighted.mean(., tsaTotals), 2)
        ))
    ) %>%
    ungroup()
  
  career_stats$slugSeason = "Career"
  
  return (career_stats)
  
}

# get_adj_shooting_data(nba_data, league_averages) {
#   
# }

get_seasons_list = function() {
  
  seasons_list = list()
  #seasons_list[1] = "All"
  
  k = 2022
  for (i in 1:71) {
    seasons_list[[i]] = k
    k = k - 1
  }
  
  return (seasons_list)
}

generate_scatter_plot = function(data, var_x, var_y, stat_type_x, 
                                 stat_type_y, avg = NULL) {
  
  stat_abbr = get_stat_abbr() 
  
    plot1 = ggplot(data) +
      aes(
        x = get(paste0(var_x, stat_type_x)),
        y = get((paste0(var_y, stat_type_y)))*100,
        color = namePlayer,
        text = paste(
          "Player:",
          namePlayer,
          "<br>",
          "Season:",
          slugSeason,
          "<br>",
          stat_abbr[[paste0(var_x, stat_type_x)]],
          ":",
          get(paste0(var_x, stat_type_x)),
          "<br>",
          stat_abbr[[paste0(var_y, stat_type_y)]],
          ":",
          get(paste0(var_y, stat_type_y)) * 100,
          "<br>GP:",
          countGames
        )
      ) +
      geom_point() +
      theme(legend.title = element_blank())+
      {if (!(is.null(avg))) geom_hline(
        yintercept = avg*100,
        col = "steelblue",
        linetype = "dashed"
      )} +
      scale_x_discrete(labels = NULL, breaks = NULL) +
      labs(x = stat_abbr[[paste0(var_x, stat_type_x)]],
           y = stat_abbr[[paste0(var_y, stat_type_y)]])
    
}