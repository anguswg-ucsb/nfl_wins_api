library(httr)
library(dplyr)
library(jsonlite)
library(logger)
install.packages("cleaner")
source("R/utils.R")
# Hit API and return predictions 
predict_games <- function(year, week, base_url = "http://68.183.25.9:8000/predict-new-data?") {
  
  # Construct API URL 
  url <- paste0(base_url, "year=", year, "&pred_week=", week)

  logger::log_info("\n\nSending request to nflwinpredictor API...\nRequest URL:\n{url}")
  
  # Post request to API and format responce into dataframe
  win_api <- 
    url %>% 
    httr::POST() %>% 
    httr::content(as = "text")
  
  if (grepl("Internal server error", win_api) == TRUE) {
    
    logger::log_error("\n\n{api_content}\n\nPotentially invalid inputs --> (i.e. year > current season, week > upcoming week):\nyear = {year}\nweek = {week}")
    
    return(NULL)
  }
  
  # convert JSON to tibble, arrange by highest probability of winning
  win_api <-
    win_api %>% 
    jsonlite::fromJSON() %>%
    dplyr::tibble() %>% 
    dplyr::arrange(-.pred_1)
  
  return(win_api)
    
  }

pred1 <- predict_games(
  year = 2022, 
  week = 7
)

# **************************************************************************
# **************************************************************************
# current_date <- "2020-01-05"
year = 2020
pred_week = 1
scrape_games <- function(year = NULL, pred_week = NULL) {
  
  if(is.null(pred_week)) {
    pred_week = 1
  }
  
  current_date <- Sys.Date()
  
  if(is.null(year)) {
    # new_date <- Sys.Date()
    # new_date <- "2023-01-17"
    
    # Current year
    year  <- as.numeric(substr(current_date, 1, 4))
    
    # Current month
    month <- as.numeric(substr(current_date, 6, 7))
    
    # If month is in part of season after Jan 1
    if (month %in% c(1, 2, 3, 4)) {
      
      year <- year - 1
      
    }
    
    logger::log_info("No year entered, defaulting to current season - {year}")
    
    # return(year)
  }
  
  # If the year is greater than the current season, make year = current_year
  if(year > as.numeric(substr(current_date, 1, 4))) {
    
    # current year
    year  <- as.numeric(substr(current_date, 1, 4))
    
    # current month
    month <- as.numeric(substr(current_date, 6, 7))
    
    # If month is in part of season after Jan 1
    if (month %in% c(1, 2, 3, 4)) {
      
      year <- year - 1
      
    }
    logger::log_info("Year entered was greater than current season, defaulting to current season - {year}")
    
    # return(year)
    
  # if year is before 2000, set year = 2000
  } else if(year < 2000) {
    
    year = 2000
    logger::log_info("Year entered was less than 2000 season, defaulting to {year} season")
  }
  
  logger::log_info("Return 3")
  
  # return(year)
  
  # Check and make sure pred_week is a valid week of season
  if(pred_week < 1) {
    
    # setting week to 2 if pred_week is less than 2
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - upcoming week\nSetting pred_week = 2")
    
    pred_week = 1
    
  } else if(year >= 2021 & pred_week > 18) {
    
    # setting week to max week after 2020 season (18)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - 18\nSetting pred_week = 18")
    
    pred_week = 18
    
  } else if(year < 2021 & pred_week > 17) {
    
    # setting week to max week before 2021 season (17)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - 17\nSetting pred_week = 17")
    
    pred_week = 17
    
  }
  
  # if prediction for week 1 is desired, use last seasons data
  if(pred_week == 1) {
    
    # Prior season to get metrics leading into week 1 of year
    past_year <- year - 1
    
    # Construct URL
    url  <- paste0("https://www.pro-football-reference.com/years/", past_year ,"/games.htm")
    
    # Read HTML page using URL
    page <- rvest::read_html(url)
    
    # Extract HTML nodes for table
    page_nodes <- 
      page %>%  
      rvest::html_nodes("table")
    
    # Extract season games 
    page_table <- rvest::html_table(
      page_nodes[[1]],
      header  = T,
      fill    = T, 
      convert = F
    ) %>% 
      janitor::clean_names() %>% 
      dplyr::mutate(
        home = case_when(
          x == ""  ~ 1,
          x == "@" ~ 0,
          x == "N" ~ 0
        )
      ) 
    
    # remove headers for each week
    page_table <- page_table[!grepl("Week", page_table$week), ]
    
    # remove headers break for playoff start 
    page_table <- page_table[!grepl("Playoffs", page_table$date), ]
    
    # Rename playoff columns as weeks, accounting for added game after 2020 season
    if (past_year >= 2021) {
      
      # If season is after 2020
      page_table <- 
        page_table %>% 
        dplyr::mutate(
          week = dplyr::case_when(
            week == "WildCard"  ~ "19",
            week == "Division"  ~ "20",
            week == "ConfChamp" ~ "21",
            week == "SuperBowl" ~ "22",
            TRUE                ~ week
          ),
          week     = as.numeric(week)
        ) %>% 
        dplyr::filter(week <= 18)
      
      # Rename playoff columns as weeks, accounting for fewer games before 2021
    } else {
      
      # if season is before 2021
      page_table <- 
        page_table %>% 
        dplyr::mutate(
          week = dplyr::case_when(
            week == "WildCard"  ~ "18",
            week == "Division"  ~ "19",
            week == "ConfChamp" ~ "20",
            week == "SuperBowl" ~ "21",
            TRUE                ~ week
          ),
          week     = as.numeric(week)
        ) %>% 
        dplyr::filter(week <= 17)
     
    }
    
    # parse data tables from Pro Football Reference
    outcomes <- 
      page_table %>% 
      dplyr::left_join(
        dplyr::select(nfl_teams(), team_name, win_team_abb = team_abb),
        by = c("winner_tie" = "team_name")
      ) %>% 
      dplyr::left_join(
        dplyr::select(nfl_teams(), team_name, lose_team_abb = team_abb),
        by = c("loser_tie" = "team_name")
      ) %>% 
      dplyr::select(week, date,
                    win_team  = win_team_abb,
                    x, 
                    lose_team = lose_team_abb,
                    pts_win   = pts,
                    pts_lose  = pts_2, 
                    tow, tol)  %>% 
      dplyr::mutate(
        home_team = dplyr::case_when(
          x == ""  ~ win_team,
          x == "@" ~ lose_team,
          week == 22 ~ win_team
        ),
        away_team = dplyr::case_when(
          x == ""  ~ lose_team,
          x == "@" ~ win_team,
          week == 22 ~ lose_team
        ),
        pts_win  = as.numeric(pts_win),
        pts_lose = as.numeric(pts_lose),
        game_id  = dplyr::case_when(
          week < 10 ~ paste0(past_year, "_0", week, "_", away_team, "_",  home_team),
          week >= 10 ~ paste0(past_year, "_", week, "_", away_team, "_",  home_team)
        )
      ) %>% 
      dplyr::select(-x) %>% 
      dplyr::group_by(game_id) %>% 
      dplyr::mutate(
        home_pts = dplyr::case_when(
          home_team == win_team ~ max(pts_win, pts_lose),
          home_team != win_team ~ min(pts_win, pts_lose)
        ),
        away_pts = dplyr::case_when(
          home_team == win_team ~ min(pts_win, pts_lose),
          home_team != win_team ~ max(pts_win, pts_lose)
        ),
        home_turnovers = dplyr::case_when(
          home_team == win_team ~ tow,
          home_team != win_team ~ tol
        ),
        away_turnovers = dplyr::case_when(
          home_team == win_team ~ tol,
          home_team != win_team ~ tow
        ),
        season = year
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(season, week, game_id, gameday = date, home_team, away_team,
                    home_score = home_pts, away_score = away_pts,
                    home_turnovers, away_turnovers) 
    
    # calculate win loss percentages
    record <- get_win_pct(outcomes, verbose = FALSE)
    
    # Create Score differential, home or away team ID, # of rest days
    outcomes <- 
      outcomes %>% 
      tidyr::pivot_longer(
        cols      = c(home_team, away_team),
        names_to  = "home",
        values_to = "team"
      ) %>% 
      dplyr::mutate(
        home = dplyr::case_when(
          home == "home_team" ~ 1, 
          home == "away_team" ~ 0
        ),
        score_diff = dplyr::case_when(
          home == 1 ~ home_score - away_score, 
          home == 0 ~ away_score - home_score
        ),
        home_away  = case_when(
          home == 1 ~ "home_team",
          home == 0 ~ "away_team"
        )
      ) %>% 
      dplyr::mutate(
        split_game_id = substr(game_id, 9, 20)
      ) %>% 
      dplyr::mutate(
        home_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,2],
        away_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,1],
        opponent      =  dplyr::case_when(
          home_team == team ~ away_team,
          away_team == team ~ home_team
        )
      ) %>% 
      dplyr::select(-split_game_id, -home_team, -away_team) %>% 
      dplyr::group_by(team) %>% 
      dplyr::arrange(gameday, .by_group = T) %>% 
      dplyr::mutate(
        lag_date    = lag(gameday), 
        rest_days   = as.numeric(round(difftime(gameday, lag_date), 0))
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-lag_date, -gameday, -home) %>% 
      replace(is.na(.), 7) %>% 
      dplyr::relocate(season, week, game_id, team, opponent, home_away, rest_days, score_diff, home_score, away_score) %>% 
      dplyr::left_join(
        dplyr::select(record, week, game_id, team, win, win_pct, home_win_pct, away_win_pct),
        by = c("week", "game_id", "team")
      )
    
    # Calculate ELO ratings
    elo <-  
      outcomes %>% 
      dplyr::filter(home_away == "home_team") %>%
      dplyr::select(season, week, game_id, team, opponent, win, home_score, away_score) %>% 
      get_nfl_elo()
    
    # Join ELO ratings back w/ outcomes
    outcomes <- 
      outcomes %>% 
      dplyr::left_join(
        dplyr::select(elo, week, game_id, team, elo),
        by = c("week", "game_id", "team")
      ) %>%
      dplyr::group_by(team) %>% 
      dplyr::arrange(week, .by_group = TRUE) %>% 
      dplyr::mutate(
        turnovers = dplyr::case_when(
          home_away == "home_team" ~ as.numeric(home_turnovers),
          home_away == "away_team" ~ as.numeric(away_turnovers)
        ),
        turnovers    = mean(turnovers, na.rm = T),
        score_diff   = mean(score_diff, na.rm = T)
      ) %>% 
      dplyr::slice(which.max(week)) %>% 
      dplyr::select(season, week, game_id, team, opponent, home_away, win, win_pct,
                    home_win_pct, away_win_pct, rest_days, score_diff, turnovers, elo)  %>% 
      dplyr::mutate(across(c(win_pct:away_win_pct), round, 4)) %>% 
      dplyr::ungroup()
    
    # Get schedule of upcoming games
    next_game <- get_upcoming_game(
      year        = year,
      pred_week   = pred_week,
      post_season = FALSE
    ) 
    
    # Upcoming home team stats
    home <- 
      next_game %>% 
      dplyr::left_join(
        dplyr::select(outcomes, -season, -week, -game_id, -opponent, -home_away, -win),
        by = c("home_team" = "team")
      ) 
    
    # Upcoming away team stats
    away <-
      outcomes %>% 
      dplyr::filter(team %in% home$away_team) %>% 
      dplyr::select(-season, -week, -game_id, -opponent, -home_away, -win) %>% 
      stats::setNames(
        c("away_team", 
          paste0("opp_", names(.)[names(.) != "team"])
        )
      )
    
    # Join Home team and away team stats leading up to upcoming game, used as input into models
    matchups <-
      home %>% 
      dplyr::left_join(
        away,
        by = c("away_team")
      ) %>% 
      dplyr::relocate(season, week, game_id, team = home_team, opponent = away_team)
    
    return(matchups)
    
    # If prediction week is after week 1, use current seasons data
  } else {
    
    # Construct URL
    url  <- paste0("https://www.pro-football-reference.com/years/", year ,"/games.htm")
    
    # Read HTML page using URL
    page <- rvest::read_html(url)
    
    # Extract HTML nodes for table
    page_nodes <- 
      page %>%  
      rvest::html_nodes("table")
    
    
    # Extract season games 
    page_table <- rvest::html_table(
      page_nodes[[1]],
      header  = T,
      fill    = T, 
      convert = F
    ) %>% 
      janitor::clean_names() %>% 
      dplyr::mutate(
        home = case_when(
          x == ""  ~ 1,
          x == "@" ~ 0,
          x == "N" ~ 0
        )
      ) 
    
    # remove headers for each week
    page_table <- page_table[!grepl("Week", page_table$week), ]
    
    # remove headers break for playoff start 
    page_table <- page_table[!grepl("Playoffs", page_table$date), ]
    
    # Rename playoff columns as weeks, accounting for added game after 2020 season
    if (year >= 2021) {
      
      # If season is after 2020
      page_table <- 
        page_table %>% 
        dplyr::mutate(
          week = dplyr::case_when(
            week == "WildCard"  ~ "19",
            week == "Division"  ~ "20",
            week == "ConfChamp" ~ "21",
            week == "SuperBowl" ~ "22",
            TRUE                ~ week
          ),
          week     = as.numeric(week)
        ) %>% 
        dplyr::filter(week < pred_week)
      
      # Rename playoff columns as weeks, accounting for fewer games before 2021
    } else {
      
      # if season is before 2021
      page_table <- 
        page_table %>% 
        dplyr::mutate(
          week = dplyr::case_when(
            week == "WildCard"  ~ "18",
            week == "Division"  ~ "19",
            week == "ConfChamp" ~ "20",
            week == "SuperBowl" ~ "21",
            TRUE                ~ week
          ),
          week     = as.numeric(week)
        ) %>% 
        dplyr::filter(week < pred_week)
    }
    
    # parse data tables from Pro Football Reference
    outcomes <- 
      page_table %>% 
      dplyr::left_join(
        dplyr::select(nfl_teams(), team_name, win_team_abb = team_abb),
        by = c("winner_tie" = "team_name")
      ) %>% 
      dplyr::left_join(
        dplyr::select(nfl_teams(), team_name, lose_team_abb = team_abb),
        by = c("loser_tie" = "team_name")
      ) %>% 
      dplyr::select(week, date,
                    win_team  = win_team_abb,
                    x, 
                    lose_team = lose_team_abb,
                    pts_win   = pts,
                    pts_lose  = pts_2, 
                    tow, tol)  %>% 
      dplyr::mutate(
        home_team = dplyr::case_when(
          x == ""  ~ win_team,
          x == "@" ~ lose_team,
          week == 22 ~ win_team
        ),
        away_team = dplyr::case_when(
          x == ""  ~ lose_team,
          x == "@" ~ win_team,
          week == 22 ~ lose_team
        ),
        pts_win  = as.numeric(pts_win),
        pts_lose = as.numeric(pts_lose),
        game_id  = dplyr::case_when(
          week < 10 ~ paste0(year, "_0", week, "_", away_team, "_",  home_team),
          week >= 10 ~ paste0(year, "_", week, "_", away_team, "_",  home_team)
        )
      ) %>% 
      dplyr::select(-x) %>% 
      dplyr::group_by(game_id) %>% 
      dplyr::mutate(
        home_pts = dplyr::case_when(
          home_team == win_team ~ max(pts_win, pts_lose),
          home_team != win_team ~ min(pts_win, pts_lose)
        ),
        away_pts = dplyr::case_when(
          home_team == win_team ~ min(pts_win, pts_lose),
          home_team != win_team ~ max(pts_win, pts_lose)
        ),
        home_turnovers = dplyr::case_when(
          home_team == win_team ~ tow,
          home_team != win_team ~ tol
        ),
        away_turnovers = dplyr::case_when(
          home_team == win_team ~ tol,
          home_team != win_team ~ tow
        ),
        season = year
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(season, week, game_id, gameday = date, home_team, away_team,
                    home_score = home_pts, away_score = away_pts,
                    home_turnovers, away_turnovers) 
    
    # calculate win loss percentages
    record <- get_win_pct(outcomes, verbose = FALSE)
    
    # Create Score differential, home or away team ID, # of rest days
    outcomes <- 
      outcomes %>% 
      tidyr::pivot_longer(
        cols      = c(home_team, away_team),
        names_to  = "home",
        values_to = "team"
      ) %>% 
      dplyr::mutate(
        home = dplyr::case_when(
          home == "home_team" ~ 1, 
          home == "away_team" ~ 0
        ),
        score_diff = dplyr::case_when(
          home == 1 ~ home_score - away_score, 
          home == 0 ~ away_score - home_score
        ),
        home_away  = case_when(
          home == 1 ~ "home_team",
          home == 0 ~ "away_team"
        )
      ) %>% 
      dplyr::mutate(
        split_game_id = substr(game_id, 9, 20)
      ) %>% 
      dplyr::mutate(
        home_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,2],
        away_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,1],
        opponent      =  dplyr::case_when(
          home_team == team ~ away_team,
          away_team == team ~ home_team
        )
      ) %>% 
      dplyr::select(-split_game_id, -home_team, -away_team) %>% 
      dplyr::group_by(team) %>% 
      dplyr::arrange(gameday, .by_group = T) %>% 
      dplyr::mutate(
        lag_date    = lag(gameday), 
        rest_days   = as.numeric(round(difftime(gameday, lag_date), 0))
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-lag_date, -gameday, -home) %>% 
      replace(is.na(.), 7) %>% 
      dplyr::relocate(season, week, game_id, team, opponent, home_away, rest_days, score_diff, home_score, away_score) %>% 
      dplyr::left_join(
        dplyr::select(record, week, game_id, team, win, win_pct, home_win_pct, away_win_pct),
        by = c("week", "game_id", "team")
      )
    
    # Calculate ELO ratings
    elo <-  
      outcomes %>% 
      dplyr::filter(home_away == "home_team") %>%
      dplyr::select(season, week, game_id, team, opponent, win, home_score, away_score) %>% 
      get_nfl_elo()
    
    # Join ELO ratings back w/ outcomes
    outcomes <- 
      outcomes %>% 
      dplyr::left_join(
        dplyr::select(elo, week, game_id, team, elo),
        by = c("week", "game_id", "team")
      ) %>%
      dplyr::group_by(team) %>% 
      dplyr::arrange(week, .by_group = TRUE) %>% 
      dplyr::mutate(
        turnovers = dplyr::case_when(
          home_away == "home_team" ~ as.numeric(home_turnovers),
          home_away == "away_team" ~ as.numeric(away_turnovers)
        ),
        turnovers    = mean(turnovers, na.rm = T),
        score_diff   = mean(score_diff, na.rm = T)
      ) %>% 
      dplyr::slice(which.max(week)) %>% 
      dplyr::select(season, week, game_id, team, opponent, home_away, win, win_pct,
                    home_win_pct, away_win_pct, rest_days, score_diff, turnovers, elo)  %>% 
      dplyr::mutate(across(c(win_pct:away_win_pct), round, 4)) %>% 
      dplyr::ungroup()
    
    
    # Get schedule of upcoming games
    next_game <- get_upcoming_game(
      year        = year,
      pred_week   = pred_week,
      post_season = FALSE
    ) 
    
    # Upcoming home team stats
    home <- 
      next_game %>% 
      dplyr::left_join(
        dplyr::select(outcomes, -season, -week, -game_id, -opponent, -home_away, -win),
        by = c("home_team" = "team")
      ) 
    
    # Upcoming away team stats
    away <-
      outcomes %>% 
      dplyr::filter(team %in% home$away_team) %>% 
      dplyr::select(-season, -week, -game_id, -opponent, -home_away, -win) %>% 
      stats::setNames(
        c("away_team", 
          paste0("opp_", names(.)[names(.) != "team"])
        )
      )
    
    # Join Home team and away team stats leading up to upcoming game, used as input into models
    matchups <-
      home %>% 
      dplyr::left_join(
        away,
        by = c("away_team")
      ) %>% 
      dplyr::relocate(season, week, game_id, team = home_team, opponent = away_team)
    
    return(matchups)
  }
  
}
# current_date = "2025-01-05"
scrape_games(year = 2019, pred_week = 1)
# **************************************************************************
# **************************************************************************

year = NULL
new_date <- "2022-11-17"
get_week <- function(year = NULL, pred_week, post_season = FALSE) {
  if(is.null(year)) {
    # new_date <- Sys.Date()
    # new_date <- "2023-01-17"
    
    # Current year
    year  <- as.numeric(substr(Sys.Date(), 1, 4))
    
    # Current month
    month <- as.numeric(substr(Sys.Date(), 6, 7))
    
    # If month is in part of season after Jan 1
    if (month %in% c(1, 2, 3, 4)) {
      
      year <- year - 1
      
    }
    
    logger::log_info("Return 1")
    
  return(year)
  }
  
  # If the year is greater than the current season, make year = current_year
  if(year > as.numeric(substr(Sys.Date(), 1, 4))) {
    
    # current year
    year  <- as.numeric(substr(Sys.Date(), 1, 4))
    
    # current month
    month <- as.numeric(substr(Sys.Date(), 6, 7))
    
    # If month is in part of season after Jan 1
    if (month %in% c(1, 2, 3, 4)) {
      
      year <- year - 1
      
    }
    logger::log_info("Return 2")
    
    return(year)
    
  }
  
  logger::log_info("Return 3")
  
  return(year)
}
# rm(new_date)
get_week(year = NULL)
#' @title Scrape a week of NFL matchups for a given season from ESPN.com
#' @description Calculates Elo ratings for an NFL season 
#' @param year numeric for season of interest
#' @param pred_week numeric for the week that we want to get the team matchups for
#' @param post_season logical, if TRUE, function will retrieve post season matchups. Default is FALSE, still work in progress
#' @return dataframe with the home and away teams for the desired week
get_week <- function(year = NULL, pred_week, post_season = FALSE) {
  year = NULL
  new_date <- "2022-01-17"
  if(is.null(year)) {
    # new_date <- Sys.Date()
    # new_date <- "2023-01-17"
    year  <- as.numeric(substr(new_date, 1, 4))
    month <- as.numeric(substr(new_date, 6, 7))
    new_year
    new_month
    if (month %in% c(1, 2, 3, 4)) {
      year <- year - 1
    }
  }
  year
  # Check and make sure pred_week is a valid week of season
  if(pred_week < 2) {
    
    # setting week to 2 if pred_week is less than 2
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 2 - upcoming week\nSetting pred_week = 2")
    
    pred_week = 2
    
  } else if(year >= 2021 & pred_week > 18) {
    
    # setting week to max week after 2020 season (18)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 2 - 18\nSetting pred_week = 18")
    
    pred_week = 18
    
  } else if(year < 2021 & pred_week > 17) {
    
    # setting week to max week before 2021 season (17)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 2 - 17\nSetting pred_week = 17")
    
    pred_week = 17
    
  }
  
  # Construct URL
  url  <- paste0("https://www.pro-football-reference.com/years/", year ,"/games.htm")
  
  # Read HTML page using URL
  page <- rvest::read_html(url)
  # year      = 2021
  # pred_week = 2
  # if a  week greater than 17 is entered before 2021 season, pred_week = 17
  if (year < 2021 & pred_week > 17) {
    
    pred_week <- 17
    
  } 
  # If year before 2000 is entered, year = 2000
  if (year < 2000) {
    
    year <- 2000
    # return(message("Invalid year, enter a season between 2000 and the current season"))
  }
  
  # If game is a post season game
  if (post_season == TRUE) {
    if (pred_week > 4) {
      pred_week = 4
    } 
    url <- paste0("https://www.nfl.com/schedules/", year, "/POST", pred_week, "/")
    # url <- paste0("https://www.espn.com/nfl/schedule/_/week/", pred_week,"/year/", year, "/seasontype/3")
    
  } else {
    # url <- paste0("https://www.cbssports.com/nfl/schedule/", year, "/regular/", pred_week, "/")
    # url <- paste0("https://www.espn.com/nfl/schedule/_/week/", pred_week,"/year/", year, "/seasontype/2")
    url <- paste0("https://www.espn.com/nfl/schedule/_/year/", year, "/seasontype/2")
  }
  url
  logger::log_info("\n\nRetrieving matchups:\nSeason: {year}\nWeek: {pred_week}")
  
  # url <- paste0("https://www.nfl.com/schedules/", year, "/REG", "/")
  # Read HTML page using URL
  page <- rvest::read_html(url)
  page
  # Extract HTML nodes for table
  page_nodes <-
    page %>%
    rvest::html_nodes("body")
  page_nodes[[1]]
  rvest::html_text(page_nodes)
  # empty list to add to in loop
  tbl_lst <- list()
  i = 1
  library(rvest)
  
  page <- read_html(url)
  page_div <- 
    page %>%
    rvest::html_elements("div")
  page_div
  list(page_div)
  i = 30
  div_lst <- list()
  for (i in 1:length(page_div)) {
    udiv <- as.character(page_div[i])
    
    div_lst[[i]] <- udiv
    # grepl("custom--week", udiv)
  }
  names(div_lst) <- paste0(1:length(div_lst))
  div_df <- 
    div_lst %>% 
    dplyr::bind_rows() %>% 
    tidyr::pivot_longer(everything())
  
  div_df %>% 
    dplyr::filter(grepl('class=\"DatePicker__wrapper\"', value))
  as.character(page_div[25])
  page_div %>% 
    list() %>% 
    dplyr::bind_rows()
  
  page_div
  '//*[@id="fittPageContainer"]/div[3]/div/div/section/div/section/div/div/div/div/div/div[2]/div/div[1]'
  # extract post dates
  rvest::html_elements(page_div, xpath =   '//*[@id="fittPageContainer"]/div[3]/div/div/section/div/section/div/div/div/div/div/div[2]/div/div[1]')
  # rvest::html_text2()
  page_div[33]
  html_text(page)
  # find all nodes with a class of "listing_row_price"
  listings <- html_nodes(page, css = ".Week__wrapper")
  listings
  class="custom--week"
  page %>% 
    html_nodes("div.page-container cf") %>% 
    html_text()
  for (i in 1:length(page_nodes)) {
    
    # logger::log_info("table {i} of {length(page_nodes)}")
    
    # Extract season games 
    page_table <- rvest::html_table(
      page_nodes[[i]],
      header  = F,
      fill    = T, 
      convert = T
    ) 
  }
}