library(httr)
library(dplyr)
library(jsonlite)
library(logger)
library(rvest)

source("R/utils.R")
scrape_games(2022, 5)
year = 2022 
pred_week = 5
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
sched2020 <- espnscrapeR::get_nfl_schedule(2020)
sched2021 <- espnscrapeR::get_nfl_schedule(2021)
sched <- dplyr::bind_rows(sched2020, sched2021)
library(dplyr)
tmp <- 
  sched %>% 
  dplyr::filter(season == 2020, type == 2) %>% 
  dplyr::select(matchup, matchup_short, season, type, game_id, game_date,
                home_team_abb, away_team_abb, home_win, away_win, home_score, home_record, away_record)

glimpse(tmp)
unique(sched2021$game_id) %in% unique(sched2020$game_id) 
# **************************************************************************
# **************************************************************************

#' @title Scrape data needed to make a prediction for a given week
#' @description Retrieves the necessary model input data from Profootballreference.com and returns a dataframe that can be inputted into models
#' @param year numeric for season of interest
#' @param pred_week numeric for the week that we want to get the team matchups for
#' @return dataframe with necessary model inputs
scrape_games2 <- function(year = NULL, pred_week = NULL) {
  
  # if no input given for prediction week, set to predict for week 1 
  if(is.null(pred_week)) {
    pred_week = 1
  }
  
  # current date when function is run in VM
  current_date <- Sys.Date()
  
  # if no input year is given, set prediction year to current season  
  if(is.null(year)) {

    # Current year
    year  <- as.numeric(substr(current_date, 1, 4))
    
    # Current month
    month <- as.numeric(substr(current_date, 6, 7))
    
    # If month is in part of season after Jan 1
    if (month %in% c(1, 2, 3, 4)) {
      
      year <- year - 1
      
    }
    
    logger::log_info("No year entered, defaulting to current season - {year}")
    
    
  }
  
  # If input year is greater than the current season, set year to current_year
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

  } 
  
  # if input year is before 2016, set year to 2016 (web scrape lookback limitation)
  if(year < 2016) {
    
    year <-  2016
    
    logger::log_info("Year entered was less than 2000 season, defaulting to {year} season")
  }
  
  # Check and make sure pred_week is a valid week of season
  if(pred_week < 1) {
    
    # setting week to 2 if pred_week is less than 2
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - upcoming week\nSetting pred_week = 2")
    
    pred_week = 1
    
    # Take account of added game after 2020 season 
  } else if(year >= 2021 & pred_week > 18) {
    
    # setting week to max week after 2020 season (18)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - 18\nSetting pred_week = 18")
    
    pred_week = 18
    
    # Take account of fewer games before 2021 seasons
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
    next_game <- get_matchups(
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
    next_game <- get_matchups(
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
scrape_games(year = 2018, pred_week = 1)
scrape_games2(year = 2018, pred_week = 1)
# **************************************************************************
# **************************************************************************
#' @title Scrape a week of NFL matchups for a given season from ESPN.com
#' @description Calculates Elo ratings for an NFL season 
#' @param year numeric for season of interest
#' @param pred_week numeric for the week that we want to get the team matchups for
#' @param post_season logical, if TRUE, function will retrieve post season matchups. Default is FALSE, still work in progress
#' @return dataframe with the home and away teams for the desired week
get_matchups <- function(
    year        = NULL,
    pred_week   = NULL,
    post_season = FALSE
    ) {
  
  if(is.null(pred_week)) {
    pred_week = 1
  }
  
  current_date <- Sys.Date()
  
  if(is.null(year)) {
    
    # Current year
    year  <- as.numeric(substr(current_date, 1, 4))
    
    # Current month
    month <- as.numeric(substr(current_date, 6, 7))
    
    # If month is in part of season after Jan 1
    if (month %in% c(1, 2, 3, 4)) {
      
      year <- year - 1
      
    }
    
    logger::log_info("No year entered, defaulting to current season - {year}")
    
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
    
    # if year is before 2000, set year = 2000
  } else if(year < 2016) {
    
    year <-  2016
    
    logger::log_info("Year entered was less than 2000 season, defaulting to {year} season")
  }
  
  # Check and make sure pred_week is a valid week of season
  if(pred_week < 1) {
    
    # setting week to 2 if pred_week is less than 2
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - upcoming week\nSetting pred_week = 2")
    
    pred_week <-  1
    
  } else if(year >= 2021 & pred_week > 18) {
    
    # setting week to max week after 2020 season (18)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - 18\nSetting pred_week = 18")
    
    pred_week <-  18
    
  } else if(year < 2021 & pred_week > 17) {
    
    # setting week to max week before 2021 season (17)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - 17\nSetting pred_week = 17")
    
    pred_week = 17
    
  }
  
  # if a  week greater than 17 is entered before 2021 season, pred_week = 17
  if (year < 2021 & pred_week > 17) {
    
    pred_week <- 17
    
  } 
  
  # If game is a post season game
  if (post_season == TRUE) {
    if (pred_week > 4) {
      pred_week = 4
    } 
    url <- paste0("https://www.nfl.com/schedules/", year, "/POST", pred_week, "/")

  # Non postseason games
  } else {
    
    url <- paste0("https://www.cbssports.com/nfl/schedule/", year, "/regular/", pred_week, "/")
  
  }
  
  logger::log_info("\n\nRetrieving matchups:\nSeason: {year}\nWeek: {pred_week}")
  
  
  # Read HTML page using URL
  page <- rvest::read_html(url)
  
  # Extract HTML nodes for table
  page_nodes <-
    page %>%
    rvest::html_nodes("table")
  
  # empty list to add to in loop
  tbl_lst <- list()
  
  for (i in 1:length(page_nodes)) {
    
    # logger::log_info("table {i} of {length(page_nodes)}")
    
    # Extract season games 
    page_table <- rvest::html_table(
      page_nodes[[i]],
      header  = F,
      fill    = T, 
      convert = T
    ) 
    
    # clean up table and get desired schedule for week
    page_table <- 
      page_table %>% 
      dplyr::select(away_team = X1, home_team = X2) %>% 
      dplyr::filter(!grepl("Away", away_team, ignore.case = TRUE)) %>% 
      dplyr::mutate(
        away_team = gsub("[[:punct:]]", "", away_team),
        home_team = gsub("[[:punct:]]", "", home_team)
      ) %>% 
      dplyr::mutate(
        away_abb = dplyr::case_when(
          grepl(" ", away_team) ~ gsub("(*UCP)[^;-](?<!\\b\\p{L})", "", away_team, perl=TRUE),
          TRUE ~ toupper(substr(away_team, 1, 3))
        ),
        home_abb = dplyr::case_when(
          grepl(" ", home_team) ~   gsub("(*UCP)[^;-](?<!\\b\\p{L})", "", home_team, perl=TRUE),
          TRUE ~ toupper(substr(home_team, 1, 3))
        )
      ) %>% 
      dplyr::mutate(
        season    = year,
        week      = pred_week,
        away_team = dplyr::case_when(
          away_abb == "LC"  ~ "LAC",
          away_abb == "LR"  ~ "LA",
          away_abb == "NJ"  ~ "NYJ",
          away_abb == "NG"  ~ "NYG",
          away_abb == "JAC" ~ "JAX",
          TRUE              ~ away_abb
        ),
        home_team = dplyr::case_when(
          home_abb == "LC"  ~ "LAC",
          home_abb == "LR"  ~ "LA",
          home_abb == "NJ"  ~ "NYJ",
          home_abb == "NG"  ~ "NYG",
          home_abb == "JAC" ~ "JAX",
          TRUE              ~ home_abb
        ),
        game_id   = dplyr::case_when(
          week < 10  ~ paste0(year, "_0", week, "_", away_team, "_",  home_team),
          week >= 10 ~ paste0(year, "_", week, "_", away_team, "_",  home_team)
        )
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(season, week, game_id, home_team, away_team)
    
    tbl_lst[[i]] <- page_table
  }

  
  # Bind rows of list
  upcoming_games <- dplyr::bind_rows(tbl_lst)
  
  return(upcoming_games)
  
}
get_matchups(year = 2014, pred_week = 4)
# *******************************************************************************
# *******************************************************************************

year = NULL
new_date <- "2022-11-17"
# current_date <- "2022-10-01"
current_date = "2020-11-10"
rm(current_date)

# *********************************************************************
# *********************************************************************

#' @title Find the numeric year of the NFL season year for a given date
#' @description Returns numeric season year for a given date 
#' @param date character string date, default is NULL and date will set to the date the function was run
#' @return numeric indicating the season year
get_year <- function(date = NULL) {
  
  if(is.null(date)) {
    
    # current date when function is run in VM
    date <- Sys.Date()
    
  }
  
  # Current year
  year  <- as.numeric(substr(date, 1, 4))
  
  # Current month
  month <- as.numeric(substr(date, 6, 7))
  
  # If month is in part of season after Jan 1
  if (month %in% c(1, 2, 3, 4)) {
    
    year <- year - 1
  }
  
  return(year)
}

current_date <- "2019-09-30"

#' @title Find the week of the NFL season according to a date
#' @description Calculates Elo ratings for an NFL season 
#' @param current_date character date, YYYY-MM-DD. Default is NULL and current_date will set to the date the function was run
#' @return character string of the current week of the NFL season
get_week <- function(current_date = NULL) {
  
  # if no current_date is entered, set to date function is run
  if(is.null(current_date)) {
    
    current_date <- Sys.Date()
    
  }
  
  # Current year
  year  <- as.numeric(substr(current_date, 1, 4))
  
  # Current month
  month <- as.numeric(substr(current_date, 6, 7))
  
  # If month is in part of season after Jan 1
  if (month %in% c(1, 2, 3, 4)) {
    
    year     <- year - 1
    
  }
  
  # year function is run
  sys_year <- get_year()
  
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
    ) %>% 
    dplyr::select(1:3) %>% 
    stats::setNames(c("week", "day", "datex")) 
  
  # remove headers for each week
  page_table <- page_table[!grepl("Week", page_table$week), ]
  
  # remove empty rows 
  page_table <- page_table[!apply(page_table == "", 1, any),]
  
  # page_table %>% 
  #   dplyr::group_by(week) %>% 
  #   dplyr::mutate(id = n())
  #   dplyr::summarise()
  # if year is less than current year, dates are correctly formatted, no need to parse dates
  if(year < sys_year) {
    
    # remove headers for each week
    page_table <- page_table[!grepl("Playoffs", page_table$week), ]
    
    # Create a clean date and min max of dates for each week of games
    page_table <-
      page_table %>% 
      dplyr::group_by(week) %>% 
      dplyr::mutate(
        min_date = min(datex),
        max_date = max(datex)
      ) %>% 
      dplyr::ungroup()
    
    # start and end of each NFL week, extract current week 
    current_week <- 
      page_table %>% 
      dplyr::group_by(week, min_date, max_date) %>% 
      dplyr::summarise() %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(min_date) %>% 
      dplyr::mutate(
        week = 1:n()
      ) %>% 
      dplyr::mutate(
        min_date      = as.Date(min_date),
        max_date      = as.Date(max_date),
        current_date  = current_date,
        min_date      = lag(max_date) + 1,
        min_date      = dplyr::case_when(
          is.na(min_date) ~ max_date - 1,
          TRUE            ~ min_date
        )
      ) %>% 
      dplyr::filter(current_date >= min_date, current_date <= max_date) %>% 
      .$week
    # dplyr::mutate(
    #   current_date  = current_date,
    #   min_day_dist  = abs(as.numeric(lubridate::ymd(current_date) - lubridate::ymd(min_date))),
    #   max_day_dist  = abs(as.numeric(lubridate::ymd(current_date) - lubridate::ymd(max_date))),
    #   day_dist      = (min_day_dist + max_day_dist)/2
    # ) %>% 
    # dplyr::filter(day_dist == min(day_dist)) %>%  # dplyr::filter(current_date >= max_date) %>%
    # .$week
    
  } else {
    
    # Create a clean date and min max of dates for each week of games
    page_table <-
      page_table %>% 
      dplyr::mutate(
        year     = year, 
        new_year = dplyr::case_when(
          grepl("January|February|March", datex) ~ year + 1,
          TRUE                                   ~ year
        ),
        date = as.Date(paste(datex, new_year), format='%b %d %Y')
      ) %>% 
      dplyr::group_by(week) %>% 
      dplyr::mutate(
        min_date = min(date),
        max_date = max(date)
      ) %>% 
      dplyr::ungroup()
    
    # start and end of each NFL week, extract current week 
    current_week <- 
      page_table2 %>% 
      dplyr::group_by(week, min_date, max_date) %>% 
      dplyr::summarise() %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(min_date) %>% 
      dplyr::mutate(
        current_date  = current_date,
        min_date      = lag(max_date) + 1,
        min_date      = dplyr::case_when(
          is.na(min_date) ~ max_date - 1,
          TRUE            ~ min_date
        )
      ) %>% 
      dplyr::filter(current_date >= min_date, current_date <= max_date) %>% 
      .$week
    # dplyr::mutate(
    #   current_date  = current_date,
    #   min_day_dist  = abs(as.numeric(lubridate::ymd(current_date) - lubridate::ymd(min_date))),
    #   max_day_dist  = abs(as.numeric(lubridate::ymd(current_date) - lubridate::ymd(max_date))),
    #   day_dist      = (min_day_dist + max_day_dist)/2
    # ) %>% 
    # dplyr::filter(day_dist == min(day_dist)) %>%   # dplyr::filter(current_date >= min_date, current_date <= max_date) %>% 
    # .$week
  }
  
  # if week is outside week range, set to week 1 of the current season
  if(length(current_week) == 0) {
    
    current_week <- 1
    
  }
  
  # if current week returns a preseason week, set current_week = 1
  if(grepl("Pre", current_week)) {
    
    current_week <- 1
    
  }
  
  # ensure week is a numeric
  current_week <- as.numeric(current_week)
  
  return(current_week)
  
}

# *********************************************************************
# *********************************************************************

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
