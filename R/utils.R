#' @title Returns a dataframe w/ full NFL teams and team abbreviations
#' @description Helper function used in other functions for matching full team names to abbreviated names
#' @return dataframe 
nfl_teams <- function() {
  team_df <- data.frame(
    team_name = c("Arizona Cardinals", "Atlanta Falcons" , "Baltimore Ravens",  "Buffalo Bills", 
                  "Carolina Panthers", "Chicago Bears",  "Cincinnati Bengals" ,"Cleveland Browns",
                  "Dallas Cowboys",  "Denver Broncos",  "Detroit Lions",  "Green Bay Packers", 
                  "Houston Texans","Indianapolis Colts",  "Jacksonville Jaguars", "Kansas City Chiefs",   
                  "Las Vegas Raiders",  "Oakland Raiders",  "Los Angeles Chargers", "San Diego Chargers", "Los Angeles Rams", "St. Louis Rams", 
                  "Miami Dolphins", 
                  "Minnesota Vikings",   "New England Patriots",  "New Orleans Saints", "New York Giants",         
                  "New York Jets",       "Philadelphia Eagles",  "Pittsburgh Steelers",  "San Francisco 49ers", 
                  "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Football Team", "Washington Redskins", "Washington Commanders"),
    team_abb  = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", 
                  "DET", "GB", "HOU", "IND", "JAX", "KC", "LV", "LV", "LAC", "LAC",  "LA", "LA",  "MIA", "MIN", "NE", "NO", 
                  "NYG", "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS","WAS", "WAS")
  )
  return(team_df)
}

#' @title Calculate win percentages
#' @description Calculates overall, home, and away win percentages 
#' @param df dataframe with home/away teams, week, and game outcome
#' @param verbose logical, if TRUE, prints logger message. Default is TRUE
#' @return dataframe 
get_win_pct <- function(df, verbose = TRUE) { 

  # Enable log messages 
  if(verbose == TRUE) {
    logger::log_info("\n\nGenerating home/win totals and win % ...")
  }
  
  sched <- 
    df %>% 
    tidyr::pivot_longer(
      cols      = c(home_team, away_team),
      names_to  = "home_away",
      values_to = "team"
    ) 
  
  # number of rest days between games
  rest_df <- 
    sched %>% 
    dplyr::select(season, week, game_id,team, home_away, gameday) %>% 
    dplyr::group_by(season, team) %>% 
    dplyr::arrange(week, .by_group = T) %>% 
    dplyr::mutate(
      lag_gameday = dplyr::lag(gameday), 
      rest_days   = as.numeric(round(difftime(gameday, lag_gameday), 0))
    ) %>% 
    dplyr::select(season, week, game_id, team, rest_days) %>% 
    replace(is.na(.), 7)
  
  
  wins_df <- 
    sched %>% 
    dplyr::select(game_id, season, week, team, home_away, home_score, away_score) %>% 
    dplyr::group_by(game_id, home_away) %>% 
    dplyr::mutate(
      win = case_when(
        home_away == "home_team"  & home_score > away_score ~ 1,
        home_away == "away_team" & away_score > home_score ~ 1,
        away_score == home_score ~ 0,
        TRUE ~ 0
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(team) %>% 
    dplyr::arrange(week, .by_group = T) %>% 
    dplyr::mutate(
      games     = 1:n(), 
      win_total = cumsum(win),
      win_pct   = win_total/games
    ) %>% 
    dplyr::ungroup() 
  
  # Home game win pct %
  home_games <-
    wins_df %>% 
    dplyr::filter(home_away == "home_team") %>% 
    dplyr::group_by(season, team) %>% 
    dplyr::arrange(week, .by_group = T) %>%
    dplyr::mutate(
      ngames         = 1:n(),
      home_win_total = cumsum(win),
      home_win_pct   = home_win_total/ngames
    )
  
  # Away game win pct %
  away_games <-
    wins_df %>% 
    dplyr::filter(home_away == "away_team") %>% 
    dplyr::group_by(season, team) %>% 
    dplyr::arrange(week, .by_group = T) %>%
    dplyr::mutate(
      ngames         = 1:n(),
      away_win_total = cumsum(win),
      away_win_pct   = away_win_total/ngames
    )
  
  # Final wins dataframe
  wins <-
    home_games %>% 
    dplyr::bind_rows(away_games) %>% 
    dplyr::group_by(season, team) %>% 
    dplyr::arrange(week, .by_group = T) %>%
    tidyr::fill(home_win_pct, away_win_pct, .direction = "down") %>%
    # dplyr::mutate(
    #   home_win_pct = zoo::na.locf(home_win_pct, na.rm = F),
    #   away_win_pct = zoo::na.locf(away_win_pct, na.rm = F)
    # ) %>%
    replace(is.na(.), 0) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(season, week, game_id, team, home_away, win, home_score, away_score,
                  home_win = home_win_total, 
                  away_win = away_win_total, 
                  win_pct, home_win_pct,away_win_pct) %>% 
    dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) %>% 
    dplyr::left_join(
      rest_df, 
      by = c("season", "week", "team", "game_id")
    ) %>% 
    dplyr::relocate(season, week, game_id, team, home_away, rest_days) %>% 
    dplyr::mutate(
      split_game_id = substr(game_id, 9, 20)
    ) %>% 
    dplyr::ungroup()
  
  # Replace changed team names from game ID
  wins$split_game_id <- gsub("OAK", "LV", wins$split_game_id)
  wins$split_game_id <- gsub("SD", "LAC", wins$split_game_id)
  wins$split_game_id <- gsub("STL", "LA", wins$split_game_id)
  
  wins$team <- gsub("OAK", "LV", wins$team)
  wins$team <- gsub("SD", "LAC", wins$team)
  wins$team <- gsub("STL", "LA", wins$team)
  
  wins <- 
    wins %>% 
    dplyr::mutate(
      home_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,2],
      away_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,1]
    ) %>% 
    dplyr::mutate(
      opponent  = case_when(
        home_team == team ~ away_team,
        away_team == team ~ home_team
      )
    ) %>%
    dplyr::select(-split_game_id, -home_team, -away_team) %>%
    dplyr::relocate(season,week, game_id, team, opponent, home_away, rest_days) %>%
    dplyr::filter(team != "")
  
  
  return(wins)
  
}

#' @title Calculate NFL Elo Ratings
#' @description Calculates Elo ratings for an NFL season 
#' @param nfl_season dataframe with home/away teams, week, home/away team scores, and game outcomes
#' @return dataframe 
get_nfl_elo <- function(nfl_season) {
  
  df <- 
    nfl_season %>% 
    dplyr::mutate(
      wins_home = home_score > away_score
    )
  
  nfl_er <- elo::elo.run(wins_home ~ team + opponent, data = df, k = 20) %>% 
    as.data.frame() %>% 
    dplyr::group_by(team.A) %>%
    dplyr::mutate(
      r_id = 1:n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      team_join = paste0(team.A, "_", team.B, "_", r_id)
    )
  
  nfl_elo <- 
    df %>%   
    dplyr::group_by(team) %>% 
    dplyr::mutate(
      r_id = 1:n()
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      team_join = paste0(team, "_", opponent, "_", r_id)
    ) %>% 
    dplyr::left_join(
      dplyr::select(nfl_er, team_join, elo_team = elo.A, elo_opponent = elo.B),
      by = "team_join"
    ) %>% 
    dplyr::select(-team_join, -home_score, -away_score, -wins_home, -r_id)
  
  home_rating <- 
    nfl_elo %>% 
    dplyr::group_by(team) %>% 
    dplyr::group_split()
  
  home_elo <- lapply(home_rating, FUN = function(x) {
      rate <- 
        x %>%
        dplyr::select(season, week, game_id, win, team, elo = elo_team)
    }) %>% 
      dplyr::bind_rows()
    
  away_rating <- 
    nfl_elo %>% 
    dplyr::group_by(opponent) %>% 
    dplyr::group_split()
  
  away_elo <- lapply(away_rating, FUN = function(x) {
      rate <- 
        x %>%
        dplyr::select(season, week, game_id, win, team = opponent, elo = elo_opponent)
    }) %>% 
    dplyr::bind_rows()
  
  final_elo <- dplyr::bind_rows(home_elo, away_elo)
  
  return(final_elo)
  
}

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

#' @title Find the week of the NFL season according to a date
#' @description returns week of the NFL season by a date
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
      page_table %>% 
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

#' @title Scrape a week of NFL matchups for a given season from ESPN.com
#' @description Calculates Elo ratings for an NFL season 
#' @param year numeric for season of interest
#' @param pred_week numeric for the week that we want to get the team matchups for
#' @param post_season logical, if TRUE, function will retrieve post season matchups. Default is FALSE, still work in progress
#' @return dataframe with the home and away teams for the desired week
get_upcoming_game <- function(year, pred_week, post_season = FALSE) {
  # year = 2022
  # pred_week =7
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
    url <- paste0("https://www.cbssports.com/nfl/schedule/", year, "/regular/", pred_week, "/")
    # url <- paste0("https://www.espn.com/nfl/schedule/_/week/", pred_week,"/year/", year, "/seasontype/2")
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
    # if a BYE week table, skip iteration
    # if (page_table[1, 1] == "BYE") {
    #   
    #   next
    #   
    # } else {
    #   page_table2 <- 
    #     page_table %>% 
    #     dplyr::select(X1, X2, X3) %>% 
    #     dplyr::filter(!grepl("matchup", X1, ignore.case = TRUE)) %>% 
    #     stats::setNames(c("away_team", "home_team", "result")) %>% 
    #     dplyr::mutate(
    #       id = 1:n()
    #     ) %>% 
    #     dplyr::group_by(id) %>% 
    #     dplyr::mutate(
    #       home_team =  gsub("@ ", "", home_team)
    #     ) %>% 
    #     dplyr::mutate(
    #       away_abb = dplyr::case_when(
    #         grepl(" ", away_team) ~ gsub("(*UCP)[^;-](?<!\\b\\p{L})", "", away_team, perl=TRUE),
    #         TRUE ~ toupper(substr(away_team, 1, 3))
    #       ),
    #       home_abb = dplyr::case_when(
    #         grepl(" ", home_team) ~   gsub("(*UCP)[^;-](?<!\\b\\p{L})", "", home_team, perl=TRUE),
    #         TRUE ~ toupper(substr(home_team, 1, 3))
    #       )
    #     ) %>% 
    #     dplyr::mutate(
    #       home_abb =  gsub(" ", "", home_abb),
    #       away_abb =  gsub(" ", "", away_abb)
    #       # home_abb = dplyr::case_when(
    #       #   home_abb == " WA" ~ "WAS"
    #       # )
    #       )
    # 
    #   page_table2 <- 
    #     page_table %>% 
    #     dplyr::select(X1, X2, X3) %>% 
    #     dplyr::filter(!grepl("matchup", X1, ignore.case = TRUE)) %>% 
    #     stats::setNames(c("away_team", "home_team", "result")) %>% 
    #     dplyr::mutate(
    #       id = 1:n()
    #     ) %>% 
    #     dplyr::group_by(id) %>% 
    #     dplyr::mutate(
    #       home_team = gsub("@ ", "", home_team)
    #     ) %>% 
    #     dplyr::mutate(
    #       team1 = tail(strsplit(
    #         gsub("\\(OT\\)", "",
    #              gsub('[[:digit:]]+', '', result)
    #         ), ", "
    #       )[[1]][1], 1),
    #       team2 = head(strsplit(
    #         gsub("\\(OT\\)", "",
    #              gsub('[[:digit:]]+', '', result)
    #         ), ", "
    #       )[[1]][2], 1)
    #     ) %>% 
    #     dplyr::mutate(
    #       dist  = adist(home_team, team1), 
    #       dist2 = adist(home_team, team2),
    #       home_team1 = dplyr::case_when(
    #         dist < dist2 ~ team1,
    #         dist > dist2 ~ team2
    #       ),
    #       away_team1 = dplyr::case_when(
    #         home_team1 == team1 ~ team2,
    #         home_team1 == team2 ~ team1
    #       )
    #     ) %>% 
    #     dplyr::mutate(
    #       season    = year,
    #       week      = pred_week,
    #       home_team = dplyr::case_when(
    #         grepl("LAR", home_team) ~ "LA",
    #         grepl("WSH", home_team) ~ "WAS",
    #         TRUE                    ~ home_team
    #       ),
    #       away_team = dplyr::case_when(
    #         grepl("LAR", away_team) ~ "LA",
    #         grepl("WSH", away_team) ~ "WAS",
    #         TRUE                    ~ away_team
    #       ),
    #       game_id   = dplyr::case_when(
    #         week < 10 ~ paste0(year, "_0", week, "_", away_team, "_",  home_team),
    #         week >= 10 ~ paste0(year, "_", week, "_", away_team, "_",  home_team)
    #       )
    #     ) %>% 
    #     dplyr::ungroup() %>% 
    #     dplyr::select(season, week, game_id, home_team, away_team)
    #   
    #   page_table3 <-
    #     page_table %>%
    #     dplyr::select(X1, X2) %>%
    #     # dplyr::filter(X1 != "matchup") %>%
    #     dplyr::filter(!grepl("matchup", X1, ignore.case = TRUE)) %>%
    #     stats::setNames(c("away_team", "home_team")) %>%
    #     dplyr::mutate(
    #       id = 1:n()
    #     ) %>%
    #     dplyr::group_by(id) %>%
    #     dplyr::mutate(
    #       season    = year,
    #       week      = pred_week,
    #       away_team = tail(strsplit(away_team, split = " ")[[1]], 1),
    #       home_team = tail(strsplit(home_team, split = " ")[[1]], 1),
    #       home_team = dplyr::case_when(
    #         grepl("LAR", home_team) ~ "LA",
    #         grepl("WSH", home_team) ~ "WAS",
    #         TRUE                    ~ home_team
    #       ),
    #       away_team = dplyr::case_when(
    #         grepl("LAR", away_team) ~ "LA",
    #         grepl("WSH", away_team) ~ "WAS",
    #         TRUE                    ~ away_team
    #       ),
    #       game_id   = dplyr::case_when(
    #         week < 10 ~ paste0(year, "_0", week, "_", away_team, "_",  home_team),
    #         week >= 10 ~ paste0(year, "_", week, "_", away_team, "_",  home_team)
    #       )
    #     ) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::select(season, week, game_id, home_team, away_team)
    #   
    #   
    #   tbl_lst[[i]] <- page_table
    # }
  # }
  
  # Bind rows of list
  upcoming_games <- dplyr::bind_rows(tbl_lst)
  
  # Replace some team names to match names in  other data
  # upcoming_games$home_team <- gsub("LAR", "LA", upcoming_games$home_team)
  # upcoming_games$away_team <- gsub("LAR", "LA", upcoming_games$away_team)
  # upcoming_games$home_team <- gsub("WSH", "WAS", upcoming_games$home_team)
  # upcoming_games$away_team <- gsub("WSH", "WAS", upcoming_games$away_team)
  
  return(upcoming_games)
  
}

#' @title Process HTML table nodes from profootballreference.com
#' @description Cleans pages with game details from profootballreference.com
#' @param page xml_document created by using rvest::read_html on desired page 
#' @return data.frame
process_page <- function(page) {
  
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
  
  # remove empty rows 
  page_table <- page_table[!apply(page_table == "Playoffs", 1, any),]
  
  # fix playoff week names
  fix_weeks <- 
    page_table %>% 
    dplyr::group_by(week) %>% 
    dplyr::mutate(
      min_date = min(as.Date(date))
    ) %>% 
    dplyr::group_by(week, min_date) %>% 
    dplyr::summarise() %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(min_date) %>% 
    dplyr::mutate(
      num_week = 1:n()
    )
  
  # fix weeks to numbers
  page_table <- 
    page_table %>% 
    dplyr::left_join(
      dplyr::select(fix_weeks, week, num_week),
      by = "week"
    ) %>% 
    dplyr::select(-week) %>% 
    dplyr::relocate(week = num_week) 
  
  return(page_table)
  
}

#' @title Scrape a week of NFL matchups for a given season from CBS.com
#' @description Finds the team matchups for a specific week of an NFL season
#' @param year numeric for season of interest
#' @param week numeric for the week that we want to get the team matchups for
#' @param post_season logical, if TRUE, function will retrieve post season matchups. Default is FALSE, still work in progress
#' @return dataframe with the home and away teams for the desired week
get_matchups <- function(
    year        = NULL,
    week        = NULL,
    post_season = FALSE
) {
  
  if(is.null(week)) {
    week = 1
  }
  
  # date when function is run
  current_year <- get_year()   # current_date <- Sys.Date()
  
  if(is.null(year)) {
    
    # Current year
    year  <- current_year

    logger::log_info("No year entered, defaulting to current season - {year}")
    
  }

  # If the year is greater than the current season, make year = current_year
  if(year > current_year) {
    
    # current year
    year  <- current_year
    
    logger::log_info("Year entered was greater than current season, defaulting to current season - {year}")
    
    # if year is before 2000, set year = 2000
  } else if(year < 2016) {
    
    year <-  2016
    
    logger::log_info("Year entered was less than 2000 season, defaulting to {year} season")
  }
  
  # Check and make sure week is a valid week of season
  if(week < 1) {
    
    # setting week to 2 if week is less than 2
    logger::log_info("\n\nWeek {week} invalid\nWeek must be within valid week range: 1 - upcoming week\nSetting week = 1")
    
    week <-  1
    
  } 
  
  # If season is past 2021 and prediction week is greater than 18, set week to 18
  if(year >= 2021 & week > 18) {
    
    # setting week to max week after 2020 season (18)
    logger::log_info("\n\nWeek {week} invalid\nWeek must be within valid week range: 1 - 18\nSetting week = 18")
    
    week <- 18
    
  } 
  
  # If season is before 2021 and prediction week is greater than 17, set week to 17
  if(year < 2021 & week > 17) {
    
    # setting week to max week before 2021 season (17)
    logger::log_info("\n\nWeek {week} invalid\nWeek must be within valid week range: 1 - 17\nSetting week = 17")
    
    week <- 17
    
  }

  # If game is a post season game
  if (post_season == TRUE) {
    
    if (week > 4) {
      
      week <- 4
      
    } 
    
    # post season URL TODO
    url <- paste0("https://www.nfl.com/schedules/", year, "/POST", week, "/")
    # url <- paste0("https://www.cbssports.com/nfl/schedule/", year, "/postseason/", week, "/")
    
    # Non postseason games
  } else {
    
    # regular season URL 
    url <- paste0("https://www.cbssports.com/nfl/schedule/", year, "/regular/", week, "/")
    
  }
  
  logger::log_info("\n\nRetrieving matchups:\nSeason: {year}\nWeek: {week}")
  
  
  # Read HTML page using URL
  page <- rvest::read_html(url)
  
  # Extract HTML nodes for table
  page_nodes <-
    page %>%
    rvest::html_nodes("table")
  
  # empty list to add to in loop
  tbl_lst <- list()

  # loop through each table on CBS page and clean and add to list
  for (i in 1:length(page_nodes)) {
    
    # Extract each table of games representing the days games are played 
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
          TRUE                  ~ toupper(substr(away_team, 1, 3))
        ),
        home_abb = dplyr::case_when(
          grepl(" ", home_team) ~ gsub("(*UCP)[^;-](?<!\\b\\p{L})", "", home_team, perl=TRUE),
          TRUE                  ~ toupper(substr(home_team, 1, 3))
        )
      ) %>% 
      dplyr::mutate(
        season    = year,
        week      = week,
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

#' @title Scrape data needed to make a prediction for a given week
#' @description Retrieves the necessary model input data from profootballreference.com and returns a dataframe that can be inputted into models
#' @param year numeric for season of interest
#' @param pred_week numeric for the week that we want to get the team matchups for
#' @return dataframe with necessary model inputs
scrape_games <- function(
    year      = NULL, 
    pred_week = NULL
    ) {

  # if no input given for prediction week, set to predict for week 1 
  if(is.null(pred_week)) {
    
    pred_week = 1
    
  }
  
  # current date when function is run in VM
  current_date <- Sys.Date()
  
  # current NFL season year
  current_year <- get_year()
  
  # if no input year is given, set prediction year to current season  
  if(is.null(year)) {
    
    # default returns year of current season
    year <- current_year
    
  }
  
  # If input year is greater than the current season, set year to current_year
  if(year >= current_year) {
    
    # current week of NFL season
    current_week <- get_week()
    
    # default returns year of current season
    year  <- current_year
    
    # check if pred_week is too far in the future to make prediction
    if(pred_week > current_week) {
      
      # set pred_week to current week of NFL season
      pred_week <- current_week
      
    }
    
  } 
  
  # if input year is before 2016, set year to 2016 (web scrape lookback limitation)
  if(year < 2016) {
  
    year <- 2016
    
    logger::log_info("Year entered was less than 2000 season, defaulting to {year} season")
  }
  
  # Check and make sure pred_week is a valid week of season
  if(pred_week < 1) {
    
    # setting week to 2 if pred_week is less than 2
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - upcoming week\nSetting pred_week = 1")
    
    pred_week = 1
    
  
  }
  
  # Take account of added game after 2020 season 
  if(year >= 2021 & pred_week > 18) {
    
    # setting week to max week after 2020 season (18)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 1 - 18\nSetting pred_week = 18")
    
    pred_week = 18
    
  } 
  
  # Take account of fewer games before 2021 seasons
  if(year < 2021 & pred_week > 17) {
    
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
    
    # Rename playoff columns as weeks, accounting for added game after 2020 season
    if (past_year >= 2021) {
      
      # If season is after 2020
      page_table <- process_page(
         page      = page
         ) %>% 
        dplyr::filter(week <= 18)
      
      # Rename playoff columns as weeks, accounting for fewer games before 2021
    } else {
      
      # if season is before 2021
      page_table <- process_page(
        page      = page
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
      week        = pred_week,
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
    
    # process page and extract weeks leading up to prediction week
    page_table <- process_page(
      page = page
      ) %>% 
      dplyr::filter(week < pred_week) %>% 
      stats::setNames(c("week", "day", "date", "time", "winner_tie", "x", "loser_tie", "x_2",
                 "pts", "pts_2", "yds_w", "tow", "yds_l", "tol", "home"))

    # impute NA data for testing function will operate using future weeks of data 
     # tmp <-
     #   page_table %>%
     #   dplyr::mutate(across(c(pts:tol), as.numeric)) %>%
     #   dplyr::mutate(across(c(pts:tol), impute)) %>%
     #   dplyr::mutate(
     #     pts = dplyr::case_when(
     #       pts <= pts_2 ~ pts_2 + 2,
     #       TRUE ~ pts
     #       )
     #     )

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
      dplyr::relocate(season, week, game_id, team, opponent, home_away, 
                      rest_days, score_diff, home_score, away_score) %>% 
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
      week        = pred_week,
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

