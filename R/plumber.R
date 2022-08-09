source(here::here("R", "utils.R"), local = F)
source(here::here("R", "globals.R"), local = F)

#* Retrieve input data needed to make a prediction for a given year, week
#* @param year:number NFL season
#* @param pred_week:number Week to get data for
#* @post /scrape-model-inputs
function(year, pred_week) {
  given_data <- tibble::tibble(
    year      = as.double(year),
    pred_week = as.double(pred_week)
  )
  
  scrape_games(
    year      = given_data$year[1],
    pred_week = given_data$pred_week[1]
    )
}

#* Retrieve data for desired week and generate predictions 
#* @param year:number NFL season
#* @param pred_week:number Week to get data for
#* @post /predict-new-data
function(year, pred_week) {

  given_data <- tibble::tibble(
    year      = as.double(year),
    pred_week = as.double(pred_week)
  )
  
  new_data <- scrape_games(
    year      = given_data$year[1],
    pred_week = given_data$pred_week[1]
  )
  
  parsnip::augment(win_model, new_data) %>% 
    dplyr::select(
      season, week, game_id, 
      home_team = team,
      away_team = opponent, 
      .pred_class, .pred_1, .pred_0
      )
}
