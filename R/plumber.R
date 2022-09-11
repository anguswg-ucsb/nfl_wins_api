source("/app/R/globals.R", local = FALSE)
source("/app/R/utils.R", local = FALSE)

#* @apiTitle NFL Win Predictor
#* @apiDescription This API returns NFL win predictions for a given week of the NFL season. The returned predictions are from the home team perspective (win = 1 means the home team is predicted to win). Predictions can be made for all regular season games between the 2016 season and the current season. The prediction_week input indicates the week of the NFL season that predictions will be generated for. The underlying model is trained to make predictions for the upcoming week and not any further out. If a prediction week is entered that is greater than the upcoming week of the current NFL season, the API will default to make a prediction for the current week of games.<br><br>GitHub: https://github.com/anguswg-ucsb<br>LinkedIn: https://www.linkedin.com/in/angus-watters-0521a7209/<br>Email: anguswatters@gmail.com

#* Retrieve data for desired week and generate predictions 
#* @param year:number year of the NFL season
#* @param pred_week:number week of the NFL season to predict
#* @post /predict-new-data
function(year, pred_week) {

  # input data, convert to doubles
  given_data <- data.frame(
    year      = as.double(year),
    pred_week = as.double(pred_week)
  ) 
  
  # New data from internet
  new_data <- scrape_games(
    year      = given_data$year[1],
    pred_week = given_data$pred_week[1]
  )

  # generate predictions
  generics::augment(win_model, new_data) %>% 
    dplyr::select(
      season, week, game_id, 
      home_team     = team,
      away_team     = opponent, 
      win           = .pred_class,
      home_win_prob = .pred_1,
      away_win_prob = .pred_0
      )

}


