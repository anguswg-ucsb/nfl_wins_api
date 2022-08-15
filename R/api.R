library(plumber)

# 'plumber.R' is the location of the plumber API file
pr("/app/nfl_wins_api/R/plumber.R") %>%
  pr_run(host = "0.0.0.0", port = 8000)