library(plumber)

# 'plumber.R' is the location/ of the plumber API file
plumber::pr("/app/R/plumber.R") %>%
  plumber::pr_run(host = "0.0.0.0", port = 8000)