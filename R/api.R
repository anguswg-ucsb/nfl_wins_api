library(plumber)

plumber::pr("/app/R/plumber.R") %>%
  plumber::pr_run(host = "0.0.0.0", port = 8000)
  