library(plumber)
# 'plumber.R' is the location of the file shown above
pr("R/plumber.R") %>%
  pr_run(port=8000)

