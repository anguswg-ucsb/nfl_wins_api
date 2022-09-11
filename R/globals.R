library(tidyr)
library(dplyr)
library(janitor)
library(logger)

library(httr)
library(xml2)
library(rvest)

library(parsnip)
library(broom)
library(glmnet)
library(tune)

library(elo)


# Trained Logistic Regression Model
win_model <- readRDS("/app/R/win_model_logistic_reg.rds")

