library(parsnip)
library(glmnet)
library(tune)
library(dplyr)
library(elo)
library(tidyr)
library(rvest)
library(xml2)
library(httr)
library(logger)
library(janitor)
library(stringr)
library(zoo)

# Trained Logistic Regression Model
logreg_model <- readRDS("/app/R/win_model_logistic_reg.rds")

# Trained Linear Support Vector Machine 
svm_model    <- readRDS("/app/R/win_model_svm_linear.rds")

# win_model <- readRDS("/app/R/win_model_logistic_reg.rds")
