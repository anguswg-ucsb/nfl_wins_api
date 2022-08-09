# Base image https://hub.docker.com/u/rocker/
FROM rstudio/plumber:latest

# system libraries of general use, install debian packages
RUN apt-get update -qq && apt-get install -y \
      libssl-dev \
      libcurl4-gnutls-dev

# make directory
RUN mkdir -p /app/nfl_win_api

# Copy R files into directory
COPY R /app/nfl_win_api

# set working directory
WORKDIR /app/nfl_win_api

# Copy R files into working directory
COPY . /app/nfl_win_api

# install packages
RUN R -e "install.packages(pkgs=c('plumber', 'tibble', 'dplyr', 'elo', 'parnsip', 'glmnet', 'tune', 'tidyr', 'rvest', 'httr', 'logger'))"

# open port 15782 to traffic
EXPOSE 8000

# Run api.R script to run plumber API
CMD Rscript /app/nfl_win_api/R/api.R