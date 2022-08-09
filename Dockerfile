# Base image https://hub.docker.com/u/rocker/
FROM rstudio/plumber:latest

# system libraries of general use, install debian packages
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  curl \
  libsodium-dev \
  libxml2-dev

# make directory
RUN mkdir -p /app/nfl_wins_api

# Copy R files into directory
COPY R /app/nfl_wins_api

# set working directory
WORKDIR /app/nfl_wins_api

# Copy R files into working directory
COPY . /app/nfl_wins_api

# install packages
RUN R -e "install.packages(pkgs=c('plumber', 'tibble', 'dplyr', 'elo', 'parnsip', 'glmnet', 'janitor', 'stringr', 'zoo', 'tune', 'tidyr', 'rvest', 'httr', 'logger'))"

# open port 15782 to traffic
EXPOSE 8000

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(rev(commandArgs())[1]); args <- list(host = '0.0.0.0', port = 8000); if (packageVersion('plumber') >= '1.0.0') { pr$setDocs(TRUE) } else { args$swagger <- TRUE }; do.call(pr$run, args)"]

# Run api.R script to run plumber API
CMD ["/app/nfl_wins_api/R/api.R"]