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

# This ADD line purposely breaks the Docker cache after this point. That way 
# everything gets recopied into the container every time
# https://stackoverflow.com/a/55621942/591574
# ADD https://google.com cache_bust
# make directory
RUN mkdir -p /app

# Copy R files into directory
COPY /R /app

# set working directory
WORKDIR /app

# Copy R files into working directory
COPY . /app

# RUN chmod -R 755 /app

# install packages
RUN R -e "install.packages(pkgs=c('plumber', 'tibble', 'dplyr', 'elo', 'parnsip', 'glmnet', 'janitor', 'stringr', 'zoo', 'tune', 'tidyr', 'rvest', 'httr', 'logger'))"

# open port 15782 to traffic
EXPOSE 8000

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/app/R/api.R'); pr$run(host='0.0.0.0', port=8000)"]
