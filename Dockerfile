FROM rstudio/plumber:latest

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  curl \
  libsodium-dev \
  libxml2-dev

# make directory
RUN mkdir -p /app

# Copy R files into directory
COPY /R /app

# set working directory
WORKDIR /app

# Copy R files into working directory
COPY . /app

# install packages
RUN R -e "install.packages(pkgs=c('dplyr', 'tidyr', 'janitor', 'elo', 'parsnip', 'broom', 'glmnet', 'tune', 'rvest', 'httr', 'xml2', 'logger'))"

# open port 15782 to traffic
EXPOSE 8000

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/app/R/api.R'); pr$run(host='0.0.0.0', port=8000)"]

