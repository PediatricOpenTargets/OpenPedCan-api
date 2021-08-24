FROM rocker/r-ver:4.1.0

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  libssl-dev=1.1.1f-1ubuntu2.5 \
  libcurl4-gnutls-dev=7.68.0-1ubuntu2.6 \
  git \
  curl \
  && rm -rf /var/lib/apt/lists/* 

RUN install2.r --error \
  tidyverse \
  plumber

RUN mkdir /home/OpenPedCan-api
WORKDIR /home/OpenPedCan-api

# Clone https://github.com/PediatricOpenTargets/OpenPedCan-analysis and checkout
# a specific commit
#
# Adapted from https://stackoverflow.com/a/34003075/4638182 and
# https://ryanfb.github.io/etc/2015/07/29/git_strategies_for_docker.html
#
# f9656a849e09edeebb3be5d4a4a6e5d83fd6ad43 points to v7
RUN git clone https://github.com/PediatricOpenTargets/OpenPedCan-analysis.git \
  && cd OpenPedCan-analysis \
  && git checkout -q f9656a849e09edeebb3be5d4a4a6e5d83fd6ad43

RUN cd OpenPedCan-analysis && bash download-data.sh

# Copy docker build working directory contents to docker image
# /home/OpenPedCan-api/
COPY / /home/OpenPedCan-api/

EXPOSE 80
ENTRYPOINT ["Rscript", "--vanilla", "main.R"]
