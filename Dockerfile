FROM rocker/r-ver:4.1.0

# hadolint ignore=DL3008
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  libssl-dev \
  libcurl4-gnutls-dev \
  git \
  curl \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /home/OpenPedCan-api

# Clone https://github.com/PediatricOpenTargets/OpenPedCan-analysis and checkout
# a specific commit
#
# Adapted from https://stackoverflow.com/a/34003075/4638182 and
# https://ryanfb.github.io/etc/2015/07/29/git_strategies_for_docker.html
#
# 4d32e16efd489e93bf91981db219e618af6df902 points to v8
#
# hadolint ignore=DL3003
RUN git clone https://github.com/PediatricOpenTargets/OpenPedCan-analysis.git \
  && cd OpenPedCan-analysis \
  && git checkout -q 4d32e16efd489e93bf91981db219e618af6df902

# hadolint ignore=DL3003,DL3059
RUN cd OpenPedCan-analysis && bash download-data.sh

# Install R X11 runtime dependencies
#
# Adapted from
# https://github.com/rocker-org/rocker-versioned/blob/dff37a27698cfe8cda894845fa194ecb5f668d84/X11/Dockerfile
#
# hadolint ignore=DL3059,DL3008
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libx11-6 \
    libxss1 \
    libxt6 \
    libxext6 \
    libsm6 \
    libice6 \
    xdg-utils \
  && rm -rf /var/lib/apt/lists/*

# hadolint ignore=DL3059
RUN install2.r --error \
  tidyverse \
  plumber \
  rprojroot \
  jsonlite \
  ggthemes

# Copy docker build working directory contents to docker image
# /home/OpenPedCan-api/
COPY / /home/OpenPedCan-api/

EXPOSE 80
ENTRYPOINT ["Rscript", "--vanilla", "main.R"]
