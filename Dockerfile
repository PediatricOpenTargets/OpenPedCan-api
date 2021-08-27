FROM rocker/r-ver:4.1.0
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  libssl-dev \
  libcurl4-gnutls-dev \
  && rm -rf /var/lib/apt/lists/* \
  && R -e "install.packages('plumber')"
COPY / /
EXPOSE 80
ENTRYPOINT ["Rscript", "main.R"]