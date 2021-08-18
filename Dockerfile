FROM rocker/r-ver:4.1.0
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  libssl-dev=1.1.1f-1ubuntu2.5 \
  libcurl4-gnutls-dev=7.68.0-1ubuntu2.6 \
  && rm -rf /var/lib/apt/lists/* \
  && R -e "install.packages('plumber')"
COPY / /
EXPOSE 80
ENTRYPOINT ["Rscript", "main.R"]