# GPLV>=3.0
# Using a base container image from the Rocker Project
# They make R containers including for specific R versions
FROM rocker/r-ver:4.1.1

ENV S6_VERSION=v1.21.7.0
# using the preview version of RStudio to get the latest features
ENV RSTUDIO_VERSION=preview
ENV PATH=/usr/lib/rstudio-server/bin/:$PATH

# Installing some system dependencies
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    libpq5 openssh-client openssh-server \
    libxml2 libxml2-dev libglpk-dev libxt-dev

# Using Rocker's convenient scripts to install:
# RStudio & pandoc which will be needed for Rmarkdown
RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh

# Set global Renv cach location
RUN echo "RENV_PATHS_CACHE=/renv/cache" >> /usr/local/lib/R/etc/Renviron

# Expose RStudio's default port
EXPOSE 8787

CMD ["/init"]
