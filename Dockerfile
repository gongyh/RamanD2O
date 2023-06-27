FROM ubuntu:20.04

LABEL maintainer "Yanhai Gong <gongyh@qibebt.ac.cn>"

# Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
RUN useradd docker \
        && mkdir /home/docker \
        && chown docker:docker /home/docker \
        && addgroup docker staff

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update \
        && apt-get install -y --no-install-recommends \
           apt-utils ed less locales vim-tiny wget ca-certificates \
           apt-transport-https gsfonts gnupg2 \
        && rm -rf /var/lib/apt/lists/*

# Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
        && locale-gen en_US.utf8 \
        && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/" > /etc/apt/sources.list.d/cran.list

# note the proxy for gpg
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc

# system libraries of general use
RUN apt-get update && apt-get install -y \
  sudo \
  pandoc pandoc-citeproc \
  libcurl4-gnutls-dev \
  libcairo2-dev \
  libxt-dev \
  libssl-dev libssh2-1-dev libssl1.1 \
  file cmake \
  libxml2 libxml2-dev \
  libsasl2-dev \
  libharfbuzz-dev libfribidi-dev \
  libgtk-3-dev \
  libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
  && rm -rf /var/lib/apt/lists/*

ENV R_BASE_VERSION 4.2.3
# Now install R and littler, and create a link for littler in /usr/local/bin
# Also set a default CRAN repo, and make sure littler knows about it too
RUN apt-get update \
        && apt-get install -y --no-install-recommends \
                r-base=${R_BASE_VERSION}* \
                r-base-dev=${R_BASE_VERSION}* \
                r-recommended=${R_BASE_VERSION}* \
        && rm -rf /var/lib/apt/lists/*

# install dependencies of the RamanD2O app
RUN R -e "install.packages(c('docopt', 'pkgdepends', 'pak'))" && rm -rf /tmp/*
RUN R -e "library(pak); pkg_install(c('shiny', 'shinydashboard', 'shinyjs', 'shinyFiles', \
  'shinybusy', 'shinyalert', 'shinydisconnect', 'shinycssloaders', 'shinytoastr', \
  'DT', 'fresh', 'devtools', 'plotly', 'fs', 'ggpubr', 'ggplot2', 'stringr', \
  'RColorBrewer', 'dplyr', 'compiler', 'mongolite', 'zip', \
  'baseline', 'permute', 'Rtsne', 'markdown', 'randomForest', \
  'r-hyperspec/hyperSpec', 'r-hyperspec/hySpc.ggplot2', \
  'RinteRface/shinydashboardPlus')); cache_clean()" && rm -rf /tmp/*

# copy the app to the image
RUN mkdir /root/RamanD2O
COPY inst/shinyapp /root/RamanD2O

COPY Rprofile.site /usr/lib/R/etc/
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/RamanD2O')"]
