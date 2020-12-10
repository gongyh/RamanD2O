FROM ubuntu:18.04

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

RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" > /etc/apt/sources.list.d/cran.list

# note the proxy for gpg
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

ENV R_BASE_VERSION 3.6.1

# Now install R and littler, and create a link for littler in /usr/local/bin
# Also set a default CRAN repo, and make sure littler knows about it too
RUN apt-get update \
        && apt-get install -y --no-install-recommends \
                littler r-cran-littler \
                r-base=${R_BASE_VERSION}* \
                r-base-dev=${R_BASE_VERSION}* \
                r-recommended=${R_BASE_VERSION}* \
        && echo 'options(repos = c(CRAN = "https://cloud.r-project.org/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site \
        && echo 'source("/etc/R/Rprofile.site")' >> /etc/littler.r \
        && ln -s /usr/share/doc/littler/examples/install.r /usr/local/bin/install.r \
        && ln -s /usr/share/doc/littler/examples/install2.r /usr/local/bin/install2.r \
        && ln -s /usr/share/doc/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
        && ln -s /usr/share/doc/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
        && install.r docopt \
        && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
        && rm -rf /var/lib/apt/lists/*

# system libraries of general use
RUN apt-get update && apt-get install -y \
  sudo \
  pandoc \
  pandoc-citeproc \
  libcurl4-gnutls-dev \
  libcairo2-dev \
  libxt-dev \
  libssl-dev \
  libssh2-1-dev \
  libssl1.0.0 \
  file \
  libxml2 \
  libxml2-dev \
  libsasl2-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinyjs', 'shinyFiles', \
  'shinybusy', 'shinyalert', 'shinydisconnect', 'shinycssloaders', 'shinytoastr', \
  'DT', 'fresh', 'devtools', 'plotly', 'fs', 'ggpubr', 'ggplot2', 'stringr', \
  'RColorBrewer', 'dplyr', 'compiler', 'mongolite', 'zip'), \
  repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('RinteRface/shinydashboardPlus')"

# install dependencies of the RamanD2O app
RUN R -e "install.packages(c('baseline', 'permute', 'Rtsne', 'randomForest'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('hyperSpec', 'hySpc.ggplot2'), \
  repos=c('https://r-hyperspec.github.io/pkg-repo/', getOption('repos')))"

# copy the app to the image
RUN mkdir /root/RamanD2O
COPY inst/shinyapp /root/RamanD2O

COPY Rprofile.site /usr/lib/R/etc/
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/RamanD2O')"]
