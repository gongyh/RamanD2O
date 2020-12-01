FROM openanalytics/r-base

LABEL maintainer "Yanhai Gong <gongyh@qibebt.ac.cn>"

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
RUN R -e "install.packages(c('baseline', 'permute'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('hyperSpec', 'hySpc.ggplot2'), \
  repos=c('https://r-hyperspec.github.io/pkg-repo/', getOption('repos')))"

# copy the app to the image
RUN mkdir /root/RamanD2O
COPY inst/shinyapp /root/RamanD2O

COPY Rprofile.site /usr/lib/R/etc/
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/RamanD2O')"]
