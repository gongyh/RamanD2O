FROM ubuntu:22.04

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

# Configure default locale, see https://kgithub.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
        && locale-gen en_US.utf8 \
        && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" > /etc/apt/sources.list.d/cran.list

# note the proxy for gpg
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc

# system libraries of general use
RUN apt-get update && apt-get install -y \
  sudo \
  pandoc pandoc-citeproc \
  libcairo2-dev \
  libxt-dev \
  libssl-dev libssh2-1-dev libssl3 \
  file cmake \
  libxml2 libxml2-dev \
  libsasl2-dev \
  libharfbuzz-dev libfribidi-dev \
  libgtk-3-dev \
  libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
  git libcurl4-openssl-dev libgit2-dev \
  && rm -rf /var/lib/apt/lists/*

ENV R_BASE_VERSION 4.4.1
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
        && install.r docopt pak pkgdepends \
        && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
        && rm -rf /var/lib/apt/lists/*

# install dependencies of the RamanD2O app
RUN R -e "library(pak); pkg_install(c('shiny', 'shinydashboard', 'shinyjs', 'shinyFiles', \
  'shinybusy', 'shinyalert', 'shinydisconnect', 'shinycssloaders', 'shinytoastr', \
  'DT', 'fresh', 'devtools', 'plotly', 'fs', 'ggpubr', 'ggplot2', 'stringr', \
  'RColorBrewer', 'dplyr', 'compiler', 'mongolite', 'zip', 'fawda123/ggord', \
  'baseline', 'permute', 'Rtsne', 'markdown', 'caret', 'randomForest', 'DALEX', 'vivo', \
  'OmicsPLS', 'mdatools', 'foreach', 'parallel', 'doParallel', 'signal', 'EMD', \
  'hht', 'pracma', 'r-hyperspec/hyperSpec', 'r-hyperspec/hySpc.ggplot2', \
  'RinteRface/shinydashboardPlus')); cache_clean()" && rm -rf /tmp/*

#ADD "https://www.random.org/cgi-bin/randbyte?nbytes=10&format=h" skipcache
ADD . /RamanD2O
RUN R -e "library(pak); pkg_install('/RamanD2O',dependencies=T); cache_clean()" && rm -rf /tmp/*

# shiny
COPY Rprofile.site /usr/lib/R/etc/
EXPOSE 3838

CMD ["R", "-e", "RamanD2O::runRamanD2O()"]
