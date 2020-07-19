FROM rocker/r-base

MAINTAINER Michal Jakubczak "michal.jakubczak@ardigen.com"

RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libxml2-dev \
  libssl-dev \
  libgit2-dev \
  qpdf
  
RUN mkdir /install
COPY . /install

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_deps(pkg='/install', dependencies=TRUE)"
RUN export OPENSSL_CONF=/etc/ssl/ && R -e "devtools::check('/install')"

RUN R CMD INSTALL /install
# RUN rm -rf /install

EXPOSE 3838

CMD ["Rscript", "-e", "FutureManager::demo()"]
