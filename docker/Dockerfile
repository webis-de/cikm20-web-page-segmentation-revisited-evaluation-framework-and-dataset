FROM johanneskiesel/tasks:ubuntu-20.10

ARG DEBIAN_FRONTEND=noninteractive
ARG DEBCONF_NONINTERACTIVE_SEEN=true
ARG TZ_AREA=Europe
ARG TZ_ZONE=Berlin

RUN truncate -s0 /tmp/preseed.cfg && \
    echo "tzdata tzdata/Areas select $TZ_AREA" >> /tmp/preseed.cfg && \
    echo "tzdata tzdata/Zones/$TZ_AREA select $TZ_ZONE" >> /tmp/preseed.cfg && \
    debconf-set-selections /tmp/preseed.cfg && \
    rm -f /etc/timezone /etc/localtime && \
    apt-get update

RUN apt-get install -y \
      libgdal-dev \
      libudunits2-dev \
      r-base 

RUN R -e "install.packages( \
      c('jsonlite', 'optparse', 'png', 'raster', 'sf', 'sp'), \
      dependencies=TRUE, \
      repos='http://cran.rstudio.com/')"

RUN apt-get install -y \
      imagemagick \
      wget

USER user
RUN wget https://github.com/webis-de/cikm20-web-page-segmentation-revisited-evaluation-framework-and-dataset/archive/master.zip && \
    unzip master.zip && \
    ln -s cikm20-web-page-segmentation-revisited-evaluation-framework-and-dataset-master cikm20
