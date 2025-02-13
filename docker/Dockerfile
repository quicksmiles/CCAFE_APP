# this Dockerfile expects the build context to be the CCAFE_APP repo root, i.e.
# one folder above ./docker/

FROM --platform=linux/amd64 rocker/shiny-verse:4.4.1 AS base

WORKDIR /srv/shiny-server/app/


# -------------------------------------------------------------------
# --- install packages using renv, with cache via built-time volume
# -------------------------------------------------------------------

# install renv, which we'll need to restore everything else
RUN R -e 'install.packages("renv")'

# install system deps required for some R packages
RUN apt-get update -qq && apt-get install -y \
  build-essential cmake

# use renv to restore into this image
COPY ./renv.lock ./renv.lock
COPY ./renv/settings.json ./renv/activate.R ./renv/

# put the library somewhere else, so it doesn't conflict with things
# make it writeable by the shiny user
RUN mkdir -p /opt/renv/library/ && \
    chmod -R ugo+rw /opt/renv/library/
ENV RENV_PATHS_LIBRARY=/opt/renv/library
ENV RENV_PATHS_CACHE=/opt/renv/cache

# set libPaths() to our project library via load(), install packages via
# restore(), then isolates the project via isolate().
# (the isolate() call is to separate the installed packages from the global cache
# so the symlinks to the build-time volume that's valid for just this RUN
# command doesn't break when the volume is removed after this line ends.)
# (just in case, we move the cache to a temp folder, then copy it back into the
# image from there, since the build-time volume's contents would no longer be
# available after it's unmounted.)
RUN --mount=type=cache,target=/opt/renv/cache \
    R -e "renv::load() ; renv::restore() ; renv::isolate() " && \
    chmod -R ugo+rw /opt/renv/library/ && \
    mkdir -p /tmp/opt/renv/cache && \
    cp -R /opt/renv/cache/* /tmp/opt/renv/cache

# as mentioned above, restore cached folder back into the image filesystem
RUN mkdir -p /opt/renv/cache/ && \
    cp -R /tmp/opt/renv/cache/* /opt/renv/cache/


# -------------------------------------------------------------------
# --- configure shiny-server, copy in app, etc.
# -------------------------------------------------------------------

COPY ./docker/shiny-config/shiny-server.conf /etc/shiny-server/shiny-server.conf

# copy entire working directory into /srv/shiny-server/app/
COPY . .
# make the folder owned by shiny so we can do operations on it
RUN chown -R shiny:shiny /srv/shiny-server/app/

# try to mitigate permissions issues that occur i think from
# the app running as shiny, not root
RUN mkdir -p /srv/shiny-server/app/renv/library/ && chmod -R ugo+rw /srv/shiny-server/app/renv/library/

EXPOSE 3838
