# Builds the server docker image
#
# See https://docs.docker.com/engine/reference/builder/
# See https://hub.docker.com/_/haskell/
#
# TODO: Separate the build environment from the runtime environment.
#       See: https://www.fpcomplete.com/blog/2015/05/haskell-web-server-in-5mb
#
FROM haskell:7.10

WORKDIR /opt/server

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./openeth.cabal /opt/server/

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal install --only-dependencies -j4

# Add and Install Application Code
COPY *.hs /opt/server/

RUN cabal install --prefix /opt/server

ENTRYPOINT ["/opt/server/bin/openeth"]
CMD []
