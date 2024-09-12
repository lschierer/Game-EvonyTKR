FROM arm64v8/perl:5.40.0 AS build
WORKDIR /src/
COPY . .
RUN ./scripts/build.sh
FROM perl:latest AS deploy
WORKDIR /src
COPY --from=build /src/Game-EvonyTKR-*.tar.gz .
RUN tar zxvf *.tar.gz
WORKDIR /src/Game-EvonyTKR
RUN tar zxf ../*.tar.gz -C . --strip-components=1
RUN perl Build.PL
WORKDIR /srv/
EXPOSE 8080
