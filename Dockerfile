FROM perl:latest AS build
WORKDIR /srv/Game::EvonyTKR
COPY . .
RUN ./bin/build.sh
RUN dzil install 
FROM perl:latest AS deploy
COPY . .
COPY --from=build ./GAME-EVONYTKR-*.tar.gz .
RUN tar zxvf *.tar.gz
RUN cd Game-EvonyTKR-* && perl Build.PL
EXPOSE 8080
