FROM perl:latest
WORKDIR /srv/Game::EvonyTKR
COPY . .
RUN cpanm --installdeps --notest --with-feature=accelerate .
EXPOSE 4000
CMD plackup -p 4000 bin/app.psgi
