FROM arm64v8/perl:5.40.0 
WORKDIR /src/
COPY Game-EvonyTKR-*.tar.gz .
RUN tar zxvf *.tar.gz
WORKDIR /src/Game-EvonyTKR
RUN tar zxf ../*.tar.gz -C . --strip-components=1
RUN cpm install -g --with-all
RUN perl Build.PL
RUN ./Build
WORKDIR /srv/
EXPOSE 8080
CMD [ "EvonyTKRBackend.pl" ]