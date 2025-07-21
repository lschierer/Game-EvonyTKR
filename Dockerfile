FROM perl
WORKDIR /opt/game-evony_t_k_r
COPY . .
RUN cpanm --installdeps -n .
EXPOSE 3000
CMD ./script/game-evony_t_k_r prefork
