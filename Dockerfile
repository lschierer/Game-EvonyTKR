FROM perl:5.42 as builder
WORKDIR /opt/game-evony_t_k_r
COPY . .
# dependencies for the graphics modules I need
RUN apt-get update && apt-get install -y \
  libgd-dev \
  libjpeg-dev \
  libpng-dev \
  libfreetype6-dev \
  zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*
# Install dependencies and AWS CLI v2
RUN apt-get update && \
  apt-get install -y curl unzip less groff && \
  curl "https://awscli.amazonaws.com/awscli-exe-linux-aarch64.zip" -o "/tmp/awscliv2.zip" && \
  unzip /tmp/awscliv2.zip -d /tmp && \
  /tmp/aws/install && \
  rm -rf /tmp/aws /tmp/awscliv2.zip
RUN cpanm utf8::all
RUN cpanm Module::Build
RUN perl Build.PL
RUN cpanm --installdeps -n . || (mkdir -p /error-logs && cp /root/.cpanm/work/*/build.log /error-logs/ && false)

# Debug stage - only used when build fails
FROM builder as debug
CMD ["cat", "/error-logs/build.log"]

# Final stage
FROM builder
# Continue with your normal build...
WORKDIR /opt/game-evony_t_k_r

EXPOSE 3000
CMD ./scripts/game-evonytkr prefork
