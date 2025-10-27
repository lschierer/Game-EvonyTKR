FROM perl:5.42 as npmBuilder

# Install Node.js and pnpm
RUN curl -fsSL https://deb.nodesource.com/setup_24.x | bash -
RUN apt-get install -y nodejs
ENV PNPM_HOME="/pnpm"
ENV PATH="$PNPM_HOME:$PATH"
RUN corepack enable
RUN corepack prepare pnpm@latest --activate

WORKDIR /app

# Copy package files first for better caching
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./

# Install Node.js dependencies
# This is the specific ones that Mojolicious will need to serve as assets.
RUN pnpm install --frozen-lockfile --prod

FROM perl:5.42 as builder

WORKDIR /opt/Game-EvonyTKR

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
RUN apt-get install -y netcat-openbsd
RUN cpanm utf8::all
RUN cpanm Module::Build
RUN perl Build.PL
RUN cpanm --installdeps -n . || (mkdir -p /error-logs && cp /root/.cpanm/work/*/build.log /error-logs/ && false)
RUN ./Build manifest
RUN ./Build
RUN ./Build install

# Final stage
FROM builder
# Continue with your normal build...
WORKDIR /opt/Game-EvonyTKR

COPY --from=npmBuilder /app/node_modules ./

# Create a non-root user
# Mojolicious will create the log directory using Path::Tiny
# assuming the home directory exists.
# First create the group
RUN groupadd mojo
# create the user:
# -m create the home directory
# -s set the default shell
# -g set the default group
RUN useradd -ms /bin/bash -g mojo mojo


# because of the way the CMD is written,
# we might not actually be using the installed version
# but might be using the version from here
RUN chown -R mojo:mojo /opt/Game-EvonyTKR
RUN chown -R mojo:mojo /home/mojo

# create an init script to set /home/mojo/var permissions

RUN apt-get update && \
  apt-get install -y sudo gosu && rm -rf /var/lib/apt/lists/*

# Install Pandoc
RUN curl -L https://github.com/jgm/pandoc/releases/download/3.2/pandoc-3.2-1-arm64.deb -o /tmp/pandoc.deb \
  && dpkg -i /tmp/pandoc.deb \
  && rm /tmp/pandoc.deb

# Verify installation
RUN pandoc --version

COPY share/scripts/dockerEntrypoint.sh /usr/local/bin/dockerEntrypoint.sh
RUN chmod +x /usr/local/bin/dockerEntrypoint.sh

# Command to run your application
EXPOSE 3000
ENTRYPOINT ["/usr/local/bin/dockerEntrypoint.sh"]
#CMD ["nc", "-v", "-l", "0.0.0.0", "3000"]
#CMD ["perl", "scripts/game-evonytkr", "prefork", "-m", "preprod"]
#CMD ["perl", "scripts/game-evonytkr", "prefork", "-m", "production"]
