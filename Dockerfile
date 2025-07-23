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
COPY share/package.json share/pnpm-lock.yaml ./share/

# Install Node.js dependencies
# This is the specific ones that Mojolicious will need to serve as assets.
RUN cd share && pnpm install --frozen-lockfile --prod

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
COPY --from=npmBuilder /app/share/node_modules ./share/

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

RUN cat > /usr/local/bin/fix_mojo_var.sh << 'EOF'
#!/bin/bash
set -e

mkdir -p /home/mojo/var/log || exit 1
chown -R mojo:mojo /home/mojo/var || exit 2
EOF

# Set the user
USER mojo

# Set HOME environment variable explicitly
ENV HOME=/home/mojo

# Command to run your application
EXPOSE 3000
CMD ["nc", "-v", "-l", "0.0.0.0", "3000"]
#CMD ["perl", "scripts/game-evonytkr", "prefork", "-m", "development"]
#CMD ["perl", "scripts/game-evonytkr", "prefork", "-m", "production"]
