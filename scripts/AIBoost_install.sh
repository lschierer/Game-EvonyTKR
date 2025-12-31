#!/bin/bash

# Define common variables first
LIBOMP_DIR=""
OS_TYPE=$(uname)

if [ "$OS_TYPE" == "Darwin" ]; then
    # macOS (assuming Homebrew is installed and libomp is keg-only)
    echo "Detected macOS. Using Homebrew paths."
    # Use brew --prefix to dynamically find the path, works for Intel & ARM
    LIBOMP_DIR=$(brew --prefix libomp)

    # Set environment variables for macOS build
    export LDFLAGS="-L${LIBOMP_DIR}/lib"
    export CPPFLAGS="-I${LIBOMP_DIR}/include"
    export PKG_CONFIG_PATH="${LIBOMP_DIR}/lib/pkgconfig:$PKG_CONFIG_PATH"

elif [ "$OS_TYPE" == "Linux" ]; then
    # Linux (e.g., Ubuntu/Debian on EC2)
    echo "Detected Linux. Installing dependencies via apt-get."

    # Install system package dependencies
    # The package name for OpenMP dev libraries is typically libomp-dev or libgomp1
    sudo apt-get update && sudo apt-get install -y libomp-dev build-essential

    # On Linux, system libraries are in standard paths (/usr/lib, /usr/include)
    # so explicit LDFLAGS/CPPFLAGS might not be necessary, but defining them doesn't hurt.
    # We can rely on standard linker paths.
fi

# Attempt to install the Perl module using cpanm (preferred)
echo "Attempting to install AI::XGBoost..."
# If cpanm isn't installed: curl -L https://cpanmin.us | perl - --sudo App::cpanminus
cpanm --notest Alien::XGBoost
cpanm --notest AI::XGBoost

# Verify the installation
perl -e 'use AI::XGBoost; print "AI::XGBoost installed successfully\n"'
