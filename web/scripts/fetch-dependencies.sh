#!/usr/bin/env bash

set -euxo pipefail

# Create a static repository to store things we'll include directly into the
# resulting webpack build. This let's us add on specific things to the git
# repository so we don't check in random garbage like font files.
mkdir -p $PWD/src/static

# Fetch IcoFont as a dependency.
if [ ! -f "$PWD/src/static/icofont.zip" ]; then
    pushd $PWD/src/static
    curl -s 'https://icofont.com/process/download?type=1&uid=1552920712' > $PWD/icofont.zip
    unzip $PWD/icofont.zip
    popd
fi
