#!/usr/bin/env bash

# Create a static repository to store things we'll include directly into the
# resulting webpack build. This let's us add on specific things to the git
# repository so we don't check in random garbage like font files.
mkdir -p src/static

# Fetch IcoFont as a dependency.
file src/static/icofont.zip || (
    pushd src/static
    curl -s 'https://icofont.com/process/download?type=1&uid=1552920712' > icofont.zip
    yes | unzip icofont.zip
    popd
)

# Generate Types from Backend
(
    pushd ..
    stack run -- generate-types --file web/src/api/types.tsx
    popd
)
