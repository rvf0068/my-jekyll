#!/bin/bash

# Generate _config.yml from .blog-config
# This ensures Jekyll and Org-mode use the same configuration

set -e

CONFIG_FILE=".blog-config"
OUTPUT_FILE="_config.yml"

if [ ! -f "$CONFIG_FILE" ]; then
    echo "Error: $CONFIG_FILE not found"
    exit 1
fi

# Source the configuration
source "$CONFIG_FILE"

# Generate _config.yml
cat > "$OUTPUT_FILE" << EOF
title: ${SITE_TITLE}
description: ${SITE_DESCRIPTION}
baseurl: "${SITE_BASEURL}"
url: "${SITE_URL}"
permalink: /:year/:month/:day/:title.html

# Author and social
github_username: ${GITHUB_USERNAME}
author: ${AUTHOR_NAME}

# Default values for posts
defaults:
  - scope:
      path: "_posts"
      type: "posts"
    values:
      layout: "post"
      author: "${AUTHOR_NAME}"

# Plugins for enhanced functionality
plugins:
  - jekyll-sitemap
  - jekyll-feed

# Performance optimizations
sass:
  style: compressed
exclude:
  - scripts/
  - .github/
  - vendor/
  - Gemfile
  - Gemfile.lock
  - README.md
  - .blog-config
  - org/_posts/
  - org/index.org
  - "**/*.pdf"
EOF

echo "Generated $_config.yml from $CONFIG_FILE"