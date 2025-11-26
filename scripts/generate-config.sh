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

# Giscus comments configuration
giscus:
  repo: "${GISCUS_REPO}"
  repo_id: "${GISCUS_REPO_ID}"
  category: "${GISCUS_CATEGORY}"
  category_id: "${GISCUS_CATEGORY_ID}"
  mapping: "${GISCUS_MAPPING}"
  strict: "${GISCUS_STRICT}"
  reactions_enabled: "${GISCUS_REACTIONS_ENABLED}"
  emit_metadata: "${GISCUS_EMIT_METADATA}"
  input_position: "${GISCUS_INPUT_POSITION}"
  theme: "${GISCUS_THEME}"
  lang: "${GISCUS_LANG}"

# Performance optimizations
sass:
  style: compressed
exclude:
EOF

# Add excludes from configuration
for item in $JEKYLL_EXCLUDES; do
    echo "  - $item" >> "$OUTPUT_FILE"
done

echo "Generated $_config.yml from $CONFIG_FILE"