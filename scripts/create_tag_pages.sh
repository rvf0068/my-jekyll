#!/bin/bash

# Create category and tag pages for Jekyll blog
# This script generates individual pages for each category and tag

CATEGORIES_DIR="categories"
TAGS_DIR="tags"

# Create directories if they don't exist
mkdir -p "$CATEGORIES_DIR"
mkdir -p "$TAGS_DIR"

echo "Generating category and tag pages..."

# Extract categories and tags from all posts
echo "Extracting categories from posts..."
categories=$(grep -h "^categories:" _posts/*.html | sed 's/categories: //' | tr ' ' '\n' | sort -u | grep -v '^$')

echo "Extracting tags from posts..."  
tags=$(grep -h "^tags:" _posts/*.html | sed 's/tags: //' | tr ' ' '\n' | sort -u | grep -v '^$')

# Generate category pages
echo "Generating category pages..."
for category in $categories; do
    # Convert to lowercase and replace spaces/special chars with hyphens for filename
    filename=$(echo "$category" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g' | sed 's/--*/-/g' | sed 's/^-\|-$//g')
    
    cat > "$CATEGORIES_DIR/$filename.html" << EOF
---
layout: category
category: $category
title: "Category: $category"
permalink: /categories/$filename/
---
EOF
    
    echo "Created: categories/$filename.html"
done

# Generate tag pages
echo "Generating tag pages..."
for tag in $tags; do
    # Convert to lowercase and replace spaces/special chars with hyphens for filename
    filename=$(echo "$tag" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g' | sed 's/--*/-/g' | sed 's/^-\|-$//g')
    
    cat > "$TAGS_DIR/$filename.html" << EOF
---
layout: tag
tag: $tag
title: "Tag: $tag"
permalink: /tags/$filename/
---
EOF
    
    echo "Created: tags/$filename.html"
done

echo "Done! Generated pages for:"
echo "Categories: $(echo "$categories" | wc -w)"
echo "Tags: $(echo "$tags" | wc -w)"
echo ""
echo "Run 'bundle exec jekyll build' to regenerate your site."