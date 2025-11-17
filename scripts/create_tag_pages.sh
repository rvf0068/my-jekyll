#!/bin/bash

# Create category and tag pages for Jekyll blog
# This script generates individual pages for each category and tag,
# as well as the main index pages for categories and tags.
# This makes the script fully self-contained without requiring
# committed index files.

CATEGORIES_DIR="categories"
TAGS_DIR="tags"

# Create directories if they don't exist
mkdir -p "$CATEGORIES_DIR"
mkdir -p "$TAGS_DIR"

echo "Generating category and tag pages..."

# Function to generate categories index page
generate_categories_index() {
    cat > "$CATEGORIES_DIR/index.html" << 'EOF'
---
layout: default
title: "Categories"
permalink: /categories/
---

<div class="archive-header">
  <h1 class="archive-title">📂 Categories</h1>
  <p class="archive-description">Explore posts organized by category</p>
</div>

<div class="category-grid">
  {% assign categories = site.categories | sort %}
  {% for category in categories %}
    <div class="category-card">
      <h3><a href="{{ '/categories/' | append: category[0] | relative_url }}">{{ category[0] | capitalize }}</a></h3>
      <p class="category-count">{{ category[1] | size }} post{% if category[1].size != 1 %}s{% endif %}</p>
      <div class="category-preview">
        {% for post in category[1] limit:3 %}
          <a href="{{ post.url | relative_url }}" class="preview-link">{{ post.title }}</a>
        {% endfor %}
      </div>
    </div>
  {% endfor %}
</div>

<style>
.category-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 2rem;
  margin-top: 2rem;
}

.category-card {
  background: #f8f9fa;
  padding: 1.5rem;
  border-radius: 8px;
  border: 1px solid #e9ecef;
  transition: all 0.3s ease;
}

.category-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(0,0,0,0.1);
}

.category-card h3 {
  margin: 0 0 0.5rem 0;
  color: #2c3e50;
}

.category-card h3 a {
  text-decoration: none;
  color: inherit;
}

.category-count {
  color: #6c757d;
  font-size: 0.9rem;
  margin: 0 0 1rem 0;
}

.category-preview {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
}

.preview-link {
  color: #495057;
  text-decoration: none;
  font-size: 0.9rem;
  transition: color 0.2s ease;
}

.preview-link:hover {
  color: #007bff;
}

html.dark-mode .category-card {
  background: #2d2d2d;
  border-color: #404040;
}

html.dark-mode .category-card h3 {
  color: #e0e0e0;
}

html.dark-mode .category-count {
  color: #b0b0b0;
}

html.dark-mode .preview-link {
  color: #b0b0b0;
}

html.dark-mode .preview-link:hover {
  color: #66b3ff;
}
</style>
EOF
    echo "Created: categories/index.html"
}

# Function to generate tags index page
generate_tags_index() {
    cat > "$TAGS_DIR/index.html" << 'EOF'
---
layout: default
title: "Tags"
permalink: /tags/
---

<div class="archive-header">
  <h1 class="archive-title">🏷️ Tags</h1>
  <p class="archive-description">Browse posts by topic tags</p>
</div>

<div class="tags-cloud">
  {% assign tags = site.tags | sort %}
  {% for tag in tags %}
    <a href="{{ '/tags/' | append: tag[0] | relative_url }}" class="tag-cloud-item" data-count="{{ tag[1].size }}">
      {{ tag[0] }}
      <span class="tag-count">({{ tag[1].size }})</span>
    </a>
  {% endfor %}
</div>

<div class="tags-list">
  {% for tag in tags %}
    <div class="tag-section">
      <h3><a href="{{ '/tags/' | append: tag[0] | relative_url }}">{{ tag[0] | capitalize }}</a></h3>
      <div class="tag-posts">
        {% for post in tag[1] %}
          <div class="tag-post-item">
            <a href="{{ post.url | relative_url }}">{{ post.title }}</a>
            <span class="post-date">{{ post.date | date: "%B %d, %Y" }}</span>
          </div>
        {% endfor %}
      </div>
    </div>
  {% endfor %}
</div>

<style>
.tags-cloud {
  margin: 2rem 0;
  text-align: center;
}

.tag-cloud-item {
  display: inline-block;
  margin: 0.3rem;
  padding: 0.5rem 1rem;
  background: #e9ecef;
  color: #495057;
  text-decoration: none;
  border-radius: 20px;
  font-size: 0.9rem;
  transition: all 0.3s ease;
  border: 1px solid #dee2e6;
}

.tag-cloud-item:hover {
  background: #007bff;
  color: white;
  transform: translateY(-2px);
  text-decoration: none;
}

.tag-cloud-item[data-count="1"] { font-size: 0.8rem; opacity: 0.7; }
.tag-cloud-item[data-count="2"] { font-size: 0.9rem; opacity: 0.8; }
.tag-cloud-item[data-count="3"] { font-size: 1rem; opacity: 0.9; }
.tag-cloud-item[data-count="4"], 
.tag-cloud-item[data-count="5"] { font-size: 1.1rem; }

.tag-count {
  font-size: 0.8em;
  opacity: 0.7;
}

.tags-list {
  margin-top: 3rem;
}

.tag-section {
  margin-bottom: 2rem;
  padding-bottom: 1.5rem;
  border-bottom: 1px solid #e9ecef;
}

.tag-section h3 {
  color: #2c3e50;
  margin-bottom: 1rem;
}

.tag-section h3 a {
  text-decoration: none;
  color: inherit;
}

.tag-posts {
  display: grid;
  gap: 0.5rem;
}

.tag-post-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.5rem 0;
}

.tag-post-item a {
  flex: 1;
  text-decoration: none;
  color: #495057;
  margin-right: 1rem;
}

.tag-post-item a:hover {
  color: #007bff;
  text-decoration: underline;
}

.post-date {
  font-size: 0.85rem;
  color: #6c757d;
  white-space: nowrap;
}

html.dark-mode .tag-cloud-item {
  background: #404040;
  color: #e0e0e0;
  border-color: #555555;
}

html.dark-mode .tag-cloud-item:hover {
  background: #66b3ff;
  color: #ffffff;
}

html.dark-mode .tag-section {
  border-bottom-color: #404040;
}

html.dark-mode .tag-section h3 {
  color: #e0e0e0;
}

html.dark-mode .tag-post-item a {
  color: #b0b0b0;
}

html.dark-mode .tag-post-item a:hover {
  color: #66b3ff;
}

html.dark-mode .post-date {
  color: #888888;
}
</style>
EOF
    echo "Created: tags/index.html"
}

# Extract categories and tags from all posts
echo "Extracting categories from posts..."
categories=$(grep -h "^categories:" _posts/*.html | sed 's/categories: //' | tr ' ' '\n' | sort -u | grep -v '^$')

echo "Extracting tags from posts..."  
tags=$(grep -h "^tags:" _posts/*.html | sed 's/tags: //' | tr ' ' '\n' | sort -u | grep -v '^$')

# Generate index pages first
echo "Generating index pages..."
generate_categories_index
generate_tags_index

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
echo "Index pages:"
echo "- categories/index.html (main categories page)"
echo "- tags/index.html (main tags page)"
echo ""
echo "Run 'bundle exec jekyll build' to regenerate your site."