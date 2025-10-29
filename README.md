# My Jekyll Blog with Org-Mode

This is a Jekyll-based blog that uses Org-mode for authoring posts. The setup automatically converts Org files to HTML with Jekyll front matter.

## Features

- Write blog posts in Org-mode syntax
- Automatic Jekyll front matter generation from Org properties
- Support for LaTeX math content with MathJax
- **Enhanced code blocks** with syntax highlighting, line numbers, and copy-to-clipboard button
- Customizable layouts and themes

## Writing Posts

### Using Org Properties (Recommended)

The easiest way to create a new post is to use standard Org-mode properties. The Jekyll front matter will be generated automatically during the publishing process.

Create a new file in `org/_posts/` with the naming convention: `YYYY-MM-DD-title.org`

Example post structure:

```org
#+TITLE: Your Post Title
#+DATE: 2025-10-29 17:00:00 -0500
#+AUTHOR: Your Name
#+JEKYLL_LAYOUT: post
#+JEKYLL_CATEGORIES: category1 category2
#+JEKYLL_TAGS: tag1 tag2 tag3

* Introduction

Your content goes here...

* Main Content

More content...
```

#### Supported Properties

- `#+TITLE:` - The title of your post (required)
- `#+DATE:` - Post date in format `YYYY-MM-DD HH:MM:SS TIMEZONE` (required)
- `#+AUTHOR:` - Author name (optional)
- `#+JEKYLL_LAYOUT:` - Layout template (defaults to "post")
- `#+JEKYLL_CATEGORIES:` - Space-separated list of categories
- `#+JEKYLL_TAGS:` - Space-separated list of tags
- `#+FILETAGS:` - Alternative way to specify tags using Org's native filetags

### Manual Front Matter (Legacy)

You can also manually write the Jekyll front matter using `#+BEGIN_EXPORT html` blocks:

```org
#+BEGIN_EXPORT html
---
layout: post
title: "Your Post Title"
date: 2025-10-29 17:00:00 -0500
categories: category1 category2
tags: tag1 tag2
---
#+END_EXPORT

Your content goes here...
```

## Publishing

To publish your Org files to HTML:

```bash
bash scripts/emacs_headless_publish.sh
```

This will:
1. Convert all `.org` files in `org/_posts/` to `.html` files in `_posts/`
2. Automatically generate Jekyll front matter from Org properties
3. Process any resources (images, etc.) to the `assets/` directory

## Project Structure

```
.
├── org/                    # Source Org files
│   └── _posts/            # Blog posts in Org format
├── _posts/                # Generated HTML posts with front matter
├── _layouts/              # Jekyll layout templates
├── _includes/             # Jekyll includes
├── assets/                # Static assets (images, CSS, JS)
├── scripts/               # Build and utility scripts
├── org_publish.el         # Emacs publishing configuration
└── _config.yml           # Jekyll configuration
```

## How It Works

The `org_publish.el` file contains Emacs Lisp code that:

1. Configures the Org-mode HTML exporter
2. Defines custom export options for Jekyll-specific properties
3. Implements a filter function that:
   - Extracts metadata from Org properties
   - Generates Jekyll front matter
   - Prepends it to the HTML output
   - Fixes any baseurl-related link issues

The front matter is automatically generated only for files in the `_posts/` directory and only when it's not already present.

## Configuration

### Jekyll Base URL

The baseurl is configured in `org_publish.el`:

```elisp
(setq jekyll-baseurl "/my-jekyll")
```

Update this to match your Jekyll site's baseurl setting.

### Publishing Projects

The publishing configuration defines three projects:
- `main-site`: Publishes Org files from `org/` to the root directory
- `posts`: Publishes blog posts from `org/_posts/` to `_posts/`
- `resources`: Copies resources to the `assets/` directory

## Requirements

- Emacs (tested with version 29.3)
- Org-mode (comes with Emacs)
- Jekyll (for serving the site)

## License

[Add your license here]
