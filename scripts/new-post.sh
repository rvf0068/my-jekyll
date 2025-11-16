#!/bin/bash

# Blog post template generator
# Creates new Org-mode posts with proper front matter

set -e

POSTS_DIR="org/_posts"
DATE=$(date +"%Y-%m-%d")
TIME=$(date +"%H:%M")

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log() {
    echo -e "${GREEN}$1${NC}"
}

info() {
    echo -e "${BLUE}$1${NC}"
}

# Function to create a basic post template
create_basic_post() {
    local title="$1"
    local slug="$2"
    local filename="${DATE}-${slug}.org"
    local filepath="${POSTS_DIR}/${filename}"
    
    cat > "$filepath" << EOF
#+TITLE: $title
#+DATE: $DATE $TIME
#+AUTHOR: rvf0068
#+JEKYLL_LAYOUT: post
#+JEKYLL_CATEGORIES: 
#+JEKYLL_TAGS: 
#+HAS_MATH: no

* Introduction

Write your introduction here.

* Main Content

Your main content goes here.

* Conclusion

Wrap up your post here.
EOF

    log "✅ Created basic post: $filepath"
}

# Function to create a math-heavy post
create_math_post() {
    local title="$1"
    local slug="$2"
    local filename="${DATE}-${slug}.org"
    local filepath="${POSTS_DIR}/${filename}"
    
    cat > "$filepath" << EOF
#+TITLE: $title
#+DATE: $DATE $TIME
#+AUTHOR: rvf0068
#+JEKYLL_LAYOUT: post
#+JEKYLL_CATEGORIES: mathematics
#+JEKYLL_TAGS: math theory
#+HAS_MATH: yes

* Introduction

This post discusses mathematical concepts with equations and proofs.

* Mathematical Content

** Theorem
\begin{equation}
\label{org-main-theorem}
f(x) = \sum_{n=0}^{\infty} \frac{f^{(n)}(a)}{n!}(x-a)^n
\end{equation}

** Proof

The proof follows from... See equation \eqref{org-main-theorem}.

* Conclusion

Mathematical conclusions here.
EOF

    log "✅ Created math post: $filepath"
}

# Function to create a programming post
create_code_post() {
    local title="$1"
    local slug="$2"
    local filename="${DATE}-${slug}.org"
    local filepath="${POSTS_DIR}/${filename}"
    
    cat > "$filepath" << EOF
#+TITLE: $title
#+DATE: $DATE $TIME
#+AUTHOR: rvf0068
#+JEKYLL_LAYOUT: post
#+JEKYLL_CATEGORIES: programming
#+JEKYLL_TAGS: code tutorial
#+HAS_MATH: no
#+HAS_PYTHON_CELLS: yes

* Introduction

This post demonstrates programming concepts with interactive examples.

* Code Examples

** Basic Example

#+BEGIN_SRC python
def hello_world():
    print("Hello, World!")

hello_world()
#+END_SRC

** Interactive Python

#+BEGIN_SRC python-cell
# This creates an interactive cell
import numpy as np

x = np.array([1, 2, 3, 4, 5])
print(f"Array: {x}")
print(f"Mean: {np.mean(x)}")
#+END_SRC

* Conclusion

Programming insights and next steps.
EOF

    log "✅ Created code post: $filepath"
}

# Function to show help
show_help() {
    cat << EOF
📝 Blog Post Template Generator

Usage: ./scripts/new-post.sh <type> "<title>" [slug]

Types:
  basic     Basic blog post template
  math      Math-heavy post with equations  
  code      Programming post with code blocks

Examples:
  ./scripts/new-post.sh basic "My First Post"
  ./scripts/new-post.sh math "Introduction to Calculus" "intro-calculus"
  ./scripts/new-post.sh code "Python Tutorial"

If slug is not provided, it will be generated from the title.
EOF
}

# Function to generate slug from title
generate_slug() {
    echo "$1" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g' | sed 's/--*/-/g' | sed 's/^-\|-$//g'
}

# Main script
if [ $# -lt 2 ]; then
    show_help
    exit 1
fi

TYPE="$1"
TITLE="$2"
SLUG="${3:-$(generate_slug "$TITLE")}"

# Create posts directory if it doesn't exist
mkdir -p "$POSTS_DIR"

info "Creating $TYPE post: '$TITLE'"
info "Slug: $SLUG"

case "$TYPE" in
    "basic")
        create_basic_post "$TITLE" "$SLUG"
        ;;
    "math")
        create_math_post "$TITLE" "$SLUG"
        ;;
    "code")
        create_code_post "$TITLE" "$SLUG"
        ;;
    *)
        echo "Error: Unknown post type '$TYPE'"
        show_help
        exit 1
        ;;
esac

info "📁 Post created in: $POSTS_DIR/${DATE}-${SLUG}.org"
info "🚀 Run './scripts/dev.sh serve' to start development server"