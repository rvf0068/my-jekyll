#!/bin/bash

# Comprehensive development workflow script
# Usage: ./scripts/dev.sh [command]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${GREEN}[$(date +'%H:%M:%S')] $1${NC}"
}

warn() {
    echo -e "${YELLOW}[$(date +'%H:%M:%S')] ⚠️  $1${NC}"
}

error() {
    echo -e "${RED}[$(date +'%H:%M:%S')] ❌ $1${NC}"
}

# Get baseurl from .blog-config file
get_baseurl() {
    if [ -f ".blog-config" ]; then
        grep "^SITE_BASEURL=" .blog-config | cut -d'"' -f2
    else
        echo "/"  # fallback to root
    fi
}

# Function to check prerequisites
check_prereqs() {
    log "🔍 Checking prerequisites..."
    
    if ! command -v emacs &> /dev/null; then
        error "Emacs not found. Please install Emacs."
        exit 1
    fi
    
    if ! command -v bundle &> /dev/null; then
        error "Bundler not found. Please install Ruby and Bundler."
        exit 1
    fi
    
    if [ ! -f "Gemfile" ]; then
        error "Gemfile not found. Are you in the project root?"
        exit 1
    fi
    
    log "✅ Prerequisites check passed"
}

# Function to install dependencies
install_deps() {
    log "📦 Installing dependencies..."
    bundle install
    log "✅ Dependencies installed"
}

# Function to build org files
build_org() {
    log "📝 Converting Org files to HTML..."
    # Ensure config is up to date before publishing
    if [ ! -f "_config.yml" ] || [ ".blog-config" -nt "_config.yml" ] || [ "scripts/generate-config.sh" -nt "_config.yml" ]; then
        log "🔄 Generating _config.yml from .blog-config..."
        ./scripts/generate-config.sh
    fi
    ./scripts/emacs_headless_publish.sh
    log "✅ Org files converted"
}

# Function to generate category/tag pages
build_tags() {
    log "🏷️  Generating category and tag pages..."
    ./scripts/create_tag_pages.sh
    log "✅ Category and tag pages generated"
}

# Function to add an Org-mode file from anywhere on the computer
add_post() {
    ./scripts/add-post.sh "$@"
}

# Function to build Jekyll site
build_jekyll() {
    log "🏗️  Building Jekyll site..."
    bundle exec jekyll build
    log "✅ Jekyll build complete"
}

# Function to serve the site
serve() {
    BASEURL=$(get_baseurl)
    log "🚀 Starting development server..."
    log "📡 Site will be available at: http://localhost:4000${BASEURL}/"
    bundle exec jekyll serve --host localhost --port 4000 --livereload --incremental
}

# Function to do a complete build
build() {
    log "🔨 Starting complete build process..."
    build_org
    build_tags
    build_jekyll
    log "🎉 Build complete! Site is ready in _site/"
}

# Function to clean build artifacts
clean() {
    log "🧹 Cleaning build artifacts..."
    
    if [ -d "_site" ]; then
        rm -rf _site
        log "🗑️  Removed _site/"
    fi
    
    if [ -d ".jekyll-cache" ]; then
        rm -rf .jekyll-cache
        log "🗑️  Removed .jekyll-cache/"
    fi
    
    if [ -d ".sass-cache" ]; then
        rm -rf .sass-cache
        log "🗑️  Removed .sass-cache/"
    fi
    
    log "✅ Clean complete"
}

# Function to show help
help() {
    cat << EOF
🚀 Jekyll Blog Development Tool

Usage: ./scripts/dev.sh [command]

Commands:
  help      Show this help message
  install   Install dependencies
  build     Full build (org → HTML → Jekyll)
  serve     Start development server with live reload
  clean     Clean build artifacts
  org       Convert Org files to HTML only
  tags      Generate category/tag pages only
  jekyll    Build Jekyll site only
  add       Add an external Org-mode file as a post

Examples:
  ./scripts/dev.sh install     # Install dependencies
  ./scripts/dev.sh build       # Complete build
  ./scripts/dev.sh serve       # Start dev server
  ./scripts/dev.sh clean build # Clean then build
  ./scripts/dev.sh add /path/to/note.org

📚 More info: See README.md for detailed documentation
EOF
}

# Main command dispatcher
case "${1:-help}" in
    "install")
        check_prereqs
        install_deps
        ;;
    "build")
        check_prereqs
        build
        ;;
    "serve")
        check_prereqs
        log "🔄 Building before serving..."
        build_org
        build_tags
        serve
        ;;
    "clean")
        clean
        ;;
    "org")
        check_prereqs
        build_org
        ;;
    "tags")
        build_tags
        ;;
    "jekyll")
        check_prereqs
        build_jekyll
        ;;
    "add")
        shift
        add_post "$@"
        ;;
    "help"|*)
        help
        ;;
esac