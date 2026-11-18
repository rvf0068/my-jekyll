#!/bin/bash

# Add an Org-mode file outside the blog repository as a Jekyll post.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
POSTS_DIR="$PROJECT_ROOT/org/_posts"

usage() {
    cat <<'EOF'
Usage: ./scripts/add-post.sh <org-file> [slug] [--force] [--today]

Copies an Org-mode file from anywhere on the computer into org/_posts.
Missing Jekyll metadata is added to the copy; the source file is unchanged.

Options:
  --force  Replace an existing post with the same generated filename
  --today  Use today's date for the generated post, overriding its source date
  --help   Show this help

Examples:
  ./scripts/add-post.sh /home/rafael/Dropbox/org/reading-notes/20220209100918-proof_of_nerve_theorem.org
  ./scripts/add-post.sh /path/to/note.org nerve-theorem
  ./scripts/add-post.sh /path/to/note.org --today
EOF
}

get_author() {
    if [ -f "$PROJECT_ROOT/.blog-config" ]; then
        local author
        author="$(sed -n 's/^AUTHOR_NAME="\([^"]*\)".*/\1/p' "$PROJECT_ROOT/.blog-config" | head -n 1)"
        if [ -n "$author" ]; then
            echo "$author"
            return
        fi
    fi
    echo "rvf0068"
}

get_property() {
    local property="$1"
    local file="$2"
    sed -n "s/^#+[[:space:]]*$property:[[:space:]]*//Ip" "$file" | head -n 1
}

slugify() {
    echo "$1" |
        tr '[:upper:]' '[:lower:]' |
        sed 's/[^a-z0-9]/-/g; s/--*/-/g; s/^-\|-$//g'
}

if [ "$#" -eq 0 ]; then
    usage
    exit 1
fi

SOURCE=""
SLUG=""
FORCE=false
TODAY=false

for arg in "$@"; do
    case "$arg" in
        --force)
            FORCE=true
            ;;
        --today)
            TODAY=true
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        -*)
            echo "Error: Unknown option: $arg" >&2
            usage >&2
            exit 1
            ;;
        *)
            if [ -z "$SOURCE" ]; then
                SOURCE="$arg"
            elif [ -z "$SLUG" ]; then
                SLUG="$arg"
            else
                echo "Error: Too many arguments" >&2
                usage >&2
                exit 1
            fi
            ;;
    esac
done

if [ -z "$SOURCE" ]; then
    echo "Error: An Org-mode file is required" >&2
    exit 1
fi

if [ ! -f "$SOURCE" ]; then
    echo "Error: File not found: $SOURCE" >&2
    exit 1
fi

case "$SOURCE" in
    *.org) ;;
    *)
        echo "Error: Source must be an Org-mode (.org) file" >&2
        exit 1
        ;;
esac

SOURCE="$(cd "$(dirname "$SOURCE")" && pwd)/$(basename "$SOURCE")"

TITLE="$(get_property 'TITLE' "$SOURCE")"
if [ -z "$TITLE" ]; then
    TITLE="$(get_property 'title' "$SOURCE")"
fi
if [ -z "$TITLE" ]; then
    TITLE="$(basename "$SOURCE" .org)"
    TITLE="$(printf '%s\n' "$TITLE" | sed -E 's/^[0-9]{14}[-_]//')"
    TITLE="$(printf '%s\n' "$TITLE" | sed 's/[-_]/ /g')"
fi

if [ -z "$SLUG" ]; then
    SLUG="$(slugify "$TITLE")"
fi
SLUG="$(slugify "$SLUG")"

DATE="$(get_property 'DATE' "$SOURCE")"
if [ "$TODAY" = true ]; then
    DATE_PART="$(date +"%Y-%m-%d")"
else
    if [ -z "$DATE" ]; then
        DATE="$(get_property 'date' "$SOURCE")"
    fi

    if [ -n "$DATE" ]; then
        DATE_PART="$(printf '%s\n' "$DATE" | grep -oE '[0-9]{4}-[0-9]{2}-[0-9]{2}' | head -n 1)"
    fi

    if [ -z "${DATE_PART:-}" ]; then
        BASENAME="$(basename "$SOURCE" .org)"
        DATE_PART="$(printf '%s\n' "$BASENAME" | grep -oE '[0-9]{4}[0-1][0-9][0-3][0-9]' | head -n 1)"
        if [ -n "$DATE_PART" ]; then
            DATE_PART="$(printf '%s' "$DATE_PART" | sed 's/\([0-9]\{4\}\)\([0-9]\{2\}\)\([0-9]\{2\}\)/\1-\2-\3/')"
        fi
    fi

    if [ -z "${DATE_PART:-}" ]; then
        DATE_PART="$(date -r "$SOURCE" +"%Y-%m-%d")"
    fi
fi

AUTHOR="$(get_author)"
mkdir -p "$POSTS_DIR"
DESTINATION="$POSTS_DIR/$DATE_PART-$SLUG.org"

if [ -e "$DESTINATION" ] && [ "$FORCE" != true ]; then
    echo "Error: Destination already exists: $DESTINATION" >&2
    echo "Use --force to replace it." >&2
    exit 1
fi

TEMP="$(mktemp)"
trap 'rm -f "$TEMP"' EXIT

{
    if [ -z "$(get_property 'TITLE' "$SOURCE")" ] && [ -z "$(get_property 'title' "$SOURCE")" ]; then
        echo "#+TITLE: $TITLE"
    fi
    if [ "$TODAY" = true ] || { [ -z "$(get_property 'DATE' "$SOURCE")" ] && [ -z "$(get_property 'date' "$SOURCE")" ]; }; then
        echo "#+DATE: $DATE_PART"
    fi
    if [ -z "$(get_property 'AUTHOR' "$SOURCE")" ] && [ -z "$(get_property 'author' "$SOURCE")" ]; then
        echo "#+AUTHOR: $AUTHOR"
    fi
    if [ -z "$(get_property 'JEKYLL_LAYOUT' "$SOURCE")" ]; then
        echo "#+JEKYLL_LAYOUT: post"
    fi
    if [ -z "$(get_property 'HAS_MATH' "$SOURCE")" ]; then
        echo "#+HAS_MATH: yes"
    fi
    echo ""
    if [ "$TODAY" = true ]; then
        sed '/^#+[[:space:]]*DATE:/Id' "$SOURCE"
    else
        cat "$SOURCE"
    fi
} > "$TEMP"

cp "$TEMP" "$DESTINATION"
rm -f "$TEMP"
trap - EXIT

echo "Added post: $DESTINATION"
echo "Source unchanged: $SOURCE"
