# Enhanced Code Blocks Feature

This document describes the enhanced code block feature added to the blog.

## Overview

Code snippets in Org-mode posts are now enhanced with:
1. **Syntax highlighting** using Prism.js
2. **Line numbers** for easy reference
3. **Copy-to-clipboard button** for convenience
4. **Language badges** showing the programming language

## Implementation

### Files Added/Modified

1. **`_includes/head.html`** - Added Prism.js CSS and custom code-blocks.css
2. **`_layouts/default.html`** - Added Prism.js JavaScript libraries and custom code-blocks.js
3. **`css/code-blocks.css`** - Custom styling for enhanced code blocks
4. **`assets/js/code-blocks.js`** - JavaScript to enhance code blocks with interactivity

### How It Works

1. **Org-mode Export**: When you export Org-mode files, code blocks are wrapped in:
   ```html
   <div class="org-src-container">
     <pre class="src src-{language}">code here</pre>
   </div>
   ```

2. **JavaScript Enhancement**: The `code-blocks.js` script:
   - Detects all `.org-src-container` elements
   - Extracts the language from the `src-{language}` class
   - Wraps code in proper Prism.js markup
   - Adds line numbers via Prism's line-numbers plugin
   - Adds a language badge
   - Adds a copy-to-clipboard button

3. **Prism.js Highlighting**: The Prism.js library applies syntax highlighting automatically using the language identifier.

## Supported Languages

The system supports many programming languages including:
- Python, Bash/Shell
- JavaScript, TypeScript
- Java, C, C++, C#
- Ruby, Go, Rust, PHP
- HTML, XML, CSS
- SQL, JSON, YAML
- Markdown
- Lisp variants (Elisp, Emacs-Lisp, Scheme, Clojure)

And many more via Prism.js's autoloader plugin.

## Usage in Org-mode

To add a code block in your Org-mode post:

```org
#+BEGIN_SRC python
def hello_world():
    print("Hello, World!")
    
hello_world()
#+END_SRC
```

The language identifier (e.g., `python`) will be used for syntax highlighting.

## Features

### Syntax Highlighting
Code is highlighted with appropriate colors for keywords, strings, comments, etc.

### Line Numbers
Each line is numbered on the left side for easy reference.

### Copy Button
- Appears on hover (always visible on mobile)
- Clicking copies the entire code block to clipboard
- Shows "Copied!" feedback after successful copy
- Works in both modern and older browsers

### Language Badge
A small badge in the top-left corner shows the programming language.

## Browser Compatibility

- Modern browsers: Uses Clipboard API
- Older browsers: Falls back to `document.execCommand('copy')`
- Mobile-friendly with responsive design

## Styling

The code blocks use:
- Light gray background (#f8f8f8)
- Subtle border
- Rounded corners
- Proper spacing and padding
- Computer Modern Typewriter font (matching the site's typography)

## Dependencies

External CDN resources (loaded from CDNs):
- Prism.js core (v1.29.0)
- Prism.js autoloader plugin (for language support)
- Prism.js line-numbers plugin

All dependencies are loaded from cdnjs.cloudflare.com for reliability and performance.

## Future Enhancements

Possible improvements:
- Dark theme support
- Customizable color schemes
- Download code as file option
- Code folding for long snippets
