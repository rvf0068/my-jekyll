# My Jekyll Blog with Org-Mode

This is a Jekyll-based blog that uses Org-mode for authoring posts. The setup automatically converts Org files to HTML with Jekyll front matter.

## ⚠️ Important Configuration Warning

**DO NOT EDIT `_config.yml` DIRECTLY.**

This file is auto-generated from `.blog-config` during the build process. Any changes made directly to `_config.yml` will be overwritten.

To change site configuration:
1. Edit `.blog-config`
2. Run `./scripts/dev.sh build` (or `./scripts/generate-config.sh`)

## Features

- Write blog posts in Org-mode syntax
- Automatic Jekyll front matter generation from Org properties
- Support for LaTeX math content with MathJax
- **Automatic equation referencing** with numbered links for LaTeX equations (requires specific formatting, see below)
- **Enhanced code blocks** with syntax highlighting (Prism.js), line numbers, copy-to-clipboard button, and language badges
- **Interactive Python cells** powered by Pyodide - run Python code directly in the browser
- **GitHub Discussions-powered comments** with Giscus integration
- Customizable layouts and themes

## Creating Standalone Pages

Not everything belongs in the regular post stream. Use the `page` layout to add evergreen sections such as “About”, “Work with me”, or “Now”. The recommended flow keeps Org as the single source of truth and exports each page into the `pages/` directory that Jekyll reads.

1. Create an Org file under `org/_pages/`, e.g., `org/_pages/about-this-blog.org`. Wrap the front matter in an HTML export block—`#+BEGIN_EXPORT html` / `#+END_EXPORT`—just like `org/index.org` does:

  ```org
  #+BEGIN_EXPORT html
  ---
  layout: page
  title: "Example Page"
  permalink: /example/
  description: "Optional short summary for meta tags."
  ---
  #+END_EXPORT
  ```

2. Add the body of your page using Org headings, lists, and special blocks. See `org/_pages/about-this-blog.org` for a ready-to-edit template.
3. Run the publisher so the Org source is converted into `pages/example.html`:

  ```bash
  ./scripts/dev.sh build
  ```

4. Link to the page from `org/index.org`, `_includes/header.html`, or anywhere else using `{{ site.baseurl }}/example/`.

The shared `page` layout lives in `_layouts/page.html`, so every standalone page gets consistent typography and spacing without duplicating markup. If you truly need a hand-written Markdown/HTML page, drop it directly under `pages/`, but remember that Org sources in `org/_pages/` are exported automatically during each build.

## Writing Posts

### Using Org Properties (Recommended)

The easiest way to create a new post is to use standard Org-mode properties. The Jekyll front matter will be generated automatically during the publishing process.

Create a new file in `org/_posts/` with the naming convention: `YYYY-MM-DD-title.org`

Example post structure:

```org
#+TITLE: Your Post Title
#+DATE: 2025-12-01 12:00:00 -0500
#+AUTHOR: Your Name
#+JEKYLL_LAYOUT: post
#+JEKYLL_CATEGORIES: category1 category2
#+JEKYLL_TAGS: tag1 tag2 tag3
#+HAS_MATH: yes
#+HAS_PYTHON_CELLS: no

* Introduction

Your content goes here...

* Main Content

More content...
```

### Performance Optimization

The blog supports conditional loading of heavy JavaScript libraries to improve page load performance:

#### MathJax (Mathematical Content)
- **Default**: MathJax loads automatically for backward compatibility
- **To disable**: Add `#+HAS_MATH: no` to skip MathJax loading (~150KB saved)
- **Use case**: Text-only posts, programming tutorials, or any content without mathematical formulas

#### Pyodide (Interactive Python)
- **Default**: Pyodide does not load (saves ~6MB)
- **To enable**: Add `#+HAS_PYTHON_CELLS: yes` to load Pyodide for interactive Python execution
- **Use case**: Posts with `python-cell` source blocks for live code execution

#### Examples

**Lightweight text post:**
```org
#+TITLE: Simple Blog Post
#+DATE: 2025-12-01
#+HAS_MATH: no
```
→ Fastest loading, no heavy libraries

**Math-heavy post:**
```org
#+TITLE: Advanced Calculus Tutorial
#+DATE: 2025-12-01
#+HAS_MATH: yes
```
→ Loads MathJax for equation rendering

**Interactive Python post:**
```org
#+TITLE: Python Data Analysis Demo
#+DATE: 2025-12-01
#+HAS_PYTHON_CELLS: yes
```
→ Loads Pyodide for live Python execution

**Full-featured post:**
```org
#+TITLE: Complete Tutorial
#+DATE: 2025-12-01
#+HAS_MATH: yes
#+HAS_PYTHON_CELLS: yes
```
→ Loads both MathJax and Pyodide

### Figure and Table Labeling

Org-mode provides native support for labeling and referencing figures and tables, which works both for HTML export (blog) and LaTeX export:

#### Figures

To label a figure, use the `#+NAME:` directive followed by `#+CAPTION:` before the image link:

```org
#+ATTR_HTML: :width 400 :alt description :align center
#+ATTR_LATEX: :width 0.5\textwidth
#+NAME: fig:my-figure
#+CAPTION: Description of my figure
[[file:image.png]]
```

Then reference it in your text using a standard org link:

```org
See Figure [[fig:my-figure]] for details.
```

The figure will be automatically numbered (e.g., "Figure 1", "Figure 2") and the reference will become a clickable link to that figure.

#### Tables

Tables work the same way:

```org
#+NAME: tab:my-table
#+CAPTION: Description of my table
| Column 1 | Column 2 |
|----------+----------|
| Data 1   | Data 2   |
| Data 3   | Data 4   |
```

Reference it with:

```org
Table [[tab:my-table]] shows the results.
```

#### Key Features

- **Automatic numbering**: Figures and tables are numbered sequentially (Figure 1, Figure 2, Table 1, Table 2)
- **Clickable links**: References become HTML anchors that navigate to the figure/table
- **LaTeX compatibility**: The same syntax works when exporting to LaTeX/PDF
- **Multiple references**: You can reference the same figure or table multiple times
- **Inline figures**: Multiple figures can be included in the same section with different attributes

#### Example

```org
* Results

#+NAME: fig:plot1
#+CAPTION: Sample data visualization
[[file:plot.png]]

#+NAME: tab:results
#+CAPTION: Numerical results
| Method | Accuracy |
|--------+----------|
| A      | 95%      |
| B      | 92%      |

As shown in Figure [[fig:plot1]], the trend is clear. The detailed values
in Table [[tab:results]] confirm this pattern.
```

### Equation Referencing

The system automatically processes LaTeX equation labels and references. **Note: This feature relies on specific formatting.**

- When you write equations with `\label{org...}` tags, they are converted to HTML anchors placed **before** the equation block
- Equation numbers are displayed flush-right inside equations using MathJax's `\tag{}` command (like in textbooks)
- When you use `\eqref{org...}` to reference equations, they are converted to clickable links with numbered labels
- Equations are numbered sequentially in the order they appear in the document
- The same equation can be referenced multiple times, always showing the same number

Example:

```org
\begin{equation}
\label{org1234567}
E = mc^2
\end{equation}

This is Einstein's famous equation \eqref{org1234567}.
```

This will be converted to HTML with:
- An anchor placed before the equation: `<span id="org1234567"></span>`
- The equation with a number tag: `\begin{equation}\tag{1} E = mc^2 \end{equation}`
- A clickable link: `<a href="#org1234567">(1)</a>`

**Important Notes:**
- Org-mode automatically generates `\label{orgXXXXXX}` tags for equations during export, so you can use `\eqref{orgXXXXXX}` to reference them. The exact label ID is generated by org-mode.
- The anchor is placed **outside** the equation environment to ensure MathJax can properly render the equation (HTML tags inside LaTeX blocks break rendering).
- **You must use `\label{org...}` exactly as shown.** Other label formats may not be processed correctly by the custom Elisp filter.

### Mathematical Special Blocks

The site includes automatic styling for mathematical special blocks commonly used in academic writing. These blocks are rendered with distinctive visual styling:

#### Definition Blocks

```org
#+begin_definition
A *group* is a set \(G\) together with a binary operation \(\cdot: G \times G \to G\) satisfying:
1. *Closure*: For all \(a, b \in G\), we have \(a \cdot b \in G\)
2. *Associativity*: \((a \cdot b) \cdot c = a \cdot (b \cdot c)\)
#+end_definition
```

Renders with:
- Bold "Definition" header on its own line
- Content in italics (including lists)
- Blue left border for visual distinction

#### Theorem, Lemma, Corollary, and Proposition Blocks

```org
#+begin_theorem
*(Lagrange's Theorem)* Let \(G\) be a finite group and \(H\) a subgroup of \(G\).
Then the order of \(H\) divides the order of \(G\).
#+end_theorem

#+begin_lemma
A non-empty subset \(H\) is a subgroup if and only if \(ab^{-1} \in H\).
#+end_lemma

#+begin_corollary
If \(G\) is a group of prime order \(p\), then \(G\) is cyclic.
#+end_corollary

#+begin_proposition
Every finite group has at least one element of prime order.
#+end_proposition
```

Each renders with:
- Bold header ("Theorem", "Lemma", "Corollary", or "Proposition") on its own line
- Optional theorem name (in bold within the content) displayed on the same line as the statement
- Content in normal font (not italics)
- Distinct colored left borders (orange for theorem, teal for lemma, purple for corollary, green for proposition)

#### Proof Blocks

```org
#+begin_proof
Consider the left cosets of \(H\) in \(G\). Each coset has exactly \(|H|\) elements.
Thus \(|H|\) divides \(|G|\).
#+end_proof
```

Renders with:
- Italic "Proof:" header on its own line
- Content in normal font
- Black square symbol (■) automatically added at the end
- Gray left border

**Note:** If your proof already contains `\qed` or another end marker, you may see two symbols. The styling automatically adds the square symbol for consistency.

#### Example and Remark Blocks

```org
#+begin_exampleblock
Let \(G = \mathbb{Z}_6\). The subgroups are \(\{0\}, \{0, 3\}, \{0, 2, 4\}, \mathbb{Z}_6\).
#+end_exampleblock

#+begin_remark
Note that the converse of Lagrange's Theorem is false in general.
The group \(A_4\) has order 12, but has no subgroup of order 6.
#+end_remark
```

Each renders with:
- Bold header ("Example" or "Remark") on its own line
- Content in normal font
- Distinct colored left borders (yellow/gold for example, blue-gray for remark)
- Automatic numbering (e.g., "Example 1", "Remark 1")

### Interactive Python Cells with Pyodide

This blog supports interactive Python code execution directly in the browser using Pyodide. You can create executable Python cells using the special `python-cell` source block type.

#### Basic Usage

To create an interactive Python cell, use `#+begin_src python-cell` instead of the regular `#+begin_src python`:

```org
#+begin_src python-cell
2 + 2
#+end_src
```

This will automatically generate an interactive cell with:
- A text area containing the code
- A "Run" button to execute the code
- An output area to display results

#### How It Works

The export filter automatically:
1. Detects `python-cell` source blocks during export
2. Generates unique IDs for each cell
3. Creates HTML with textarea, button, and output elements
4. Adds Pyodide initialization code (loaded once per page)
5. Sets up JavaScript to handle code execution

#### Matplotlib Support

The filter automatically detects when your code uses matplotlib and generates appropriate HTML for image output. Any code containing `plt.show()`, `plt.plot()`, or other matplotlib plotting functions will display images instead of text output:

```org
#+begin_src python-cell
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 2*np.pi, 100)
y = np.sin(x)

plt.plot(x, y)
plt.title("Sine Wave")
plt.xlabel("x")
plt.ylabel("sin(x)")
plt.grid(True)
plt.show()
#+end_src
```

When matplotlib is detected, the system:
- Loads matplotlib and numpy via micropip
- Configures matplotlib to use the AGG backend
- Captures the plot as a PNG image
- Displays it as a base64-encoded image in the output area
- Clears the plot after rendering (using `plt.clf()`)

#### Example with Sympy

You can also use Sympy for symbolic mathematics:

```org
#+begin_src python-cell
import sympy as sp
x = sp.symbols('x')
expr = sp.expand((x + 2)**3)
expr
#+end_src
```

The Sympy package will be loaded automatically when your code imports it.

#### Features

- **No manual HTML/JavaScript required** - just write Python code in org-mode
- **Automatic package loading** - matplotlib, numpy, and sympy are loaded as needed
- **Multiple cells per page** - each cell has unique IDs and independent state
- **Editable code** - users can modify and re-run the code in their browser
- **Fast execution** - Pyodide compiles to WebAssembly for near-native performance

#### Complete Demonstration

Create posts with Python cells to demonstrate interactive computations, Sympy symbolic math, and matplotlib plotting capabilities.

### Citations and Bibliography

This blog uses Org-mode's native citation system (org-cite) to manage references. Citations are automatically processed during export and a bibliography is generated from the cited works.

#### Setting up Citations

1. Create or use the `references.bib` file in the root directory with your BibTeX entries
2. Add a bibliography reference to your Org file header:

```org
#+TITLE: Your Post Title
#+BIBLIOGRAPHY: ../../references.bib
```

3. Use the `[cite:@key]` syntax to cite works in your text:

```org
This is supported by recent research [cite:@authorkey2020].
Multiple citations can be combined [cite:@key1;@key2;@key3].
```

4. Add the bibliography print directive where you want the reference list to appear:

```org
* References

#+print_bibliography:
```

#### Example

```org
#+TITLE: Research Post with Citations
#+DATE: 2025-12-01
#+BIBLIOGRAPHY: ../../references.bib

* Introduction

Poset topology has been studied extensively [cite:@quillen1978homotopy].
Further developments were made [cite:@bjorner1995topology].

* Conclusion

This demonstrates the citation system.

#+print_bibliography:
```

The bibliography will be automatically generated from the cited works, formatted with author names, year, and publication details.

## Development Scripts

This project includes several utility scripts to streamline the development workflow.

### dev.sh - Comprehensive Development Tool

The `./scripts/dev.sh` script provides a unified interface for all development tasks:

```bash
# Show help and available commands
./scripts/dev.sh help

# Install dependencies
./scripts/dev.sh install

# Complete build process (Org → HTML → Jekyll)
./scripts/dev.sh build

# Start development server with live reload
./scripts/dev.sh serve

# Clean build artifacts
./scripts/dev.sh clean

# Individual build steps
./scripts/dev.sh org     # Convert Org files only
./scripts/dev.sh tags    # Generate category/tag pages only
./scripts/dev.sh jekyll  # Build Jekyll site only
```

#### Features

- **Prerequisites checking**: Verifies Emacs, Bundler, and project structure
- **Colored output**: Clear visual feedback with timestamps
- **Error handling**: Stops on first error with clear messages
- **Incremental builds**: Option to run individual build steps
- **Live reload**: Development server with automatic page refresh
- **Clean builds**: Remove all generated files and caches

#### Typical Workflow

```bash
# First time setup
./scripts/dev.sh install

# Daily development
./scripts/dev.sh serve    # Builds and starts dev server

# Manual builds
./scripts/dev.sh clean build
```

### new-post.sh - Post Template Generator

The `./scripts/new-post.sh` script creates properly formatted Org-mode posts with all necessary front matter:

```bash
# Create a basic blog post
./scripts/new-post.sh basic "My First Post"

# Create a math-heavy post
./scripts/new-post.sh math "Introduction to Calculus" "intro-calculus"

# Create a programming tutorial
./scripts/new-post.sh code "Python Data Analysis"
```

#### Post Types

**Basic Post Template** (`basic`):
- Simple structure with Introduction, Main Content, Conclusion
- Minimal front matter for general blog posts
- No special library loading

**Math Post Template** (`math`):
- Pre-configured with `#+HAS_MATH: yes` for MathJax loading
- Example theorem/proof structure with LaTeX equations
- Mathematics category and tags
- Equation numbering and referencing examples

**Code Post Template** (`code`):
- Includes `#+HAS_PYTHON_CELLS: yes` for interactive Python
- Example code blocks and interactive Python cells
- Programming category and tags
- Mix of syntax-highlighted and executable code

#### Features

- **Automatic date/time**: Uses current date and time in proper format
- **Slug generation**: Converts titles to URL-friendly slugs automatically
- **Custom slugs**: Optional third parameter to override auto-generated slug
- **Proper front matter**: All required Org properties pre-filled
- **Category defaults**: Sensible defaults based on post type
- **Ready to edit**: Opens with example content to guide structure

#### Examples

```bash
# Auto-generated slug
./scripts/new-post.sh basic "Understanding Machine Learning"
# Creates: YYYY-MM-DD-understanding-machine-learning.org

# Custom slug
./scripts/new-post.sh math "Advanced Topology" "topology-advanced"
# Creates: YYYY-MM-DD-topology-advanced.org

# Code tutorial
./scripts/new-post.sh code "NumPy Tutorial"
# Creates: YYYY-MM-DD-numpy-tutorial.org
```

After creating a post, run `./scripts/dev.sh serve` to start the development server and preview your new post.

### Enhanced Code Blocks

Code snippets in Org-mode posts are automatically enhanced with:

1. **Syntax highlighting** using Prism.js - supports Python, JavaScript, Bash, Java, C/C++, Ruby, Go, Rust, PHP, HTML, CSS, SQL, JSON, YAML, Markdown, Lisp variants, and many more
2. **Line numbers** for easy reference
3. **Copy-to-clipboard button** - appears on hover (always visible on mobile), shows "Copied!" feedback
4. **Language badges** showing the programming language in the top-left corner

#### Usage

Simply write code blocks in standard Org-mode syntax:

```org
#+BEGIN_SRC python
def hello_world():
    print("Hello, World!")

hello_world()
#+END_SRC
```

The enhancement happens automatically during export. The language identifier (e.g., `python`) is used for syntax highlighting.

#### Implementation Details

- **JavaScript Enhancement**: The `assets/js/code-blocks.js` script detects `.org-src-container` elements and wraps them in proper Prism.js markup
- **Styling**: Custom styling in `css/code-blocks.css` with Computer Modern Typewriter font
- **Browser Compatibility**: Uses modern Clipboard API with fallback to `document.execCommand('copy')` for older browsers
- **Dependencies**: Prism.js core, autoloader, and line-numbers plugins loaded from cdnjs.cloudflare.com

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
├── _config.yml                    # Jekyll configuration (AUTO-GENERATED)
├── .blog-config                   # Primary configuration file
├── _includes/                     # Jekyll template includes
│   ├── footer.html                # Site footer template
│   ├── head.html                  # HTML head with scripts/CSS
│   └── header.html                # Site header with navigation
├── _layouts/                      # Jekyll layout templates
│   ├── default.html               # Base site layout
│   └── post.html                  # Blog post layout
├── _posts/                        # Generated HTML posts (auto-generated)
├── assets/                        # Static assets
│   ├── _posts/                    # Post-specific assets
│   └── js/                        # JavaScript files
│       ├── code-blocks.js         # Code enhancement functionality
│       ├── dark-mode.js           # Dark mode toggle
│       └── search.js              # Site search functionality
├── categories/                    # Category pages
│   └── index.html                 # Category listing page
├── css/                          # Stylesheets
│   ├── app.css                   # Main application styles
│   ├── code-blocks.css           # Code block styling
│   ├── math-blocks.css           # Mathematical content styling
│   ├── tables.css                # Table formatting
│   └── vendor/                   # Third-party CSS
│       ├── reset.css             # CSS reset
│       └── syntax.css            # Syntax highlighting
├── Gemfile                       # Ruby dependencies
├── index.html                    # Site homepage
├── org/                          # Source Org files
│   ├── _drafts/                  # Draft posts
│   ├── _posts/                   # Blog posts in Org format
│   └── index.org                 # Homepage source
├── org_publish.el                # Emacs publishing configuration
├── references.bib                # Bibliography database
├── scripts/                      # Development and build scripts
│   ├── create_tag_pages.sh       # Generate category/tag pages
│   ├── dev.sh                    # Main development script
│   ├── emacs_headless_publish.sh # Org-to-HTML conversion
│   └── new-post.sh               # Post template generator
└── tags/                         # Tag pages
    └── index.html                # Tag listing page
```

## How It Works

The `org_publish.el` file contains Emacs Lisp code that:

1. Configures the Org-mode HTML exporter
2. Enables org-cite for native citation support with BibTeX files
3. Defines custom export options for Jekyll-specific properties
4. Implements filter functions that:
   - **Convert `python-cell` source blocks** to interactive Pyodide cells with proper HTML/JavaScript
   - **Detect matplotlib usage** and generate image-based output for plots
   - Process LaTeX equation labels (`\label{org...}`) and references (`\eqref{org...}`)
   - Place HTML anchors **before** equation blocks (not inside them, to avoid breaking MathJax rendering)
   - Add `\tag{n}` inside equations to display equation numbers flush-right (like textbooks)
   - Convert equation references to clickable links with numbered labels
   - Convert LaTeX special characters (e.g., `{\"o}` → `ö`) to proper HTML entities
   - Extract metadata from Org properties
   - Generate Jekyll front matter
   - Prepend it to the HTML output
   - Fix any baseurl-related link issues

The front matter is automatically generated only for files in the `_posts/` directory and only when it's not already present.

Citations are processed by Org-mode's built-in `org-cite` system which:
- Parses `[cite:@key]` references in the text
- Looks up entries in the specified BibTeX file
- Generates inline citations in (Author, Year) format
- Creates a formatted bibliography at the `#+print_bibliography:` location

## Configuration

This blog uses a **shared configuration system** that eliminates hard-coded parameters and enables easy customization.

### How It Works

1. **Single source of truth**: All site configuration lives in `.blog-config`
2. **Auto-generation**: `_config.yml` is generated from `.blog-config` automatically
3. **Cross-tool compatibility**: Both Jekyll and Emacs Lisp read the same configuration
4. **GitHub Actions ready**: Deployment workflow generates config automatically

**Important**: Never edit `_config.yml` directly! All changes should be made in `.blog-config`, then run `./scripts/generate-config.sh` to regenerate the Jekyll configuration.

### Configuration Files

#### .blog-config (Primary Configuration)
```bash
# Site Identity
SITE_TITLE="My Org-Jekyll Blog"
SITE_DESCRIPTION="A blog powered by Org-mode and Jekyll"
SITE_URL="https://username.github.io"
SITE_BASEURL="/repository-name"

# Author Information
AUTHOR_NAME="username"
GITHUB_USERNAME="username"

# Development Settings
DEV_HOST="localhost"
DEV_PORT="4000"

# Feature Flags
ENABLE_MATHJAX="true"
ENABLE_SEARCH="true"
ENABLE_DARK_MODE="true"

# Jekyll Excludes (Space-separated list)
JEKYLL_EXCLUDES="scripts/ .github/ vendor/ Gemfile Gemfile.lock README.md .blog-config org/_posts/ org/index.org **/*.pdf"
```

#### Generated Files (Auto-created)
- `_config.yml` - Generated by `scripts/generate-config.sh`
- Templates use `{{site.github_username}}` instead of hard-coded values

### Local Development

```bash
# Edit your configuration
emacs .blog-config

# Build and serve (auto-generates _config.yml)
./scripts/dev.sh serve
```

### GitHub Actions Deployment

The included GitHub Actions workflow automatically:

1. **Generates `_config.yml`** from `.blog-config`
2. **Builds Org files** using the shared configuration
3. **Builds and deploys** Jekyll site to GitHub Pages

No additional setup required - just push to deploy!

### Technical Implementation

#### Cross-Tool Configuration Reading
**Emacs Lisp** (in `org_publish.el`):
```elisp
(defun read-blog-config (key)
  "Read configuration value for KEY from .blog-config file."
  (when (file-exists-p ".blog-config")
    (with-temp-buffer
      (insert-file-contents ".blog-config")
      (goto-char (point-min))
      (when (re-search-forward (concat "^" key "=\"\([^\"]*\)\"") nil t)
	(match-string 1)))))

(setq jekyll-baseurl (or (read-blog-config "SITE_BASEURL") "/"))
```

**Bash** (in `scripts/generate-config.sh`):
```bash
# Source the configuration and generate _config.yml
source ".blog-config"
cat > "_config.yml" << EOF
title: ${SITE_TITLE}
baseurl: "${SITE_BASEURL}"
# ... rest of config
EOF
```

#### Build Process Integration
- **`dev.sh`**: Auto-regenerates config when `.blog-config` is newer than `_config.yml`
- **`new-post.sh`**: Reads author from `.blog-config` for post templates
- **Templates**: Use Jekyll variables like `{{site.github_username}}` instead of hard-coded values

### Multi-Site Support

Each blog directory maintains its own `.blog-config`, enabling:
- Multiple blogs on the same machine without conflicts
- Easy template sharing and customization
- Simple deployment to different GitHub repositories

### File Management

**Tracked in Git:**
- ✅ `.blog-config` (source configuration)
- ✅ `scripts/generate-config.sh` (generation script)
- ✅ Templates with variables

**Auto-generated (excluded from Git):**
- 🔧 `_config.yml` (generated from `.blog-config`)
- 🔧 `_posts/*.html` (generated from `org/_posts/*.org`)
- 🔧 Category/tag pages

### Publishing Projects

The publishing configuration defines three projects:
- `main-site`: Publishes Org files from `org/` to the root directory
- `posts`: Publishes blog posts from `org/_posts/` to `_posts/`
- `resources`: Copies resources to the `assets/` directory

## Comments System

This blog uses **Giscus** for comments, which provides a modern, privacy-friendly commenting system powered by GitHub Discussions.

### Features

- 🗨️ **GitHub Discussions-based**: Comments stored as GitHub Discussions
- 🔒 **Privacy-friendly**: No tracking, no ads, open source
- 💬 **Rich formatting**: Full Markdown support with syntax highlighting
- ⚡ **Real-time**: Instant updates and notifications
- 🎨 **Theme integration**: Automatically matches your site's appearance
- 📱 **Responsive**: Works on all devices
- 🔍 **Searchable**: Comments are indexed by GitHub

### Setup (Already Configured)

The blog is configured with the following Giscus settings:
- **Repository**: `rvf0068/my-jekyll`
- **Category**: "General" (GitHub Discussions category)
- **Mapping**: `pathname` (each page gets its own discussion)
- **Theme**: `dark` (can be changed in `_config.yml`)

### Configuration

Comments are configured in `.blog-config` (the primary configuration file):

```bash
# Giscus Comments Configuration
GISCUS_REPO="rvf0068/my-jekyll"
GISCUS_REPO_ID="R_kgDOP75IWQ"
GISCUS_CATEGORY="General"
GISCUS_CATEGORY_ID="DIC_kwDOP75IWc4Cx5N8"
GISCUS_MAPPING="pathname"
GISCUS_STRICT="1"
GISCUS_REACTIONS_ENABLED="1"
GISCUS_EMIT_METADATA="0"
GISCUS_INPUT_POSITION="bottom"
GISCUS_THEME="dark"
GISCUS_LANG="en"
```

**Note:** After modifying `.blog-config`, run `./scripts/generate-config.sh` to regenerate `_config.yml` with the new settings.

### Setting up Giscus for your own blog

If you are forking this repository or using this code for your own blog, you need to generate your own Giscus IDs. Follow these steps:

1.  **Prepare your GitHub Repository:**
    *   Ensure your repository is **public**.
    *   Enable **Discussions** in your repository settings.
    *   Enable **GitHub Pages** in your repository settings (Settings > Pages). In the "Build and deployment" section, under "Source", select **GitHub Actions** (instead of "Deploy from a branch").

2.  **Install the Giscus App:**
    *   Install the [Giscus App](https://github.com/apps/giscus) on your repository.
    *   Grant it access to the repository where discussions will be hosted.

3.  **Get your IDs from Giscus.app:**
    *   Go to [giscus.app](https://giscus.app).
    *   Enter your repository name (e.g., `yourusername/your-repo`).
    *   Scroll down to the **Discussion Category** section.
    *   Select a category (e.g., "General" or "Announcements"). **Note:** It is recommended to use a category where "Announcements" are allowed or create a specific one for comments.
    *   Scroll down to the **Enable giscus** section.
    *   Look for the `<script>` tag generated by the tool.
    *   Find the values for `data-repo-id` and `data-category-id`.

4.  **Update `.blog-config`:**
    *   Copy the `data-repo-id` value to `GISCUS_REPO_ID`.
    *   Copy the `data-category-id` value to `GISCUS_CATEGORY_ID`.
    *   Update `GISCUS_REPO` with your repository name.
    *   Update `GISCUS_CATEGORY` with the category name you selected.

### Disabling Comments

To disable comments on a specific post, add this to the post's front matter:

```yaml
---
title: "Your Post Title"
comments: false
---
```

Or in Org-mode format:
```org
#+TITLE: Your Post Title
#+JEKYLL_COMMENTS: false
```

### Customization

**Theme Options** (change `GISCUS_THEME` in `.blog-config`):
- `light`, `dark`, `dark_dimmed`
- `preferred_color_scheme` (automatic based on user's system)
- `transparent_dark`, `dark_high_contrast`

**Position Options** (change `GISCUS_INPUT_POSITION`):
- `bottom` (default) - Comment box at bottom
- `top` - Comment box at top

**After making changes:**
1. Edit the appropriate variable in `.blog-config`
2. Run `./scripts/generate-config.sh` to update `_config.yml`
3. Rebuild the site with `./scripts/dev.sh build` or `./scripts/dev.sh serve`

### Moderation

- Comments are moderated through GitHub Discussions
- Repository owners/collaborators can moderate all comments
- Standard GitHub moderation tools apply (delete, edit, lock discussions)
- Spam protection through GitHub's built-in systems

### Privacy

- Users must have GitHub accounts to comment
- No anonymous comments (reduces spam)
- No external tracking or cookies
- All data stored on GitHub (GDPR compliant)

## Requirements

- **Emacs** (tested with version 29.3)
- **Org-mode** (comes with Emacs)
- **Ruby** (version 3.0 or higher recommended)
- **Bundler** (install with `gem install bundler`)
- **Jekyll** (installed via Bundler)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/yourusername/your-repo.git
    cd your-repo
    ```

2.  **Install dependencies:**
    Run the installation script to install the required Ruby gems (Jekyll, etc.):
    ```bash
    ./scripts/dev.sh install
    ```
    *Note: This runs `bundle install` internally.*

3.  **Configure your blog:**
    Edit `.blog-config` to set your site title, author name, and other settings.

4.  **Build and Serve:**
    ```bash
    ./scripts/dev.sh serve
    ```
    This will build the site and start a local server at `http://localhost:4000`.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

### What's Covered

- ✅ **Framework code**: Scripts, templates, configurations, CSS, JavaScript
- ✅ **Integration work**: Org-mode publishing, Jekyll setup, Pyodide integration  
- ✅ **Documentation**: Setup instructions, examples, tutorials
- ❌ **Blog content**: Individual posts and articles remain under their authors' copyright

Feel free to use this blog framework for your own projects, whether personal, academic, or commercial!

