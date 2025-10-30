# My Jekyll Blog with Org-Mode

This is a Jekyll-based blog that uses Org-mode for authoring posts. The setup automatically converts Org files to HTML with Jekyll front matter.

## Features

- Write blog posts in Org-mode syntax
- Automatic Jekyll front matter generation from Org properties
- Support for LaTeX math content with MathJax
- **Automatic equation referencing** with numbered links for LaTeX equations
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

For a complete example, see `org/_posts/2025-10-30-generated-images.org`.

### Equation Referencing

The system automatically processes LaTeX equation labels and references:

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

**Note:** 
- Org-mode automatically generates `\label{orgXXXXXX}` tags for equations during export, so you can use `\eqref{orgXXXXXX}` to reference them. The exact label ID is generated by org-mode.
- The anchor is placed **outside** the equation environment to ensure MathJax can properly render the equation (HTML tags inside LaTeX blocks break rendering).

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
#+DATE: 2025-10-30
#+BIBLIOGRAPHY: ../../references.bib

* Introduction

Poset topology has been studied extensively [cite:@quillen1978homotopy].
Further developments were made [cite:@bjorner1995topology].

* Conclusion

This demonstrates the citation system.

#+print_bibliography:
```

The bibliography will be automatically generated from the cited works, formatted with author names, year, and publication details.

Example post with citations: `org/_posts/2025-10-30-sample-with-citations.org`

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
2. Enables org-cite for native citation support with BibTeX files
3. Defines custom export options for Jekyll-specific properties
4. Implements a filter function that:
   - Processes LaTeX equation labels (`\label{org...}`) and references (`\eqref{org...}`)
   - Places HTML anchors **before** equation blocks (not inside them, to avoid breaking MathJax rendering)
   - Adds `\tag{n}` inside equations to display equation numbers flush-right (like textbooks)
   - Converts equation references to clickable links with numbered labels
   - Converts LaTeX special characters (e.g., `{\"o}` → `ö`) to proper HTML entities
   - Extracts metadata from Org properties
   - Generates Jekyll front matter
   - Prepends it to the HTML output
   - Fixes any baseurl-related link issues

The front matter is automatically generated only for files in the `_posts/` directory and only when it's not already present.

Citations are processed by Org-mode's built-in `org-cite` system which:
- Parses `[cite:@key]` references in the text
- Looks up entries in the specified BibTeX file
- Generates inline citations in (Author, Year) format
- Creates a formatted bibliography at the `#+print_bibliography:` location

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
