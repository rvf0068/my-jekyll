# Migration Guide: From Manual Front Matter to Org Properties

This guide shows how to migrate existing posts from manual Jekyll front matter to the new automatic generation using Org properties.

## Example: Before and After

### Before (Manual Front Matter)

Old way of writing posts with `#+BEGIN_EXPORT html` blocks:

```org
#+BEGIN_EXPORT html
---
layout: post
title: "Sample Org Post with Features"
date: 2025-10-03 12:00:00 -0500
categories: sample org jekyll
tags: org-mode features
---
<script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/3.2.2/es5/tex-mml-chtml.js" async></script>
#+END_EXPORT

This is a sample Org-mode post demonstrating various syntax features.

* URL Links
Check out the official [[https://orgmode.org][Org-mode website]] for more details.

* LaTeX Math Content
Org-mode supports inline math like \( E = mc^2 \) and display math:

\[
\int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}
\]
```

### After (Automatic with Org Properties)

New simplified way using Org properties:

```org
#+TITLE: Sample Org Post with Features
#+DATE: 2025-10-03 12:00:00 -0500
#+JEKYLL_LAYOUT: post
#+JEKYLL_CATEGORIES: sample org jekyll
#+JEKYLL_TAGS: org-mode features

This is a sample Org-mode post demonstrating various syntax features.

* URL Links
Check out the official [[https://orgmode.org][Org-mode website]] for more details.

* LaTeX Math Content
Org-mode supports inline math like \( E = mc^2 \) and display math:

\[
\int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}
\]
```

## Migration Steps

1. **Extract front matter values**: Identify the layout, title, date, categories, and tags from your existing `#+BEGIN_EXPORT html` block.

2. **Add Org properties**: Add the corresponding properties at the top of your file:
   - `layout: post` → `#+JEKYLL_LAYOUT: post`
   - `title: "..."` → `#+TITLE: ...` (no quotes needed)
   - `date: ...` → `#+DATE: ...`
   - `categories: ...` → `#+JEKYLL_CATEGORIES: ...`
   - `tags: ...` → `#+JEKYLL_TAGS: ...`

3. **Handle special scripts**: If you have scripts like MathJax in the front matter, you have two options:
   - Move them to the Jekyll layout template (`_layouts/post.html`)
   - Keep them in a small `#+BEGIN_EXPORT html` block in the body

4. **Remove the manual front matter block**: Delete the entire `#+BEGIN_EXPORT html ... #+END_EXPORT` block that contains the front matter.

5. **Test**: Run the publishing script and verify the output HTML has the correct front matter.

## Benefits of the New Approach

- **Cleaner Org files**: No need for HTML export blocks at the beginning
- **Standard Org syntax**: Uses familiar Org-mode properties
- **Better integration**: Works seamlessly with other Org-mode tools
- **Easier to maintain**: Properties are clearly defined and easy to update
- **Consistent formatting**: Automatic generation ensures consistent front matter format

## Backward Compatibility

The old manual front matter approach still works! The system detects if front matter is already present (by checking if content starts with `---`) and won't add it again. This means you can:

- Keep old posts as-is without modification
- Gradually migrate posts as you edit them
- Mix both approaches in the same repository

## Notes on Tag Specification

You can specify tags in two ways:

1. Using `#+JEKYLL_TAGS:` (Jekyll-specific)
2. Using `#+FILETAGS:` (standard Org property)

Both will be included in the generated front matter. If you use both, they will be combined.

Example:
```org
#+FILETAGS: :demo:math:tutorial:
#+JEKYLL_TAGS: org-mode configuration
```

Generates:
```yaml
tags: org-mode configuration demo math tutorial
```

## Special Cases

### Posts without Categories or Tags

If you don't specify categories or tags, they will simply be omitted from the front matter:

```org
#+TITLE: Simple Post
#+DATE: 2025-10-29 12:00:00 -0500
```

Generates:
```yaml
---
layout: post
title: "Simple Post"
date: 2025-10-29 12:00:00 -0500
---
```

### Custom Layouts

You can specify custom layouts using `#+JEKYLL_LAYOUT:`:

```org
#+TITLE: About Page
#+JEKYLL_LAYOUT: page
```

If not specified, it defaults to "post".

### Author Information

You can optionally include author information:

```org
#+TITLE: My Post
#+DATE: 2025-10-29 12:00:00 -0500
#+AUTHOR: John Doe
```

## Need Help?

If you encounter issues during migration, check:

1. Property names are spelled correctly (case-sensitive)
2. Date format matches Jekyll's expected format
3. The publishing script runs without errors
4. Generated HTML files in `_posts/` have proper front matter
