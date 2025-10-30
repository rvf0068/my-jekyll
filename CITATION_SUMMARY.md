# Citation Feature Summary

This document summarizes the citation feature implementation for the Jekyll blog.

## Files Created

1. **references.bib** - BibTeX bibliography file containing:
   - `posetfiber` - Björner, Wachs, and Welker (2005) paper as requested
   - `quillen1978homotopy` - Quillen's foundational paper on poset topology
   - `stanley1997enumerative` - Stanley's book on enumerative combinatorics
   - `bjorner1995topology` - Björner's chapter on topological methods

2. **org/_posts/2025-10-30-sample-with-citations.org** - Sample post demonstrating:
   - Citations using Org-mode footnote syntax `[fn:key]`
   - Mathematical equations with LaTeX labels and cross-references
   - Online images from Wikipedia/Wikimedia Commons
   - Mathematical theorem environments
   - Bibliography section with formatted references

3. **_posts/2025-10-30-sample-with-citations.html** - Generated HTML with:
   - Proper Jekyll front matter
   - Clickable citation superscripts linking to footnotes
   - Numbered equations with clickable references
   - External images loaded via HTTPS URLs
   - Styled theorem blocks

## Implementation Details

### Citation Approach
The blog uses **Org-mode footnotes** for citations rather than the newer `oc.el` citation syntax. This approach:
- Provides maximum compatibility across Org-mode versions
- Allows manual formatting control over bibliography entries
- Works reliably without additional package dependencies (citeproc)
- Renders as HTML footnotes with backlinks

### Org-mode Configuration
Modified `org_publish.el` to:
- Load `oc` (org-cite) module for potential future use
- Configure basic citation export processor
- Maintain existing equation reference processing

### Example Usage

In your Org post:
```org
The seminal work by Quillen [fn:quillen] established...

* References
[fn:quillen] Quillen, D. (1978). Homotopy properties...
```

This generates:
```html
The seminal work by Quillen <sup><a id="fnr.1" href="#fn.1">1</a></sup>...

<div class="footdef">
  <sup><a id="fn.1" href="#fnr.1">1</a></sup>
  Quillen, D. (1978). Homotopy properties...
</div>
```

## Features Demonstrated in Sample Post

1. **Citations**: Four references cited throughout the text with superscript numbers
2. **Mathematics**: Three numbered equations with cross-references using `\eqref{}`
3. **Online Images**: Two images from external URLs (Hasse diagram, simplicial complex)
4. **Theorem Environment**: Custom styled theorem block with bold heading
5. **Bibliography**: Formatted reference list at the end of the post

## Future Enhancements

If needed, the system can be enhanced to:
- Integrate CSL (Citation Style Language) for automated formatting
- Use BibTeX entries directly with `oc-csl` processor
- Add support for different citation styles (APA, Chicago, etc.)
- Automatically generate bibliography from BibTeX keys

The current implementation prioritizes simplicity and reliability while maintaining academic citation standards.
