/**
 * Enhanced code blocks with syntax highlighting, line numbers, and copy-to-clipboard
 */
(function() {
  'use strict';

  // Language mapping from Org-mode class names to Prism language identifiers
  const languageMap = {
    'python': 'python',
    'bash': 'bash',
    'shell': 'bash',
    'javascript': 'javascript',
    'js': 'javascript',
    'typescript': 'typescript',
    'ts': 'typescript',
    'java': 'java',
    'c': 'c',
    'cpp': 'cpp',
    'csharp': 'csharp',
    'ruby': 'ruby',
    'go': 'go',
    'rust': 'rust',
    'php': 'php',
    'html': 'markup',
    'xml': 'markup',
    'css': 'css',
    'sql': 'sql',
    'json': 'json',
    'yaml': 'yaml',
    'markdown': 'markdown',
    'md': 'markdown',
    'elisp': 'lisp',
    'emacs-lisp': 'lisp',
    'lisp': 'lisp',
    'scheme': 'scheme',
    'clojure': 'clojure'
  };

  /**
   * Extract language from Org-mode class name (e.g., "src-python" -> "python")
   */
  function extractLanguage(pre) {
    const classList = pre.className.split(' ');
    for (let className of classList) {
      if (className.startsWith('src-')) {
        return className.substring(4);
      }
    }
    return null;
  }

  /**
   * Get Prism language identifier from Org-mode language
   */
  function getPrismLanguage(orgLanguage) {
    if (!orgLanguage) return 'none';
    const lower = orgLanguage.toLowerCase();
    return languageMap[lower] || lower;
  }

  /**
   * Enhance a code block with Prism highlighting, line numbers, and copy button
   */
  function enhanceCodeBlock(pre) {
    const container = pre.parentElement;
    if (!container || !container.classList.contains('org-src-container')) {
      return;
    }

    // Extract language
    const orgLanguage = extractLanguage(pre);
    const prismLanguage = getPrismLanguage(orgLanguage);

    // Get the code content
    const code = pre.textContent;

    // Create new code element with proper Prism classes
    const codeElement = document.createElement('code');
    codeElement.className = 'language-' + prismLanguage;
    codeElement.textContent = code;

    // Update pre element classes for Prism
    pre.className = 'language-' + prismLanguage + ' line-numbers';
    pre.innerHTML = '';
    pre.appendChild(codeElement);

    // Add language badge if language is identified
    if (orgLanguage && orgLanguage !== 'none') {
      const badge = document.createElement('span');
      badge.className = 'code-language-badge';
      badge.textContent = orgLanguage;
      container.insertBefore(badge, pre);
    }

    // Add copy button
    const copyButton = document.createElement('button');
    copyButton.className = 'copy-code-button';
    copyButton.textContent = 'Copy';
    copyButton.setAttribute('aria-label', 'Copy code to clipboard');
    copyButton.addEventListener('click', function() {
      copyCodeToClipboard(code, copyButton);
    });
    container.appendChild(copyButton);

    // Apply Prism highlighting
    if (window.Prism) {
      Prism.highlightElement(codeElement);
      // Trigger line numbers plugin if available
      if (window.Prism && window.Prism.plugins && window.Prism.plugins.lineNumbers) {
        window.Prism.plugins.lineNumbers.resize(pre);
      }
    }
  }

  /**
   * Copy code to clipboard
   */
  function copyCodeToClipboard(code, button) {
    // Use modern clipboard API if available
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(code).then(function() {
        showCopySuccess(button);
      }).catch(function(err) {
        console.error('Failed to copy code: ', err);
        fallbackCopy(code, button);
      });
    } else {
      // Fallback for older browsers
      fallbackCopy(code, button);
    }
  }

  /**
   * Fallback copy method for older browsers
   */
  function fallbackCopy(code, button) {
    const textArea = document.createElement('textarea');
    textArea.value = code;
    textArea.style.position = 'fixed';
    textArea.style.left = '-9999px';
    document.body.appendChild(textArea);
    textArea.select();
    
    try {
      document.execCommand('copy');
      showCopySuccess(button);
    } catch (err) {
      console.error('Failed to copy code: ', err);
      button.textContent = 'Failed';
      setTimeout(function() {
        button.textContent = 'Copy';
      }, 2000);
    }
    
    document.body.removeChild(textArea);
  }

  /**
   * Show visual feedback when code is copied
   */
  function showCopySuccess(button) {
    const originalText = button.textContent;
    button.textContent = 'Copied!';
    button.classList.add('copied');
    
    setTimeout(function() {
      button.textContent = originalText;
      button.classList.remove('copied');
    }, 2000);
  }

  /**
   * Initialize all code blocks on the page
   */
  function initCodeBlocks() {
    // Find all Org-mode code blocks
    const codeBlocks = document.querySelectorAll('.org-src-container pre[class*="src-"]');
    
    codeBlocks.forEach(function(pre) {
      enhanceCodeBlock(pre);
    });
  }

  // Initialize when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initCodeBlocks);
  } else {
    // DOM is already loaded
    initCodeBlocks();
  }
})();
