// Code block enhancement for org-mode with Prism.js syntax highlighting
// Integrates Prism.js while preserving custom features (copy button, language badge, dark mode)

document.addEventListener('DOMContentLoaded', function() {
    console.log('Starting code block enhancement with Prism.js...');
    enhanceCodeBlocks();
});

function enhanceCodeBlocks() {
    const codeContainers = document.querySelectorAll('.org-src-container');
    console.log('Found', codeContainers.length, 'code containers');

    codeContainers.forEach(function(container, index) {
        // Skip if already processed
        if (container.dataset.enhanced) {
            return;
        }

        const codeBlock = container.querySelector('pre.src');
        if (!codeBlock) {
            return;
        }

        // Get language from org-mode class
        const classList = Array.from(codeBlock.classList);
        let language = 'text';
        for (const className of classList) {
            if (className.startsWith('src-')) {
                language = className.substring(4);
                break;
            }
        }

        // Map org-mode language names to Prism language names
        const languageMap = {
            'emacs-lisp': 'lisp',
            'elisp': 'lisp',
            'shell': 'bash',
            'sh': 'bash',
            'c++': 'cpp',
            'python-cell': 'python'
        };
        const prismLanguage = languageMap[language] || language;

        // Apply Prism.js syntax highlighting
        const code = codeBlock.querySelector('code') || codeBlock;
        const codeContent = code.textContent;
        
        // Create new code element for Prism
        const newCode = document.createElement('code');
        newCode.className = 'language-' + prismLanguage;
        newCode.textContent = codeContent;
        
        // Replace pre content with Prism-ready code
        codeBlock.innerHTML = '';
        codeBlock.appendChild(newCode);
        
        // Run Prism highlighting
        if (typeof Prism !== 'undefined') {
            Prism.highlightElement(newCode);
        }

        // Create custom toolbar
        const toolbar = document.createElement('div');
        toolbar.className = 'code-toolbar-custom';

        // Language badge
        const languageBadge = document.createElement('span');
        languageBadge.className = 'language-badge';
        languageBadge.textContent = language.toUpperCase();

        // Copy button
        const copyButton = document.createElement('button');
        copyButton.className = 'copy-button';
        copyButton.textContent = 'Copy';

        // Copy functionality with multiple fallback methods
        copyButton.addEventListener('click', function(e) {
            e.preventDefault();
            e.stopPropagation();

            const textToCopy = newCode.textContent || newCode.innerText;
            console.log('Copy clicked, text length:', textToCopy.length);

            // Try modern clipboard API first
            if (navigator.clipboard && window.isSecureContext) {
                navigator.clipboard.writeText(textToCopy).then(function() {
                    console.log('Copied via clipboard API');
                    showSuccess();
                }).catch(function(err) {
                    console.log('Clipboard API failed:', err);
                    tryFallbackCopy(textToCopy);
                });
            } else {
                // Use fallback method
                tryFallbackCopy(textToCopy);
            }

            function tryFallbackCopy(text) {
                console.log('Trying fallback copy method');
                const textArea = document.createElement('textarea');
                textArea.value = text;
                textArea.style.cssText = 'position: fixed; left: -9999px; top: -9999px; opacity: 0;';
                document.body.appendChild(textArea);
                textArea.focus();
                textArea.select();

                try {
                    const successful = document.execCommand('copy');
                    console.log('Fallback copy result:', successful);
                    if (successful) {
                        showSuccess();
                    } else {
                        showError();
                    }
                } catch (err) {
                    console.error('Fallback copy failed:', err);
                    showError();
                } finally {
                    document.body.removeChild(textArea);
                }
            }

            function showSuccess() {
                const originalText = copyButton.textContent;
                copyButton.textContent = 'Copied!';
                copyButton.classList.add('success');
                setTimeout(function() {
                    copyButton.textContent = originalText;
                    copyButton.classList.remove('success');
                }, 2000);
            }

            function showError() {
                const originalText = copyButton.textContent;
                copyButton.textContent = 'Error';
                copyButton.classList.add('error');
                setTimeout(function() {
                    copyButton.textContent = originalText;
                    copyButton.classList.remove('error');
                }, 1500);
            }
        });

        // Assemble toolbar
        toolbar.appendChild(languageBadge);
        toolbar.appendChild(copyButton);
        container.appendChild(toolbar);

        // Mark as processed
        container.dataset.enhanced = 'true';

        console.log('Enhanced code block:', language);
    });
}
