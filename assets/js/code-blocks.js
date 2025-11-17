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
        toolbar.style.cssText = 'position: absolute; top: 8px; right: 8px; display: flex; gap: 8px; z-index: 10;';

        // Language badge
        const languageBadge = document.createElement('span');
        languageBadge.className = 'language-badge';
        languageBadge.textContent = language.toUpperCase();

        // Style based on current mode
        const isDarkMode = document.documentElement.classList.contains('dark-mode');
        const badgeStyle = isDarkMode
            ? 'background-color: #3e4451; color: #abb2bf; border: 1px solid #5c6370; border-radius: 12px; padding: 3px 10px; font-size: 0.75rem; font-weight: 500; text-transform: uppercase; letter-spacing: 0.5px; cursor: default;'
            : 'background-color: #f8f9fa; color: #6c757d; border: 1px solid #dee2e6; border-radius: 12px; padding: 3px 10px; font-size: 0.75rem; font-weight: 500; text-transform: uppercase; letter-spacing: 0.5px; cursor: default;';
        languageBadge.style.cssText = badgeStyle;

        // Copy button
        const copyButton = document.createElement('button');
        copyButton.className = 'copy-button';
        copyButton.textContent = 'Copy';

        const buttonStyle = isDarkMode
            ? 'background-color: #61dafb; color: #282c34; border: none; border-radius: 4px; padding: 4px 8px; font-size: 0.8rem; cursor: pointer; min-width: 50px; transition: background-color 0.2s;'
            : 'background-color: #007bff; color: white; border: none; border-radius: 4px; padding: 4px 8px; font-size: 0.8rem; cursor: pointer; min-width: 50px; transition: background-color 0.2s;';
        copyButton.style.cssText = buttonStyle;

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
                const originalBg = copyButton.style.backgroundColor;
                copyButton.textContent = 'Copied!';
                copyButton.style.backgroundColor = '#28a745';
                setTimeout(function() {
                    copyButton.textContent = originalText;
                    copyButton.style.backgroundColor = originalBg;
                }, 2000);
            }

            function showError() {
                const originalText = copyButton.textContent;
                const originalBg = copyButton.style.backgroundColor;
                copyButton.textContent = 'Error';
                copyButton.style.backgroundColor = '#dc3545';
                setTimeout(function() {
                    copyButton.textContent = originalText;
                    copyButton.style.backgroundColor = originalBg;
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
    
    // Set up dark mode listener to update existing toolbars
    observeDarkModeChanges();
}

function observeDarkModeChanges() {
    const observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
            if (mutation.attributeName === 'class' && mutation.target === document.documentElement) {
                updateToolbarsForMode();
            }
        });
    });
    
    observer.observe(document.documentElement, {
        attributes: true,
        attributeFilter: ['class']
    });
}

function updateToolbarsForMode() {
    const isDarkMode = document.documentElement.classList.contains('dark-mode');
    
    // Update language badges
    document.querySelectorAll('.language-badge').forEach(function(badge) {
        if (isDarkMode) {
            badge.style.cssText = 'background-color: #3e4451; color: #abb2bf; border: 1px solid #5c6370; border-radius: 12px; padding: 3px 10px; font-size: 0.75rem; font-weight: 500; text-transform: uppercase; letter-spacing: 0.5px; cursor: default;';
        } else {
            badge.style.cssText = 'background-color: #f8f9fa; color: #6c757d; border: 1px solid #dee2e6; border-radius: 12px; padding: 3px 10px; font-size: 0.75rem; font-weight: 500; text-transform: uppercase; letter-spacing: 0.5px; cursor: default;';
        }
    });
    
    // Update copy buttons
    document.querySelectorAll('.copy-button').forEach(function(button) {
        if (isDarkMode) {
            button.style.cssText = 'background-color: #61dafb; color: #282c34; border: none; border-radius: 4px; padding: 4px 8px; font-size: 0.8rem; cursor: pointer; min-width: 50px; transition: background-color 0.2s;';
        } else {
            button.style.cssText = 'background-color: #007bff; color: white; border: none; border-radius: 4px; padding: 4px 8px; font-size: 0.8rem; cursor: pointer; min-width: 50px; transition: background-color 0.2s;';
        }
    });
}
