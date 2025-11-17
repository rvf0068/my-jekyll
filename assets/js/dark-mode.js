/**
 * Dark Mode Toggle
 * Respects system preference and remembers user choice
 */
class DarkMode {
  constructor() {
    this.darkModeKey = 'blog-dark-mode';
    this.init();
  }

  init() {
    // The dark mode preference has already been applied by the inline script in head.html
    // We just need to set up the toggle and system change listener

    // Wait for DOM to be ready before creating toggle
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', () => this.createToggle());
    } else {
      this.createToggle();
    }

    this.listenForSystemChanges();
  }

  enableDarkMode() {
    document.documentElement.classList.add('dark-mode');
    document.documentElement.classList.remove('light-mode');
  }

  disableDarkMode() {
    document.documentElement.classList.remove('dark-mode');
    document.documentElement.classList.add('light-mode');
  }

  toggle() {
    const isDark = document.documentElement.classList.contains('dark-mode');

    if (isDark) {
      this.disableDarkMode();
      localStorage.setItem(this.darkModeKey, 'light');
    } else {
      this.enableDarkMode();
      localStorage.setItem(this.darkModeKey, 'dark');
    }

    // Update button icon immediately
    this.updateToggleIcon();

    // Update Giscus theme to match new mode
    this.updateGiscusTheme();
  }

  updateToggleIcon() {
    const toggle = document.querySelector('.dark-mode-toggle');
    if (toggle) {
      const isDark = document.documentElement.classList.contains('dark-mode');
      toggle.innerHTML = isDark ? '☀️' : '🌙';
    }
  }

  updateGiscusTheme() {
    // Update Giscus theme to match current mode
    const iframe = document.querySelector('iframe.giscus-frame');
    if (iframe) {
      const isDark = document.documentElement.classList.contains('dark-mode');
      const theme = isDark ? 'dark' : 'light';

      iframe.contentWindow.postMessage(
	{ giscus: { setConfig: { theme: theme } } },
	'https://giscus.app'
      );
    }
  }

  createToggle() {
    // Check if toggle already exists
    if (document.querySelector('.dark-mode-toggle')) return;

    const toggle = document.createElement('button');
    toggle.className = 'dark-mode-toggle';
    toggle.setAttribute('aria-label', 'Toggle dark mode');
    toggle.setAttribute('type', 'button');

    // Set initial icon
    const isDark = document.documentElement.classList.contains('dark-mode');
    toggle.innerHTML = isDark ? '☀️' : '🌙';

    // Add click event listener with proper event handling
    toggle.addEventListener('click', (e) => {
      e.preventDefault();
      e.stopPropagation();
      this.toggle();
    });

    // Add to body to avoid conflicts with search
    document.body.appendChild(toggle);
  }

  listenForSystemChanges() {
    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', (e) => {
      // Only apply system preference if user hasn't manually set a preference
      if (!localStorage.getItem(this.darkModeKey)) {
	if (e.matches) {
	  this.enableDarkMode();
	} else {
	  this.disableDarkMode();
	}
	this.updateToggleIcon();
      }
    });
  }
}

// Initialize dark mode
new DarkMode();
