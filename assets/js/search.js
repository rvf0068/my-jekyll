/**
 * Client-side search for Jekyll blog
 * Lightweight, no external dependencies
 */
class BlogSearch {
  constructor() {
    this.posts = [];
    this.searchIndex = null;
    this.init();
  }

  async init() {
    // Load search index
    try {
      const baseUrl = window.siteBaseUrl || '';
      // Remove trailing slash if present to avoid double slashes
      const cleanBaseUrl = baseUrl.endsWith('/') ? baseUrl.slice(0, -1) : baseUrl;
      const response = await fetch(`${cleanBaseUrl}/search.json`);
      this.posts = await response.json();
      this.buildSearchIndex();
      this.setupSearchUI();
    } catch (error) {
      console.warn('Search functionality unavailable:', error);
    }
  }

  buildSearchIndex() {
    this.searchIndex = this.posts.map(post => ({
      ...post,
      searchText: `${post.title} ${post.content} ${post.categories.join(' ')} ${post.tags.join(' ')}`.toLowerCase()
    }));
  }

  search(query) {
    if (!query || query.length < 2) return [];
    
    const terms = query.toLowerCase().split(' ').filter(term => term.length > 1);
    
    return this.searchIndex.filter(post => 
      terms.every(term => post.searchText.includes(term))
    ).slice(0, 10); // Limit results
  }

  setupSearchUI() {
    // Add search box to header content for better integration
    const headerContent = document.querySelector('.header-content');
    if (!headerContent) return;

    const searchHTML = `
      <div class="header-search">
        <input type="text" id="search-input" placeholder="Search posts..." />
        <div id="search-results" style="display: none;"></div>
      </div>
    `;
    
    headerContent.insertAdjacentHTML('beforeend', searchHTML);
    
    const searchInput = document.getElementById('search-input');
    const searchResults = document.getElementById('search-results');
    
    searchInput.addEventListener('input', (e) => {
      const query = e.target.value;
      const results = this.search(query);
      
      if (query.length < 2) {
        searchResults.style.display = 'none';
        return;
      }
      
      this.displayResults(results, searchResults);
    });
  }

  displayResults(results, container) {
    if (results.length === 0) {
      container.innerHTML = '<div class="search-no-results">No posts found</div>';
    } else {
      container.innerHTML = results.map(post => `
        <div class="search-result">
          <h4><a href="${post.url}">${post.title}</a></h4>
          <p class="search-meta">${post.date} • ${post.categories.join(', ')}</p>
          <p class="search-excerpt">${post.excerpt || ''}</p>
        </div>
      `).join('');
    }
    
    container.style.display = 'block';
  }
}

// Initialize search when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => new BlogSearch());
} else {
  new BlogSearch();
}