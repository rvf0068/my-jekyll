;; Org publishing configuration for Jekyll
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Try to install htmlize, but don't fail if unavailable
(ignore-errors
  (unless (package-installed-p 'htmlize)
    (package-install 'htmlize)))

(require 'ox-html)
(require 'ox-publish)
;; Require htmlize only if available
(when (locate-library "htmlize")
  (require 'htmlize))

;; Configuration for Jekyll baseurl
(setq jekyll-baseurl "/my-jekyll")

;; Disable syntax highlighting (htmlize) if not available
;; Set to nil to use inline CSS when htmlize is available
(setq org-html-htmlize-output-type nil)

;; Function to generate Jekyll front matter from org properties
(defun org-jekyll-front-matter (info)
  "Generate Jekyll front matter from org-mode properties.
INFO is the plist used as a communication channel."
  (let* ((title (org-export-data (plist-get info :title) info))
         (date (org-export-data (plist-get info :date) info))
         (author (org-export-data (plist-get info :author) info))
         (filetags (plist-get info :filetags))
         ;; Get custom properties
         (layout (or (plist-get info :jekyll-layout) "post"))
         (categories (plist-get info :jekyll-categories))
         (tags-prop (plist-get info :jekyll-tags))
         ;; Combine tags from both sources
         (all-tags (delete-dups
                    (append
                     (when tags-prop (split-string tags-prop))
                     (when filetags filetags))))
         ;; Build front matter
         (front-matter (concat "---\n"
                               "layout: " layout "\n"
                               (when (and title (not (string-empty-p title)))
                                 (format "title: \"%s\"\n" title))
                               (when (and date (not (string-empty-p date)))
                                 (format "date: %s\n" date))
                               (when (and author (not (string-empty-p author)))
                                 (format "author: %s\n" author))
                               (when categories
                                 (format "categories: %s\n" categories))
                               (when all-tags
                                 (format "tags: %s\n" (mapconcat 'identity all-tags " ")))
                               "---\n")))
    front-matter))

;; Mathematical reference link prefixes
(defvar org-math-link-prefixes '("thm" "cor" "lem" "prf" "def" "pro" "eq")
  "List of prefixes for mathematical reference links that should get auto-descriptions.")

;; Function to add descriptions to mathematical reference links before export
;; This processes links like [[thm:lagrange]] to become [[thm:lagrange][lagrange]]
(defun org-add-math-link-descriptions ()
  "Add descriptions to mathematical reference links in the current buffer.
Links with prefixes from `org-math-link-prefixes` will get
automatic descriptions derived from the label name."
  (save-excursion
    (goto-char (point-min))
    (let ((prefix-regex (concat "\\("
                                (mapconcat (lambda (p) (concat p ":"))
                                          org-math-link-prefixes
                                          "\\|")
                                "\\)")))
      (while (re-search-forward
              (concat "\\[\\[" prefix-regex "\\([^]]+\\)\\]\\]")
              nil t)
        (let* ((prefix (match-string 1))
               (label (match-string 2))
               (link-with-desc (format "[[%s%s][%s]]" prefix label label)))
          (replace-match link-with-desc t t))))))

;; Hook to run before publishing
(defun org-publish-before-processing-hook (backend)
  "Add math link descriptions before export."
  (when (org-export-derived-backend-p backend 'html)
    (org-add-math-link-descriptions)))

(add-hook 'org-export-before-processing-hook 'org-publish-before-processing-hook)

;; Custom function to fix Jekyll baseurl links and add front matter
;; This prevents org-mode from converting paths starting with baseurl to file:// URLs
(defun org-html-final-function (contents backend info)
  "Post-process HTML to fix Jekyll baseurl links and add Jekyll front matter."
  (when (org-export-derived-backend-p backend 'html)
    ;; Replace file:// URLs with proper HTTP paths for baseurl
    (let ((file-url-pattern (concat "href=\"file:/+" jekyll-baseurl "/"))
          (http-path (concat "href=\"" jekyll-baseurl "/")))
      (setq contents (replace-regexp-in-string file-url-pattern http-path contents)))
    
    ;; Add Jekyll front matter for posts (only if not already present)
    (when (and (string-match-p "/_posts/" (or (plist-get info :output-file) ""))
               (not (string-prefix-p "---" contents)))
      (let ((front-matter (org-jekyll-front-matter info)))
        (setq contents (concat front-matter contents)))))
  contents)

;; Only add the filter if it's not already in the list
(unless (memq 'org-html-final-function org-export-filter-final-output-functions)
  (add-to-list 'org-export-filter-final-output-functions 'org-html-final-function))

;; Custom property extraction for Jekyll metadata
(defun org-jekyll-get-property (property info)
  "Get PROPERTY value from INFO plist or from file keywords."
  (or (plist-get info property)
      (org-export-data (plist-get info property) info)))

;; Add custom export options for Jekyll properties
(eval-after-load 'ox
  '(progn
     (add-to-list 'org-export-options-alist
                  '(:jekyll-layout "JEKYLL_LAYOUT" nil nil t))
     (add-to-list 'org-export-options-alist
                  '(:jekyll-categories "JEKYLL_CATEGORIES" nil nil t))
     (add-to-list 'org-export-options-alist
                  '(:jekyll-tags "JEKYLL_TAGS" nil nil t))))

(setq org-publish-project-alist
      '(("main-site"
         :base-directory "./org"
         :base-extension "org"
         :publishing-directory "./"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         :with-title t
         :with-toc nil)
        ("posts"
         :base-directory "./org/_posts"
         :base-extension "org"
         :publishing-directory "./_posts"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         :with-title t
         :with-toc nil)
        ("resources"
         :base-directory "./org"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|svg"
         :publishing-directory "./assets"
         :recursive t
         :publishing-function org-publish-attachment)
        ("themkat" :components ("main-site" "posts" "resources"))))

(provide 'org_publish)
