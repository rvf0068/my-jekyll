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

;; Custom function to fix Jekyll baseurl links in final HTML output
;; This prevents org-mode from converting paths starting with baseurl to file:// URLs
(defun org-html-final-function (contents backend info)
  "Post-process HTML to fix Jekyll baseurl links."
  (when (org-export-derived-backend-p backend 'html)
    ;; Replace file:// URLs with proper HTTP paths for baseurl
    (let ((file-url-pattern (concat "href=\"file:/+" jekyll-baseurl "/"))
          (http-path (concat "href=\"" jekyll-baseurl "/")))
      (setq contents (replace-regexp-in-string file-url-pattern http-path contents))))
  contents)

(add-to-list 'org-export-filter-final-output-functions 'org-html-final-function)

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
