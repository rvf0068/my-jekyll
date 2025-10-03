;; Org publishing configuration for Jekyll
;; Place this in the repo root

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'htmlize)
  (package-install 'htmlize))

(require 'ox-html)
(require 'ox-publish)
(require 'htmlize)

(setq org-html-htmlize-output-type nil)  ; Use inline CSS

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
        ("resources"
         :base-directory "./org"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|svg"
         :publishing-directory "./"
         :recursive t
         :publishing-function org-publish-attachment)
        ("themkat" :components ("main-site" "resources"))))

(provide 'org_publish)
