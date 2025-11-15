;; Org publishing configuration for Jekyll
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(require 'ox-html)
(require 'ox-publish)

;; Enable org-cite for citation support
(require 'oc)
(require 'oc-basic)
(require 'oc-biblatex)

;; Configure citation export processors to use basic style for HTML
(setq org-cite-export-processors
      '((html basic)
        (latex biblatex)
        (t basic)))

;; Set global bibliography file (absolute path from repo root)
(setq org-cite-global-bibliography 
      (list (expand-file-name "./references.bib")))

;; Macro to select different values based on export backend
(defmacro by-backend (&rest body)
  "Select export backend-specific values.
BODY is a list of backend-value pairs, e.g., (latex \"file.tex\") (t \"file.png\")."
  `(cl-case org-export-current-backend ,@body))

;; Configuration for Jekyll baseurl
(setq jekyll-baseurl "/my-jekyll")

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
                     filetags)))
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

;; Counter for generating unique cell IDs
(defvar org-python-cell-counter 0
  "Counter for generating unique Python cell IDs.")

;; Track if pyodide has been initialized on the page
(defvar org-python-pyodide-initialized nil
  "Flag to track if Pyodide initialization code has been added.")

;; Function to detect if code contains matplotlib
(defun org-python-code-uses-matplotlib (code)
  "Return non-nil if CODE uses matplotlib (plt.show() or plt.plot() etc)."
  (or (string-match-p "plt\\.show()" code)
      (string-match-p "plt\\.plot(" code)
      (string-match-p "plt\\.scatter(" code)
      (string-match-p "plt\\.bar(" code)
      (string-match-p "plt\\.hist(" code)
      (string-match-p "plt\\.imshow(" code)
      (string-match-p "import matplotlib" code)
      (string-match-p "from matplotlib" code)))

;; Function to detect if code contains sympy
(defun org-python-code-uses-sympy (code)
  "Return non-nil if CODE uses sympy."
  (or (string-match-p "import sympy" code)
      (string-match-p "from sympy" code)
      (string-match-p "sp\\." code)
      (string-match-p "sympy\\." code)))

;; Function to convert python-cell source blocks to HTML
(defun org-python-cell-block-filter (text backend info)
  "Convert python-cell source blocks to interactive Pyodide cells.
TEXT is the transcoded HTML content.
BACKEND is the export backend being used.
INFO is the export plist."
  (if (org-export-derived-backend-p backend 'html)
      (progn
        ;; Reset counter for each document
        (setq org-python-cell-counter 0)
        (setq org-python-pyodide-initialized nil)
        
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          
          ;; Find all python-cell source blocks
          ;; Org exports them as <div class="org-src-container"><pre class="src src-python-cell">
          (while (re-search-forward 
                  "<div class=\"org-src-container\">\\s-*<pre class=\"src src-python-cell\">\\(\\(?:.\\|\n\\)*?\\)</pre>\\s-*</div>"
                  nil t)
            (let* ((code-html (match-string 1))
                   ;; Decode HTML entities back to plain text
                   (code (with-temp-buffer
                           (insert code-html)
                           (goto-char (point-min))
                           (while (re-search-forward "<span[^>]*>\\([^<]*\\)</span>" nil t)
                             (replace-match "\\1"))
                           (goto-char (point-min))
                           (while (search-forward "&lt;" nil t)
                             (replace-match "<"))
                           (goto-char (point-min))
                           (while (search-forward "&gt;" nil t)
                             (replace-match ">"))
                           (goto-char (point-min))
                           (while (search-forward "&amp;" nil t)
                             (replace-match "&"))
                           (goto-char (point-min))
                           (while (search-forward "&quot;" nil t)
                             (replace-match "\""))
                           (buffer-string)))
                   (cell-id (format "pycell%d" (setq org-python-cell-counter 
                                                     (1+ org-python-cell-counter))))
                   (is-matplotlib (org-python-code-uses-matplotlib code))
                   (is-sympy (org-python-code-uses-sympy code))
                   (pyodide-init (if (not org-python-pyodide-initialized)
                                     (progn
                                       (setq org-python-pyodide-initialized t)
                                       "<script>\n  const pyodideReady = loadPyodide();\n</script>\n\n")
                                   ""))
                   ;; Build package list based on detected libraries
                   (packages (append
                              (when is-matplotlib '("matplotlib" "numpy"))
                              (when is-sympy '("sympy"))))
                   (packages-str (if packages
                                    (format "[%s]"
                                            (mapconcat (lambda (p) (format "\"%s\"" p))
                                                      packages ", "))
                                  "[]"))
                   (html-code (cond
                               ;; Matplotlib cell: output is an image
                               (is-matplotlib
                                (format "%s<!-- Python cell %s with matplotlib%s -->\n<div class=\"py-cell\">\n  <textarea id=\"%s-code\" rows=\"8\" cols=\"60\">%s</textarea><br>\n  <button id=\"%s-run\">Run</button><br>\n  <div id=\"%s-out\"></div>\n</div>\n\n<script>\n(async () => {\n  const pyodide = await pyodideReady;\n  await pyodide.loadPackage([\"micropip\"]);\n  await pyodide.runPythonAsync(`\nimport micropip\nawait micropip.install(%s)\n  `);\n  \n  document.getElementById(\"%s-run\").onclick = async () => {\n    const code = document.getElementById(\"%s-code\").value;\n    try {\n      const result = await pyodide.runPythonAsync(`\nimport matplotlib\nmatplotlib.use(\"AGG\")\nimport io, base64\n${code}\nbuf = io.BytesIO()\nplt.savefig(buf, format='png')\nbuf.seek(0)\nimg_data = base64.b64encode(buf.read()).decode()\nplt.clf()\nbuf.close()\nimg_data\n      `);\n      const img = document.createElement(\"img\");\n      img.src = \"data:image/png;base64,\" + result;\n      const out = document.getElementById(\"%s-out\");\n      out.innerHTML = \"\";\n      out.appendChild(img);\n    } catch (err) {\n      document.getElementById(\"%s-out\").textContent = err;\n    }\n  };\n})();\n</script>"
                                        pyodide-init cell-id
                                        (if is-sympy " and sympy" "")
                                        cell-id code
                                        cell-id
                                        cell-id
                                        packages-str
                                        cell-id cell-id
                                        cell-id
                                        cell-id))
                               ;; Sympy cell: output is text but needs sympy package
                               (is-sympy
                                (format "%s<!-- Python cell %s with sympy -->\n<div class=\"py-cell\">\n  <textarea id=\"%s-code\" rows=\"4\" cols=\"40\">%s</textarea><br>\n  <button id=\"%s-run\">Run</button>\n  <pre id=\"%s-out\"></pre>\n</div>\n\n<script>\n(async () => {\n  const pyodide = await pyodideReady;\n  await pyodide.loadPackage([\"micropip\"]);\n  await pyodide.runPythonAsync(`\nimport micropip\nawait micropip.install([\"sympy\"])\n  `);\n  \n  document.getElementById(\"%s-run\").onclick = async () => {\n    const code = document.getElementById(\"%s-code\").value;\n    try {\n      const result = await pyodide.runPythonAsync(code);\n      document.getElementById(\"%s-out\").textContent = result;\n    } catch (err) {\n      document.getElementById(\"%s-out\").textContent = err;\n    }\n  };\n})();\n</script>"
                                        pyodide-init cell-id
                                        cell-id code
                                        cell-id
                                        cell-id
                                        cell-id cell-id
                                        cell-id
                                        cell-id))
                               ;; Regular cell: output is text
                               (t
                                (format "%s<!-- Python cell %s -->\n<div class=\"py-cell\">\n  <textarea id=\"%s-code\" rows=\"4\" cols=\"40\">%s</textarea><br>\n  <button id=\"%s-run\">Run</button>\n  <pre id=\"%s-out\"></pre>\n</div>\n\n<script>\n(async () => {\n  const pyodide = await pyodideReady;\n  document.getElementById(\"%s-run\").onclick = async () => {\n    const code = document.getElementById(\"%s-code\").value;\n    try {\n      const result = await pyodide.runPythonAsync(code);\n      document.getElementById(\"%s-out\").textContent = result;\n    } catch (err) {\n      document.getElementById(\"%s-out\").textContent = err;\n    }\n  };\n})();\n</script>"
                                        pyodide-init cell-id
                                        cell-id code
                                        cell-id
                                        cell-id
                                        cell-id cell-id
                                        cell-id
                                        cell-id)))))
              (replace-match html-code t t)))
          
          (buffer-string)))
    text))

;; Hook to run before publishing
(defun org-publish-before-processing-hook (backend)
  "Add math link descriptions before export."
  (when (org-export-derived-backend-p backend 'html)
    (org-add-math-link-descriptions)))

(add-hook 'org-export-before-processing-hook 'org-publish-before-processing-hook)

;; Function to process equation labels and references
(defun org-process-equation-references (contents)
  "Process LaTeX equation labels and references in HTML CONTENTS.
Converts \\label{org...} to HTML anchors and \\eqref{org...} to links
with numbered descriptions based on the order of labels in the document.
Places anchors BEFORE equation blocks and adds \\tag{n} inside equations for display."
  (let ((label-counter 0)
        (label-map (make-hash-table :test 'equal)))
    ;; First pass: find all \label{org...} and assign numbers
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (while (re-search-forward "\\\\label{\\(org[^}]+\\)}" nil t)
        (let ((label (match-string 1)))
          (unless (gethash label label-map)
            (setq label-counter (1+ label-counter))
            (puthash label label-counter label-map))))
      
      ;; Second pass: process equation blocks with \label{org...}
      ;; Move anchor before \begin{equation} and add \tag{n} inside equation
      (goto-char (point-min))
      (while (re-search-forward "\\\\begin{equation}\\s-*\\(\n\\s-*\\)*\\\\label{\\(org[^}]+\\)}" nil t)
        (let* ((label (match-string 2))
               (number (gethash label label-map))
               (anchor (format "<span id=\"%s\"></span>\n\\begin{equation}\\tag{%d}" label number)))
          (replace-match anchor t t)))
      
      ;; Third pass: replace \eqref{org...} with HTML links
      (goto-char (point-min))
      (while (re-search-forward "\\\\eqref{\\(org[^}]+\\)}" nil t)
        (let* ((label (match-string 1))
               (number (gethash label label-map))
               (link (if number
                        (format "<a href=\"#%s\">(%d)</a>" label number)
                      ;; If label not found, keep original text
                      (match-string 0))))
          (replace-match link t t)))
      
      (buffer-string))))

;; Custom function to fix Jekyll baseurl links and add front matter
;; This prevents org-mode from converting paths starting with baseurl to file:// URLs
(defun org-html-final-function (contents backend info)
  "Post-process HTML to fix Jekyll baseurl links and add Jekyll front matter."
  (when (org-export-derived-backend-p backend 'html)
    ;; Guard against double-processing - if already processed, return as-is
    (unless (string-match-p "<!-- PROCESSED-BY-ORG-HTML-FINAL-FUNCTION -->" contents)
      ;; Process python-cell blocks first
      (setq contents (org-python-cell-block-filter contents backend info))
      
      ;; Add Pyodide script if python cells were found
      (when (string-match-p "const pyodideReady = loadPyodide()" contents)
        (setq contents (concat "<script src=\"https://cdn.jsdelivr.net/pyodide/v0.26.0/full/pyodide.js\"></script>\n\n"
                               contents)))
      
      ;; Process equation labels and references
      (setq contents (org-process-equation-references contents))
      
      ;; Fix LaTeX special characters that weren't properly converted
      (setq contents (replace-regexp-in-string "{\\\\\"o}" "ö" contents))
      (setq contents (replace-regexp-in-string "{\\\\\"a}" "ä" contents))
      (setq contents (replace-regexp-in-string "{\\\\\"u}" "ü" contents))
      (setq contents (replace-regexp-in-string "{\\\\\"O}" "Ö" contents))
      (setq contents (replace-regexp-in-string "{\\\\\"A}" "Ä" contents))
      (setq contents (replace-regexp-in-string "{\\\\\"U}" "Ü" contents))
      (setq contents (replace-regexp-in-string "{\\\\'e}" "é" contents))
      (setq contents (replace-regexp-in-string "{\\\\'a}" "á" contents))
      (setq contents (replace-regexp-in-string "{\\\\'i}" "í" contents))
      (setq contents (replace-regexp-in-string "{\\\\'o}" "ó" contents))
      (setq contents (replace-regexp-in-string "{\\\\'u}" "ú" contents))
      
      ;; Replace file:// URLs with proper HTTP paths for baseurl
      (let ((file-url-pattern (concat "href=\"file:/+" jekyll-baseurl "/"))
            (http-path (concat "href=\"" jekyll-baseurl "/")))
        (setq contents (replace-regexp-in-string file-url-pattern http-path contents)))
      
      ;; Convert relative file links to images (from org/_posts/ to assets/)
      ;; Pattern 1: <img src="file://../../assets/filename.ext" ... />
      ;; Pattern 2: <img src="../../assets/filename.ext" ... />
      ;; Replace with: <img src="/my-jekyll/assets/filename.ext" ... />
      (setq contents (replace-regexp-in-string
                      "src=\"file://\\.\\.?/\\.\\.?/assets/\\([^\"]+\\)\""
                      (concat "src=\"" jekyll-baseurl "/assets/\\1\"")
                      contents))
      (setq contents (replace-regexp-in-string
                      "src=\"\\.\\.?/\\.\\.?/assets/\\([^\"]+\\)\""
                      (concat "src=\"" jekyll-baseurl "/assets/\\1\"")
                      contents))
      
      ;; Convert simple image filenames in posts to baseurl paths
      ;; Pattern: <img src="filename.png" ... /> in _posts files
      ;; Replace with: <img src="/my-jekyll/assets/filename.png" ... />
      (when (string-match-p "/_posts/" (or (plist-get info :output-file) ""))
        (setq contents (replace-regexp-in-string
                        "src=\"\\([^/\"]+\\.\\(png\\|jpg\\|gif\\|pdf\\)\\)\""
                        (concat "src=\"" jekyll-baseurl "/assets/\\1\"")
                        contents)))
      
      ;; Add Jekyll front matter for posts
      (when (string-match-p "/_posts/" (or (plist-get info :output-file) ""))
        (let ((front-matter (org-jekyll-front-matter info)))
          (setq contents (concat front-matter contents))))
      
      ;; Add processing marker at the end
      (setq contents (concat contents "\n<!-- PROCESSED-BY-ORG-HTML-FINAL-FUNCTION -->\n"))))
  contents)

;; Add the final processing filter
(add-to-list 'org-export-filter-final-output-functions 'org-html-final-function)

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
         :exclude "_posts"  ;; Exclude _posts subdirectory - handled by "posts" project
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
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
         :publishing-directory "./assets"
         :recursive t
         :publishing-function org-publish-attachment)
        ("all" :components ("main-site" "posts" "resources"))))

(provide 'org_publish)
