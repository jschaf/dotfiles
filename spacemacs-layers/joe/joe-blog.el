;;; joe-blog.el --- a clean html export for org-export

;;; Commentary:
;;

;;; Code:
(require 'org)
(require 'ox)
(require 'ox-html)
(require 'ox-publish)
(require 'url-util)

(eval-when-compile (require 'cl))

(defvar joe-blog-directory "~/prog/blog-redux")
(defvar joe-blog-url "http://delta46.us")

;; Prevent org-error, see http://wenshanren.org/?p=781
(defun org-font-lock-ensure ()
  (font-lock-fontify-buffer))

(setq org-html-with-latex 't)

;; We use KaTeX, not MathJax
(setq org-html-mathjax-template "")

;; Use css classes to colorize code.
(setq org-html-htmlize-output-type 'css)

(setq org-publish-project-alist
      `(("blog-redux-content"
         :author "Joe Schafer"
         :email "Joe.Schafer@delta46.us"
         :base-directory ,(concat joe-blog-directory "/posts")
         :base-extension "org"
         :publishing-directory "~/prog/blog-redux/output"
         :publishing-function tufte-publish-to-html

         ;; HTML options
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-html5-fancy t
         :html-doctype "html5"
         :html-container "section"

         ;; General Options
         :with-toc nil
         :headline-levels 3
         :table-of-contents nil
         :section-numbers nil
         :with-smart-quotes t
         )
        ("blog-redux-static"
         :base-directory "~/prog/blog-redux/static"
         :recursive t
         :base-extension "css\\|eot\\|svg\\|ttf\\|woff"
         :publishing-directory "~/prog/blog-redux/output/static"
         :publishing-function org-publish-attachment
         )
        ("blog-redux-bing-verification"
         :base-directory "~/prog/blog-redux/static"
         :base-extension "xml"
         :publishing-directory "~/prog/blog-redux/output"
         :publishing-function org-publish-attachment
         )
        ("blog-redux"
         :components ("blog-redux-content" "blog-redux-static" "blog-redux-bing-verification"))))

(defun joe-blog-compile (&optional force)
  "Compile the blog-redux project.
If FORCE is non-nil, force recompilation even if files haven't changed."
  (interactive)
  (require 'bibtex)
  (setq-default bibtex-dialect 'biblatex)
  (bibtex-set-dialect 'biblatex)
  (org-publish "blog-redux" force)
  (compile (format "make -C %s mathify" joe-blog-directory)))

(defun joe-blog-publish-to-server ()
  (interactive)
  (compile (format "make -C %s publish" joe-blog-directory)))

(defun joe-blog-compile-and-publish ()
  (interactive)
  (joe-blog-compile)
  (joe-blog-publish-to-server))

(evil-leader/set-key
  "cb" 'joe-blog-compile
  "cB" '(lambda () (interactive) (joe-blog-compile 'force))
  "cp" 'joe-blog-compile-and-publish)

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 0.5 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)



(defvar tufte-sitemap-xml-template
  "<?xml version='1.0' encoding='UTF-8'?>
<urlset xmlns='http://www.sitemaps.org/schemas/sitemap/0.9'>
%s
</urlset>")

(defvar tufte-sitemap-xml-url-template
  (concat "  <url>\n"
          "    <loc>%s</loc>\n"
          ;; "<lastmod>%s</lastmod>\n"
          ;; "<changefreq>monthly</changefreq>\n"
          ;; "<priority>0.5</priority>\n"
          "  </url>"))

(defun tufte-publish-sitemap ()
  (interactive)

  (let* ((sitemap-output-file (concat joe-blog-directory "/output/sitemap.xml"))
         (urls
          (mapcar #'(lambda (file)
                      (let ((post-name (file-name-sans-extension
                                        (file-name-nondirectory file))))
                        (when (string-equal post-name "index") (setq post-name ""))
                        (concat joe-blog-url "/" post-name)))
                  (directory-files (concat joe-blog-directory "/posts") t ".org$")))
         (url-xmls (mapconcat #'(lambda (url) (format tufte-sitemap-xml-url-template url))
                              urls "\n"))
         (sitemap-xml (format tufte-sitemap-xml-template url-xmls)))
    (write-region sitemap-xml nil sitemap-output-file)))

(org-export-define-derived-backend
    'html-tufte 'html
  :export-block "HTML-Tufte"
  :menu-entry '(?H "Export as Tufte HTML" tufte-export-to-html)
  :translate-alist
  '((footnote-reference . tufte-footnote-reference)
    (inner-template . tufte-inner-template)
    (link . tufte-link)
    (paragraph . tufte-paragraph)
    (section . tufte-section)
    (src-block . tufte-src-block)
    (template . tufte-html-template)))

(defvar tufte-footnote-separator "")

(defvar tufte-sidenote-reference-format
  (concat  "<label for='%s' class='margin-toggle sidenote-number'></label>"
           "<input type='checkbox' id='%s' class='margin-toggle'/>"))

(defvar tufte-sidenote-definition-format
  "<span itemprop='citation' class='sidenote'>%s</span>")

(defun tufte-format-sidenote-reference (n def refcnt)
  "Format footnote reference N with definition DEF into HTML."
  (let* ((extra (if (= refcnt 1) "" (format ".%d"  refcnt)))
         (id (format "sn-%s%s" n extra)))

    (message "id is %s" id)
    (concat
     (format tufte-sidenote-reference-format id id)
     "\n"
     (format tufte-sidenote-definition-format def))))

(defvar tufte-marginnote-symbol "&#8853;"
 "⊕" )

(defvar tufte-marginnote-reference-format
  (concat  "<label for='%s' class='margin-toggle'>"
           tufte-marginnote-symbol
           "</label>"
           "<input type='checkbox' id='%s' class='margin-toggle'/>"))

(defvar tufte-marginnote-definition-format "<span class='marginnote'>%s</span>")

(defun tufte-format-marginnote (n def refcnt)
  "Format footnote reference N with definition DEF into HTML."
  (let* ((extra (if (= refcnt 1) "" (format ".%d"  refcnt)))
         (id (format "sn-%s%s" n extra)))
    (concat
     (format tufte-marginnote-reference-format id id)
     "\n"
     (format tufte-marginnote-definition-format def))))


;; Only change is removing the footnote body.
(defun tufte-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat

   "<article itemscope itemtype='http://schema.org/Article'>\n"

   "<header>\n"
   (let ((title (plist-get info :title)))
     (format "<h1 itemprop='headline'>%s</h1>\n" (org-export-data (or title "") info)))

   "</header>\n"

   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))

   ;; Document contents.
   contents

   ;; Bibliography
   (org-ref-get-html-bibliography)

   ;; Footer
   "<footer>"
   (if (org-export-get-date info "%Y")
       (format "Published on <time itemprop='datePublished' datetime='%s'>%s</time>"
               (org-export-get-date info "%Y-%m-%d")
               (org-export-get-date info "%d %B %Y"))
     "Written")
   " by <span itemprop='author'>Joe Schafer</span>."
   "</footer>\n"

   "</article>\n"
   ))

(defvar tufte-main-header
  "<header id='main-header'>
  <nav><a href='/'><span>Joe Schafer's Blog<span></a></nav>
</header>")

(setq org-html-divs
      '((preamble  "div" "preamble")
        (content   "div" "content")
        (postamble "footer" "main-footer")))

(defun tufte-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   "<!DOCTYPE html>\n"
   "<html>\n"
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   ;; (org-html--build-mathjax-config info)
   "</head>\n"
   "<body itemscope itemtype='http://schema.org/Blog'>\n"
   tufte-main-header
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   "<main>"
   contents
   "</main>"

   ;; Postamble.
   "<footer>"
   "<span itemprop='author'>Joe Schafer</span> © <span itemprop='copyrightYear'>2015</span>."
   " Built with Emacs, caffeine,  Oxford commas, and Org-Mode."
   "</footer>"

   ;; Closing document.
   "</body>\n</html>"))
;;;; Footnote Reference

(defun tufte-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((prev (org-export-get-previous-element footnote-reference info))
         (footnote-def (org-export-get-footnote-definition
                        footnote-reference info))
         (footnote-text (if (eq (org-element-type footnote-def)
                                'org-data)
                            ;; a full org-data AST
                            (org-trim (org-export-data footnote-def info))
                          ;; plain text
                          (org-trim (org-export-data footnote-def info)))))
    (concat

     ;; Insert separator between two footnotes in a row.
     (when (eq (org-element-type prev) 'footnote-reference)
       tufte-footnote-separator)

     (cond
      ((not (org-export-footnote-first-reference-p footnote-reference info))
       (tufte-format-sidenote-reference
        (org-export-get-footnote-number footnote-reference info)
        footnote-text
        100))

      ;; Inline definitions are secondary strings.
      ((eq (org-element-property :type footnote-reference) 'inline)
       (tufte-format-marginnote
        (org-export-get-footnote-number footnote-reference info)
        footnote-text
        1))

      ;; Non-inline footnotes definitions are full Org data.
      (t (tufte-format-sidenote-reference
          (org-export-get-footnote-number footnote-reference info)
          footnote-text 1))))))

(defun tufte-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-export-get-parent paragraph))
         (parent-type (org-element-type parent))
         (style '((footnote-definition " class=\"footpara\"")))
         (extra (or (cadr (assoc parent-type style)) "")))
    (cond
     ;; Leading paragraph in a list item have no tags.
     ((and (eq (org-element-type parent) 'item)
           (= (org-element-property :begin paragraph)
              (org-element-property :contents-begin parent)))
      contents)

     ;; Standalone image.
     ((org-html-standalone-image-p paragraph info)
      (let ((caption
             (let ((raw (org-export-data
                         (org-export-get-caption paragraph) info))
                   (org-html-standalone-image-predicate
                    'org-html--has-caption-p))
               (if (not (org-string-nw-p raw)) raw
                 (concat
                  "<span class='figure-number'>"
                  (format (org-html--translate "Figure %d:" info)
                          (org-export-get-ordinal
                           (org-element-map paragraph 'link
                             'identity info t)
                           info nil 'org-html-standalone-image-p))
                  "</span> " raw))))
            (label (org-element-property :name paragraph)))
        (org-html--wrap-image contents info caption label)))

     ;; Footnote definition.  Don't put in a paragraph tag because the sidenote
     ;; is a span.  A p tag inside a span will cause nothing to render.
     ((eq parent-type 'footnote-definition)
      contents)

     ;; Regular paragraph.
     (t (format "<p>\n%s</p>" contents)))))

(defun tufte-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
          (caption (org-export-get-caption src-block))
          (code (org-html-format-code src-block info))
          (label (let ((lbl (org-element-property :name src-block)))
                   (if (not lbl) ""
                     (format " id='%s'"
                             (org-export-solidify-link-text lbl))))))
      (if (not lang) (format "<pre class='code'%s>\n%s</pre>" label code)
        (if (not caption) ""
          (format "<label class='org-src-name'>%s</label>"
                  (org-export-data caption info)))
        (format "\n<pre class='code src src-%s'%s>%s</pre>" lang label code)))))

(defun tufte-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let* ((class-num (+ (org-export-get-relative-level parent info)
                           (1- org-html-toplevel-hlevel)))
             (section-number
              (mapconcat
               'number-to-string
               (org-export-get-headline-number parent info) "-")))
        ;; Build return value.
        contents))))

(defun tufte-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((home (when (plist-get info :html-link-home)
                 (org-trim (plist-get info :html-link-home))))
         (use-abs-url (plist-get info :html-link-use-abs-url))
         (link-org-files-as-html-maybe
          (function
           (lambda (raw-path info)
             "Treat links to `file.org' as links to `file.html', if needed.
           See `org-html-link-org-files-as-html'."
             (message "Calling link-org-files-as-html-maybe with %s" raw-path)
             (cond
              ((and org-html-link-org-files-as-html
                    (string= ".org"
                             (downcase (file-name-extension raw-path "."))))
               (concat (file-name-sans-extension raw-path)))
              (t
               raw-path)))))
         (type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (org-string-nw-p desc))
         (path
          (cond
           ((member type '("http" "https" "ftp" "mailto"))
            (url-insert-entities-in-string
             (org-link-escape
              (org-link-unescape
               (concat type ":" raw-path)) org-link-escape-chars-browser)))
           ((string= type "file")
            ;; Treat links to ".org" files as ".html", if needed.
            (setq raw-path
                  (funcall link-org-files-as-html-maybe raw-path info))
            ;; If file path is absolute, prepend it with protocol
            ;; component - "file:".
            (cond
             ((file-name-absolute-p raw-path)
              (setq raw-path (concat "file:" raw-path)))
             ((and home use-abs-url)
              (setq raw-path (concat (file-name-as-directory home) raw-path))))
            ;; Add search option, if any.  A search option can be
            ;; relative to a custom-id or a headline title.  Any other
            ;; option is ignored.
            (let ((option (org-element-property :search-option link)))
              (cond ((not option) raw-path)
                    ((eq (aref option 0) ?#) (concat raw-path option))
                    ;; External fuzzy link: try to resolve it if path
                    ;; belongs to current project, if any.
                    ((eq (aref option 0) ?*)
                     (concat
                      raw-path
                      (let ((numbers
                             (org-publish-resolve-external-fuzzy-link
                              (org-element-property :path link) option)))
                        (and numbers (concat "#sec-"
                                             (mapconcat 'number-to-string
                                                        numbers "-"))))))
                    (t raw-path))))
           (t raw-path)))
         ;; Extract attributes from parent's paragraph.  HACK: Only do
         ;; this for the first link in parent (inner image link for
         ;; inline images).  This is needed as long as attributes
         ;; cannot be set on a per link basis.
         (attributes-plist
          (let* ((parent (org-export-get-parent-element link))
                 (link (let ((container (org-export-get-parent link)))
                         (if (and (eq (org-element-type container) 'link)
                                  (org-html-inline-image-p link info))
                             container
                           link))))
            (and (eq (org-element-map parent 'link 'identity info t) link)
                 (org-export-read-attribute :attr_html parent))))
         (attributes
          (let ((attr (org-html--make-attribute-string attributes-plist)))
            (if (org-string-nw-p attr) (concat " " attr) "")))
         protocol)
    (cond
     ;; Image file.
     ((and org-html-inline-images
           (org-export-inline-image-p link org-html-inline-image-rules))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (if (not destination) desc
          (format "<a href='#%s'%s>%s</a>"
                  (org-export-solidify-link-text
                   (org-element-property :value destination))
                  attributes desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (case (org-element-type destination)
          ;; ID link points to an external file.
          (plain-text
           (let ((fragment (concat "ID-" path))
                 ;; Treat links to ".org" files as ".html", if needed.
                 (path (funcall link-org-files-as-html-maybe
                                destination info)))
             (format "<a href='%s#%s'%s>%s</a>"
                     path fragment attributes (or desc destination))))
          ;; Fuzzy link points nowhere.
          ((nil)
           (format "<i>%s</i>"
                   (or desc
                       (org-export-data
                        (org-element-property :raw-link link) info))))
          ;; Link points to a headline.
          (headline
           (let ((href
                  ;; What href to use?
                  (cond
                   ;; Case 1: Headline is linked via it's CUSTOM_ID
                   ;; property.  Use CUSTOM_ID.
                   ((string= type "custom-id")
                    (org-element-property :CUSTOM_ID destination))
                   ;; Case 2: Headline is linked via it's ID property
                   ;; or through other means.  Use the default href.
                   ((member type '("id" "fuzzy"))
                    (format "sec-%s"
                            (mapconcat 'number-to-string
                                       (org-export-get-headline-number
                                        destination info) "-")))
                   (t (error "Shouldn't reach here"))))
                 ;; What description to use?
                 (desc
                  ;; Case 1: Headline is numbered and LINK has no
                  ;; description.  Display section number.
                  (if (and (org-export-numbered-headline-p destination info)
                           (not desc))
                      (mapconcat 'number-to-string
                                 (org-export-get-headline-number
                                  destination info) ".")
                    ;; Case 2: Either the headline is un-numbered or
                    ;; LINK has a custom description.  Display LINK's
                    ;; description or headline's title.
                    (or desc (org-export-data (org-element-property
                                               :title destination) info)))))
             (format "<a href='#%s'%s>%s</a>"
                     (org-export-solidify-link-text href) attributes desc)))
          ;; Fuzzy link points to a target or an element.
          (t
           (let* ((path (org-export-solidify-link-text path))
                  (org-html-standalone-image-predicate 'org-html--has-caption-p)
                  (number (cond
                           (desc nil)
                           ((org-html-standalone-image-p destination info)
                            (org-export-get-ordinal
                             (org-element-map destination 'link
                               'identity info t)
                             info 'link 'org-html-standalone-image-p))
                           (t (org-export-get-ordinal
                               destination info nil 'org-html--has-caption-p))))
                  (desc (cond (desc)
                              ((not number) "No description for this link")
                              ((numberp number) (number-to-string number))
                              (t (mapconcat 'number-to-string number ".")))))
             (format "<a href='#%s'%s>%s</a>" path attributes desc))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" path)))
        (format "<a href='#%s'%s%s>%s</a>"
                fragment
                (org-trim
                 (format (concat "class='coderef'"
                                 " onmouseover='CodeHighlightOn(this, '%s');'"
                                 " onmouseout='CodeHighlightOff(this, '%s');'")
                         fragment fragment))
                attributes
                (format (org-export-get-coderef-format path desc)
                        (org-export-resolve-coderef path info)))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'html))
     ;; External link with a description part.
     ((and path desc) (format "<a href='%s'%s>%s</a>" path attributes desc))
     ;; External link without a description part.
     (path (format "<a href='%s'%s>%s</a>" path attributes path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<i>%s</i>" desc)))))

(defun tufte-advice-create-index-folder (orig-fun &rest args)
  "Patch `org-export-output-file-name' to return my-post/index.html.
Argument ORIG-FUN the function being advised.
Optional argument ARGS the arguments to ORIG-FUN."

  (let* ((orig-output (apply orig-fun args))
         (new-output (concat (file-name-sans-extension orig-output) "/index.html")))
    (if (equal (file-name-nondirectory orig-output) "index.html")
        orig-output
      (make-directory (file-name-directory new-output) t)
      new-output)))

(defun tufte-advice-escape-html-citations (orig-fun &rest args)
  "Patch ORIG-FUN to escape HTML entities.
Argument ORIG-FUN the function being advised.
Optional argument ARGS the arguments to ORIG-FUN."
  (url-insert-entities-in-string (apply orig-fun args)))

;;;###autoload
(defun tufte-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between '<body>' and '</body>' tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (advice-add 'org-export-output-file-name
              :around #'tufte-advice-create-index-folder)
  (advice-add 'org-ref-reftex-get-bib-field
              :around #'tufte-advice-escape-html-citations)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'html file
      async subtreep visible-only body-only ext-plist))
  (advice-remove 'org-ref-reftex-get-bib-field
                 #'tufte-advice-escape-html-citations)
  (advice-remove 'org-export-output-file-name
                 #'tufte-advice-create-index-folder))


;;;###autoload
(defun tufte-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  ;; Reset the org-ref citation counter.
  (setq tufte-citation-counts (make-hash-table :test 'equal))
  (advice-add 'org-export-output-file-name
              :around #'tufte-advice-create-index-folder)
  (advice-add 'org-ref-reftex-get-bib-field
              :around #'tufte-advice-escape-html-citations)

  (org-publish-org-to 'html-tufte filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension "html"))
                      plist pub-dir)

  (advice-remove 'org-ref-reftex-get-bib-field
                 #'tufte-advice-escape-html-citations)
  (advice-remove 'org-export-output-file-name
                 #'tufte-advice-create-index-folder)
  (tufte-publish-sitemap))

;; Counter for the number of citations.  We need this because if we cite an item
;; multiple times, the id must be unique.
(defvar tufte-citation-counts (make-hash-table :test 'equal))

;;; org-ref.  This overrides a defmacro call in org-ref.
(defun org-ref-format-cite (keyword desc format)
  (let ((key keyword)
        num-cites
        key-unique)
    ;; Increment the counter or initialize
    (setq num-cites (if (gethash key tufte-citation-counts)
                        (puthash key (1+ (gethash key tufte-citation-counts))
                                 tufte-citation-counts)
                      (puthash key 1 tufte-citation-counts)))
    (setq key-unique (format "%s.%d" key num-cites))
    (cond
     ;; HTML
     ((eq format 'html)
      (concat
       (format tufte-sidenote-reference-format key-unique key-unique)
       (format tufte-sidenote-definition-format
               (concat (org-ref-get-bibtex-entry-html key)
                       (when desc (format ", %s" desc))))))

     (t
      (error "I removed extra backends for org-ref on 29 November 2015.")))))


;; Override org-ref
(defun org-ref-get-bibtex-entry-html (key)
  "Return an html string for the bibliography entry corresponding to KEY."
  (let ((output))
    (setq output (org-ref-get-bibtex-entry-citation key))
    ;; unescape the &
    (setq output (replace-regexp-in-string "\\\\&" "&" output))
    ;; hack to replace {} around text
    (setq output (replace-regexp-in-string "{" "" output))
    (setq output (replace-regexp-in-string "}" "" output))
    ;; get rid of empty parens
    (setq output (replace-regexp-in-string "()" "" output))
    ;; get rid of empty link and doi
    (setq output (replace-regexp-in-string " <a href=''>link</a>\\." "" output))
    ;; change double dash to single dash
    (setq output (replace-regexp-in-string "--" "-" output))
    (setq output (replace-regexp-in-string " <a href='http://dx\\.doi\\.org/'>doi</a>\\." "" output))
    output))

(setq org-ref-bibliography-entry-format
      `(("article"       . "%a, <a href='%U'>%t</a>, <i>%j</i>, (%y)")
        ("book"          . "%a, %t, %u (%y).")
        ("inproceedings" . "%a, <a href='%U'>%t</a> %b, %u (%y)")
        ("legislation"   . "%a, <a href='%U'>%t</a>, (%y)")
        ("mvbook"          . "%a, %t, %u (%y).")
        ("online"        . "%a, <a href='%U'>%t</a>")
        ("proceedings"   . "%e, %t in %S, %u (%y).")
        ("report"        . "%a, <a href='%U'>%t</a>")
        ("techreport"    . "%a, %t, %i, %u (%y).")
        ))


;; %l   The BibTeX label of the citation.
;; %a   List of author names, see also `reftex-cite-punctuation'.
;; %2a  Like %a, but abbreviate more than 2 authors like Jones et al.
;; %A   First author name only.
;; %e   Works like %a, but on list of editor names. (%2e and %E work a well)

;; It is also possible to access all other BibTeX database fields:
;; %b booktitle     %c chapter        %d edition    %h howpublished
;; %i institution   %j journal        %k key        %m month
;; %n number        %o organization   %p pages      %P first page
;; %r address       %s school         %u publisher  %t title
;; %v volume        %y year
;; %B booktitle, abbreviated          %T title, abbreviated
;; %U url
;; %D doi
;; %S series

;; Usually, only %l is needed.  The other stuff is mainly for the echo area
;; display, and for (setq reftex-comment-citations t).

;; %< as a special operator kills punctuation and space around it after the
;; string has been formatted.

;; A pair of square brackets indicates an optional argument, and RefTeX
;; will prompt for the values of these arguments.

(require 'bibtex)

(add-to-list 'bibtex-biblatex-entry-alist
             '("Legislation" "Legislation"
               (("title"
                 ("year" nil nil 0) ("date" nil nil 0))
                nil
                (("translator") ("annotator") ("commentator") ("subtitle") ("titleaddon")
                 ("editor") ("editora") ("editorb") ("editorc")
                 ("journalsubtitle") ("issuetitle") ("issuesubtitle")
                 ("language") ("origlanguage") ("series") ("volume") ("number") ("eid")
                 ("issue") ("month") ("pages") ("version") ("note") ("issn")
                 ("addendum") ("pubstate") ("doi") ("eprint") ("eprintclass")
                 ("eprinttype") ("url") ("urldate")))))

;; Override
(defun org-ref-get-html-bibliography ()
  "Create an html bibliography when there are keys."
  (let ((keys (org-ref-get-bibtex-keys)))
    (when keys
      (concat
       "<section>\n"
       "<h2 class='org-ref-bib-h1' id='bibliography'>Bibliography</h2>\n"
       (mapconcat (lambda (x) (concat "<p>"
                                      (org-ref-get-bibtex-entry-html x)
                                      "</p>"))
                  keys "\n")
       "</section>\n"))))



(provide 'joe-blog)
