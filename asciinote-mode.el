;;; asciinote-mode.el --- Major mode for writing notes in Asciidoc -*- lexical-binding: t; -*-

;;; Commentary:
;; still developing

;;; Code:
;;;
;;;
;;;
(defconst an-mode-version "0.0.1"
  "Asciinote mode version number.")

(defconst an-output-buffer-name "*asciinote-output*"
  "Name of the temporary buffer for asciinote mode command output.")

;;; Customizable Variables

(defgroup asciinote nil
  "Major mode for editing notes in Asciidoc."
  :prefix "an-"
  :group 'text)

(defcustom an-command
  "asciidoctor -"
  "Command to generate an HTML output. The default accepts input from stdin."
  :group 'asciinote
  :type '(choice (string :tag "Shell command")
                 function))

(defcustom an-attributes
  '(("linkcss" . "")
    ("docinfodir" . "/home/sz/docs/pensieve/assets")
    ("docinfo" . "shared, private")
    ("stylesdir" . "/home/sz/docs/pensieve/assets")
    ("stylesheet" . "/home/sz/docs/pensieve/assets/ngismook.css"))
  "Attributes added to the asciidoctor command line options. Overrides the attributes specified in the document."
  :group 'asciinote
  :type 'alist)


(defun an-get-attributes (x)
  "Return the command line option string of asciidoc attributes from an alist `X'."
  (let ((parse-attributes (lambda (x)
               (let ((key (car x))
                     (val (cdr x)))
                 (if val
                     (format "--attribute %s=\"%s\"" key val)
                   (format "--attribute %s" key))))))
      (string-join (mapcar parse-attributes x) " ")))


(defun an-asciidoctor-command-string (input-file output-file &optional options)
  "Return a command line string to invoke asciidoctor to build the `OUTPUT-FILE' from `INPUT-FILE' with `OPTIONS'. If the files are nil, `-' is provided to indicate the use of stdin and stdout."
  (let ((outfn (or output-file "-"))
        (infn (or input-file "-"))
        (attributes (an-get-attributes an-attributes)))
    (string-join (list "asciidoctor" attributes options "-o" outfn infn) " ")))

(defun an-run-asciidoctor
    (begin-region end-region to-buffer output-name &rest options)
  "Run asciidoctor on region specified by `BEGIN-REGION' and `END-REGION'. If `TO-BUFFER' is not nil, `OUTPUT-NAME' is treated as a file name to write the outputput to; otherwise it is treated as a buffer name to dump the outputs. `OPTIONS' are options used by asciidoctor command line."
       (if to-buffer
           (let ((buf (get-buffer-create output-name)))
             (with-current-buffer buf
               (setq buffer-read-only nil)
               (erase-buffer)
               (call-process-region begin-region end-region
                                    "/bin/bash" nil buf nil
                                    shell-command-switch
                                    (an-asciidoctor-command-string nil nil options))))
         (let ((to-run (an-asciidoctor-command-string nil output-name options)))
             (call-process-region begin-region end-region
                              "/bin/bash" nil 1 nil
                              shell-command-switch
                              to-run))))




;; (defun an-preview (&optional output-buffer-name)
;;   "Run the `an-command' on the current buffer and view the output in the browser. When OUTPUT-BUFFER-NAME is given, insert the output in the buffer with that name."
;;   (interactive)
;;   ;; (if (buffer-file-name)
;;   ;;     (write-region (point-min) (point-max)
;;   ;;                   (convert-standard-filename
;;   ;;                    (make-temp-file
;;   ;;                     (expand-file-name buffer-file-name temp-dir))) nil 'no-message))
;;    (browse-url-of-buffer
;;    (an-standalone (or output-buffer-name an-output-buffer-name))))

(defun an-preview-if-mode ()
  "Run a preview job if the mode is asciinote-mode."
  (interactive)
  (if (derived-mode-p 'asciinote-mode)
      ;; if the buffer has a file, then generate a rendered html file
      (if buffer-file-name
          (let ((output-file-name
                 (concat (file-name-sans-extension buffer-file-name) ".html")))
            (an-run-asciidoctor (point-min) (point-max)
                                nil output-file-name)
            )
        ;; if the buffer doesn't have a name, output to a temp buffer
        (an-run-asciidoctor (point-min) (point-max)
                            1 an-output-buffer-name))))



(add-to-list 'auto-mode-alist '("\\.adoc\\'" . asciinote-mode))

;;; Font lock
(defvar an-header-face 'an-header-face
  "Face for base headers.")

;;; regexp generators from adoc-mode by sensorflo

;; bug: if qualifier is "+", and the thing to match starts at the end of a
;;      line (i.e. the first char is newline), then wrongly this regexp does
;;      never match.
;; Note: asciidoc uses Python's \s to determine blank lines, while _not_
;;       setting either the LOCALE or UNICODE flag, see
;;       Reader.skip_blank_lines. Python uses [ \t\n\r\f\v] for it's \s . So
;;       the horizontal spaces are [ \t].
(defun adoc-re-content (&optional qualifier)
  "Matches content, possibly spawning multiple non-blank lines"
  (concat
   "\\(?:"
   ;; content on initial line
   "." (or qualifier "*") "?"
   ;; if content spawns multiple lines
   "\\(?:\n"
     ;; complete non blank lines
     "\\(?:[ \t]*\\S-.*\n\\)*?"
     ;; leading content on last line
     ".*?"
   "\\)??"
   "\\)"))

(defun adoc-re-quote-precondition (not-allowed-chars)
  "Regexp that matches before a (un)constrained quote delimiter.
NOT-ALLOWED-CHARS are chars not allowed before the quote."
  (concat
     "\\(?:"
       "^"
     "\\|"
       "\\="
     "\\|"
       ; or *not* after
       ; - an backslash
       ; - user defined chars
       "[^" not-allowed-chars "\\\n]"
     "\\)"))

(defvar adoc-re-inline-attr
  (rx (zero-or-one (group "[" (+? (not (any "]" "["))) "]"))))

(defun adoc-re-unconstrained-quote (ldel &optional rdel)
  (unless rdel (setq rdel ldel))
  (let ((qldel (regexp-quote ldel))
        (qrdel (regexp-quote rdel)))
    (concat
     (adoc-re-quote-precondition "")
     ;"\\(\\[[^][]+?\\]\\)?"
     adoc-re-inline-attr
     "\\(" qldel "\\)"
     "\\(" (adoc-re-content "+") "\\)"
     "\\(" qrdel "\\)")))

;; AsciiDoc src for constrained quotes
;; # The text within constrained quotes must be bounded by white space.
;; # Non-word (\W) characters are allowed at boundaries to accommodate
;; # enveloping quotes.
;;
;; reo = re.compile(r'(?msu)(^|[^\w;:}])(\[(?P<attrlist>[^[\]]+?)\])?' \
;;     + r'(?:' + re.escape(lq) + r')' \
;;     + r'(?P<content>\S|\S.*?\S)(?:'+re.escape(rq)+r')(?=\W|$)')
(defun adoc-re-constrained-quote (ldel &optional rdel)
  "
subgroups:
1 attribute list [optional]
2 starting del
3 enclosed text
4 closing del"
  (unless rdel (setq rdel ldel))
  (let ((qldel (regexp-quote ldel))
        (qrdel (regexp-quote rdel)))
    (concat
     ;; added &<> because those are special chars which are substituted by a
     ;; entity, which ends in ;, which is prohibited in the ascidoc.conf regexp
     (adoc-re-quote-precondition "A-Za-z0-9;:}&<>")
     ;; "\\(\\[[^][]+?\\]\\)?"
     adoc-re-inline-attr
     "\\(" qldel "\\)"
     "\\([^ \t\n]\\|[^ \t\n]" (adoc-re-content) "[^ \t\n]\\)"
     "\\(" qrdel "\\)"
     ;; BUG: now that Emacs doesn't has look-ahead, the match is too long, and
     ;; adjancted quotes of the same type wouldn't be recognized.
     "\\(?:[^A-Za-z0-9\n]\\|[ \t]*$\\)")))

(defun adoc-re-quote (type ldel &optional rdel)
  (cond
   ((eq type 'adoc-constrained)
    (adoc-re-constrained-quote ldel rdel))
   ((eq type 'adoc-unconstrained)
    (adoc-re-unconstrained-quote ldel rdel))
   (t
    (error "Invalid type"))))


;; from asciidoc.conf: ^= +(?P<title>[\S].*?)( +=)?$
;; asciidoc src code: Title.isnext reads two lines, which are then parsed by
;; Title.parse. The second line is only for the underline of two line titles.
(defun adoc-re-one-line-title (level)
  "Returns a regex matching a one line title of the given LEVEL.
When LEVEL is nil, a one line title of any level is matched.
match-data has these sub groups:
1 leading delimiter inclusive whites between delimiter and title text
2 title's text exclusive leading/trailing whites
3 trailing delimiter with all whites
4 trailing delimiter only inclusive whites between title text and delimiter
0 only chars that belong to the title block element
==  my title  ==  n
---12------23------
            4--4"
  (let* ((del (if level
                 (make-string (+ level 1) ?=)
               (concat "=\\{1," (+ adoc-title-max-level 1) "\\}"))))
    (concat
     "^\\(" del "[ \t]+\\)"		      ; 1
     "\\([^ \t\n].*?\\)"                          ; 2
     ;; using \n instad $ is important so group 3 is guaranteed to be at least 1
     ;; char long (except when at the end of the buffer()). That is important to
     ;; to have a place to put the text property adoc-reserved on.
     "\\(\\([ \t]+" del "\\)?[ \t]*\\(?:\n\\|\\'\\)\\)" ))) ; 3 & 4

(defface an-doc-title
  '((((class color) (min-colors 88)) :weight bold :foreground "orange"))
  "document title face")

(defvar an-regex-header
  "^\\(=\\|#\\) \\(\\w.*\\)$\\n?")

(defun an-fontify-headings (last)
  "Add text properties to headings from point to LAST."
  )

(defun url-to-cite (url)
  "Fetch the pdf or html from the given URL and parse it for the metainfo, and insert a proper asciidoc citation with these info."
  )


;; Syntax highlighting keywords for asciidoc mode
(defvar an-mode-font-lock-keywords
  (list
   `(,(adoc-re-one-line-title 0)
     (1 font-lock-keyword-face)
     (2 'an-doc-title))
   `(,(adoc-re-one-line-title 1)
     (1 font-lock-keyword-face)
     (2 'bold))
   `(,(adoc-re-one-line-title 2)
     (1 font-lock-keyword-face)
     (2 'bold))
   `(,(adoc-re-quote 'adoc-constrained "*")
     (1 font-lock-keyword-face t t) ; attr list
     (2 font-lock-keyword-face) ; open del
     (3 'bold) ;text
     (4 font-lock-keyword-face) ; close
                                )))

(defun an-preview-browser ()
  "Display the same-name file with html extension using the system default browser."
  (interactive)
  (let ((output-file-name
         (concat (file-name-sans-extension buffer-file-name) ".html")))
    (if (file-exists-p output-file-name)
        (browse-url-of-file output-file-name)
      (message (format-message
                "The target file does not exist." output-file-name)))))

(defvar asciinote-mode-map nil "Keymap for `asciinote-mode'")


(define-derived-mode asciinote-mode text-mode "Asciinote"
  "Major mode for editing notes in Asciinote"
  (add-hook 'after-save-hook #'an-preview-if-mode t t)
  (setq font-lock-defaults
         '(an-mode-font-lock-keywords
           nil nil nil
           (font-lock-multiline t)
           )))

(setq asciinote-mode-map (make-sparse-keymap))
;; I do not explicitly set the keymaps here. The users need to do it themselves.


(provide 'asciinote-mode)
;;; asciinote-mode.el ends here
