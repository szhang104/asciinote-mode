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

(defcustom an-command-needs-filename nil
  "Set to non-nil if `an-command' does not accept input from stdin.
Instead, it will be passed a filename as the final commandline option."
  :group 'asciinote
  :type 'boolean)

(defun an-asciidoctor-command-string (input-file output-file &optional options)
  "Return a command line string for invoking asciidoctor to build an `OUTPUT-FILE' from `INPUT-FILE' with `OPTIONS'. If the files are nil, `-' is provided to indicate the use of stdin and stdout"
  (let ((outfn (or output-file "-"))
        (infn (or input-file "-")))
    (string-join (list "asciidoctor" options "-o" outfn infn) " ")))

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
                              to-run)
             (message "command: %s" to-run))))




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
            (browse-url-of-file output-file-name))
        ;; if the buffer doesn't have a name, output to a temp buffer
        (an-run-asciidoctor (point-min) (point-max)
                            1 an-output-buffer-name))))

(define-derived-mode asciinote-mode text-mode "Asciinote"
  "Major mode for editing notes in Asciinote"
  (add-hook 'after-save-hook #'an-preview-if-mode t t))


(add-to-list 'auto-mode-alist '("\\.adoc\\'" . asciinote-mode))

(provide 'asciinote-mode)
;;; asciinote-mode.el ends here
