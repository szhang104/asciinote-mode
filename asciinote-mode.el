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

(defcustom an-command "asciidoctor -"
  "Command to generate an HTML output. The default accepts input from stdin."
  :group 'asciinote
  :type '(choice (string :tag "Shell command")
                 function))

(defcustom an-command-needs-filename nil
  "Set to non-nil if `an-command' does not accept input from stdin.
Instead, it will be passed a filename as the final commandline option."
  :group 'asciinote
  :type 'boolean)


(defun an-run (&optional output-buffer-name)
  "Run `an-command' on buffer, sending the output to OUTPUT-BUFFER-NAME."
  (interactive)
  (save-window-excursion
    (let ((begin-region)
          (end-region))
      (setq begin-region (point-min)
            end-region (point-max))
      (unless output-buffer-name
        (setq output-buffer-name an-output-buffer-name))
      (let ((exit-code
             (cond
              ;; need to handle the case when `an-command' does not read from stdin
              ((and (stringp an-command) an-command-needs-filename)
               (user-error "Must use a file"))
              ;; default case: pass region to `an-command' via stdin
              (t
               (let ((buf (get-buffer-create output-buffer-name)))
                 (with-current-buffer buf
                   (setq buffer-read-only nil)
                   (erase-buffer))
                 (if (stringp an-command)
                     (call-process-region begin-region end-region
                                          "/bin/bash" nil buf nil
                                          shell-command-switch an-command)
                   (funcall an-command begin-region end-region buf)
                   ;; If there is no error from `an-command', assume the successful result with exit-code 0
                   0))))))
        (unless (eq exit-code 0)
          (user-error "%s failed with exit code %s"
                      an-command exit-code))))
    output-buffer-name))



(defun an-standalone (&optional output-buffer-name)
  "Provide standalone HTML output, and insert the output in the buffer named OUTPUT-BUFFER-NAME."
  (interactive)
  (setq output-buffer-name (an-run output-buffer-name))
  (with-current-buffer output-buffer-name
    (set-buffer output-buffer-name)
    (goto-char (point-min))
    (html-mode))
  output-buffer-name)


(defun an-preview (&optional output-buffer-name)
  "Run the `an-command' on the current buffer and view the output in the browser. When OUTPUT-BUFFER-NAME is given, insert the output in the buffer with that name."
  (interactive)
  (browse-url-of-buffer
   (an-standalone (or output-buffer-name an-output-buffer-name))))

(defun an-preview-if-mode ()
  "Run a preview job if the mode is asciinote-mode."
  (interactive)
  (if (derived-mode-p 'asciinote-mode)
      (an-preview)
    nil))

(define-derived-mode asciinote-mode text-mode "Asciinote"
  "Major mode for editing notes in Asciinote"
  (add-hook 'after-save-hook #'an-preview-if-mode t t))


(add-to-list 'auto-mode-alist '("\\.adoc\\'" . asciinote-mode))

(provide 'asciinote-mode)
;;; asciinote-mode.el ends here
