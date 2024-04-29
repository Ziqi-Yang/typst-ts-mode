;;; typst-ts-compile.el --- Compile Typst Files  -*- lexical-binding: t; -*-
;; Copyright (C) 2023 The typst-ts-mode Project Contributors

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'compile)

(defgroup typst-ts-compile nil
  "Typst TS Compilation."
  :prefix "typst-ts-compile"
  :group 'typst-ts)

(defcustom typst-ts-compile-executable-location "typst"
  "The location or name(if in variable `exec-path') for Typst executable."
  :type 'string
  :group 'typst-ts-compile)

(defcustom typst-ts-compile-options ""
  "User defined compile options for `typst-ts-compile'.
The compile options will be passed to the end of
`<typst-executable> compile <current-file>' command."
  :type 'string
  :group 'typst-ts)

(defcustom typst-ts-compile-before-compilation-hook nil
  "Hook runs after compile."
  :type 'hook
  :group 'typst-ts)

(defcustom typst-ts-compile-after-compilation-hook nil
  "Hook runs after compile.
Note the requirement of this hook is the same as `compilation-finish-functions'.
Also note that this hook runs with typst buffer(the buffer you are editing) as
the current buffer."
  :type 'hook
  :group 'typst-ts)

(defun typst-ts-compile--compilation-finish-function (cur-buffer)
  "Compilation finish function.
For `typst-ts-compile-after-compilation-hook' and
`compilation-finish-functions'.  CUR-BUFFER: original typst buffer, in case
user set `display-buffer-alist' option for compilation buffer to switch to
compilation buffer before compilation."
  (lambda (compilation-buffer msg)
    (unwind-protect
        (with-current-buffer cur-buffer
          (run-hook-with-args 'typst-ts-compile-after-compilation-hook compilation-buffer msg))
      (remove-hook 'compilation-finish-functions
                   (typst-ts-compile--compilation-finish-function cur-buffer)))))

(defun typst-ts-compile ()
  "Compile current typst file."
  (interactive)
  (run-hooks typst-ts-compile-before-compilation-hook)

  ;; The reason to take such a awkward solution is that `compilation-finish-functions'
  ;; should be a global variable and also its functions. It doesn't work if we
  ;; define them inside a let binding.
  (add-hook 'compilation-finish-functions
            (typst-ts-compile--compilation-finish-function (current-buffer)))
  (compile
   (format "%s compile %s %s"
           typst-ts-compile-executable-location
           (file-name-nondirectory buffer-file-name)
           typst-ts-compile-options)
   'typst-ts-compilation-mode))

(defun typst-ts-compile-get-result-pdf-filename (&optional buffer check)
  "Get the result PDF filename based on the name of BUFFER.
If BUFFER is nil, it means use the current buffer.
CHECK: non-nil mean check the file existence.
Return nil if the BUFFER has not associated file or the there is
no compiled pdf file when CHECK is non-nil."
  (when-let ((typst-file (buffer-file-name buffer)))
    (let ((res (concat (file-name-base typst-file) ".pdf")))
      (if check
          (when (file-exists-p res)
            res)
        res))))


(defun typst-ts-mode-compile-and-preview--compilation-finish-function (cur-buffer)
  "For `typst-ts-compile-and-preview' and `compilation-finish-functions'.
CUR-BUFFER: original typst buffer, in case user set
`display-buffer-alist' option for compilation buffer to switch to compilation
buffer before compilation."
  (lambda (_b _msg)
    (unwind-protect
        (browse-url (typst-ts-compile-get-result-pdf-filename cur-buffer))
      (remove-hook 'compilation-finish-functions
                   (typst-ts-mode-compile-and-preview--compilation-finish-function cur-buffer)))))

;;;###autoload
(defun typst-ts-compile-and-preview ()
  "Compile & Preview.
Assuming the compile output file name is in default style."
  (interactive)
  ;; use a local variable version of `compilation-finish-functions' to shadow
  ;; global version doesn't work
  (add-hook 'compilation-finish-functions
            (typst-ts-mode-compile-and-preview--compilation-finish-function
             (current-buffer)))
  (typst-ts-compile))

(defvar typst-ts-compilation-mode-error
  (cons (rx bol "error:" (+ not-newline) "\n" (+ blank) "┌─ "
            (group (+ not-newline)) ":" ;; file
            (group (+ num)) ":"         ;; start-line
            (group (+ num)) "\n")       ;; start-col
        '(1 2 3))
  "Regexp for Error in compilation buffer.")

;;;###autoload
(define-compilation-mode typst-ts-compilation-mode "Typst Compilation"
  "Customized major mode for typst watch compilation."
  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'typst-error typst-ts-compilation-mode-error))
  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'typst-error))

(provide 'typst-ts-compile)

;;; typst-ts-compile.el ends here
