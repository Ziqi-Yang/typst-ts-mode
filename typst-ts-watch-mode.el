;;; typst-ts-watch-mode.el --- Watch typst file  -*- lexical-binding: t; -*-
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

;; Minor mode for watching(hot compile) current typst file.

;;; Code:

(require 'typst-ts-compile)

(defgroup typst-ts-watch nil
  "Typst TS Watch."
  :prefix "typst-ts-watch"
  :group 'typst-ts)

(define-minor-mode typst-ts-watch-mode
  "Watch(hot compile) current typst file."
  :lighter " [Watch]"
  :group 'typst-ts-watch
  (if typst-ts-watch-mode
      (typst-ts-watch-start)
    (typst-ts-watch-stop)))

(defcustom typst-ts-watch-options ""
  "User defined compile options for `typst-ts-watch'.
The compile options will be passed to the
`<typst-executable> watch <current-file>' sub-command."
  :type 'string
  :group 'typst-ts-watch)

(defcustom typst-ts-watch-process-name "*Typst-Watch*"
  "Process name for `typst watch' sub-command."
  :type 'string
  :group 'typst-ts-watch)

(defcustom typst-ts-watch-process-buffer-name "*Typst-Watch*"
  "Process buffer name for `typst watch' sub-command."
  :type 'string
  :group 'typst-ts-watch)

(defcustom typst-ts-watch-auto-display-compilation-error t
  "Whether the typst watch process buffer should be displayed automatically.
This means the buffer will be displayed when error occurs, hide when error
is eliminated."
  :type 'boolean
  :group 'typst-ts-watch)

(defcustom typst-ts-watch-display-buffer-parameters
  '(display-buffer-at-bottom
    (window-height . fit-window-to-buffer))
  "Display buffer parameters.
Note that since the major mode of typst watch buffer is derived from compilation
 mode. If you have a rule like `((derived-mode . compilation-mode) ...)' in
your `display-buffer-alist', then this option will be covered by that rule."
  :type 'symbol
  :group 'typst-ts-watch)

(defvar typst-ts-before-watch-hook nil
  "Hook runs before compile.")

(defvar typst-ts-after-watch-hook nil
  "Hook runs after compile.")

(defun typst-ts--watch-process-filter (proc output)
  "Filter the `typst watch' process output.
Only error will be transported to the process buffer.
See `(info \"(elisp) Filter Functions\")'.
PROC: process; OUTPUT: new output from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (let ((window (get-buffer-window))
            (re (rx bol "error:" (+ not-newline) "\n" (+ blank) "┌─ "
                    (+ not-newline) ":"  ; file
                    (+ num) ":"  ; start-line
                    (+ num) "\n"  ; start-col
                    (+ (+ (or blank num)) "│" (* not-newline) "\n")))
            (next-match-start-pos 0)
            res-output)
        (while (string-match re output next-match-start-pos)
          (setq res-output (concat
                            res-output
                            (when res-output "\n")
                            (substring output (match-beginning 0) (match-end 0)))
                next-match-start-pos (match-end 0)))
        ;; Insert the Error text
        (if (not res-output)
            (when (and typst-ts-watch-auto-display-compilation-error window)
              (delete-window window))
          (insert res-output)
          (goto-char (point-min))
          (when typst-ts-watch-auto-display-compilation-error
            (typst-ts-watch-display-buffer)))))))

;;;###autoload
(defun typst-ts-watch-display-buffer ()
  "Display typst watch process buffer."
  (interactive)
  (when (and (called-interactively-p 'interactive)
             (not (buffer-live-p typst-ts-watch-process-buffer-name)))
    (user-error "The typst watch process buffer '%s' is not alive!" typst-ts-watch-process-buffer-name))
  (let ((buf (get-buffer-create typst-ts-watch-process-buffer-name)))
    (display-buffer buf typst-ts-watch-display-buffer-parameters)))

;;;###autoload
(defun typst-ts-watch-start ()
  "Watch(hot compile) current typst file."
  (interactive)
  (run-hooks typst-ts-before-watch-hook)
  (with-current-buffer (get-buffer-create typst-ts-watch-process-buffer-name)
    (erase-buffer)
    (unless (eq major-mode 'typst-ts-compilation-mode)
      (typst-ts-compilation-mode)
      (read-only-mode -1)))
  (set-process-filter
   (start-process-shell-command
    typst-ts-watch-process-name typst-ts-watch-process-buffer-name
    (format "%s watch %s %s"
            typst-ts-compile-executable-location
            (file-name-nondirectory buffer-file-name)
            typst-ts-watch-options))
   'typst-ts--watch-process-filter)
  (message "Start Watch :3"))

;;;###autoload
(defun typst-ts-watch-stop ()
  "Stop watch process."
  (interactive)
  (delete-process typst-ts-watch-process-name)
  ;; delete associated watch process buffer and window
  (let ((window (get-buffer-window typst-ts-watch-process-buffer-name)))
    (kill-buffer typst-ts-watch-process-buffer-name)
    (when window
      (delete-window window)))
  (run-hooks typst-ts-after-watch-hook)
  (message "Stop Watch :‑."))

(provide 'typst-ts-watch-mode)
;;; typst-ts-watch-mode.el ends here
