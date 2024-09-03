;;; typst-ts-editing.el --- Helper functions for editing Typst documents -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 The typst-ts-mode Project Contributors

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

;; For item node, it's recommended to use `+' rather than `<num>.'.  Operations
;; for `<num>.' may not be implemented comprehensively.

;;; Code:

(require 'outline)
(require 'typst-ts-core)

(defun typst-ts-mode-heading-up ()
  "Switch the current heading with the heading above."
  (interactive)
  (typst-ts-mode-meta--dwim 'up))

(defun typst-ts-mode-heading-down ()
  "Switch the current heading with the heading below."
  (interactive)
  (typst-ts-mode-meta--dwim 'down))

(defun typst-ts-mode-heading-increase ()
  "Increase the heading level."
  (interactive)
  (typst-ts-mode-meta--dwim 'right))

(defun typst-ts-mode-heading-decrease ()
  "Decrease heading level."
  (interactive)
  (typst-ts-mode-meta--dwim 'left))

(defun typst-ts-mode-heading--at-point-p ()
  "Whether the current line is a heading.
Return the heading node when yes otherwise nil."
  (let ((node (treesit-node-parent
               (treesit-node-at
                (save-excursion
                  (beginning-of-line-text)
                  (point))))))
    (if (string= (treesit-node-type node) "heading")
        node
      nil)))

(defun typst-ts-mode-meta--dwim (direction)
  "Do something depending on the context with meta key + DIRECTION.
`left': `typst-ts-mode-heading-decrease',
`right': `typst-ts-mode-heading-increase',
`up': `typst-ts-mode-heading-up',
`down': `typst-ts-mode-heading-down'.
When there is no relevant action to do it will execute the relevant function in
the `GLOBAL-MAP' (example: `right-word')."
  (let ((heading (typst-ts-mode-heading--at-point-p))
        ;; car function, cdr string of function for `substitute-command-keys'
        (call-me/string
         (pcase direction
           ('left
            (cons #'outline-promote
                  "\\[typst-ts-mode-heading-decrease]"))
           ('right
            (cons #'outline-demote
                  "\\[typst-ts-mode-heading-decrease]"))
           ('up
            (cons #'outline-move-subtree-up
                  "\\[typst-ts-mode-heading-up]"))
           ('down
            (cons #'outline-move-subtree-down
                  "\\[typst-ts-mode-heading-down]"))
           (_ (error "%s is not one of: `right' `left'" direction)))))
    (if heading
        (call-interactively (car call-me/string))
      (call-interactively
       (keymap-lookup global-map (substitute-command-keys (cdr call-me/string)))))))

(defun typst-ts-mode-meta-return (&optional arg)
  "Depending on context, insert a heading or insert an item.
The new heading is created after the ending of current heading.
Using ARG argument will ignore the context and it will insert a heading instead."
  (interactive "P")
  (let ((node (typst-ts-core-get-parent-of-node-at-bol-nonwhite)))
    (cond
     (arg (typst-ts-mode-insert--heading nil))
     ((string= (treesit-node-type node) "item")
      (typst-ts-mode-insert--item node))
     (t
      (typst-ts-mode-insert--heading node)))))

(defun typst-ts-mode-return (&optional arg)
  "Handle RET depends on condition.
When prefix ARG is non-nil, call global return function."
  (interactive "P")
  (let (execute-result)
    (unless current-prefix-arg
      (setq
       execute-result
       (catch 'execute-result
         (let* ((cur-pos (point))
                (cur-node (treesit-node-at cur-pos))
                (cur-node-type (treesit-node-type cur-node))
                (parent-node (treesit-node-parent cur-node))  ; could be nil
                (parent-node-type (treesit-node-type parent-node))
                node)
           ;; (message "%s %s" cur-node parent-node)
           (cond
            ;; on item node end
            ((and (eolp)
                  (setq node (typst-ts-core-get-parent-of-node-at-bol-nonwhite))
                  (equal (treesit-node-type node) "item"))
             (let* ((child-node (treesit-node-child node 1))
                    (next-line-node-type
                     (treesit-node-type
                      (typst-ts-core-get-parent-of-node-at-bol-nonwhite
                       (save-excursion
                         (forward-line 1)
                         (point))))))
               (if child-node
                   (if (not (equal next-line-node-type "item"))
                       (typst-ts-mode-insert--item node)
                     (call-interactively #'newline))
                 ;; no text means delete the item on current line: (item -)
                 (beginning-of-line)
                 (kill-line)
                 ;; whether the previous line is in an item
                 (let* ((prev-line-node-type
                         (treesit-node-type
                          (typst-ts-core-get-parent-of-node-at-bol-nonwhite
                           (save-excursion
                             (forward-line -1)
                             (point))))))
                   (if (equal "item" prev-line-node-type)
                       (progn
                         (kill-line)
                         (forward-line -1)
                         (end-of-line)
                         (call-interactively #'newline))
                     (indent-according-to-mode)))))
             (throw 'execute-result 'success))
            )))))
    ;; execute default action if not successful
    (unless (eq execute-result 'success)
      ;; we only need to look for global keybinding, see `(elisp) Active Keymaps'
      (let ((global-ret-function
             (global-key-binding (kbd "RET"))))
        (if (not current-prefix-arg)
            (call-interactively global-ret-function)
          (if (yes-or-no-p
               (format
                "Execute function `%s' without/with the given prefix argument?"
                global-ret-function))
              (let ((current-prefix-arg nil))
                (call-interactively global-ret-function))
            (call-interactively global-ret-function)))))))

(defun typst-ts-mode-insert--item (node)
  "Insert an item after NODE.
NODE must be an item node!
This function respects indentation."
  (let* (;; +, -, or <num>.
         (item-type (treesit-node-text
                     (treesit-node-child node 0)))
         (item-number (string-to-number item-type))
         (item-end (treesit-node-end node))
         (node-bol-column (typst-ts-core-column-at-pos
                           (typst-ts-core-get-node-bol node))))
    (goto-char item-end)
    (newline)
    (indent-line-to node-bol-column)
    (insert (if (= item-number 0)  ; not a number type
                item-type
              (concat (number-to-string (1+ item-number)) "."))
            " ")))

(defun typst-ts-mode-insert--heading (node)
  "Insert a heading after the section that NODE is part of.
When there is no section it will insert a heading below point."
  (let* ((section
          (treesit-parent-until
           node
           (lambda (node)
             (string= (treesit-node-type node) "section"))
           t))
         ;; first child is heading
         (heading (treesit-node-child section 0))
         (heading-level (treesit-node-type (treesit-node-child heading 0))))
    (if section
        (goto-char (treesit-node-end section))
      ;; no headings so far
      (setq heading-level "=")
      (forward-line 1))
    ;; something can be in the next line/section, the heading needs be on its own line
    ;; this has to be done after `goto-char' because it will invalidate the node
    (newline)
    (forward-line -1)
    ;; insert the heading and indent
    (insert heading-level " ")
    (indent-according-to-mode)))

(defun typst-ts-mode-cycle (&optional _arg)
  "Cycle."
  (interactive "P")
  (let (execute-result)
    (setq
     execute-result
     ;; plz manually throw `\'success' to `execute-result'
     (catch 'execute-result
       (when-let* ((cur-pos (point))
                   (cur-node (treesit-node-at cur-pos))
                   (cur-node-type (treesit-node-type cur-node))
                   (parent-node (treesit-node-parent cur-node))  ; could be nil
                   (parent-node-type (treesit-node-type parent-node)))
         (cond
          ((equal parent-node-type "raw_blck")
           (insert-tab)
           (throw 'execute-result 'success))

          ((or (equal cur-node-type "parbreak")
               (equal parent-node-type "item")
               ;; please turn on whitespace-mode to test the following conditions
               (eobp)
               (eq (point) (1- (point-max))))
           (when-let* ((cur-line-bol
                        (save-excursion
                          (back-to-indentation)
                          (point)))
                       (prev-nonwhite-pos (save-excursion
                                            (goto-char cur-line-bol)
                                            (skip-chars-backward "\s\r\n\t")
                                            (1- (point))))
                       ((and (not (eq prev-nonwhite-pos 0))  ; first line
                             (not (eq  ; has previous sibling
                                   (line-number-at-pos prev-nonwhite-pos)
                                   (line-number-at-pos (point))))))
                       (prev-nonwhite-line-node
                        (treesit-node-at prev-nonwhite-pos))
                       (prev-nonwhite-line-bol
                        ;; TODO typst-ts-core-get-node-bol
                        (save-excursion
                          (goto-char prev-nonwhite-pos)
                          (back-to-indentation)
                          (point)))
                       (prev-nonwhite-line-top-node
                        (treesit-node-parent
                         (treesit-node-at prev-nonwhite-line-bol)))
                       (cur-line-bol-column (typst-ts-core-column-at-pos cur-line-bol))
                       (prev-nonwhite-line-top-node-start-column
                        (typst-ts-core-column-at-pos
                         (treesit-node-start prev-nonwhite-line-top-node))))
             (cond
              ;; 1. el
              ;; 2. psy| <- can toggle indent (of all its descendant nodes)
              ((and
                (equal (treesit-node-type prev-nonwhite-line-top-node) "item")
                ;; previous nonwhite-line ending is not '\' character
                (not (equal (treesit-node-type prev-nonwhite-line-node) "linebreak")))

               (let* ((cur-line-top-node
                       (typst-ts-core-get-parent-of-node-at-bol-nonwhite))
                      (cur-line-top-node-start-column
                       (typst-ts-core-column-at-pos
                        (treesit-node-start cur-line-top-node)))
                      (offset
                       (- cur-line-top-node-start-column
                          prev-nonwhite-line-top-node-start-column)))
                 (if (= offset 0)
                     (typst-ts-core-for-lines-covered-by-node
                      cur-line-top-node
                      (lambda ()
                        (let ((pos (point)))
                          (indent-line-to
                           (+ (typst-ts-core-column-at-pos
                               (typst-ts-core-line-bol-pos))
                              typst-ts-mode-indent-offset))
                          ;; (goto-char (+ typst-ts-mode-indent-offset point))
                          )))
                   (typst-ts-core-for-lines-covered-by-node
                    cur-line-top-node
                    (lambda ()
                      (let ((pos (point)))
                        (indent-line-to
                         (max (- (typst-ts-core-column-at-pos
                                  (typst-ts-core-line-bol-pos))
                                 offset)
                              0))
                        ;; (goto-char (- pos typst-ts-mode-indent-offset))
                        )))))
               (throw 'execute-result 'success)
               ))))
          (t nil)))))
    ;; execute default action if not successful
    (unless (eq execute-result 'success)
      (call-interactively (global-key-binding (kbd "TAB"))))))

(provide 'typst-ts-editing)

;;; typst-ts-editing.el ends here
