;;; typst-ts-core.el --- core functions for typst-ts-mode -*- lexical-binding: t; -*-
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

;; Utility functions for typst-ts-mode

;;; Code:

(require 'treesit)

;; don't use 'treesit.c' since package Emacs distribution may separate the source
;; code from Emacs binary
(declare-function treesit-parser-list "treesit" t t)

(defcustom typst-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `typst-ts-mode'."
  :type 'natnum
  :group 'typst-ts)

(defun typst-ts-core-column-at-pos (point)
  "Get the column at position POINT."
  (save-excursion
    (goto-char point)
    (current-column)))

(defun typst-ts-core-get-node-bol (node)
  "Get the NODE's indentation offset (at node beginning)."
  (save-excursion
    (goto-char (treesit-node-start node))
    (back-to-indentation)
    (point)))

(defun typst-ts-core-line-bol-nonwhite-pos (&optional pos)
  "POS."
  (save-excursion
    (when pos
      (goto-char pos))
    (back-to-indentation)
    (point)))

(defun typst-ts-core-get-node-at-bol-nonwhite (&optional pos)
  "Get node at the first non-whitespace character at line beginning.
If POS is given, operate on the line that POS locates at."
  (save-excursion
    (when pos
      (goto-char pos))
    (back-to-indentation)
    (treesit-node-at (point))))

(defun typst-ts-core-get-parent-of-node-at-bol-nonwhite (&optional pos)
  "See `typst-ts-core-get-node-at-bol-nonwhite'.
POS.  May return nil."
  (treesit-node-parent
   (typst-ts-core-get-node-at-bol-nonwhite pos)))

(defun typst-ts-core-for-lines-covered-by-node (node fn)
  "Execute FN on all lines covered by NODE.
Currently the effect of FN shouldn't change line number."
  (let ((ns (treesit-node-start node))
        ;; use line number not position since when editing, position of node end
        ;; changes, but the information is not updated
        (ne-line-num (line-number-at-pos (treesit-node-end node))))
    (save-excursion
      (goto-char ns)
      (while (< (line-number-at-pos) ne-line-num)
        (funcall fn)
        (forward-line 1))
      ;; in case the last line is the last line of buffer, we separate this
      ;; operation from while loop
      (funcall fn))))


;; Emacs 29 doesn't support string type PRED, so this function is created for
;; convenience
(defun typst-ts-core-parent-util-type (node type include-node)
  "See `treesit-parent-until'.
TYPE is an regexp expression for matching types.
NODE TYPE INCLUDE-NODE."
  (treesit-parent-until
   node
   (lambda (node)
     (string-match-p type (treesit-node-type node)))
   include-node))

(defun typst-ts-core-prev-sibling-ignore-types (node types)
  "Find previous sibling node ignoring nodes whose type matches TYPES.
NODE: current node.
TYPES is an regexp expression."
  (let* ((prev-node (treesit-node-prev-sibling node))
         (prev-node-type (treesit-node-type prev-node)))
    (while (and prev-node-type
                (string-match-p types prev-node-type))
      (setq
       prev-node (treesit-node-prev-sibling prev-node)
       prev-node-type (treesit-node-type prev-node)))
    prev-node))

(defun typst-ts-core-node-get (node instructions)
  "Get things from NODE by INSTRUCTIONS.
It's a copy of Emacs 30's `treesit-node-get' function."
  (declare (indent 1))
  (if (fboundp 'treesit-node-get)
      (treesit-node-get node instructions)
    (while (and node instructions)
      (pcase (pop instructions)
        ('(field-name) (setq node (treesit-node-field-name node)))
        ('(type) (setq node (treesit-node-type node)))
        (`(child ,idx ,named) (setq node (treesit-node-child node idx named)))
        (`(parent ,n) (dotimes (_ n)
                        (setq node (treesit-node-parent node))))
        (`(text ,no-property) (setq node (treesit-node-text node no-property)))
        (`(children ,named) (setq node (treesit-node-children node named)))
        (`(sibling ,step ,named)
         (dotimes (_ (abs step))
           (setq node (if (> step 0)
                          (treesit-node-next-sibling node named)
                        (treesit-node-prev-sibling node named)))))))
    node))

(provide 'typst-ts-core)

;;; typst-ts-core.el ends here
