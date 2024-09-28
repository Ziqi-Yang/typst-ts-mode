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
  (call-interactively #'outline-move-subtree-up))

(defun typst-ts-mode-heading-down ()
  "Switch the current heading with the heading below."
  (interactive)
  (call-interactively #'outline-move-subtree-down))

(defun typst-ts-mode-heading-left ()
  "Increase the heading level."
  (interactive)
  (call-interactively #'outline-promote))

(defun typst-ts-mode-heading-right ()
  "Decrease heading level."
  (interactive)
  (call-interactively #'outline-demote))

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

(defun typst-ts-mode-item--at-point-p ()
  "Return item node when point is on item.
Otherwise nil."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (x) (string= (treesit-node-type x)
                                             "item"))))

(defun typst-ts-mode-item--with-siblings ()
  "Return (prev current next numbered-p) items.

The last item in the last tells you if the list is numbered (t) or not (nil).

When current does not have a previous or next sibling,
the index for it will be nil.

Being a different item type does not count as sibling, ex:
1. foo
- bar

When point is not on an item node return nil."
  (when-let* ((node (typst-ts-mode-item--at-point-p))
              (get-item-type (lambda (x)
                               (treesit-node-text (treesit-node-child x 0))))
              (item-type (funcall get-item-type node))
              (node-numbered-p t)
              (same-item-type (lambda (x)
                                (let ((type (funcall get-item-type x)))
                                  (or (string= type item-type)
                                      ;; are they numbers?
                                      (and node-numbered-p
                                           (not (= (string-to-number type)
                                                   0)))))))
              (only-if (lambda (x) (and (string= (treesit-node-type x)
                                                 "item")
                                        (funcall same-item-type x)
                                        x))))
    (setq node-numbered-p (not (= (string-to-number item-type) 0)))
    (cond
     ((not node) node)
     (node (list (funcall only-if (treesit-node-prev-sibling node))
                 node
                 (funcall only-if (treesit-node-next-sibling node))
                 node-numbered-p)))))

(defun typst-ts-mode--swap-regions (start1 end1 start2 end2)
  "Swap region between START1 and END1 with region between START2 and END2."
  (let ((text1 (buffer-substring start1 end1))
        (text2 (buffer-substring start2 end2))
        (marker1-start (make-marker))
        (marker1-end (make-marker))
        (marker2-start (make-marker))
        (marker2-end (make-marker)))
    (set-marker marker1-start start1)
    (set-marker marker1-end end1)
    (set-marker marker2-start start2)
    (set-marker marker2-end end2)

    (delete-region marker1-start marker1-end)
    (delete-region marker2-start marker2-end)

    (goto-char marker1-start)
    (insert text2)

    (goto-char marker2-start)
    (insert text1)

    (set-marker marker1-start nil)
    (set-marker marker1-end nil)
    (set-marker marker2-start nil)
    (set-marker marker2-end nil)))

(defun typst-ts-mode-item--move (direction)
  "Moves item node up or down (swap).
DIRECTION should be `up' or `down'."
  (let* ( previous current next swap-with numbered-p
          (bind (lambda ()
                  (pcase direction
                    ('up
                     (setq swap-with previous))
                    ('down
                     (setq swap-with next))
                    (_ (error "%s is not one of: `up' `down'" direction))))))
    (seq-setq (previous current next numbered-p)
              (typst-ts-mode-item--with-siblings))
    (unless current
      (error "Point is not on an item"))
    (funcall bind)
    (unless swap-with
      (user-error "There is no %s item to swap with"
                  (if (eq direction 'up) "previous" "next")))
    ;; numbers may need to be swapped
    (when numbered-p
      (let* ((number1 (treesit-node-child current 0))
             (number2 (treesit-node-child swap-with 0))
             (current-begin (treesit-node-start number1))
             (current-end (treesit-node-end number1))
             (other-begin (treesit-node-start number2))
             (other-end (treesit-node-end number2)))
        (save-excursion
          (typst-ts-mode--swap-regions current-begin current-end
                                       other-begin other-end))))
    ;; the nodes must be reinitialized
    (seq-setq (previous current next numbered-p)
              (typst-ts-mode-item--with-siblings))
    (funcall bind)
    (let ((current-begin (treesit-node-start current))
          (current-end (treesit-node-end current))
          (other-begin (treesit-node-start swap-with))
          (other-end (treesit-node-end swap-with))
          (column (current-column)))
      (typst-ts-mode--swap-regions current-begin current-end
                                   other-begin other-end)
      (move-to-column column))))

(defun typst-ts-mode-item-up ()
  "Move the item at point up."
  (interactive)
  (typst-ts-mode-item--move 'up))

(defun typst-ts-mode-item-down ()
  "Move the item at point down."
  (interactive)
  (typst-ts-mode-item--move 'down))

(defun typst-ts-mode-meta-up ()
  "See `typst-ts-mode-meta--dwim'."
  (interactive)
  (call-interactively (typst-ts-mode-meta--dwim 'up)))

(defun typst-ts-mode-meta-down ()
  "See `typst-ts-mode-meta--dwim'."
  (interactive)
  (call-interactively (typst-ts-mode-meta--dwim 'down)))

(defun typst-ts-mode-meta-left ()
  "See `typst-ts-mode-meta--dwim'."
  (interactive)
  (call-interactively (typst-ts-mode-meta--dwim 'left)))

(defun typst-ts-mode-meta-right ()
  "See `typst-ts-mode-meta--dwim'."
  (interactive)
  (call-interactively (typst-ts-mode-meta--dwim 'right)))

(defun typst-ts-mode-meta--dwim (direction)
  "Return function depending on the context with meta key + DIRECTION.

When point is at heading:
`left': `typst-ts-mode-heading-decrease',
`right': `typst-ts-mode-heading-increase',
`up': `typst-ts-mode-heading-up',
`down': `typst-ts-mode-heading-down'.

When point is at item list:
`up': `typst-ts-mode-item-up'
`down': `typst-ts-mode-item-down'

When there is no relevant action to do it will return the relevant function in
the `GLOBAL-MAP' (example: `right-word')."
  (let* ((prefix "typst-ts-mode-")
         (mid (cond
               ((typst-ts-mode-heading--at-point-p) "heading")
               ((and (typst-ts-mode-item--at-point-p)
                     ;; does not exist, maybe will exist at some point
                     (not (or (eq 'left direction)
                              (eq 'right direction))))
                "item")
               (t nil)))
         (end
          (pcase direction
            ('left
             "-left")
            ('right
             "-right")
            ('up
             "-up")
            ('down
             "-down")
            (_ (error "DIRECTION: %s is not one of: `right' `left', `up', `down'"
                      direction)))))
    (if (not mid)
        (keymap-lookup global-map (substitute-command-keys
                                   (concat "\\[" prefix "meta" end "]")))
      (intern-soft (concat prefix mid end)))))

(defun typst-ts-mode-meta-return (&optional arg)
  "Depending on context, insert a heading or insert an item.
The new heading is created after the ending of current heading.
Using ARG argument will ignore the context and it will insert a heading instead."
  (interactive "P")
  (let ((item-node (treesit-parent-until
                    (treesit-node-at (line-beginning-position))
                    (lambda (node)
                      (string= "item" (treesit-node-type node)))))
        (node (typst-ts-core-get-parent-of-node-at-bol-nonwhite)))
    (cond
     (arg (typst-ts-mode-insert--heading nil))
     (item-node
      (typst-ts-mode-insert--item item-node))
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
                  (setq node (typst-ts-core-parent-util-type
                              (typst-ts-core-get-parent-of-node-at-bol-nonwhite)
                              "item" t t)))
             (let* ((item-node node)
                    (has-children (treesit-node-child item-node 1))
                    (next-line-pos
                     (save-excursion
                       (forward-line 1)
                       (point)))
                    (next-line-node
                     (typst-ts-core-get-parent-of-node-at-bol-nonwhite
                      next-line-pos))
                    (next-line-top-node  ; get container type or `item' type node
                     (typst-ts-core-parent-util-type
                      next-line-node
                      (regexp-opt '("code" "item"))
                      t)))
               (if has-children
                   ;; example:
                   ;; - #[| <- return
                   ;; ]
                   (if (and next-line-top-node
                            ;; end of buffer situation (or next line is the end
                            ;; line (and no newline character))
                            (not (equal
                                  (line-number-at-pos next-line-pos)
                                  (line-number-at-pos (point-max)))))
                       (call-interactively #'newline)
                     (typst-ts-mode-insert--item item-node))
                 ;; no text means delete the item on current line: (item -)
                 (delete-region (line-beginning-position) (line-end-position))
                 ;; whether the previous line is in an item
                 (let* ((prev-line-item-node
                         (typst-ts-core-parent-util-type
                          (typst-ts-core-get-parent-of-node-at-bol-nonwhite
                           (save-excursion
                             (forward-line -1)
                             (point)))
                          "item" t t)))
                   (if prev-line-item-node
                       (progn
                         (delete-line)
                         (forward-line -1)
                         (end-of-line)
                         (call-interactively #'newline))
                     (indent-according-to-mode)))))
             (throw 'execute-result 'success))
            )))))
    ;; execute default action if not successful
    (unless (eq execute-result 'success)
      ;; we only need to look for global keybinding, see `(elisp) Active Keymaps'
      (let ((global-ret-function (global-key-binding (kbd "RET"))))
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

(defun typst-ts-editing--indent-item-node-lines (node offset)
  (let ((item-node-min-column
         (typst-ts-core-column-at-pos
          (typst-ts-core-line-bol-nonwhite-pos
           (treesit-node-start node)))))
    (if (< (+ item-node-min-column offset) 0)
        (setq offset (- item-node-min-column)))
    (typst-ts-core-for-lines-covered-by-node
     node
     (lambda ()
       (indent-line-to
        (+ (typst-ts-core-column-at-pos
            (typst-ts-core-line-bol-nonwhite-pos))
           offset))))))

(defun typst-ts-mode-cycle (&optional _arg)
  "Cycle."
  (interactive "P")
  (let (execute-result node)
    (setq
     execute-result
     ;; plz manually throw `\'success' to `execute-result'
     (catch 'execute-result
       (when-let* ((cur-pos (point))
                   (cur-node (treesit-node-at cur-pos))
                   (cur-node-type (treesit-node-type cur-node))
                   (cur-line-nonwhite-bol-node
                    (typst-ts-core-get-node-at-bol-nonwhite))
                   (cur-line-nonwhite-bol-node-type
                    (treesit-node-type cur-line-nonwhite-bol-node))
                   (parent-node (treesit-node-parent cur-node))  ; could be nil
                   (parent-node-type (treesit-node-type parent-node)))
         (cond
          ((equal parent-node-type "raw_blck")
           (insert-tab)
           (throw 'execute-result 'success))


          ((setq node
                 (typst-ts-core-parent-util-type
                  cur-line-nonwhite-bol-node "item" t t))
           (let* ((cur-item-node node)
                  (prev-significant-node
                   (typst-ts-core-prev-sibling-ignore-types
                    cur-item-node
                    "parbreak"))
                  (prev-significant-node-type
                   (treesit-node-type prev-significant-node))
                  prev-item-node)

             (if (equal prev-significant-node-type "item")
                 (setq prev-item-node prev-significant-node)
               (if (equal
                    "item"
                    (treesit-node-type
                     (treesit-node-parent prev-significant-node)))
                   (setq prev-item-node (treesit-node-parent
                                         prev-significant-node))))

             ;; (message "%s, %s" cur-item-node prev-item-node)

             (unless prev-item-node
               (throw 'execute-result 'default))

             (let* ((cur-item-node-start-column
                     (typst-ts-core-column-at-pos
                      (treesit-node-start cur-item-node)))
                    (prev-item-node-start-column
                     (typst-ts-core-column-at-pos
                      (treesit-node-start prev-item-node)))
                    (offset
                     (- cur-item-node-start-column
                        prev-item-node-start-column)))
               (if (>= offset typst-ts-mode-indent-offset)
                   (typst-ts-editing--indent-item-node-lines
                    cur-item-node
                    (- (+ offset typst-ts-mode-indent-offset)))
                 (typst-ts-editing--indent-item-node-lines
                  cur-item-node
                  (- typst-ts-mode-indent-offset (abs offset)))))

             (throw 'execute-result 'success)))

          (t nil)))))
    ;; execute default action if not successful
    (unless (eq execute-result 'success)
      (call-interactively (global-key-binding (kbd "TAB"))))))

(provide 'typst-ts-editing)

;;; typst-ts-editing.el ends here
