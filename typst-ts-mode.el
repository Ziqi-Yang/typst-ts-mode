;;; typst-ts-mode.el --- tree-sitter support for Typst  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Keywords: typst languages tree-sitter
;; URL: https://git.sr.ht/~meow_king/typst-ts-mode
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29.1"))

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

(require 'treesit)

(defgroup typst-ts nil
  "Tree Sitter enabled Typst Writing."
  :prefix "typst-ts"
  :group 'text
  :group 'languages)

(defgroup typst-ts-markup nil
  "Tree Sitter enabled Typst Writing."
  :prefix "typst-ts-markup"
  :group 'typst-ts)

(defcustom typst-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `json-ts-mode'."
  :type 'integer
  :group 'typst-ts)

(defcustom typst-ts-markup-header-same-height nil
  "Whether to make header face in markup context share the same height."
  :type 'boolean
  :group 'typst-ts-markup)

(defcustom typst-ts-markup-header-scale
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "Header Scale."
  :type 'list
  :set (lambda (symbol value)
         (set-default symbol value)
         (when typst-ts-markup-header-same-height
           (set-default symbol (make-list (length value) 1.0))))
  :set-after typst-ts-markup-header-same-height
  :group 'typst-ts-markup)

(defface typst-ts-markup-header-face
  '((t :weight bold))
  "The basic face for Typst ts markup headers.")

(defface typst-ts-markup-header-face-1
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 0 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-face-2
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 1 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-face-3
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 2 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-face-4
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 3 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-face-5
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 4 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-face-6
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 5 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")


(defvar typst-ts-mode-font-lock-rules
  '(;; Typst font locking
    :language typst
    :feature comment
    '((comment) @font-lock-comment-face)

    :language typst
    :feature markup
    '((heading) @)))

(defun typst-ts-mode-comment-setup()
  "Setup comment related stuffs for typst-ts-mode."
  ;; stolen from `c-ts-common-comment-setup'
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (or (seq "/" (+ "/"))
                                         (seq "/" (+ "*")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end)
                             (seq (+ "*") "/"))))))


;;;###autoload
(define-derived-mode typst-ts-mode text-mode "Typst"
  "Major mode for editing Typst, powered by tree-sitter."
  :group 'typst
  ;; TODO syntax table
  ;; :syntax-table typst-ts-mode--syntax-table

  (unless (treesit-ready-p 'typst)
    (error "Tree-sitter for Typst isn't available"))
  (treesit-parser-create 'typst)

  ;; Comments.
  (typst-ts-mode-comment-setup)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}()[]" electric-indent-chars))

  ;; Font Lock TODO
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules typst-ts-mode-font-lock-rules))
  (setq-local treesit-font-lock-feature-list
              ;; TODO
              '((comment markup)
                (markup code)
                (builtin)
                (operator ponctuation)
                ;; (bracket delimiter error function operator property variable)
                ))


  (add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))

  (treesit-major-mode-setup))

(provide 'typst-ts-mode)
