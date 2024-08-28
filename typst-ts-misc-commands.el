;;; typst-ts-misc-commands.el --- Miscellaneous commands for typst-ts-mode -*- lexical-binding: t; -*-
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

;; Miscellaneous commands

;;; Code:

(require 'treesit)

;; (defgroup typst-ts-mc nil
;;   "Typst ts miscellaneous commands."
;;   :prefix "typst-ts-misc-commands"
;;   :group 'typst-ts)

(defun typst-ts-mc-install-grammar ()
  (interactive)
  (let ((treesit-language-source-alist treesit-language-source-alist))
    (add-to-list 'treesit-language-source-alist
                 '(typst "https://github.com/Ziqi-Yang/tree-sitter-typst"))
    (treesit-install-language-grammar 'typst)))


(defun typst-ts-mc-export-to-markdown ()
  (interactive)

  ;; for simplicity
  (unless buffer-file-name
    (user-error "You should save the file first!"))

  (when (equal (file-name-extension buffer-file-name) "md")
    (user-error "Couldn't operate on a Typst file with `md' as its extension!"))

  (let* ((base-path (file-name-directory buffer-file-name))
         (file-name (file-relative-name buffer-file-name base-path))
         (output-file-name
          (file-name-with-extension file-name "md")))
    (async-shell-command
     (concat "pandoc -o " output-file-name " " file-name))))

(defun typst-ts-mc-search-typst-symbol ()
  (interactive)
  (browse-url "https://typst.app/docs/reference/symbols/sym/"))

(defun typst-ts-mc-recognize-typst-symbol ()
  (interactive)
  (browse-url "https://detypify.quarticcat.com/"))

(defun typst-ts-mc-search-package ()
  (interactive)
  (browse-url "https://typst.app/universe"))


(provide 'typst-ts-misc-commands)

;;; typst-ts-misc-commands.el ends here
