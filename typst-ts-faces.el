;;; typst-ts-faces.el --- typst-ts-mode faces  -*- lexical-binding: t; -*-
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

;; Face definitions.

;;; Code:

(defgroup typst-ts-faces nil
  "Typst tree sitter faces."
  :prefix "typst-ts-faces"
  :group 'typst-ts)

(defcustom typst-ts-markup-header-same-height t
  "Whether to make header face in markup context share the same height.
Note it only works when user choose `max' level of fontification precision
level.  See `typst-ts-mode-fontification-precision-level'."
  :type 'boolean
  :group 'typst-ts-faces)

(defcustom typst-ts-markup-header-scale
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "Header Scale."
  :type '(list integer integer integer integer integer integer)
  :set (lambda (symbol value)
         (set-default symbol value)
         (when typst-ts-markup-header-same-height
           (set-default symbol (make-list (length value) 1.0))))
  :set-after '(typst-ts-markup-header-same-height)
  :group 'typst-ts-faces)

;; Face =========================================================================
(defface typst-ts-watch-modeline-indicator-face
  '((t :inherit (underline bold)))
  "Face for typst watch modeline indicator.")

;; Common Face ==================================================================

(defface typst-ts-shorthand-face
  '((t :inherit shadow))
  "Face for linebreak.")

(defface typst-ts-error-face
  '((t :inherit font-lock-warning-face))
  "Face for linebreak.")

;; Markup Faces =================================================================

(defface typst-ts-markup-header-indicator-face
  '((t :weight bold))
  "Face for Typst ts markup header indicator.")

(defface typst-ts-markup-header-face
  '((t :weight bold))
  "Face for Typst ts markup headers text.")

(defface typst-ts-markup-header-indicator-face-1
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 0 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'.")

(defface typst-ts-markup-header-face-1
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 0 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-indicator-face-2
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 1 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'.")

(defface typst-ts-markup-header-face-2
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 1 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-indicator-face-3
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 2 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'.")

(defface typst-ts-markup-header-face-3
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 2 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-indicator-face-4
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 3 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'.")

(defface typst-ts-markup-header-face-4
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 3 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-indicator-face-5
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 4 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'.")

(defface typst-ts-markup-header-face-5
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 4 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-header-indicator-face-6
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 5 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'.")

(defface typst-ts-markup-header-face-6
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 5 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'.")

(defface typst-ts-markup-url-face
  '((t :inherit link))
  "Face for url.")

(defface typst-ts-markup-emphasis-indicator-face
  '((t :inherit italic))
  "Face for emphasis.")

(defface typst-ts-markup-emphasis-face
  '((t :inherit italic))
  "Face for emphasis.")

(defface typst-ts-markup-strong-indicator-face
  '((t :inherit bold))
  "Face for strong.")

(defface typst-ts-markup-strong-face
  '((t :inherit bold))
  "Face for strong.")

(defface typst-ts-markup-item-face
  '((t :inherit shadow))
  "Face for whole term, use in min and middle fontification level.
See `typst-ts-mode-fontification-precision-level'.")

(defface typst-ts-markup-item-indicator-face
  '((t :inherit shadow))
  "Face for item.")

(defface typst-ts-markup-term-face
  '((t :inherit shadow))
  "Face for whole term, use in min and middle fontification level.
See `typst-ts-mode-fontification-precision-level'.")

(defface typst-ts-markup-term-indicator-face
  '((t :inherit shadow))
  "Face for term indicator.")

(defface typst-ts-markup-term-term-face
  '((t :inherit bold))
  "Face for term text.")

(defface typst-ts-markup-term-description-face
  '((t :inherit italic))
  "Face for term description.")

(defface typst-ts-markup-quote-face
  '((t :inherit shadow))
  "Face for quote.")

(defface typst-ts-markup-linebreak-face
  '((t :inherit escape-glyph))
  "Face for linebreak.")

(defface typst-ts-markup-escape-face
  '((t :inherit escape-glyph))
  "Face for linebreak.")

(defface typst-ts-markup-raw-indicator-face
  '((t :inherit shadow))
  "Face for rawblock and rawspan indicator.")

(defface typst-ts-markup-raw-blob-face
  '((t :inherit shadow))
  "Face for rawblock and rawspan blob.")

(defface typst-ts-markup-rawblock-face
  '((t :inherit shadow))
  "Face for whole raw block, use in min and middle fontification level.
See `typst-ts-mode-fontification-precision-level'.")

(defface typst-ts-markup-rawblock-indicator-face
  '((t :inherit typst-ts-markup-raw-indicator-face))
  "Face for rawblock indicator.")

(defface typst-ts-markup-rawblock-lang-face
  '((t :inherit font-lock-type-face))
  "Face for rawspan ident.")

(defface typst-ts-markup-rawblock-blob-face
  '((t :inherit typst-ts-markup-raw-blob-face))
  "Face for rawblock blob.")

(defface typst-ts-markup-rawspan-face
  '((t :inherit shadow))
  "Face for whole raw span, use in min and middle fontification level.
See `typst-ts-mode-fontification-precision-level'.")

(defface typst-ts-markup-rawspan-indicator-face
  '((t :inherit typst-ts-markup-raw-indicator-face))
  "Face for rawspan indicator.")

(defface typst-ts-markup-rawspan-blob-face
  '((t :inherit typst-ts-markup-raw-blob-face))
  "Face for rawspan blob.")

(defface typst-ts-markup-label-face
  '((t :inherit homoglyph))
  "Face for label.")

(defface typst-ts-markup-reference-face
  '((t :inherit homoglyph))
  "Face for reference.")

;; Code Faces ===================================================================

(defface typst-ts-code-indicator-face
  '((t :inherit shadow))
  "Face for code indicator #.")

;; Math Faces ===================================================================

(defface typst-ts-math-indicator-face
  '((t :inherit shadow))
  "Face for math indicator $.")

(provide 'typst-ts-faces)

;;; typst-ts-faces.el ends here
