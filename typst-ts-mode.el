;;; typst-ts-mode.el --- Tree Sitter support for Typst  -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024 The typst-ts-mode Project Contributors

;; Version: 0.10.0
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Maintainer: Ziqi Yang <mr.meowking@anche.no>
;;   Huan Nguyen <nguyenthieuhuan@gmail.com>
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

;; Tree Sitter Support for Typst

;;; Code:

(require 'treesit)
(require 'outline)

(require 'typst-ts-embedding-lang-settings)
(require 'typst-ts-core)
(require 'typst-ts-faces)
(require 'typst-ts-compile)
(require 'typst-ts-watch-mode)
(require 'typst-ts-edit-indirect)
(require 'typst-ts-editing)
(require 'typst-ts-lsp)
(require 'typst-ts-misc-commands)
(require 'typst-ts-transient)

(defgroup typst-ts nil
  "Tree Sitter enabled Typst Writing."
  :prefix "typst-ts"
  :group 'text
  :group 'languages)


(defcustom typst-ts-mode-grammar-location nil
  "Specify typst tree sitter grammar file location.
This is used for grammar minimum version check.  The modified time of the
grammar file is used for comparing.
This variable is used in `typst-ts-mode-check-grammar-version'."
  :type '(choice (string :tag "typst tree sitter grammar file location")
                 (const :tag "Don't enable grammar version check" nil))
  :group 'typst-ts)

;; `git log -n1 --date=raw` or `git log -n1 --format="%at"`
(defvar typst-ts-mode--grammar-minimum-version-timestamp 1713791627
  "Timestamp for the minimum supported typst tree sitter grammar version.")

(defcustom typst-ts-mode-enable-raw-blocks-highlight nil
  "Whether to enable raw block highlighting.
NOTE: currently only support Emacs 30 (master branch)."
  :type 'boolean
  :group 'typst-ts)

;; ==============================================================================
;; TODO typst has three modes (namely 'markup', 'code' and 'math')
;; Currently only add common settings to syntax table
(defvar typst-ts-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; comment
    (modify-syntax-entry  ?/     ". 124b"  st)
    (modify-syntax-entry  ?*     ". 23"    st)
    (modify-syntax-entry  ?\n    "> b"     st)
    st))

(defvar typst-ts-mode-font-lock-settings
  (treesit-font-lock-rules
   :language 'typst
   :feature 'comment
   '((comment) @font-lock-comment-face)

   ;; `ref' should be placed before `shorthand' to render properly
   ;; e.g. @CitationKey[p.~7]
   :language 'typst
   :feature 'markup-basic  ; part 1
   '((ref) @typst-ts-markup-reference-face)
   
   :language 'typst
   :feature 'common
   '((shorthand) @typst-ts-shorthand-face
     (ERROR) @typst-ts-error-face)
   
   :language 'typst
   :feature 'markup-basic  ; part 2
   `(,@(if typst-ts-markup-header-same-height
           '((heading _ @typst-ts-markup-header-indicator-face (text) @typst-ts-markup-header-face))
         '((heading "=" @typst-ts-markup-header-indicator-face-1
                    (text) @typst-ts-markup-header-face-1)
           (heading "==" @typst-ts-markup-header-indicator-face-2
                    (text) @typst-ts-markup-header-face-2)
           (heading "===" @typst-ts-markup-header-indicator-face-3
                    (text) @typst-ts-markup-header-face-3)
           (heading "====" @typst-ts-markup-header-indicator-face-4
                    (text) @typst-ts-markup-header-face-4)
           (heading "=====" @typst-ts-markup-header-indicator-face-5
                    (text) @typst-ts-markup-header-face-5)
           (heading "======" @typst-ts-markup-header-indicator-face-6
                    (text) @typst-ts-markup-header-face-6)))
     (emph
      "_" @typst-ts-markup-emphasis-indicator-face
      (text) @typst-ts-markup-emphasis-face
      "_" @typst-ts-markup-emphasis-indicator-face)
     (strong
      "*" @typst-ts-markup-strong-indicator-face
      (text) @typst-ts-markup-strong-face
      "*" @typst-ts-markup-strong-indicator-face)
     (item
      "-" @typst-ts-markup-item-indicator-face)
     (term
      "/" @typst-ts-markup-term-indicator-face
      term: (text) @typst-ts-markup-term-term-face
      ":" @typst-ts-markup-term-indicator-face
      (text) @typst-ts-markup-term-description-face)
     (escape) @typst-ts-markup-escape-face
     (raw_span
      "`" @typst-ts-markup-rawspan-indicator-face
      (blob) @typst-ts-markup-rawspan-blob-face
      "`" @typst-ts-markup-rawspan-indicator-face)
     (raw_blck
      "```" @typst-ts-markup-rawblock-indicator-face
      (ident) :? @typst-ts-markup-rawblock-lang-face
      ;; NOTE let embedded language fontify blob
      ,@(if typst-ts-mode-enable-raw-blocks-highlight
            '((blob) @typst-ts-mode-highlight-raw-block-fn)
          '((blob) @typst-ts-markup-rawblock-blob-face))
      "```" @typst-ts-markup-rawblock-indicator-face)
     (label) @typst-ts-markup-label-face  ; TODO more precise highlight (upstream)
     )

   :language 'typst
   :feature 'markup-standard
   '((linebreak) @typst-ts-markup-linebreak-face
     (url) @typst-ts-markup-url-face
     (quote) @typst-ts-markup-quote-face)
   
   ;; please note that some feature there also in the math mode
   :language 'typst
   :feature 'code-basic
   '("#" @typst-ts-code-indicator-face
     ;; "end" @typst-ts-code-indicator-face ;; "end" is nothing but only a indicator
     (string) @font-lock-string-face
     (bool) @font-lock-constant-face
     (none) @font-lock-constant-face
     (auto) @font-lock-constant-face

     (in ["in" "not"] @font-lock-keyword-face)
     (and "and" @font-lock-keyword-face)
     (or "or" @font-lock-keyword-face)
     (not "not" @font-lock-keyword-face)
     (let "let" @font-lock-keyword-face)
     (branch ["if" "else"] @font-lock-keyword-face)
     (while "while" @font-lock-keyword-face)
     (for ["for" "in"] @font-lock-keyword-face)
     (import "import" @font-lock-keyword-face)
     (as "as" @font-lock-keyword-face)
     (include "include" @font-lock-keyword-face)
     (show "show" @font-lock-keyword-face)
     (set "set" @font-lock-keyword-face)
     (context "context" @font-lock-keyword-face)
     (return "return" @font-lock-keyword-face)
     (flow ["break" "continue"] @font-lock-keyword-face)

     (call ;; function
      item: (ident) @font-lock-function-call-face)
     (call ;; method
      item: (field field: (ident) @font-lock-function-call-face))
     (tagged field: (ident) @font-lock-variable-name-face)
     (field field: (ident) @font-lock-constant-face))

   :language 'typst
   :feature 'code-standard
   '((ident) @font-lock-variable-use-face)

   :language 'typst
   :feature 'code-extended
   '((number) @font-lock-number-face

     (content ["[" "]"] @font-lock-punctuation-face)
     (sign ["+" "-"] @font-lock-operator-face)
     (add "+" @font-lock-operator-face)
     (sub "-" @font-lock-operator-face)
     (mul "*" @font-lock-operator-face)
     (div "/" @font-lock-operator-face)
     (cmp ["==" "<=" ">=" "!=" "<" ">"] @font-lock-operator-face)
     (wildcard) @font-lock-operator-face

     ["(" ")" "{" "}"] @font-lock-punctuation-face
     ["," ";" ".." ":" "sep"] @font-lock-punctuation-face
     "assign" @font-lock-punctuation-face
     (field "." @font-lock-punctuation-face))

   :language 'typst
   :feature 'math-basic
   '((math "$" @typst-ts-math-indicator-face))

   :language 'typst
   :feature 'math-standard
   '((symbol) @font-lock-constant-face
     (letter) @font-lock-constant-face)

   :language 'typst
   :feature 'math-extended
   '((fraction "/" @font-lock-operator-face)
     (fac "!" @font-lock-operator-face)
     (attach ["^" "_"] @font-lock-operator-face)
     (align) @font-lock-operator-face))
  
  "Font lock rules for `typst-ts-mode'.
If you want to enable/disable specific font lock feature, please change
`treesit-font-lock-level' or modify `typst-ts-mode-font-lock-feature-list'.")

(defun typst-ts-mode-highlight-raw-block-fn (blob-node _override _start _end)
  "A function used in function `typst-ts-mode-font-lock-rules'.
This function assign `typst-ts-markup-rawblock-blob-face' to those raw block
whose language cannot be found or be loaded.
BLOB-NODE."
  (let* ((bns (treesit-node-start blob-node))
         (bne (treesit-node-end blob-node))
         (lang-node? (treesit-node-prev-sibling blob-node))
         lang-node lang lang-mode)

    (when (equal (treesit-node-type lang-node?) "ident")
      (setq lang-node lang-node?))

    (when lang-node
      (setq lang (gethash
                  (downcase (treesit-node-text lang-node))
                  typst-ts-els-tag-lang-map))
      (when lang
        (setq lang-mode (typst-ts-els-get-lang-mode (symbol-name lang)))))

    (if lang-mode
        (typst-ts-els-fontify-raw-block lang-mode bns bne)
      (put-text-property bns bne 'face 'typst-ts-markup-rawblock-blob-face))))

(defconst typst-ts-mode-font-lock-feature-list
  '((comment common)
    (markup-basic code-basic math-basic)
    (markup-standard code-standard math-standard)
    (code-extended math-extended)))


(defun typst-ts-mode-indent--grand-parent-bol (_node parent _bol)
  "Return the grand parent beginning of line position.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-parent parent)))
    (back-to-indentation)
    (point)))

(defun typst-ts-mode-indent--no-node-section-container-p (node parent _bol)
  "Whether the current structure is nil -> parbreak -> container -> section.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (unless node
    (let* ((parent-type (treesit-node-type parent))
           (gp-node (treesit-node-parent parent))
           (gp-node-type (treesit-node-type gp-node))
           (ggp-node-type (treesit-node-type (treesit-node-parent gp-node))))
      (and
       (equal "parbreak" parent-type)
       (string-match-p typst-ts-mode--container-node-types-regexp gp-node-type)
       (equal "section" ggp-node-type)))))

(defvar typst-ts-mode-indent-rules
  ;; debug tips:
  ;; use `typst-ts/util/setup-indent-debug-environment' function in `side/utils.el'
  ;; it basically does these (with some extra trivial stuffs):
  ;; 1. `toggle-debug-on-error' to make sure you indentation code error report
  ;; 2. enable `treesit--indent-verbose' to see what indentation rule matches
  ;; 3. `treesit-inspect-mode' or `treesit-inspect-node-at-point'

  ;; `indentation-test.typ' file is used for testing indentation.

  ;; no-node situation: often in insert mode > hit return at the line ending
  ;; `typst-ts-mode-indent-line-function' is created for handling end of buffer
  ;;  edge cases

  ;; Note electric-pair-mode will auto insert newline character when condition meets
  ;; see `typst-ts-mode-electric-pair-open-newline-between-pairs-psif'
  ;; It may be better to turn off `electric-pair-open-newline-between-pairs'
  `((typst
     ;; ((lambda (node parent bol)  ; NOTE
     ;;    (message "%s %s %s %s %s" node parent
     ;;             (treesit-node-parent parent)
     ;;             (treesit-node-parent (treesit-node-parent parent)) bol)
     ;;    nil)
     ;;  parent-bol 0)

     ((and no-node (parent-is "source_file")) prev-line 0)
     ((parent-is "source_file") column-0 0)

     ((n-p-gp ,(regexp-opt '(")" "]" "}" "$"))
              ,typst-ts-mode--container-node-types-regexp
              nil)
      parent-bol 0)

     ;; math
     ;; math align, example:
     ;; sum_(k=0)^n k
     ;;   &= 1 + ... + n \
     ((node-is "align") parent-bol typst-ts-mode-indent-offset)

     ;; code field, example:
     ;; "a b c"
     ;;   .split(" ")
     ((n-p-gp "." "field" nil) parent-bol typst-ts-mode-indent-offset)

     ((parent-is "comment") prev-adaptive-prefix 0)

     ;; item - child item
     ((and (node-is "item") (parent-is "item")) parent-bol
      typst-ts-mode-indent-offset)
     
     ;; multi-line item
     ;; -  #[hi] foo
     ;;    bar
     ;; my try with `prev-adaptive-prefix' failed even after set the
     ;; `adaptive-fill-regexp'
     ((match nil "item" nil 2 nil)
      typst-ts-mode--indentation-multiline-item-get-anchor 0)
     
     ;; item - new item content should follow its previous line's indentation
     ;; level
     ;; e.g.
     ;; -  hi | <- return (newline command)
     ((and no-node
           typst-ts-mode--indentation-prev-line-is-item-p
           ;; not in container
           ;; example:
           ;; - hi
           ;;   hi #[
           ;;     - hello | <- return
           ;;   ]
           typst-ts-mode--indentation-editing-not-inside-code-container-p)
      typst-ts-mode--indentation-multiline-item-get-anchor_ 0)

     ;; raw block
     ;; whether normally or in insertion, the current node is always nil...
     ((n-p-gp nil "blob" "raw_blck")
      no-indent 0)

     ((match "```" "raw_blck" nil 2 3 )
      parent-bol 0)

     ;; inside container && container is direct child of "section" (headline)
     (typst-ts-mode-indent--no-node-section-container-p
      great-grand-parent 0)
     ((n-p-gp nil ,typst-ts-mode--container-node-types-regexp "section")
      grand-parent 0)

     ;; inside container
     ((and no-node (n-p-gp nil "parbreak" ,typst-ts-mode--container-node-types-regexp))
      typst-ts-mode-indent--grand-parent-bol typst-ts-mode-indent-offset)
     ((parent-is ,typst-ts-mode--container-node-types-regexp)
      parent-bol typst-ts-mode-indent-offset)

     (no-node parent-bol 0)

     ((parent-is "ERROR") no-indent 0)

     ;; example: (item (text) (text) (text)) when `(text)' is in different line
     (catch-all prev-line 0)))
  "Tree-sitter indent rules for `typst-ts-mode'.")

(defun typst-ts-mode--indentation-multiline-item-get-anchor (_node parent _bol)
  "Return the start of second child of PARENT."
  (treesit-node-start (treesit-node-child parent 1)))

(defun typst-ts-mode--indentation-multiline-item-get-anchor_ (_node _parent _bol)
  "Return the start of second child of the current item.
This function is meant to be used when user hits a return key."
  (treesit-node-start
   (treesit-node-child
    (typst-ts-core-parent-util-type
     (treesit-node-at
      (save-excursion
        (forward-line -1)
        (back-to-indentation)
        (point)))
     "item" t)
    1)))

(defun typst-ts-mode--indentation-prev-line-is-item-p (_node _parent _bol)
  (save-excursion
    (forward-line -1)
    (back-to-indentation)
    (typst-ts-core-parent-util-type
     (treesit-node-at (point))
     "item" t)))

(defun typst-ts-mode--indentation-editing-not-inside-code-container-p
    (node parent _bol)
  "NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (let* ((gp-node (treesit-node-parent parent))
         (ggp-node (treesit-node-parent gp-node)))
    (not (and
          (null node)
          (equal (treesit-node-type parent) "parbreak")
          (equal (treesit-node-type gp-node) "content")
          (equal (treesit-node-type ggp-node) "code")))))


(defun typst-ts-mode-comment-setup()
  "Setup comment related stuffs for `typst-ts-mode'."
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

(defun typst-ts-mode--imenu-function-defintion-p (node)
  "Whether NODE is a function defintion node."
  (let* ((parent-node (treesit-node-parent node))
         (grandparent-node (treesit-node-parent parent-node)))
    (and (equal (treesit-node-type node) "ident")
         (equal (treesit-node-type parent-node) "call")
         (equal (treesit-node-field-name parent-node) "pattern")
         (equal (treesit-node-type grandparent-node) "let"))))

(defun typst-ts-mode--imenu-name-function (node)
  "Generate name of NODE for displaying in Imenu."
  (treesit-node-text node))

;; outline-minor-mode
(defconst typst-ts-mode-outline-regexp "^[[:space:]]*\\(=+\\)"
  "Regexp identifying Typst header.")

(defconst typst-ts-mode-outline-heading-alist
  '(("=" . 1) ("==" . 2) ("===" . 3) ("====" . 4) ("=====" . 5) ("======" . 6))
  "See `outline-heading-alist'.")

(defun typst-ts-mode-outline-level ()
  "Return the level of the heading at point."
  (save-excursion
    (end-of-line)
    (if (re-search-backward typst-ts-mode-outline-regexp nil t)
        (- (match-end 1) (match-beginning 1))
      0)))


;;;###autoload
(defun typst-ts-mode-preview (file)
  "Open the result compile file.
FILE: file path for the result compile file."
  (interactive (list (typst-ts-compile-get-result-pdf-filename)))
  ;; don't use `browse-url-of-file', which cannot open non-english documents
  (browse-url file))

;;;###autoload
(defvar-keymap typst-ts-mode-map
  "C-c C-c" #'typst-ts-compile  ; use prefix argument to do preview
  "C-c C-w" #'typst-ts-watch-mode
  "C-c C-p" #'typst-ts-mode-preview

  "M-<left>" #'typst-ts-mode-meta-left
  "M-<right>" #'typst-ts-mode-meta-right
  "M-<down>" #'typst-ts-mode-meta-down
  "M-<up>" #'typst-ts-mode-meta-up
  "M-<return>" #'typst-ts-mode-meta-return

  ;; don't bind <return>
  ;; Binding a command to "<return>" is generally a bug.
  ;; Emacs will first look for a binding for `return` and if it finds one
  ;; it'll use it in preference to a binding for `RET`, regardless of the
  ;; relative precedence of the keymaps involved.
  "TAB" #'typst-ts-mode-cycle
  "RET" #'typst-ts-mode-return
  "C-c '" #'typst-ts-edit-indirect)


(defun typst-ts-mode-indent-line-function ()
  "A simple wrapper of `treesit-indent' for handle indentation edge cases.
It is useful to handle end of buffer situation (please turn on `whitespace-mode'
to see that it's actually end of buffer).  Basically, if we are at the end of
buffer, the node, parent passed to our treesit indentation function will be nil,
source_file, which is not desired.
If we are before a '\n' character, then the node and its parent probably are
nil and parbreak."
  (when (eobp)
    (insert "\n")
    (backward-char))
  (treesit-indent))

(defun typst-ts-mode-electric-pair-open-newline-between-pairs-psif ()
  "Custom version of `electric-pair-open-newline-between-pairs-psif'.
It provide the ability to automatically open a new line for '$' character."
  (when (and (if (functionp electric-pair-open-newline-between-pairs)
                 (funcall electric-pair-open-newline-between-pairs)
               electric-pair-open-newline-between-pairs)
             (eq last-command-event ?\n)
             (< (1+ (point-min)) (point) (point-max))
             (let ((cb (save-excursion
                         (skip-chars-backward "\t\s")
                         (char-before (1- (point)))))
                   (ca (char-after)))
               (or (eq cb (matching-paren ca))
                   (and (eq cb ?\$) (eq ca ?\$)))))
    (save-excursion (newline 1 t))))

(defun typst-ts-mode-check-grammar-version ()
  "Check typst tree sitter grammar version.
May not be correct(modified time can be the download time, copied time, etc.),
but it does help prevent some error cases."
  (when typst-ts-mode-grammar-location
    (let ((min-time (time-convert typst-ts-mode--grammar-minimum-version-timestamp nil))
          (mod-time
           (file-attribute-modification-time
            (file-attributes typst-ts-mode-grammar-location))))
      (when (time-less-p mod-time min-time)
        (message
         (propertize
          (format "Please ensure that you have installed the latest \
typst tree sitter grammar (at least %s)!" (current-time-string min-time))
          'face '(:weight bold :foreground "firebrick")))))))

(defun typst-ts-mode-after-hook-function ()
  "Run after all hooks in `typst-ts-mode-hook'."
  ;; patch `electric-pair-post-self-insert-function' function
  (when electric-pair-mode
    ;; add-function :override buffer-locally doesn't work, so we do this...
    (remove-hook 'post-self-insert-hook 'electric-pair-post-self-insert-function t)
    (add-hook 'post-self-insert-hook
              'typst-ts-mode-electric-pair-open-newline-between-pairs-psif
              t))

  ;; Set Compile Command
  (ignore-errors
    (unless compile-command
      (setq-local
       compile-command
       (format "%s compile %s %s"
               typst-ts-compile-executable-location
               (file-name-nondirectory buffer-file-name)
               typst-ts-compile-options))))

  ;; Although without enabling `outline-minor-mode' also works, enabling it
  ;; provides outline ellipsis (if you use `set-display-table-slot' to set)
  (outline-minor-mode t)

  ;; FIXME
  ;; necessary for
  ;; `typst-ts-mode-cycle'(`typst-ts-editing--indent-item-node-lines')
  ;; since it calculate offset based on character
  ;; (maybe also some indentation rules)
  (indent-tabs-mode -1)

  (typst-ts-mode-check-grammar-version))

;;;###autoload
(define-derived-mode typst-ts-mode text-mode "Typst"
  "Major mode for editing Typst, powered by tree-sitter."
  :group 'typst
  :syntax-table typst-ts-mode-syntax-table
  :after-hook
  (typst-ts-mode-after-hook-function)

  (unless (treesit-ready-p 'typst)
    (user-error "Tree-sitter for Typst isn't available"))

  (setq-local treesit-primary-parser (treesit-parser-create 'typst))

  ;; Comments.
  (typst-ts-mode-comment-setup)

  ;; Electric
  (setq-local
   ;; &: math align
   ;; .: code field
   electric-indent-chars (append "{}()[]$&." electric-indent-chars)
   electric-pair-pairs '((?\" . ?\")
                         (?\{ . ?\})
                         (?\( . ?\))
                         (?\[ . ?\])
                         (?\$ . ?\$)))

  ;; Font Lock
  (setq-local treesit-font-lock-settings typst-ts-mode-font-lock-settings)
  (setq-local treesit-font-lock-feature-list typst-ts-mode-font-lock-feature-list)

  ;; Indentation
  (setq-local treesit-simple-indent-rules typst-ts-mode-indent-rules)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              ;; Here we uses a trick. In the docs of
              ;; `treesit-simple-imenu-settings', the second parameter should
              ;; be a regexp string. However, it can be anything that
              ;; the PRED in `treesit-thing-settings' can be
              ;; For emacs 30, there are some restriction (second param must be
              ;; regexp string) when you use default settings for outline
              ;; (outline from imenu) see `treesit-major-mode-setup' and
              ;; `treesit-outline-predicate'
              `(("Functions" typst-ts-mode--imenu-function-defintion-p nil
                 typst-ts-mode--imenu-name-function)
                ("Headings" "^heading$" nil typst-ts-mode--imenu-name-function)))

  (setq-local treesit-defun-type-regexp
              (regexp-opt '("let" "math")))

  ;; treesit-defun-name-function

  ;; (setq-local treesit-thing-settings
  ;;             `((typst ())))

  ;; Outline
  (if nil  ; (>= emacs-major-version 30)
      ;; FIXME maybe it's a upstream bug. Circle top-level section will cycle all the content below
      (setq treesit-outline-predicate (regexp-opt '("section" "source_file")))
    (setq-local outline-regexp typst-ts-mode-outline-regexp)
    (setq-local outline-level #'typst-ts-mode-outline-level))
  (setq-local outline-heading-alist typst-ts-mode-outline-heading-alist)

  (treesit-major-mode-setup)

  (setq-local indent-line-function #'typst-ts-mode-indent-line-function))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))


(provide 'typst-ts-mode)

;;; typst-ts-mode.el ends here
