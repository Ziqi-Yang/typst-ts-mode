;;; typst-ts-lsp.el --- Eglot tinymist integration  -*- lexical-binding: t; -*-
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

(require 'eglot)

(defgroup typs-ts-lsp nil
  "Typst TS eglot integration with tinymist."
  :prefix "typst-ts-compile"
  :group 'typst-ts)

(defcustom typst-ts-lsp-download-path (file-name-concat (locate-user-emacs-file ".cache") "lsp" "tinymist" "tinymist")
  "Install path for the language server."
  :group 'tools
  :group 'typst-ts-lsp
  :type 'file)

(add-to-list 'eglot-server-programs
             `((typst-ts-mode)
               .
               ,(eglot-alternatives `(,typst-ts-lsp-download-path "tinymist"))))

(defun typst-ts-lsp-download-binary ()
  "Download latest tinymist binary to `typst-ts-lsp-download-path'.
Will override old versions."
  (interactive)
  (unless (file-exists-p typst-ts-lsp-download-path)
    (make-directory (file-name-directory typst-ts-lsp-download-path) t))
  (url-copy-file
   (concat "https://github.com/Myriad-Dreamin/tinymist/releases/latest/download/tinymist-"
           (pcase system-type
             ('gnu/linux "linux")
             ('darwin "darwin")
             ('windows-nt "win32")
             (_ "linux"))
           ;; TODO too lazy to find out all the arch suffixes
           "-x64")
   typst-ts-lsp-download-path t)
  (set-file-modes typst-ts-lsp-download-path
                  (logior (file-modes typst-ts-lsp-download-path) #o100)))

(provide 'typst-ts-lsp)
;;; typst-ts-lsp.el ends here