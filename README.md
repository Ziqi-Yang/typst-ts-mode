# Typst Tree-Sitter Mode

Tree Sitter support for Typst. Minimum Emacs version requirement: 29.  

[Discussion on Zulip](https://meow-place.zulipchat.com/)  
[Tickets](https://todo.sr.ht/~meow_king/typst-ts-mode): Issues, feature requests, etc.  
[Announce](https://lists.sr.ht/~meow_king/typst-ts-mode-announce) Announcements. Subscribe to it to receive the latest news for `typst-ts-mode`.  
[Send a Patch](https://lists.sr.ht/~meow_king/typst-ts-mode-dev)  

## Requirement

1. Emacs >= 29

2. Latest [Typst](https://github.com/typst/typst).  
`typst 0.11.0`

3. Tree Sitter grammar for Typst: https://github.com/uben0/tree-sitter-typst  
   commit: `4610172f312e8ce5184e6882be5ad1a1cd800fbe -  Mon Apr 22 15:13:47 2024 +0200`  
   To install the grammar, you can execute the following elisp code (once):  

   ``` emacs-lisp
   (add-to-list 'treesit-language-source-alist
                '(typst "https://github.com/uben0/tree-sitter-typst"))
   (treesit-install-language-grammar 'typst)
   ```

   Note there are also [tree sitter modules](https://github.com/casouri/tree-sitter-module), [treesit-auto](https://github.com/renzmann/treesit-auto) and [treesit-langs](https://github.com/kiennq/treesit-langs) for you to easily manage your tree sitter modes and grammars. For vanilla solution, there is my [configuration](https://github.com/Ziqi-Yang/.emacs.d/blob/main/languages/l-treesit.el) for reference.

## Installation

Example configuration.
``` emacs-lisp
(use-package typst-ts-mode
  :ensure (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :custom
  ;; (optional) If you want to ensure your typst tree sitter grammar version is greater than the minimum requirement
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory)))
```

## Keys

**C-c C-c c**   : **typst-ts-compile-and-preview**  
**C-c C-c C**   : **typst-ts-compile**  
**C-c C-c w**   : **typst-ts-watch-mode**  
**C-c C-c p**   : **typst-ts-mode-preview**  
**M-\<left\>**  : **typst-ts-mode-heading-decrease**  
**M-\<right\>** : **typst-ts-mode-heading-increase**  
**M-\<up\>**    : **typst-ts-mode-heading-up**  
**M-\<down\>**  : **typst-ts-mode-heading-down**  
**M-\<return\>**: **typst-ts-mode-meta-return**  
**\<return\>**  : **typst-ts-mode-return**  
**TAB**         : **typst-ts-mode-cycle**  
**C-c '**       : **typst-ts-edit-indirect** (requires <https://github.com/Fanael/edit-indirect/>)

*NOTE*: `outline-minor-mode` is enabled by `typst-ts-mode`, so you can use command 
defined by `outline-minor-mode` such as `outline-cycle`.

## Customization Options

You can view all the custom options in Emacs using `customize` command. Go to `typst-ts` group.  

Here are some options you may find useful:  
1. **typst-ts-mode-indent-offset** (default 4)  
   If you want to have `org-indent-mode` like behavior, you can use [outline-indent-mode](https://sr.ht/~meow_king/outline-indent-mode/) plugin.
2. **typst-ts-compile-executable-location**  
   You can set a custom typst executable location.
3. **typst-ts-watch-options**.  
   Set this to `--open` so typst will automatically open the compiled file for you when you enter `typst-ts-watch-mode`.
4. **typst-ts-compile-options**.  
   Note that setting `--open` has no use for this customization variable. You can execute command `async-shell-command` and input `typst compile <file> --open && sleep 1` to view what is happening. Use command `typst-ts-compile-and-preview` if you want to view the output document after compiling.
5. **typst-ts-watch-auto-display-compilation-error**. (default `t`)  
   This variable controls whether `typst watch` process buffer will appear when an error occurs, and disappear when there is no error.
   You may find `auto-save-visited-mode`, [auto-save](https://github.com/manateelazycat/auto-save) or [super-save](https://github.com/bbatsov/super-save) useful (or annoying).
6. **typst-ts-compile-before-compilation-hook** and **typst-ts-compile-after-compilation-hook**  
7. **typst-ts-mode-grammar-location**: used for grammar version check at major mode start.

### Fontification
1. **typst-ts-mode-fontification-precise-level** (default `'middle`)  
   Available values: `min`, `middle` and `max`. Different level affects the precision
   of the fontification. For example, to fontify `- item`, we may fontify the whole expression
   using one face, or two faces (one for `-`, and one for `item`). Note it is related to the performance of fontification process, especially the first fontification process (when you open the file).  
2. **typst-ts-markup-header-same-height** and **typst-ts-markup-header-scale**  
   Control header height. Note that it only works when **typst-ts-mode-fontification-precise-level**
   is set to `max`.
3. Override default font lock rules  
   Please see the documentation of **typst-ts-mode-font-lock-rules**, you can find 
   how to override the whole font lock rules or only small part of the font lock
   rules.

### Raw block highlighting
_This is an experimental feature_  
Only support tree-sitter languages.  
**Note**: Currently only support Emacs 30 (master branch). But non-treesit font lock for raw block feature will be added soon (both 29 and 30).  
For more detailed documentation about raw block highlighting see 
[this documentation](./doc/raw-block-highlighing.md)  
1. **typst-ts-mode-enable-raw-blocks-highlight** (default `nil`)  
2. **typst-ts-mode-highlight-raw-blocks-at-startup** (default `nil`)  
3. **typst-ts-highlight-raw-block-langs-not-in-predefined-settings** (default `t`)  

### Consult Imenu Integration
If you use `consult-iemnu`
command [consult](https://github.com/minad/consult), you way want this setting.
``` emacs-lisp
(setq
 consult-imenu-config
 (append consult-imenu-config
         '((typst-ts-mode :topLevel "Headings" :types
                          ((?h "Headings" typst-ts-markup-header-face)
                           (?f "Functions" font-lock-function-name-face))))))
```

## Contribute

Please work on `develop` branch, which will be combined into `main` branch every one week or so if there are new commits.  
This document [./side/sync_with_upstream.typ](./side/sync_with_upstream.typ) shows information related to synchronous status with upstream grammar.

### Tree Sitter Resources
1. `(info "(elisp) Parser-based Font Lock")` and `(info "(elisp) Parsing Program Source")` (Use `eval-expression` command to jump to the corresponding Info Manual Node)
2. [Let’s Write a Tree-Sitter Major Mode - Matering Emacs](https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode)
3. [ts-query-highlight](https://sr.ht/~meow_king/ts-query-highlight/). I wrote this package to highlight tree sitter queries. This package is used in the process of building `typst-ts-mode`.
4. All common treesit operations can be viewd using command `shortdoc`

### Package Development

#### How to load all files used by this packages?
This snippet is taken from [The Emacs Package Developer’s Handbook - Byte-compile and load directory](https://github.com/alphapapa/emacs-package-dev-handbook?tab=readme-ov-file#byte-compile-and-load-directory) with some modification.

```emacs-lisp
(defun others/byte-compile-and-load-directory (directory)
  "Byte-compile and load all elisp files in DIRECTORY.
Interactively, directory defaults to `default-directory' and asks
for confirmation."
  (interactive (list default-directory))
  (let* ((load-path (cons directory load-path))
         (files (directory-files directory 't (rx ".el" eos))))
    (dolist (file files)
      (byte-compile-file file 'load))))
```
#### How to eval a buffer and override the origin definitions
You can use `eval-defun` to eval and override a function definition. However, to eval and override a buffer, `eval-buffer` doesn't work.  
This snippet is taken from [How do I force re-evaluation of a defvar? - François Févotte - emacs.stackexchange.com](https://emacs.stackexchange.com/a/2302) with some modification.

```emacs-lisp
(defun others/eval-buffer ()
"Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))
```

#### How to do lint
I recommend you to use [makem.sh](https://github.com/alphapapa/makem.sh). However, you can choose anything you like.   
To use it, first `cd` into the project root directory, then: 
```emacs-lisp
wget https://raw.githubusercontent.com/alphapapa/makem.sh/master/makem.sh
chmod +x ./makem.sh
```
To do `lint`, you can execute shell command `./makem.sh lint`

## Co-Maintainer

[Huan Nguyen](https://sr.ht/~huan)
