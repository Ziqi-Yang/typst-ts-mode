;;; typst-ts-embedding-lang-settings.el --- Embedding Languages Settings  -*- lexical-binding: t; -*-
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

;; Functionality to embed other languages in typst documentation.

;;; Code:
(require 'treesit)
(require 'typst-ts-core)

(defvar typst-ts-els-tag-lang-map
  #s(hash-table
     size 588
     test equal
     data
     (
      "txt" txt "asp" asp "asa" asp
      "as" actionscript "actionscript" actionscript "applescript" applescript
      "script editor" applescript "bat" bat "cmd" bat
      "build" build "c#" c-sharp "csx" c-sharp
      "cs" c-sharp "c++" cpp "h++" cpp
      "hxx" cpp "cpp" cpp "hpp" cpp
      "ipp" cpp "hh" cpp "inl" cpp
      "cxx" cpp "cc" cpp "cp" cpp
      "c" c "css.liquid" css "css" css
      "css.erb" css "cljs" clojure "clj" clojure
      "cljc" clojure "edn" clojure "clojure" clojure
      "di" d "d" d "patch" diff
      "diff" diff "escript" erlang "erlang" erlang
      "erl" erlang "hrl" erlang "emakefile" erlang
      "yaws" yaws "gitattributes" attributes ".gitattributes" attributes
      "attributes" attributes "tag_editmsg" commit_editmsg "commit_editmsg" commit_editmsg
      "merge_msg" commit_editmsg ".gitconfig" gitconfig ".gitmodules" gitconfig
      "gitconfig" gitconfig "gitignore" exclude ".gitignore" exclude
      "exclude" exclude ".git" .git "gitlog" gitlog
      "mailmap" .mailmap ".mailmap" .mailmap "git-rebase-todo" git-rebase-todo
      "go" go "gv" dot "dot" dot
      "gvy" groovy "gradle" groovy "groovy" groovy
      "jenkinsfile" groovy "html" html "htm" html
      "shtml" html "xhtml" html "hs" haskell
      "haskell" haskell "lhs" lhs "ipynb" json
      "sublime-macro" json "sublime-project" json "sublime-completions" json
      "sublime-keymap" json "sublime-settings" json "sublime-mousemap" json
      "sublime-commands" json "sublime-color-scheme" json "pipfile.lock" json
      "sublime-menu" json "sublime-build" json "json" json
      "sublime-theme" json "jsp" jsp "java" java
      "bsh" java "javadoc" javadoc "properties" properties
      "htc" javascript "js" javascript "javascript" javascript
      "bib" bibtex "bibtex" bibtex "ltx" latex
      "latex" latex "sty" tex "cls" tex
      "tex" tex "clisp" commonlisp "fasl" lisp
      "el" lisp "lisp" lisp "l" lisp
      "mud" lisp "lsp" lisp "cl" lisp
      "scm" lisp "ss" lisp "lua" lua
      "makefile.am" makefile "mk" makefile "makefile.in" makefile
      "make" makefile "makefile" makefile "gnumakefile" makefile
      "mak" makefile "ocamlmakefile" makefile "mdown" markdown
      "markdn" markdown "md" markdown "markdown" markdown
      "multimarkdown" multimarkdown "matlab" matlab "ml" ocaml
      "ocaml" ocaml "mli" ocaml "mll" ocamllex
      "ocamllex" ocamllex "mly" ocamlyacc "ocamlyacc" ocamlyacc
      "camlp4" camlp4 "objective-c++" objective-c++ "mm" objective-c++
      "objective-c" objective-c "h" objective-c "m" objective-c
      "php" php "phps" php "php3" php
      "phpt" php "phtml" php "php7" php
      "php5" php "php4" php "pascal" pascal
      "dpr" pascal "p" pascal "pas" pascal
      "pm" perl "pmc" perl "perl" perl
      "pl" perl "pc" perl "pod" perl
      "t" perl "py" python "pxi.in" python
      "pxi" python "rpy" python "sconstruct" python
      "pyi" python "gypi" python "pyw" python
      "bazel" python "python" python "pyx.in" python
      "wscript" python "pxd" python "pyx" python
      "sconscript" python "pxd.in" python "snakefile" python
      "py3" python "gyp" python "vpy" python
      "bzl" python "cpy" python "r" r
      "rprofile" r "rd" rd "rails" rails
      "rhtml" rails "erb" rails "html.erb" rails
      "js.erb" js.erb "haml" haml "sass" haml
      "builder" rxml "rxml" rxml "erbsql" erbsql
      "sql.erb" erbsql "re" re "rest" restructuredtext
      "restructuredtext" restructuredtext "rst" restructuredtext "capfile" ruby
      "simplecov" ruby "thor" ruby "berksfile" ruby
      "thorfile" ruby "cheffile" ruby "podspec" ruby
      "config.ru" ruby "rjs" ruby "vagrantfile" ruby
      "ruby" ruby "deliverfile" ruby "rb" ruby
      "guardfile" ruby "ruby.rail" ruby "irbrc" ruby
      "fastfile" ruby "fcgi" ruby "rabl" ruby
      "rantfile" ruby "prawn" ruby "appfile" ruby
      "appraisals" ruby "scanfile" ruby "brewfile" ruby
      "podfile" ruby "gemfile" ruby "gemspec" ruby
      "jbuilder" ruby "rake" ruby "rbx" ruby
      "snapfile" ruby "rakefile" ruby "cgi" ruby
      "rust" rust "rs" rust "ddl" sql
      "dml" sql "sql" sql "scala" scala
      "sbt" scala "sc" scala "pkgbuild" bash
      "zsh" bash "bash" bash "ash" bash
      ".zprofile" bash "sh" bash "eclass" bash
      ".bash_functions" bash ".bash_logout" bash ".bash_completions" bash
      ".bash_aliases" bash "ebuild" bash ".bash_profile" bash
      ".zshrc" bash ".textmate_init" bash ".zlogin" bash
      ".zshenv" bash ".profile" bash ".bash_variables" bash
      ".bash_login" bash ".zlogout" bash ".bashrc" bash
      "shell-unix-generic" shell-unix-generic "commands-builtin-shell-bash" commands-builtin-shell-bash "adp" adp
      "tcl" tcl "textile" textile "xsd" xml
      "rss" xml "xslt" xml "dtml" xml
      "opml" xml "tld" xml "rng" xml
      "xml" xml "svg" xml "xaml" xml
      "yaml" yaml "yml" yaml "sublime-syntax" yaml
      "awk" awk "ads" ada "adb" ada
      "gpr" ada "ada" ada ".htpasswd" envvars
      "htpasswd" envvars ".htaccess" envvars "htaccess" envvars
      "htgroups" envvars ".htgroups" envvars "envvars" envvars
      "ad" adoc "asciidoc" adoc "adoc" adoc
      "nasm" yasm "yasm" yasm "asm" yasm
      "mac" yasm "inc" yasm "h.in" h.in
      "hxx.in" hh.in "h++.in" hh.in "hh.in" hh.in
      "hpp.in" hh.in "cmake" cmake "cmakelists.txt" cmake
      "cmakecache" cmakecache "cmakecache.txt" cmakecache "cmakecommands" cmakecommands
      "tsv" csv "csv" csv "cabal" cabal
      "cson" coffeescript "coffeescript" coffeescript "coffee" coffeescript
      "coffee.erb" coffeescript "cakefile" coffeescript "cpuinfo" cpuinfo
      "tab" crontab "cron.d" crontab "crontab" crontab
      "crystal" crystal "cr" crystal "dart" dart
      "dockerfile" dockerfile "env.template" dotenv ".env.production" dotenv
      ".flaskenv" dotenv ".envrc" dotenv ".env.testing" dotenv
      ".env.dusk.local" dotenv "env.example" dotenv "env.sample" dotenv
      ".env.development" dotenv ".env.defaults" dotenv ".env.test" dotenv
      ".env" dotenv ".env.local" dotenv ".env.prod" dotenv
      ".env.dist" dotenv ".env.test.local" dotenv ".env.dev" dotenv
      ".env.production.local" dotenv "env" dotenv "dotenv" dotenv
      ".env.sample" dotenv ".env.default" dotenv ".env.example" dotenv
      ".env.development.local" dotenv ".env.template" dotenv ".env.staging" dotenv
      "exs" elixir "ex" elixir "elixir" elixir
      "html.leex" html.eex "html.eex" html.eex "elm" elm
      "eml" email "msg" email "mboxz" email
      "email" email "mbx" email "fsi" fs
      "f#" fs "fsx" fs "fish" fish
      "f77" f "fpp" f "f" f
      "for" f "f08" f90 "f90" f90
      "f95" f90 "f03" f90 "namelist" namelist
      "fstab" fstab "crypttab" fstab "mtab" fstab
      "rahit" glsl "vshader" glsl "tesc" glsl
      "vert" glsl "rcall" glsl "vsh" glsl
      "frag" glsl "comp" glsl "task" glsl
      "rgen" glsl "tese" glsl "gsh" glsl
      "fs" glsl "rchit" glsl "fshader" glsl
      "vs" glsl "gshader" glsl "gs" glsl
      "rmiss" glsl "mesh" glsl "fsh" glsl
      "geom" glsl "glsl" glsl "rint" glsl
      "gql" graphql "graphqls" graphql "graphql" graphql
      "9" groff/troff "3" groff/troff "groff" groff/troff
      "4" groff/troff "6" groff/troff "8" groff/troff
      "groff/troff" groff/troff "2" groff/troff "troff" groff/troff
      "1" groff/troff "7" groff/troff "5" groff/troff
      "group" group "twig" twig "html.twig" twig
      "hosts" hosts "inf" ini "reg" ini
      "desktop" ini "url" ini ".hgrc" ini
      "cfg" ini ".editorconfig" ini ".gitlint" ini
      "hgrc" ini "ini" ini "lng" ini
      ".pylintrc" ini ".coveragerc" ini "htm.j2" htm.j2
      "xhtml.j2" htm.j2 "html.j2" htm.j2 "xml.j2" htm.j2
      "jinja" jinja2 "j2" jinja2 "jinja2" jinja2
      "libsonnet" jsonnet "libjsonnet" jsonnet "jsonnet" jsonnet
      "julia" julia "jl" julia "kts" kotlin
      "kotlin" kotlin "kt" kotlin "less" less
      "css.less" less "llvm" llvm "ll" llvm
      "lean" lean "man" manpage "manpage" manpage
      "mediawikerpanel" mediawikerpanel "wikipedia" mediawiki "wiki" mediawiki
      "mediawiki" mediawiki "meminfo" meminfo "mime.types" nginx
      "conf.erb" nginx "nginx.conf" nginx "scgi_params" nginx
      "conf" nginx "fastcgi_params" nginx "nginx" nginx
      "uwsgi_params" nginx "nimble" nim "nims" nim
      "nim" nim "ninja" ninja "nix" nix
      "orgmode" orgmode "org" orgmode "passwd" passwd
      "proto" proto "protodevel" proto "pbtxt" pb.txt
      "proto.text" pb.txt "prototxt" pb.txt "pb.txt" pb.txt
      "textpb" pb.txt "pp" puppet "puppet" puppet
      "epp" puppet "purescript" purescript "purs" purescript
      "qml" qml "qmlproject" qml "rkt" racket
      "racket" racket "rego" rego "requirements.txt" requirements.txt
      "requirements.in" requirements.txt "pip" requirements.txt "resolv.conf" resolv
      "resolv" resolv "resource" robot "robot" robot
      "scss" scss "cm" sml "sig" sml
      "sml" sml "skim" slim "slim" slim
      "strace" strace "styl" stylus "stylus" stylus
      "sol" solidity "solidity" solidity "vy" vyper
      "vyper" vyper "jq" jq "svlt" svelte
      "svelte" svelte "swift" swift "svh" systemverilog
      "systemverilog" systemverilog "sv" systemverilog "vh" systemverilog
      "pipfile" toml "tml" toml "cargo.lock" toml
      "toml" toml "pdm.lock" toml "gopkg.lock" toml
      "poetry.lock" toml "tfstate" tfstate "terraform" terraform
      "tf" terraform "tfvars" terraform "hcl" terraform
      "done.txt" todo.txt "todo.txt" todo.txt "typescript" typescript
      "ts" typescript "mts" typescript "cts" typescript
      "typescriptreact" typescriptreact "tsx" typescriptreact "verilog" verilog
      "v" verilog ".gvimrc" viml "_gvimrc" viml
      "viml" viml "gvimrc" viml "vim" viml
      ".vimrc" viml "vimrc" viml "_vimrc" viml
      "vue" vue "zig" zig "gp" gnuplot
      "plt" gnuplot "gnu" gnuplot "gnuplot" gnuplot
      "gpl" gnuplot "plot" gnuplot "http" http
      "log" log "show-nonprintable" show-nonprintable "pub" authorized_keys
      "authorized_keys" authorized_keys "authorized_keys2" authorized_keys "known_hosts" known_hosts
      "known_hosts.old" known_hosts "ssh_config" ssh_config "sshd_config" sshd_config
      "syslog" syslog "varlink" varlink "typc" typst
      "typ" typst "typst" typst
      ))
  "Raw block tag -> tree sitter language map.")


;; refer to markdown-get-lang-mode
(defun typst-ts-els-get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (let ((lang-norm (downcase lang)))
    (cl-find-if
     #'(lambda (mode) (and mode (fboundp mode)))
     (list
      (and (treesit-language-available-p (intern lang-norm))
           (intern (concat lang-norm "-ts-mode")))
      (intern (concat lang-norm "-mode"))))))

;; refer to `markdown-fontify-code-block-natively'
(defun typst-ts-els-fontify-raw-block (lang-mode start end)
  "Fontify a raw block using traditional approach.
LANG-MODE START END."
  (let ((string (buffer-substring-no-properties start end))
        (modified (buffer-modified-p))
        (typst-doc-buffer (current-buffer)) pos next)
    (remove-text-properties start end '(face nil))
    (with-current-buffer
        (get-buffer-create
         (concat " *typst-raw_blck-fontification:" (symbol-name lang-mode)))
      ;; Make sure that modification hooks are not inhibited in
      ;; the org-src-fontification buffer in case we're called
      ;; from `jit-lock-function' (Bug#25132).
      (let ((inhibit-modification-hooks nil))
        (delete-region (point-min) (point-max))
        (insert string " ")) ;  so there's a final property change
      (unless (eq major-mode lang-mode) (funcall lang-mode))
      (font-lock-ensure)
      (setq pos (point-min))
      (while (setq next (next-single-property-change pos 'face))
        (let ((val (get-text-property pos 'face)))
          (when val
            (put-text-property
             (+ start (1- pos)) (1- (+ start next)) 'face
             val typst-doc-buffer)))
        (setq pos next)))
    (set-buffer-modified-p modified)))

(provide 'typst-ts-embedding-lang-settings)

;;; typst-ts-embedding-lang-settings.el ends here
