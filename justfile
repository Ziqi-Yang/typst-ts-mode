set dotenv-load

emacs := env("EMACS", "emacs")
test-file := env("TEST_FILE", "./test/basic-syntax.typ")

eval:
    {{emacs}} -Q --debug-init -L . --eval "(progn (require 'typst-ts-mode) (setq typst-ts-mode-enable-raw-blocks-highlight t))" {{test-file}}

clean:
    rm -f ./*.elc

lint:
    # makem: https://github.com/alphapapa/makem.sh
    ./makem.sh lint
