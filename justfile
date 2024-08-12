set dotenv-load

emacs := env("EMACS", "emacs")
test-file := env("TEST_FILE", "./test/basic-syntax.typ")

eval:
    {{emacs}} -Q --debug-init -L . --eval "(require 'typst-ts-mode)" {{test-file}}

clean:
    rm -f ./*.elc

lint:
    # makem: https://github.com/alphapapa/makem.sh
    ./makem.sh lint
