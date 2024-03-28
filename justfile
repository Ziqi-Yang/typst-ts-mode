els-settings-test:
    emacs --batch -l ./typst-ts-embedding-lang-settings.el \
    -l ~/.emacs.d/.local/elpaca/repos/emacs-kotlin-ts-mode/kotlin-ts-mode.el \
    -l ~/.emacs.d/.local/elpaca/repos/mermaid-ts-mode/mermaid-ts-mode.el \
    --eval "(typst-ts-embedding-lang-settings-test)"

lint:
    ./makem.sh lint

lint-29:
    ./makem.sh lint -E ~/myBin/emacs29.2
