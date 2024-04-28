els-settings-test:
    emacs --batch -l ./typst-ts-utils.el \
    -l ./typst-ts-embedding-lang-settings.el \
    -l ~/.config/emacs/.local/elpaca/repos/emacs-kotlin-ts-mode/kotlin-ts-mode.el \
    -l ~/.config/emacs/.local/elpaca/repos/mermaid-ts-mode/mermaid-ts-mode.el \
    --eval "(typst-ts-embedding-lang-settings-test)"

lint:
    # makem: https://github.com/alphapapa/makem.sh
    ./makem.sh lint
