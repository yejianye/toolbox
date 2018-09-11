(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (names pythonic anaconda-mode helm-gtags ggtags counsel-gtags csv-mode ob-ipython ein request-deferred websocket s mvn meghanada maven-test-mode groovy-mode groovy-imports pcache gradle-mode ensime sbt-mode scala-mode company-emacs-eclim eclim sql-indent outshine zenburn-theme yasnippet-snippets yapfify yaml-mode yafolding ws-butler writeroom-mode winum which-key web-mode web-beautify vue-mode volatile-highlights vi-tilde-fringe uuidgen use-package unfill treemacs-projectile treemacs-evil toc-org tagedit symon string-inflection sr-speedbar spaceline-all-the-icons solarized-theme smeargle slim-mode scss-mode sayid sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe rjsx-mode restclient-helm restart-emacs rbenv rake rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode powershell popwin pippel pip-requirements persp-mode password-generator paradox overseer orgit org-projectile org-present org-pomodoro org-mime org-download org-cliplink org-bullets org-brain open-junk-file ob-restclient ob-http nameless mwim move-text minitest markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc jinja2-mode jade-mode indent-guide impatient-mode hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-helm flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu emmet-mode elisp-slime-nav editorconfig dumb-jump diminish define-word cython-mode counsel-projectile company-web company-tern company-statistics company-restclient company-ansible company-anaconda column-enforce-mode coffee-mode cnfonts clojure-snippets clojure-cheatsheet clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby bundler auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile ansible-doc ansible aggressive-indent adaptive-wrap ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-automatically-star t)
 '(safe-local-variable-values
   (quote
    ((eval progn
           (pp-buffer)
           (indent-buffer))
     (eval toggle-word-wrap -1)
     (sync-remote-dir . "prod-bastion.glowing.net:~/glow_ansible")
     (eval progn
           (put
            (quote defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote setting/defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote s/defn)
            (quote clojure-doc-string-elt)
            2)
           (define-clojure-indent
             (assert 1)
             (assoc 1)
             (ex-info 1)
             (execute-sql! 2)
             (expect 0)
             (match 1)
             (merge-with 1)
             (with-redefs-fn 1))))))
 '(writeroom-fullscreen-effect (quote maximized)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
