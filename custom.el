(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "Okular")
     (output-pdf "PDF Tools")
     (output-html "xdg-open")))
 '(cdlatex-auto-help-delay 0)
 '(custom-safe-themes
   '("8e9b37a4eb68542bb3f6d76afd303fb45196f0c536863b61b9b76a64bdc421ef" default))
 '(evil-want-Y-yank-to-eol nil)
 '(exec-path
   '("/home/dwuggh/.local/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/bin" "/usr/lib/jvm/default/bin" "/usr/bin/site_perl" "/usr/bin/vendor_perl" "/usr/bin/core_perl" "/var/lib/snapd/snap/bin" "/usr/lib/emacs/26.3/x86_64-pc-linux-gnu"))
 '(lsp-haskell-process-args-hie nil)
 '(lsp-haskell-process-path-hie "ghcide")
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t nil)
     ("" "ctex" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" nil nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "amsmath" t nil)
     ("" "textcomp" t nil)
     ("" "amssymb" t nil)
     ("" "capt-of" nil nil)
     ("" "hyperref" nil nil)))
 '(package-check-signature 'allow-unsigned)
 '(package-selected-packages
   '(csv-mode smart-input-source auctex-latexmk goldendict utop tuareg caml seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rbenv rake ocp-indent ob-elixir mvn minitest meghanada maven-test-mode lsp-java groovy-mode groovy-imports pcache gradle-mode git-gutter-fringe+ fringe-helper git-gutter+ flycheck-ocaml merlin flycheck-mix flycheck-credo emojify emoji-cheat-sheet-plus dune company-emoji chruby bundler inf-ruby browse-at-remote alchemist elixir-mode yaml-mode zenburn-theme zen-and-art-theme youdao-dictionary yasnippet-snippets yapfify ws-butler writeroom-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toxi-theme toml-mode toc-org tide tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stickyfunc-enhance srefactor spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme racer pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme prettier-js posframe popwin planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pdf-tools pcre2el password-generator paradox overseer orgit organic-green-theme org-projectile org-present org-pomodoro org-mime org-journal org-edit-latex org-download org-cliplink org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme nodejs-repl noctilux-theme naquadah-theme nameless mwim mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-svn magit-section magit-gitflow magic-latex-buffer madhat2r-theme macrostep lush-theme lsp-ui lsp-python-ms lsp-haskell lorem-ipsum livid-mode live-py-mode link-hint light-soap-theme kaolin-themes json-navigator json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme intero inkpot-theme indent-guide importmagic impatient-mode hybrid-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate google-c-style golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme fuzzy font-lock+ flyspell-correct-helm flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-haskell flycheck-elsa flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme emr emmet-mode elisp-slime-nav editorconfig dumb-jump dracula-theme dotenv-mode doom-themes django-theme disaster diminish devdocs define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dap-mode dante dakrone-theme dactyl-mode cython-mode cyberpunk-theme cpp-auto-include company-ycmd company-web company-rtags company-reftex company-ghci company-ghc company-cabal company-c-headers company-auctex company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmm-mode clues-theme clean-aindent-mode chocolate-theme cherry-blossom-theme centered-cursor-mode cdlatex ccls cargo busybee-theme bubbleberry-theme blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile attrap apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ace-link ace-jump-helm-line ac-ispell))
 '(pyim-punctuation-dict
   '(("'" "‘" "’")
     ("\"" "“" "”")
     ("_" "——")
     ("^" "…")
     ("]" "】")
     ("[" "【")
     ("@" "◎")
     ("?" "？")
     (">" "》")
     ("=" "＝")
     ("<" "《")
     (";" "；")
     (":" "：")
     ("/" "、")
     ("." "。")
     ("-" "－")
     ("," "，")
     ("+" "＋")
     ("*" "×")
     (")" "）")
     ("(" "（")
     ("&" "※")
     ("%" "％")
     ("#" "＃")
     ("!" "！")
     ("`" "・")
     ("~" "～")
     ("}" "』")
     ("|" "÷")
     ("{" "『"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
