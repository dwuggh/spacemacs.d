;;; packages.el --- mylatex layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: dwuggh <dwuggh@GOLIATH>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `mylatex-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `mylatex/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `mylatex/pre-init-PACKAGE' and/or
;;   `mylatex/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst mylatex-packages
  '(
    auctex
    cdlatex

    ;; completion
    (company-auctex :requires company)
    (company-reftex :requires company)
    ;; lsp-latex
    yasnippet
    which-key
    (magic-latex-buffer :toggle latex-enable-magic)

    (reftex :location built-in)

    smartparens
    evil-matchit

    ;; tags
    helm-gtags
    ggtags

    ;; spell check
    flycheck
    flyspell
    typo

    ))

(defun mylatex/init-auctex ()
  (use-package auctex
    :defer t
    :init
    (progn
      (setq TeX-command-default latex-build-command
            TeX-auto-save t
            TeX-parse-self t
            TeX-syntactic-comment t
            ;; Synctex support
            TeX-source-correlate-start-server nil
            ;; Don't insert line-break at inline math
            LaTeX-fill-break-at-separators nil)
      (when latex-enable-auto-fill
        (add-hook 'LaTeX-mode-hook 'mylatex/auto-fill-mode))
      (when latex-enable-folding
        (add-hook 'LaTeX-mode-hook 'TeX-fold-mode))
      (add-hook 'LaTeX-mode-hook 'latex-math-mode)
      (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
      (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

    :config
    (progn
      ;; Key bindings for plain TeX
      (dolist (mode '(tex-mode latex-mode context-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "\\"  'TeX-insert-macro                            ;; C-c C-m
          "-"   'TeX-recenter-output-buffer                  ;; C-c C-l
          "%"   'TeX-comment-or-uncomment-paragraph          ;; C-c %
          ";"   'comment-or-uncomment-region                 ;; C-c ; or C-c :
          ;; TeX-command-run-all runs compile and open the viewer
          "a"   'TeX-command-run-all                         ;; C-c C-a
          "b"   'latex/build
          "k"   'TeX-kill-job                                ;; C-c C-k
          "l"   'TeX-recenter-output-buffer                  ;; C-c C-l
          "m"   'TeX-insert-macro                            ;; C-c C-m
          "n"   'TeX-next-error                              ;; C-c `
          "N"   'TeX-previous-error                          ;; M-g p
          "v"   'TeX-view                                    ;; C-c C-v
          ;; TeX-doc is a very slow function
          "hd"  'TeX-doc
          "xb"  'mylatex/font-bold
          "xc"  'mylatex/font-code
          "xe"  'mylatex/font-emphasis
          "xi"  'mylatex/font-italic
          "xr"  'mylatex/font-clear
          "xo"  'mylatex/font-oblique
          "xfc" 'mylatex/font-small-caps
          "xff" 'mylatex/font-sans-serif
          "xfr" 'mylatex/font-serif)
        (when dotspacemacs-major-mode-emacs-leader-key
          (spacemacs/set-leader-keys-for-major-mode mode
            dotspacemacs-major-mode-emacs-leader-key 'TeX-command-master))
        (when dotspacemacs-major-mode-leader-key
          (spacemacs/set-leader-keys-for-major-mode mode
            dotspacemacs-major-mode-leader-key 'TeX-command-master))
        (when latex-enable-folding
          (spacemacs/set-leader-keys-for-major-mode mode
            ;; the following commands are mostly not autoloaded, but that's fine
            ;; because `TeX-fold-mode' is added to `LaTeX-mode-hook'
            "z=" 'TeX-fold-math
            "zb" 'TeX-fold-buffer
            "zB" 'TeX-fold-clearout-buffer
            "ze" 'TeX-fold-env
            "zI" 'TeX-fold-clearout-item
            "zm" 'TeX-fold-macro
            "zp" 'TeX-fold-paragraph
            "zP" 'TeX-fold-clearout-paragraph
            "zr" 'TeX-fold-region
            "zR" 'TeX-fold-clearout-region
            "zz" 'TeX-fold-dwim))
        (spacemacs/declare-prefix-for-mode mode "mh" "help")
        (spacemacs/declare-prefix-for-mode mode "mx" "text/fonts")
        (spacemacs/declare-prefix-for-mode mode "mz" "fold"))

      ;; Key bindings specific to LaTeX
      (spacemacs/set-leader-keys-for-major-mode 'latex-mode
        "*"   'LaTeX-mark-section      ;; C-c *
        "."   'LaTeX-mark-environment  ;; C-c .
        "c"   'LaTeX-close-environment ;; C-c ]
        "e"   'LaTeX-environment       ;; C-c C-e
        "ii"   'LaTeX-insert-item       ;; C-c C-j
        "s"   'LaTeX-section           ;; C-c C-s
        "fe"  'LaTeX-fill-environment  ;; C-c C-q C-e
        "fp"  'LaTeX-fill-paragraph    ;; C-c C-q C-p
        "fr"  'LaTeX-fill-region       ;; C-c C-q C-r
        "fs"  'LaTeX-fill-section      ;; C-c C-q C-s
        "pb"  'preview-buffer
        "pc"  'preview-clearout
        "pd"  'preview-document
        "pe"  'preview-environment
        "pf"  'preview-cache-preamble
        "pp"  'preview-at-point
        "pr"  'preview-region
        "ps"  'preview-section
        "xB"  'mylatex/font-medium
        "xr"  'mylatex/font-clear
        "xfa" 'mylatex/font-calligraphic
        "xfn" 'mylatex/font-normal
        "xfu" 'mylatex/font-upright)
      (spacemacs/declare-prefix-for-mode 'latex-mode "mi" "insert")
      (spacemacs/declare-prefix-for-mode 'latex-mode "mp" "preview")
      (spacemacs/declare-prefix-for-mode 'latex-mode "mf" "fill"))
    )))

(defun mylatex/init-company-auctex ()
  (use-package company-auctex
    :defer t
    :init (spacemacs|add-company-backends
            :backends
            (company-auctex-macros
             company-auctex-symbols
             company-auctex-environments)
            :modes LaTeX-mode)))

(defun mylatex/init-company-reftex ()
  (use-package company-reftex
    :defer t
    :init (spacemacs|add-company-backends
            :backends
            company-reftex-labels
            company-reftex-citations
            :modes LaTeX-mode)))

(defun mylatex/init-lsp-latex ()
  (use-package lsp-latex
    :defer t
    ;; (add-hook 'latex-mode-hook 'lsp)
    ))

(defun mylatex/post-init-lsp-latex ()
  (add-hook 'tex-mode-hook 'lsp)
  (add-hook 'LaTeX-mode-hook 'lsp)
  )

(defun mylatex/init-cdlatex ()
  (use-package cdlatex
    :defer t
    ;; :init
    ;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
    ;; (add-hook 'LaTeX-mode-hook 'cdlatex-mode)
    ;; (add-hook 'tex-mode-hook 'cdlatex-mode)
    ))

(defun mylatex/post-init-cdlatex ()
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'LaTeX-mode-hook 'cdlatex-mode)
  (add-hook 'tex-mode-hook 'cdlatex-mode)
  )

(defun mylatex/pre-init-auctex-latexmk ()
  (spacemacs|use-package-add-hook tex
    :post-config
    (auctex-latexmk-setup)))

(defun mylatex/init-auctex-latexmk ()
  (use-package auctex-latexmk
    :defer t
    :init (setq auctex-latexmk-inherit-TeX-PDF-mode t)))

(defun mylatex/init-company-reftex ()
  (use-package company-reftex
    :defer t
    :init (spacemacs|add-company-backends
            :backends
            company-reftex-labels
            company-reftex-citations
            :modes LaTeX-mode)))


(defun mylatex/post-init-evil-matchit ()
  (add-hook 'LaTeX-mode-hook 'evil-matchit-mode))

(defun mylatex/post-init-flycheck ()
  (spacemacs/enable-flycheck 'LaTeX-mode))

(defun mylatex/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'LaTeX-mode-hook))

(defun mylatex/init-reftex ()
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t)
  (spacemacs/declare-prefix-for-mode 'latex-mode "mr" "reftex")
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "rc"    'reftex-citation
    "rg"    'reftex-grep-document
    "ri"    'reftex-index-selection-or-word
    "rI"    'reftex-display-index
    "r TAB" 'reftex-index
    "rl"    'reftex-label
    "rp"    'reftex-index-phrase-selection-or-word
    "rP"    'reftex-index-visit-phrases-buffer
    "rr"    'reftex-reference
    "rs"    'reftex-search-document
    "rt"    'reftex-toc
    "rT"    'reftex-toc-recenter
    "rv"    'reftex-view-crossref))

(defun mylatex/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'latex-mode))

(defun mylatex/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'latex-mode))

(defun mylatex/post-init-ggtags ()
  (add-hook 'latex-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun mylatex/post-init-smartparens ()
  (add-hook 'LaTeX-mode-hook 'smartparens-mode))

(defun mylatex/post-init-typo ()
  ;; Typo mode isn't useful for LaTeX.
  (defun spacemacs//disable-typo-mode ()
    (typo-mode -1))
  (add-hook 'LaTeX-mode-hook 'spacemacs//disable-typo-mode))

(defun mylatex/post-init-yasnippet ()
  (add-hook 'LaTeX-mode-hook 'spacemacs/load-yasnippet))

(defun mylatex/post-init-which-key ()
  (push '((nil . "\\`mylatex/font-\\(.+\\)\\'") . (nil . "\\1"))
        which-key-replacement-alist))

(defun mylatex/init-magic-latex-buffer ()
  (use-package magic-latex-buffer
    :defer t
    :init
    (progn
      (add-hook 'TeX-update-style-hook 'magic-latex-buffer)
      (setq magic-latex-enable-block-highlight t
            magic-latex-enable-suscript t
            magic-latex-enable-pretty-symbols t
            magic-latex-enable-block-align nil
            magic-latex-enable-inline-image nil))))

;;; packages.el ends here
