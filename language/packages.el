;;; packages.el --- language layer packages file for Spacemacs.
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
;; added to `language-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `language/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `language/pre-init-PACKAGE' and/or
;;   `language/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst language-packages
  '(
    (goldendict
     :location (recipe :fetcher local))

    youdao-dictionary

    ;; pyim
    rime
    smart-input-source
    ;; (pyim-greatdict :location (recipe :fetcher github :repo "tumashu/pyim-greatdict"))
    ))

(defun language/init-goldendict ()
  (use-package goldendict
    :defer t
    :ensure t
    :init
    (spacemacs/set-leader-keys "ou" 'goldendict-dwim)
    )
  )

(defun language/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :init
    (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+))
  )

(defun language/init-rime ()
  (use-package rime
    :custom
    (default-input-method "rime")
    (setq-default rime-show-candidate 'posframe)
    (setq-default rime-user-data-dir "~/.config/fcitx/rime/")))

(defun language/init-smart-input-source ()
  (use-package smart-input-source
    :init
    (setq-default smart-input-source-english nil)
    (setq-default smart-input-source-other "rime")
    (setq-default smart-input-source-do-get (lambda() current-input-method))
    (setq-default smart-input-source-do-set (lambda(source) (set-input-method source)))

    ;; (require 'subr-x)
    ;; (setq-default smart-input-source-english "1")
    ;; (setq-default smart-input-source-other "2")
    ;; (setq-default smart-input-source-do-get
    ;;               (lambda() (string-trim (shell-command-to-string "fcitx-remote"))))
    ;; (setq-default smart-input-source-do-set
    ;;               (lambda(source)
    ;;                 (pcase source
    ;;                   ("1" (string-trim (shell-command-to-string "fcitx-remote -c")))
    ;;                   ("2" (string-trim (shell-command-to-string "fcitx-remote -o"))))))


    :config
    ;; enable the /cursor color/ mode
    (smart-input-source-global-cursor-color-mode nil)
    ;; enable the /respect/ mode
    (smart-input-source-global-respect-mode t)
    ;; enable the /follow context/ mode for all buffers
    (smart-input-source-global-follow-context-mode t)
    ;; enable the /inline english/ mode for all buffers
    (smart-input-source-global-inline-mode t)
    ))

;; (defun language/init-pyim ()
;;   (use-package pyim
;;     :config
;;     (setq default-input-method "pyim")
;;     (setq pyim-default-scheme 'quanpin)
;;     (setq greatdict-file "~/.dicts/pyim-greatdict.pyim")
;;     (when (file-exists-p greatdict-file)
;;       (pyim-extra-dicts-add-dict
;;        `(:name "Greatdict-elpa"
;;                :file ,greatdict-file
;;                :conding utf-8-unix
;;                :dict-type pinyin-dict
;;                :elpa t)))
;;     (pyim-isearch-mode 1)
;;     (setq pyim-fuzzy-pinyin-alist
;;           '(("en" "eng")
;;             ("in" "ing")))
;;     (setq pyim-page-length 5)))

;; (defun language/init-pyim-greatdict ()
;;     (use-package pyim-greatdict
;;       :init
;;       (pyim-greatdict-enable)))
;;; packages.el ends here
