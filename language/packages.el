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

    pyim
    ;; (pyim-greatdict :location (recipe :fetcher github :repo "tumashu/pyim-greatdict"))
    )
  "The list of Lisp packages required by the language layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

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

(defun language/init-pyim ()
  (use-package pyim
    :config
    (setq default-input-method "pyim")
    (setq pyim-default-scheme 'quanpin)
    (setq greatdict-file "~/.dicts/pyim-greatdict.pyim")
    (when (file-exists-p greatdict-file)
      (pyim-extra-dicts-add-dict
       `(:name "Greatdict-elpa"
               :file ,greatdict-file
               :conding utf-8-unix
               :dict-type pinyin-dict
               :elpa t)))
    (pyim-isearch-mode 1)
    (setq pyim-fuzzy-pinyin-alist
          '(("en" "eng")
            ("in" "ing")))
    (setq pyim-page-length 5)))

;; (defun language/init-pyim-greatdict ()
;;     (use-package pyim-greatdict
;;       :init
;;       (pyim-greatdict-enable)))
;;; packages.el ends here
