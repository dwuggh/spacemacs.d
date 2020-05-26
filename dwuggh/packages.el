;;; packages.el --- dwuggh layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: DESKTOP-VDES5FK <dwuggh@DESKTOP-VDES5FK-wsl>
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
;; added to `dwuggh-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `dwuggh/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `dwuggh/pre-init-PACKAGE' and/or
;;   `dwuggh/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


(defconst dwuggh-packages
  '(
    mwim
    unfill

    ;; configuration for company
    company
    ;; support for chinese language

    ;; enhance folding
    ;; vimish-fold
    
    )
  )

(defun dwuggh/init-mwim ()
  (use-package mwim
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
      (global-set-key (kbd "C-q") 'mwim-end-of-code-or-line)
      )))

(defun dwuggh/init-unfill ()
  (use-package unfill
    :defer t
    :commands (unfill-region unfill-paragraph unfill-toggle)
    :init
    (global-set-key [remap fill-paragraph] #'unfill-toggle)))



(defun dwuggh/post-init-company ()
  (progn
    (setq company-idle-delay 0.08)
    (setq company-minimum-prefix-length 2)
    ))

;; (defun dwuggh/init-vimish-fold ()
;;    (use-package vimish-fold
;;      :ensure
;;      :config
;;      (vimish-fold-global-mode 1)
;;      :after evil)
;;    )
;;; packages.el ends here
