;;; packages.el --- org-latex layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <dwuggh@DESKTOP-VDES5FK-wsl>
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
;; added to `org-latex-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-latex/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-latex/pre-init-PACKAGE' and/or
;;   `org-latex/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst orgwithlatex-packages
  '(
    cdlatex
    org
    org-edit-latex
    )
)


(defun orgwithlatex/init-cdlatex ()
  (use-package cdlatex
    :defer t
    :init
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex)))

(defun orgwithlatex/init-org-edit-latex ()
  (use-package org-edit-latex
    :defer t
    :init
    (add-hook 'org-mode-hook 'org-edit-latex-mode)
    ))

(defun orgwithlatex/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(latex . t))))

;; (defun orgwithlatex/post-init-org ()
;;   (progn
;;     (setq org-format-latex-options (plist-put org-format-latex-options
;;                                               :scale 2.0)))
;;    )
;; (set-default 'preview-scale-function 1.2)
;;; packages.el ends here
