;;; packages.el --- EAF layer packages file for Spacemacs.
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
;; added to `EAF-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `EAF/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `EAF/pre-init-PACKAGE' and/or
;;   `EAF/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst EAF-packages
  '(
    (emacs-application-framework
     :location (recipe
                :fetcher github
                :repo "manateelazycat/emacs-application-framework")))
  )


;; (defun EAF/init-emacs-application-framework ()
;;   ""
;;   (use-package
;;     :custom
;;     (eaf-find-alternate-file-in-dired t)
;;     :config
;;     (eaf-bind-key scroll_up "C-k" eaf-pdf-viewer-keybinding)
;;     (eaf-bind-key scroll_down "C-j" eaf-pdf-viewer-keybinding)
;;     (eaf-bind-key take_photo "p" eaf-camera-keybinding))
;;   )
;;; packages.el ends here
