;;; packages.el --- Haskell Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq my-haskell-packages
      '(
        haskell-mode
        lsp-haskell

        ))

(defun my-haskell/init-haskell-mode ()
   (use-package haskell-mode
     :defer t
     )
   )
(defun my-haskell/init-lsp-haskell()
  (use-package lsp-haskell
    :defer t)
  (add-hook 'haskell-mode-hook #'lsp))

