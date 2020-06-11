;;; config.el --- Latex Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

;; commit 5fcd84d
;; AUCTeX is weird: It reports major-mode as latex-mode (since TeX-latex-mode is
;; applied as an :override advice on the basic built-in latex-mode), but its mode
;; hook is LaTeX-mode-hook, not latex-mode-hook (which is only run by the built-in
;; latex-mode). Since bind-map uses the value of major-mode, we must pass
;; latex-mode to spacemacs|define-jump-handlers. But then
;; spacemacs//init-jump-handlers-latex-mode gets added to latex-mode-hook, which
;; never gets run. So we must manualy add it to LaTeX-mode-hook.

;; [latex] Use dumb-jump as primary jump handler

;; Otherwise, the default is used, which prioritizes evil-goto-definition over
;; dumb-jump-go. Dumb Jump tends to Just Work, while evil-goto-definition doesn't
;; handle LaTeX very well, at least not without a TAGS table.

;; (spacemacs|define-jump-handlers latex-mode)

;; Even though AUCTeX uses TeX-latex-mode rather than latex-mode, major-mode
;; will still be bound to 'latex-mode (since AUCT
;; latex-mode with TeX-latex-mode), so the keymap's name should use the
;; lowercase form, since bind-map uses the value of major-mode...
(spacemacs|define-jump-handlers latex-mode dumb-jump-go)
;; ...but AUCTeX runs LaTeX-mode-hook rather than latex-mode-hook, so:
(add-hook 'LaTeX-mode-hook #'spacemacs//init-jump-handlers-latex-mode)

(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX")
  "The default command to use with `SPC m b'")

(defvar latex-enable-auto-fill t
  "Whether to use auto-fill-mode or not in tex files.")

(defvar latex-enable-folding nil
  "Whether to use `TeX-fold-mode' or not in tex/latex buffers.")

(defvar latex-enable-magic nil
  "Whether to enable \"magic\" symbols in the buffer.")

(defvar latex-nofill-env '("equation"
                           "equation*"
                           "align"
                           "align*"
                           "tabular"
                           "tikzpicture")
  "List of environment names in which `auto-fill-mode' will be inhibited.")
