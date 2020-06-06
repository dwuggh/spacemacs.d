(defun my-setup-indent (n)
  ;; java/c/c++
  (setq c-basic-offset (* n 2))
  (setq python-indent-offset (* n 2))
  ;; web development
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq typescript-indent-level n)
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  )
(my-setup-indent 2)


;; toggle window layout with treemacs buffer visible
(defun dwuggh/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows, except treemacs"
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible
       (let ((buf (current-buffer)))
         (delete-window (treemacs-get-local-window))
         (spacemacs/window-layout-toggle)
         (treemacs-select-window)
         (switch-to-buffer buf)
         (select-window (get-buffer-window buf)))
     )
    ('exists
     (spacemacs/window-layout-toggle))
    ('none
     (spacemacs/window-layout-toggle))))



(defadvice evil-ex-search-next (after dwuggh/advice-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-ex-search-previous (after dwuggh/advice-evil-ex-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))
;; (add-to-list 'company-backends '(company-dabbrev))


;; TODO only files can trigger
;; TODO paste when already exist
;; TODO interactively set new name
(defun dwuggh/paste-img-local-path (&optional register fname)
  "Paste reg as local img"
  (interactive "*P\nsnew name: ")
  ;; (message fname)
  ;; (message (type-of fname))
  (let* ((text (if register
                  (evil-get-register register)
                (current-kill 0)))
        (img-dir (concat default-directory "img/"))
        (img-name (car (reverse (split-string text "/"))))
        (img-rel-path (concat "./img/" img-name)))
    ;; (message img-rel-path)
    ;; (message img-dir)
    ;; (message img-name)
    (if (string-prefix-p "file:\/\/" text)
        (set 'text (substring text 7 nil)))
    (if (not (member "img" (directory-files default-directory)))
        (make-directory img-dir))
    ;; (if 'fname
    ;;     (set img-name fname))
    ;; (message text)
    (copy-file text img-rel-path)
    (insert img-rel-path)
    )
  )

;; file:///home/dwuggh/.vim/vimrc

;; funcs.el ends here
