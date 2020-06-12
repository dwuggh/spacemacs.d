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


(defconst dwuggh/image-extensions-default '("png" "jpeg" "jpg" "svg" "gif" "tiff" "bmp" "psd" "raw" "webp"))

(defun dwuggh/file-path-p (path)
  "return nil if `path' is neither a (start with `~/' or `/' in unix)
file path nor a string start with `file://'
return the correspond path otherwise"
  (if (string-prefix-p "file:\/\/" path)
      (set 'path (substring path 7 nil)))
  (if (file-name-absolute-p path)
      path)
  )


(defun dwuggh/file-is-image-p  (file)
  "return nil if `file' is not an image, return `file' otherwise.
NOTE: this function DO NOT check whether `file' is a available file-path.
 Consider using `dwuggh/file-path-p' first."
  (let ((extension (downcase (file-name-extension file))))
    (if (member extension dwuggh/image-extensions-default)
        file
      nil))
  )


;; TODO add name parsing
;; TODO interactively set new name
(defun dwuggh/image-copy-to-local (img &optional name)
  "copy `img' to `./img/'
`./' represents `default-directory'.
return `./img/name'"
  (when (and (set 'img (dwuggh/file-path-p img)) (dwuggh/file-is-image-p img))
    (message img)
    (if (not (member "img" (directory-files default-directory)))
        (make-directory (concat default-directory "img/")))
    (copy-file img "./img/")
    (concat "./img/" (file-name-nondirectory img))
    )
  )


;; better image pasting
;; TODO check out org-paste
(defun dwuggh/image-yank (&optional register)
  "copy image to `./img/' when pasting a image path"
  (interactive "*P")
  (let ((path (if register
                  (evil-get-register register)
                (current-kill 0))))
    (set 'path (dwuggh/image-copy-to-local path))
    (cond
     ((equal major-mode 'org-mode)
      ;; (set 'path (dwuggh/image-copy-to-local path))
      (insert (concat "[[" path "]]")))
     ((equal major-mode 'latex-mode)
      (insert (concat "\\includegraphics\[width=\\linewidth\]{" path "}")))
     (t
      (insert path))
     )))
;; (evil-global-set-key 'normal "p" 'dwuggh/image-yank)


;; TODO bind key or advice in org-mode/markdown/tex?
(defun dwuggh/image-selected-copy-to-local (beg end)
  "move image from `beg' to `end' to `./img/'"
  (interactive "r")
  (let ((text (filter-buffer-substring beg end)))
    (set 'text (dwuggh/image-copy-to-local text))
    (if text
        (progn
          (evil-delete beg end)
          (insert text)
          )
      (message "cannot convert"))))

;; (defadvice spacemacs/evil-mc-paste-after (around dwuggh/advice-evil-mc-paste activate)
;;   (if (equal major-mode 'org-mode)
;;       (progn
;;         (insert "[[]]")
;;         (backward-char 2)
;;         ())))

;; funcs.el ends here
