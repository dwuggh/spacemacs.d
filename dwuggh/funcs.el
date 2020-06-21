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

(advice-add 'evil-org-open-below :override
            (lambda (count)
              (interactive "P")
              (evil-next-line)
              (evil-org-open-above count)))


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

(advice-add 'gud-common-init :override
            (lambda (command-line massage-args marker-filter
				     &optional find-file)
  (let* ((words (split-string-and-unquote command-line))
	 (program (car words))
	 (dir default-directory)
	 ;; Extract the file name from WORDS
	 ;; and put t in its place.
	 ;; Later on we will put the modified file name arg back there.
	 (file-word (let ((w (cdr words)))
		      (while (and w (= ?- (aref (car w) 0)))
			(setq w (cdr w)))
		      (and w
			   (prog1 (car w)
			     (setcar w t)))))
	 (file-subst
	  (and file-word (substitute-in-file-name file-word)))
	 (args (cdr words))
	 ;; If a directory was specified, expand the file name.
	 ;; Otherwise, don't expand it, so GDB can use the PATH.
	 ;; A file name without directory is literally valid
	 ;; only if the file exists in ., and in that case,
	 ;; omitting the expansion here has no visible effect.
	 (file (and file-word
		    (if (file-name-directory file-subst)
			(expand-file-name file-subst)
		      file-subst)))
	 (filepart (and file-word (concat "-" (file-name-nondirectory file))))
	 (existing-buffer (get-buffer (concat "*gud" filepart "*"))))
    (select-window
     (display-buffer
      (get-buffer-create (concat "*gud" filepart "*"))
      '((display-buffer-reuse-window
        display-buffer-in-previous-window
        display-buffer-same-window display-buffer-pop-up-window))))
    (when (and existing-buffer (get-buffer-process existing-buffer))
      (error "This program is already being debugged"))
    ;; Set the dir, in case the buffer already existed with a different dir.
    (setq default-directory dir)
    ;; Set default-directory to the file's directory.
    (and file-word
	 gud-chdir-before-run
	 ;; Don't set default-directory if no directory was specified.
	 ;; In that case, either the file is found in the current directory,
	 ;; in which case this setq is a no-op,
	 ;; or it is found by searching PATH,
	 ;; in which case we don't know what directory it was found in.
	 (file-name-directory file)
	 (setq default-directory (file-name-directory file)))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    ;; Put the substituted and expanded file name back in its place.
    (let ((w args))
      (while (and w (not (eq (car w) t)))
	(setq w (cdr w)))
      ;; Tramp has already been loaded if we are here.
      (if w (setcar w (setq file (file-local-name file)))))
    (apply 'make-comint (concat "gud" filepart) program nil
	   (if massage-args (funcall massage-args file args) args))
    ;; Since comint clobbered the mode, we don't set it until now.
    (gud-mode)
    (set (make-local-variable 'gud-target-name)
	 (and file-word (file-name-nondirectory file))))
  (set (make-local-variable 'gud-marker-filter) marker-filter)
  (if find-file (set (make-local-variable 'gud-find-file) find-file))
  (setq gud-last-last-frame nil)
  (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
  (gud-set-buffer)))
;; funcs.el ends here
