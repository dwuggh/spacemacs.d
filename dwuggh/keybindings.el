(define-key evil-insert-state-map (kbd "C-k") 'evil-previous-line)


;; call occur with a sane default
(defun occur-dwim ()
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; 定义快捷键
(global-set-key (kbd "M-s o") 'occur-dwim)

;; 将 occur 的 buffer 中的光标移动方式修改为 HJKL
(evilified-state-evilify-map occur-mode-map
  :mode occur-mode)

;; another keymap of nerd-commenter: comment-or-uncomment-lines
(define-key evil-normal-state-map (kbd ",/") 'spacemacs/comment-or-uncomment-lines)
(define-key evil-visual-state-map (kbd ",/") 'spacemacs/comment-or-uncomment-lines)
(define-key evil-normal-state-map (kbd ",.") 'spacemacs/comment-or-uncomment-paragraphs)
(define-key evil-visual-state-map (kbd ",.") 'spacemacs/comment-or-uncomment-paragraphs)






