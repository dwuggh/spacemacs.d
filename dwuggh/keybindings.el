(define-key evil-insert-state-map (kbd "C-k") 'evil-previous-line)
(define-key evil-insert-state-map (kbd "C-j") 'evil-next-line)


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

(dolist (mode '(tex-mode latex-mode context-mode))
  (spacemacs/set-leader-keys-for-major-mode mode
    "v"   'dwuggh/TeX-view))

;; selective display
(defun dwuggh/selective-display ()
   "set selective display"
   (interactive)
   (set-selective-display (if selective-display nil 1)))

;; another keymap of nerd-commenter: comment-or-uncomment-lines
(define-key evil-normal-state-map (kbd ",/") 'spacemacs/comment-or-uncomment-lines)
(define-key evil-visual-state-map (kbd ",/") 'spacemacs/comment-or-uncomment-lines)
(define-key evil-normal-state-map (kbd ",.") 'spacemacs/comment-or-uncomment-paragraphs)
(define-key evil-visual-state-map (kbd ",.") 'spacemacs/comment-or-uncomment-paragraphs)

;; another key as esc to normal state
;; https://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-unordered-key-sequence t)


;; as another alternative

;; (evil-define-command dwuggh/maybe-exit ()
;;   :repeat change
;;   (interactive)
;;   (let ((modified (buffer-modified-p)))
;;     (insert "j")
;;     (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
;;                            nil 0.5)))
;;       (cond
;;        ((null evt) (message ""))
;;        ((and (integerp evt) (char-equal evt ?k))
;;         (delete-char -1)
;;         (set-buffer-modified-p modified)
;;         (push 'escape unread-command-events))
;;        (t (setq unread-command-events (append unread-command-events
;;                                               (list evt))))))))

;; (define-key evil-insert-state-map "j" #'dwuggh/maybe-exit)



;; vimish-fold, mainly copy from evil-vimishfold:
;; https://github.com/alexmurray/evil-vimish-fold
;; but change the keybinding

;; (defvar evil-vimish-fold-mode-lighter " zf"
;;   "Mode lighter for evil-vimish-fold Mode.")

;; (evil-define-operator evil-vimish-fold/create (beg end)
;;   "Create a fold from the current region.
;; See also `evil-delete-fold'."
;;   (when vimish-fold-mode
;;     (vimish-fold beg end)))

;; (evil-define-operator evil-vimish-fold/create-line (beg end)
;;   "Create a fold from the current region.
;; See also `evil-delete-fold'."
;;   :motion evil-line
;;   (interactive "<r>")
;;   (when vimish-fold-mode
;;     (vimish-fold beg end)))

;; (evil-define-command evil-vimish-fold/delete ()
;;   "Delete a fold under point.
;; See also `evil-create-fold'."
;;   (evil-fold-action evil-fold-list :delete))

;; (evil-define-command evil-vimish-fold/delete-all ()
;;   "Delete all folds."
;;   (when vimish-fold-mode
;;     (vimish-fold-delete-all)))

;; (evil-define-motion evil-vimish-fold/next-fold (count)
;;   "Go to the start of the next fold."
;;   :type inclusive
;;   (when vimish-fold-mode
;;     (unless (numberp count)
;;       (setq count 1))
;;     (dotimes (_ count nil)
;;       (vimish-fold-next-fold))))

;; (evil-define-motion evil-vimish-fold/previous-fold (count)
;;   "Go to the start of the previous fold."
;;   :type inclusive
;;   (when vimish-fold-mode
;;     (unless (numberp count)
;;       (setq count 1))
;;     (dotimes (_ count nil)
;;       (vimish-fold-previous-fold))))



;; ;;;###autoload
;; (define-minor-mode evil-vimish-fold-mode
;;   "Evil-vimish-fold-mode."
;;   :lighter evil-vimish-fold-mode-lighter
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (evil-define-key 'normal map "zj" 'evil-vimish-fold/next-fold)
;;             (evil-define-key 'motion map "zj" 'evil-vimish-fold/next-fold)
;;             (evil-define-key 'normal map "zk" 'evil-vimish-fold/previous-fold)
;;             (evil-define-key 'motion map "zk" 'evil-vimish-fold/previous-fold)
;;             (evil-define-key 'motion map "zd" 'evil-vimish-fold/delete)
;;             (evil-define-key 'normal map "zE" 'evil-vimish-fold/delete-all)
;;             (evil-define-key 'motion map "zc" 'evil-vimish-fold/create)
;;             (evil-define-key 'motion map "zC" 'evil-vimish-fold/create-line)
;;             map)
;;   (vimish-fold-mode)
;;   (if vimish-fold-mode
;;       (add-to-list 'evil-fold-list
;;                    `((vimish-fold-mode)
;;                      :delete     vimish-fold-delete
;;                      :open-all   vimish-fold-unfold-all
;;                      :close-all  vimish-fold-refold-all
;;                      :toggle     vimish-fold-toggle
;;                      :open       vimish-fold-unfold
;;                      :open-rec   nil
;;                      :close      vimish-fold-refold))
;;     (setq evil-fold-list (cl-remove-if
;;                           #'(lambda (e) (eq (caar e) 'vimish-fold-mode))
;;                           evil-fold-list))))


;; (define-key evil-normal-state-map (kbd "z a") #'vimish-fold-toggle)
;; (define-key evil-normal-state-map (kbd "z c") #'vimish-fold)
;; (define-key evil-normal-state-map (kbd "z o") #'vimish-fold-unfold)
;; (define-key evil-normal-state-map (kbd "z A") #'vimish-fold-toggle-all)
;; (define-key evil-normal-state-map (kbd "z O") #'vimish-fold-unfold-all)




