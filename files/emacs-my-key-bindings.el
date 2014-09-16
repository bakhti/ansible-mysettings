;(global-set-key (kbd "C-s-SPC") 'my/mode-line-in-header)

;; Killing text
(global-set-key (kbd "C-S-k") 'my/kill-and-retry-line)
(global-set-key (kbd "C-w") 'my/kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'my/kill-to-beginning-of-line)

;; Clever newlines
(global-set-key (kbd "C-o") 'my/open-line-and-indent)
(global-set-key (kbd "<C-return>") 'my/open-line-below)
(global-set-key (kbd "<C-S-return>") 'my/open-line-above)
(global-set-key (kbd "<M-return>") 'my/new-line-dwim)

;; Navigation bindings
(global-set-key [remap goto-line] 'my/goto-line-with-feedback)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

(define-prefix-command 'my/toggle-map)
;; The manual recommends C-c for user keys, but I like using C-x for global keys and using C-c for mode-specific keys.
(define-key ctl-x-map "t" 'my/toggle-map)
(define-key my/toggle-map "r" 'read-only-mode)
(define-key my/toggle-map "l" 'toggle-truncate-lines)
(define-key my/toggle-map "o" 'org-mode)
(define-key my/toggle-map "v" 'visual-line-mode)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'my/eval-and-replace)

(global-set-key (kbd "C-c s") 'my/swap-windows)

;;
(eval-after-load 'yaml-mode
  '(define-key yaml-mode-map (kbd "C-c h a") 'my/ansible-doc))

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(global-set-key (kbd "s-l") (λ (insert "\u03bb")))

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'my-key-bindings)
;; Local Variables:
;; mode: emacs-lisp
;; mode: allout
;; outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
;;; my-key-bindings.el ends here
