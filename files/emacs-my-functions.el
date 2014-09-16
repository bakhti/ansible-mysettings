;; http://sachachua.com/blog/2013/09/using-emacs-to-figure-out-where-i-need-to-improve-in-order-to-type-faster/
(defun my/timer-go ()
  "Quick keyboard timer."
  (interactive)
  (insert "GO\n")
  (run-with-timer 3 nil (lambda () (insert "\n")))  ; for warmup
  (run-with-timer 15 nil (lambda () ; 12 seconds + the 3-second warmup
                           (let ((col (- (point) (line-beginning-position))))
                             (insert (format " | %d | \n" col))))))

;; https://github.com/magnars/.emacs.d/
(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my/kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation"
  (interactive)
  (back-to-indentation)
  (kill-line))

;; kill region if active, otherwise kill backward word
(defun my/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun my/kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(defun my/open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun my/open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun my/open-line-and-indent ()
  (interactive)
  (newline-and-indent)
  (end-of-line 0))

(defun my/new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

(defun my/new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
                             (and (looking-back ">") (looking-at "<"))
                             (and (looking-back "\\[") (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(defun my/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun my/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

;; Ansible documentation
(defconst my-ansible-doc-buffer " *Ansible Doc*"
  "The Ansible Doc buffer.")

(defvar my-ansible-modules nil
  "List of all known Ansible modules.")

(defun my/get-ansible-modules ()
  "Get a list of all known Ansible modules."
  (unless my-ansible-modules
    (let ((lines (ignore-errors (process-lines "ansible-doc" "--list")))
          modules)
      (dolist (line lines)
        (push (car (split-string line (rx (one-or-more space)))) modules))
      (setq my-ansible-modules (sort modules 'string<))))
  my-ansible-modules)

(defun my/show-ansible-doc (module)
  "Show ansible doc for MODULE."
  (interactive
   (list (ido-completing-read "Ansible Module: "
                              (my-ansible-modules)
                              nil nil nil nil
                              (thing-at-point 'symbol 'no-properties))))
  (let ((buffer (get-buffer-create my-ansible-doc-buffer)))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process "ansible-doc" nil t t module))
      (goto-char (point-min)))
    (display-buffer buffer)))

;; Hidden mode line
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; Activate hidden-mode-line-mode
;; (hidden-mode-line-mode t)

;; Command to toggle the display of the mode-line as a header
(defvar-local header-line-format nil)
(defun my/mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format
            mode-line-format nil)
    (setq mode-line-format header-line-format
          header-line-format nil))
  (set-window-buffer nil (current-buffer)))

(provide 'my-functions)
;; Local Variables:
;; mode: emacs-lisp
;; mode: allout
;; outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
;;; my-functions.el ends here
