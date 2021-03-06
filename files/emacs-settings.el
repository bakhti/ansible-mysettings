;;; settings.el --- Emacs configuration
;;; Commentary:

;;; Code:

(diminish 'isearch-mode (string 32 #xf06e))

(use-package company
  :ensure t
  :defer t
  :idle (global-company-mode)
  :config
  (progn
    (setq company-idle-delay 0.2
          ;; min prefix of 3 chars
          company-minimum-prefix-length 3
          company-dabbrev-downcase nil
	  company-show-numbers t))
  :diminish company-mode)

(use-package dired
  :defer t
  :bind (("C-x C-j" . dired-jump))
  :config
  (progn
    (use-package dired-x
      :init (setq-default dired-omit-files-p t))
    (put 'dired-find-alternate-file 'disabled nil)
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)
    (setq ls-lisp-dirs-first t
	  dired-recursive-copies 'always
	  dired-recursive-deletes 'always)
    (add-hook 'dired-mode-hook (lambda () (hl-line-mode)))))

(use-package guide-key
  :defer t
  :config
  (setq guide-key-mode t
	guide-key/guide-key-sequence
	(quote
	 ("C-x r" "C-x n" "C-x 4" "C-x v" "C-x 8" "C-c p" "C-c i" "\e\e\m" "\e\et" "\e\el"))
	guide-key/popup-window-position 'bottom
	guide-key/recursive-key-sequence-flag t)
  :diminish guide-key-mode)

(use-package eldoc
  :defer t
  :config (setq eldoc-idle-delay 0.2)
  :diminish "ed")

(use-package erc
  :defer t
  :config
  (progn
    (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
	  erc-track-exclude-types (append '("KICK" "324" "329" "332" "333" "353" "477") erc-hide-list)
	  erc-prompt-for-password nil)))

(use-package moe-theme
  :ensure t
  :defer t
  :init (load-theme 'moe-dark))

(use-package smart-mode-line
  :init
  (progn
    (sml/setup)
    (sml/apply-theme 'dark)))

(use-package ido
  :init (progn
	  (ido-mode)
	  (ido-everywhere))
  :config
  (setq ido-auto-merge-work-directories-length nil
	ido-case-fold nil
	ido-create-new-buffer 'always
	ido-enable-flex-matching t
	ido-max-prospects 10
	ido-use-faces nil))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode))

(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :init
  (add-hook 'ibuffer-mode-hook
	    #'(lambda ()
		(ibuffer-switch-to-saved-filter-groups "default")))
  :config
  (setq ibuffer-saved-filter-groups
	(quote
	 (("default"
	   ("Dired"
	    (mode . dired-mode))
	   ("Scripts"
	    (or
	     (mode . emacs-lisp-mode)
	     (mode . lisp-mode)
	     (mode . python-mode)
	     (mode . yaml-mode)
	     (mode . conf-mode)
	     (mode . ruby-mode)
	     (mode . sh-mode)
	     (mode . tex-mode)
	     (mode . latex-mode)
	     (mode . js-mode)
	     (mode . javascript-mode)
	     (mode . json-mode)
	     (mode . magit-mode)
	     (mode . jinja2-mode)
	     (name . "*magit")))
	   ("Documents"
	    (or
	     (mode . org-mode)
	     (mode . markdown-mode)
	     (name . "^\\*Calendar\\*$")
	     (name . "^\\*info\\*$")
	     (name . "^\\*Help\\*$")
	     (name . "^diary$"))))))))

(use-package multiple-cursors
  :pre-init
  (progn
    (define-prefix-command 'my/mc-map)
    (global-set-key (kbd "\e\em") 'my/mc-map)
    (define-key my/mc-map "l" 'mc/edit-lines)
    (define-key my/mc-map "e" 'mc/edit-ends-of-lines)
    (define-key my/mc-map "b" 'mc/edit-beginnings-of-lines)
    (define-key my/mc-map "n" 'mc/mark-next-like-this)
    (define-key my/mc-map "p" 'mc/mark-previous-like-this)
    (define-key my/mc-map "a" 'mc/mark-all-like-this)
    (define-key my/mc-map "c" 'mc/insert-numbers)
    (define-key my/mc-map "s" 'mc/sort-regions)
    (define-key my/mc-map "r" 'mc/reverse-regions)))

(use-package ace-jump-mode
  :bind (("C-s-SPC" . ace-jump-mode)))

(use-package ace-window
  :config
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (setq aw-scope 'frame)))

(use-package aggressive-indent
  :config
  (add-hook 'prog-mode-hook 'aggressive-indent-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package projectile
  :init (projectile-global-mode)
  :config
  (setq projectile-mode-line (quote (:eval (format " Πρ[%s]" (projectile-project-name))))))

(use-package magit
  :bind (("\e\eg" . magit-status))
  :init (add-hook 'magit-mode-hook  '(lambda () (hl-line-mode)))
  :config
  (progn
    (diminish 'magit-auto-revert-mode)
    (setenv "GIT_PAGER" "")
    (setq magit-diff-use-overlays nil
	  magit-use-overlays nil)
    (add-hook 'magit-log-edit-mode-hook
	      #'(lambda ()
		  (set-fill-column 72)))))

(use-package python
  :mode "\\.py\\'"
  :config
  (progn
    (setq python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))"
	  python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
	  python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))"
	  python-shell-interpreter "ipython"
	  python-shell-interpreter-args ""
	  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
	  python-shell-prompt-regexp "In \\[[0-9]+\\]: ")))

(use-package org
  :config
  (progn
    (setq org-babel-default-header-args
          (cons '(:noweb . "yes")
                (assq-delete-all :noweb org-babel-default-header-args))
          org-babel-default-header-args
          (cons '(:exports . "both")
                (assq-delete-all :exports org-babel-default-header-args))
	  org-babel-python-command "python3"
	  org-completion-use-ido t
	  org-confirm-babel-evaluate nil
	  org-directory "~/Documents/Org"
	  org-edit-src-content-indentation 0
	  org-export-babel-evaluate nil
	  org-imenu-depth 3
	  org-src-fontify-natively t
	  org-src-window-setup 'current-window
	  org-tags-column -120
	  org-todo-keywords
	  (quote
	   ((sequence "TODO" "STARTED(!)" "WAIT(!)" "|" "DONE(!)" "CANCELED(!)"))))
    (setq org-agenda-files
	  '("~/Documents/Org/brightgrove.org"
	    "~/Documents/Org/personal.org"))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sh . t)
       (ruby . t)
       (python . t)
       (emacs-lisp . t)))
    ;; ensure this variable is defined
    (unless (boundp 'org-babel-default-header-args:sh)
      (setq org-babel-default-header-args:sh '()))
    ;; add a default shebang header argument shell scripts
    (add-to-list 'org-babel-default-header-args:sh
                 '(:shebang . "#!/bin/bash"))
    ;; add a default shebang header argument for python
    (add-to-list 'org-babel-default-header-args:python
                 '(:shebang . "#!/usr/bin/env python"))
					;    (add-hook 'org-mode-hook 'turn-on-flyspell)

    ))

(use-package ruby-mode
  :mode "\\(\\.rb\\|Vagrantfile\\|Berksfile\\)\\'"
  :interpreter (("ruby" . ruby-mode))
  :config
  (progn
    (use-package yari)
    (defun my-ruby-mode-hook ()
      (require 'inf-ruby)
      (inf-ruby-keys))
    (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)))

(use-package ispell
  :disabled t
  :bind ((("C-c i c" . ispell-comments-and-strings)
	  ("C-c i d" . ispell-change-dictionary)
	  ("C-c i k" . ispell-kill-ispell)
	  ("C-c i m" . ispell-message)
	  ("C-c i r" . ispell-region)))
  :config
  (progn
    (setq-default ispell-program-name "aspell")
    (setq ispell-personal-dictionary "~/.flydict"
	  ispell-extra-args '("--sug-mode=ultra" "--ignore=3"))
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (progn
    (setq flycheck-completion-system 'ido)
    (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic))
  :diminish flychek-mode)

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package flyspell
  :disabled t
  :bind ((("C-c i b" . flyspell-buffer)
	  ("C-c i f" . flyspell-mode)))
  :diminish "✈")

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package json-mode
  :mode "\\.template\\'"
  :config (setq js-indent-level 2))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :config  (setq markdown-command "markdown_py"))

(use-package notmuch
  :load-path "~/.emacs.d/lisp"
  :config
  (progn
    (setq notmuch-search-oldest-first nil)
    (define-key notmuch-search-mode-map "D"
      (lambda ()
	(interactive)
	(if (member "deleted" (notmuch-search-get-tags))
	    (notmuch-search-tag '("-deleted"))
	  (notmuch-search-tag '("+deleted" "-unread")))
	(next-line)))
    (define-key notmuch-show-mode-map "D"
      (lambda ()
	(interactive)
	(if (member "deleted" (notmuch-show-get-tags))
	    (notmuch-show-tag '("-deleted"))
	  (notmuch-show-tag '("+deleted" "-unread")))))))

(use-package my-functions
  :load-path "~/.emacs.d/lisp")

(use-package my-key-bindings
  :load-path "~/.emacs.d/lisp")
