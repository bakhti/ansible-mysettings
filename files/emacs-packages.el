;; Emacs package system
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa.org/packages/") t)
(package-initialize)

(package-refresh-contents)

(defvar my/install-packages
  '(
    ;; package management
    use-package

    ;; themeing
    moe-theme

    ;; misc
    diminish guide-key

    ;; for auto-complete
    company

    ;; editing utilities
    expand-region ido-ubiquitous ido-vertical-mode flx-ido projectile
    ace-jump-mode ace-window multiple-cursors

    ;; ruby
    inf-ruby yari

    ;; markup language
    markdown-mode yaml-mode

    ;; git
    magit git-timemachine
    ))

;; org-mode is forced manually
(package-install 'org)

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (package-install pack)))
