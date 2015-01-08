;; initalize all ELPA packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa.org/packages/"))

(package-initialize)

;; Load use-package, used for loading packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(load "~/.emacs.d/settings.el")
