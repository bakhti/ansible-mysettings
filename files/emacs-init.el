;; initalize all ELPA packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa.org/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

;; Load use-package, used for loading packages
(require 'use-package)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(load "~/.emacs.d/settings.el")
