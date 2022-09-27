;; +AUTHOR: Rijesh Panicker
;; + Date: 17-May-2020
;; Manually install straight.el instead of the bootstrapping code by cloning and running the bootstrap.el file. This is silly really!!
;; My first few packages in order:
;; 1. use-package
;; 2. Ivy, counsel and swiper in lieu of helm-mode
;; 3. which-key mode
;; Configure the select.el settings to work with use-package

;; Load the bootstrap file directly from straight.el repository
;; Deliberately took out most of the bootstrapping code that brings in the bootstrap.el file frome the site. Feel like this should be done manually.

;; Turn off the irritating ringing bell in emacs
(setq ring-bell-function 'ignore)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (load "~/.emacs.d/straight/repos/straight.el/bootstrap.el" nil 'nomessage) 

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
;; Use-package will always use straight from here on in.

;; Use-package installation of Ivy, counsel and swiper

;; set higher gc threshold to see if it changes the responsiveness
(setq gc-cons-threshold 1600000)

(use-package swiper
  :straight (:host github :repo "abo-abo/swiper"
		   :branch "master")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-x C-f" . counsel-find-file)
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-c r" . counsel-rg)
  ("C-c f" . counsel-fzf))
)

(use-package magit
   :straight (:host github :repo "magit/magit"
			:branch "master"))

;; Abo-abos hydra package and related configuration
(use-package hydra)

(defhydra hydra-zoom (global-map "<f2>")
  "Press _i_ to zoom in."
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out"))

(use-package counsel)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package better-defaults)

(use-package material-theme
  :straight (:host github :repo "cpaulik/emacs-material-theme")
  :config
  (load-theme 'material t))

;; Settings for programming in general.
;; flycheck
(use-package flycheck
  :defer t
  :config
  (add-hook 'python-mode-hook 'flycheck-mode))

;; yassnippets and snippets library
(use-package yasnippet-snippets
  :straight (:host github :repo "AndreaCrotti/yasnippet-snippets"))

(use-package yasnippet
  :config
  (yas-global-mode 1))


(use-package company
  :init
  (global-company-mode 1))

;; lsp mode and lsp-ui mode.

;; lsp-mode and a few additional ones
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-l")
  :hook (
         (lsp-mode . lsp-enable-which-key-integration))
  :commands
  (lsp lsp-deferred))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

(use-package lsp-ivy :defer t :commands lsp-ivy-workspace-symbol)
(use-package sly)
(setq inferior-lisp-program "ros -Q run")
(use-package cider)
(use-package parinfer
  :hook (
         (lisp-mode . parinfer-mode)))
;;(load (expand-file-name "~/.roswell/helper.el"))
;; SETTING CUSTOM FILE FOR custom configurations using menu.
(setq custom-file "~/.emacs.d/custom.el")

(load custom-file)
