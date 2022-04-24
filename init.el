;;; init.el --- configration
;;; Commentary:

;;; Code:
(setq load-prefer-newer t)

;;; misc
(setq delete-auto-save-files t)
(setq make-backup-files nil)
(setq-default custom-file null-device)

;;; ui
(load-theme 'tsdh-dark t)
(global-hl-line-mode)
(show-paren-mode 1)
(global-font-lock-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(setq inhibit-startup-message t)

(if window-system
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (set-scroll-bar-mode 'left)
      (cond
       ((eq system-type 'gnu/linux)
        (set-face-attribute 'default nil
                            :family "DejaVu Sans Mono"
                            :height 160)
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          '("M+ 2m" . "iso10646-1")
                          )))))

;;; key
(global-set-key (kbd "C-o") 'toggle-input-method)
(global-set-key (kbd "C-h") 'delete-backward-char)

(setq-default indent-tabs-mode nil)
(windmove-default-keybindings)

;;; bootstrap use-package
(require 'package)
(push '("melpa" . "http://melpa.org/packages/") package-archives)
(unless (require 'use-package nil 'noerror)
  (package-refresh-contents)
  (package-initialize)
  (package-install 'use-package)
  (require 'use-package))

;;; bundled packages
(use-package electric
  :bind
  (("RET" . electric-newline-and-maybe-indent)
   ("C-j" . newline)))

(use-package recentf
  :init
  (recentf-mode 1))

(use-package ido
  :custom
  (ido-mode 1)
  (ido-enable-flex-matching t)
  ;; don't search files from history
  (ido-auto-merge-work-directories-length -1))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package cua-base
  :init
  (cua-mode t)
  :custom
  (cua-enable-cua-keys nil))

(use-package whitespace
  :init
  (global-whitespace-mode)
  (set-face-background 'whitespace-trailing "Purple")
  :custom
  (whitespace-style '(face tab-mark trailing)))

(use-package linum
  :init
  (global-linum-mode))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(use-package prog-mode-hook
  :bind
  (:map prog-mode-map
    ("M-N" . next-error)
    ("M-P" . previous-error)))

(use-package verilog-mode
  :init
  (add-hook 'verilog-mode-hook #'verilog-format-on-save-mode)
  :custom
  (verilog-indent-level 2)
  (verilog-indent-level-module 2)
  (verilog-indent-level-declaration 2)
  (verilog-indent-level-behavioral 2)
  (verilog-auto-lineup nil)
  (verilog-auto-endcomments nil)
  (verilog-auto-newline nil))

(use-package python
  :init
  (add-hook 'python-mode-hook #'eglot-ensure))

(use-package cc-mode
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq-local ff-other-file-alist
                    '(("\\.cc$"  (".hh" ".h"))
                      ("\\.hh$"  (".cc" ".C"))
                      ("\\.c$"   (".h"))
                      ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm" ".cu"))
                      ("\\.C$"   (".H"  ".hh" ".h"))
                      ("\\.H$"   (".C"  ".CC"))
                      ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
                      ("\\.HH$"  (".CC"))
                      ("\\.cxx$" (".hh" ".h"))
                      ("\\.cpp$" (".hpp" ".hh" ".h"))
                      ("\\.hpp$" (".cpp" ".c"))
                      ("\\.cu$"  (".h"))))
              (c-set-style "stroustrup")
              (c-toggle-hungry-state 1)
              (ggtags-mode 1)))
  :bind
  (:map c-mode-base-map
        ("M-t" . ff-find-other-file)))

;; temporary fix. need investigation.
(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

;;; 3rd party packages
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-dabbrev-downcase nil)
  :bind
  (:map company-active-map
        ([tab] . company-complete-common-or-cycle)))

(use-package company-posframe
  :ensure t
  :init
  (company-posframe-mode 1))

(use-package migemo
  :ensure t
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")

  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix))

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-auto-save-history nil)
  :init
  (global-undo-tree-mode))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :custom
  (flycheck-checker-error-threshold nil)
  :bind
  (:map flycheck-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error)
        ("C-c f" . flycheck-buffer))
  :config
  (flycheck-add-next-checker 'python-pylint 'python-mypy))

(use-package reformatter
  :ensure t
  :demand t
  :config
  (reformatter-define verilog-format
    :program "verible-verilog-format"
    :args '("--try_wrap_long_lines" "-")))

(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook
            (lambda()
              (eglot-ensure)
              (flycheck-mode -1)))
  :custom
  (rust-format-on-save t))

(use-package clang-format
  :ensure t
  :init
  (add-hook 'before-save-hook
            (lambda()
              (when (eq major-mode 'c++-mode)
                (when (locate-dominating-file "." ".clang-format")
                  (clang-format-buffer))))))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))

(use-package eldoc-box
  :ensure t)

(use-package counsel
  :ensure t
  :bind
  (("C-x C-b" . counsel-switch-buffer)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-x M-f" . counsel-recentf)
   ("C-x C-d" . counsel-git)))

(use-package ggtags
  :ensure t)
