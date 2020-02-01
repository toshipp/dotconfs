(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(setq load-prefer-newer t)

(tool-bar-mode -1)
(menu-bar-mode -1)

;;スタートアップを消す
(setq inhibit-startup-message t)

;; package
(require 'package)
(push '("melpa" . "https://melpa.org/packages/")
      package-archives)
(package-initialize)

(require 'cl-lib)
(defvar installing-package-list
  '(
    eglot
    projectile
    helm
    helm-projectile
    helm-gtags
    company
    flycheck
    flycheck-rust
    quickrun
    undo-tree
    go-mode
    company-go
    js2-mode
    json-mode
    markdown-mode
    web-mode
    yaml-mode
    rust-mode
    toml-mode
    nginx-mode
    lua-mode
    migemo
    ))

;; auto install
(let ((not-installed (cl-loop for x in installing-package-list
                              when (not (package-installed-p x))
                              collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;; input method
(global-set-key (kbd "C-o") 'toggle-input-method)

;; mozc
(when (require 'mozc nil t)
  (setq default-input-method "japanese-mozc"))

;;ctrl-hをdelete
(global-set-key (kbd "C-h") 'delete-backward-char)

;; swap RET and C-j when electric-indent-mode is enabled.
(with-eval-after-load "electric"
  (when electric-indent-mode
     (global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
     (global-set-key (kbd "C-j") 'newline)))

;; recentf
(require 'recentf)

;; ido-mode
(ido-mode 1)

;; helm
(when (require 'helm-config nil t)
  (require 'helm)
  (setq helm-split-window-in-side-p t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x M-f") 'helm-recentf)
  (global-set-key (kbd "M-s o") 'helm-occur)
  (global-set-key (kbd "M-s i") 'helm-imenu)
  )

;; helm-projectile
(when (require 'helm-projectile nil t)
  (global-set-key (kbd "C-x C-d") 'helm-projectile))

(when (require 'company nil t)
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  )

;; migemo
(when (require 'migemo nil t)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix))

;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; helm-gtags
(with-eval-after-load "helm-gtags"
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack))

;; uniquifly
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;矩形編集モード
(cua-mode t)

;;カーソル位置の色付け
(global-hl-line-mode)

;; no tabs
(setq-default indent-tabs-mode nil)

;;テーマの設定
(load-theme 'tsdh-dark t)

;;フレームの設定
(if window-system
    (progn
      ;;スクロールバーは左
      (set-scroll-bar-mode 'left)
      (cond
       ((eq system-type 'gnu/linux)
        ;; Xのクリップボートをつかう
        (setq x-select-enable-clipboard t)
        (set-face-attribute 'default nil
                            :family "DejaVu Sans Mono"
                            :height 105)
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          '("M+ 2m" . "iso10646-1")
                          )))))

;;対応する括弧を光らせる
(show-paren-mode 1)
;;font lockを有効化
(global-font-lock-mode 1)
;;行と列を表示
(line-number-mode 1)
(column-number-mode 1)

;;オートセーブファイルを終了時に消す
(setq delete-auto-save-files t)
;;バックアップファイルを作らない
(setq make-backup-files nil)

;;shift + 矢印でウィンドウを移動
(windmove-default-keybindings)

;; whitespace
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(face tab-mark trailing))
(set-face-background 'whitespace-trailing "Purple")

;; linum
(require 'linum)
(global-linum-mode)

(require 'quickrun)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
(define-key flycheck-mode-map (kbd "C-c f") 'flycheck-buffer)
(flycheck-add-next-checker 'python-pylint 'python-mypy)

;; flymake
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

;; eglot
(require 'eglot)
(setf (alist-get 'go-mode eglot-server-programs)
      '("bingo"))

;; prog-mode common setup
(add-hook 'prog-mode-hook
          (lambda ()
            ;; simple
            (define-key prog-mode-map (kbd "M-N") 'next-error)
            (define-key prog-mode-map (kbd "M-P") 'previous-error)

            ;; tag find
            (helm-gtags-mode 1)
            ))

;; rust-mode
(add-hook 'rust-mode-hook
          (lambda ()
            (require 'eglot)
            (eglot-ensure)
            (helm-gtags-mode 0)))
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; js2-mode
(add-hook 'js2-mode-hook
          (lambda ()
            (require 'js)
            (setq js-indent-level 4)
            (set (make-local-variable 'indent-line-function) 'js-indent-line)))
(push '("\\.js\\'" . js2-mode) auto-mode-alist)

;; python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (require 'eglot)
            (eglot-ensure)
            (helm-gtags-mode 0)))

;;c-mode-hook
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "C-c c") 'compile)
             (define-key c-mode-base-map (kbd "C-c e") 'next-error)
             (define-key c-mode-base-map (kbd "M-t") 'ff-find-other-file)
             (setq ff-other-file-alist
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
             (c-set-style "k&r")
             (setq c-basic-indent 4)
             (setq c-basic-offset 4)
             (c-toggle-hungry-state 1)))
(push '("\\.h$" . c++-mode) auto-mode-alist)
(push '("\\.cu$" . c++-mode) auto-mode-alist)
(push '("\\.ic$" . c++-mode) auto-mode-alist)

;; web-mode
(push '("\\.html\\'" . web-mode) auto-mode-alist)

;; lua-mode
(add-hook 'lua-mode-hook
          (lambda ()
            (setq lua-indent-level 4)))

;; go-mode
(when (executable-find "goimports")
  (setq gofmt-command "goimports"))
(add-hook 'before-save-hook 'gofmt-before-save)
(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends)
                               '(company-go))
                          (eglot-ensure)
                          (company-mode)
                          (helm-gtags-mode 0)))

;; my interactives
(defun use-local-bin ()
  (interactive)
  (set (make-local-variable 'exec-path)
       (cons "~/.local/bin" exec-path)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-enable-cua-keys nil)
 '(flycheck-checker-error-threshold nil)
 '(helm-gtags-pulse-at-cursor nil)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(package-selected-packages
   (quote
    (eglot php-mode yaml-mode web-mode undo-tree toml-mode rust-mode quickrun nginx-mode markdown-mode lua-mode json-mode js2-mode helm-projectile helm-gtags go-mode company)))
 '(rust-format-on-save t))
