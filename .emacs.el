(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(setq load-prefer-newer t)

(tool-bar-mode -1)
(menu-bar-mode -1)

;;スタートアップを消す
(setq inhibit-startup-message t)

(require 'eieio)

;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(require 'cl)
(defvar installing-package-list
  '(
    helm
    helm-ls-git
    company
    flycheck
    flycheck-rust
    quickrun
    undo-tree
    go-mode
    js2-mode
    json-mode
    markdown-mode
    web-mode
    yaml-mode
    rust-mode
    toml-mode
    nginx-mode
    lua-mode
    ))

;; auto install
(let ((not-installed (loop for x in installing-package-list
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
(setq ido-enable-flex-matching t)

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

;; helm-ls-git
(when (require 'helm-ls-git nil t)
  (setq helm-ls-git-fuzzy-match t)
  (global-set-key (kbd "C-x C-d") 'helm-browse-project))

(when (require 'company nil t)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  ;; todo
  (setq company-backends (remove 'company-capf company-backends))
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

;; gtags
(autoload 'gtags-mode "gtags" nil t)
(setq gtags-suggested-key-mapping t)

;; uniquifly
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;矩形編集モード
(cua-mode t)
(setq cua-enable-cua-keys nil)

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
                          ;; '("Sans" . "iso10646-1")
                          '("M+ 2m regular" . "iso10646-1")
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
(with-eval-after-load "flycheck"
  (flycheck-define-checker c/c++
    "A C/C++ checker using g++."
    :command ("g++" "-std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" "-fmax-errors=20" source)
    :error-patterns  ((error line-start
                             (file-name) ":" line ":" column ":" " エラー: " (message)
                             line-end)
                      (warning line-start
                               (file-name) ":" line ":" column ":" " 警告: " (message)
                               line-end))
    :modes (c-mode c++-mode))

  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  (custom-set-variables '(flycheck-checker-error-threshold nil)))

;; prog-mode common setup
(add-hook 'prog-mode-hook
          (lambda ()
            ;; flycheck
            (flycheck-mode t)
            (define-key prog-mode-map (kbd "M-n") 'flycheck-next-error)
            (define-key prog-mode-map (kbd "M-p") 'flycheck-previous-error)
            (define-key prog-mode-map (kbd "C-c f") 'flycheck-buffer)

            ;; simple
            (define-key prog-mode-map (kbd "M-N") 'next-error)
            (define-key prog-mode-map (kbd "M-P") 'previous-error)

            ;; tag find
            (define-key prog-mode-map (kbd "M-.") 'helm-etags-select)
            (define-key prog-mode-map (kbd "M-*") 'xref-pop-marker-stack)
            ))

;; js2-mode
(add-hook 'js2-mode-hook
          (lambda ()
            (require 'js)
            (setq js-indent-level 4)
            (set (make-local-variable 'indent-line-function) 'js-indent-line)))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

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
             (gtags-mode 1)
             (flycheck-select-checker 'c/c++)
             (c-set-style "k&r")
             (setq c-basic-indent 4)
             (setq c-basic-offset 4)
             (c-toggle-hungry-state 1)))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))

;; typescript-mode
(add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; lua-mode
(add-hook 'lua-mode-hook
          (lambda ()
            (setq lua-indent-level 4)))

;; go-mode
(when (executable-find "goimports")
  (setq gofmt-command "goimports"))
(add-hook 'before-save-hook 'gofmt-before-save)
