(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; package
(when (require 'package nil t)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize))

;; mozc
(when (require 'mozc nil t)
  (setq default-input-method "japanese-mozc")
  ;; (setq mozc-candidate-style 'overlay)
  )

;; input method
(global-set-key "\C-o" 'toggle-input-method)

;;ctrl-hをdelete
(global-set-key "\C-h" 'delete-backward-char)

;; haskell-mode
(when (require 'haskell-mode nil t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index))

;; このごろ開いたファイルを表示
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-x\M-f" 'recentf-open-files)

;; auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
  (ac-config-default)
  (setq ac-use-quick-help nil)
  (setq ac-auto-show-menu 0)
  (setq ac-auto-start 2))

;; migemo
(when (require 'migemo nil t)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix))

;; web-mode
(add-to-list 'load-path "~/.emacs.d/")
(when (require 'web-mode nil t)
  (add-hook 'web-mode-hook
	    (lambda()
	      (auto-complete-mode t)))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;矩形編集モード
(cua-mode t)
(setq cua-enable-cua-keys nil)

;;スタートアップを消す
(setq inhibit-startup-message t)

;;カーソル位置の色付け
(global-hl-line-mode)

;; no tabs
(setq-default indent-tabs-mode nil)

;;フレームの設定
(if window-system
    (progn
      ;;スクロールバーは左
      (set-scroll-bar-mode 'left)
      ;;テーマの設定
      (if (string-match "^24" emacs-version)
	  (load-theme 'tsdh-dark t)
	(progn
	  (add-to-list 'default-frame-alist '(background-color . "gray15"))
	  (add-to-list 'default-frame-alist '(foreground-color . "grey90"))
	  (add-to-list 'default-frame-alist '(cursor-color . "pink"))))
      ;;透過率
      (add-to-list 'default-frame-alist '(alpha . 95))
      ;; フレームのサイズ
      (add-to-list 'default-frame-alist '(width . 80))
      (add-to-list 'default-frame-alist '(height . 30))
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

;; 末尾のスペースを紫
(set-face-background 'trailing-whitespace "Purple")

;; linum
(require 'linum)
(global-linum-mode)

;; flycheck
(when (require 'flycheck nil t)
  (flycheck-define-checker c/c++
    "A C/C++ checker using g++."
    :command ("g++" "-std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" "-fmax-errors=20" source)
    :error-patterns  ((error line-start
			     (file-name) ":" line ":" column ":" " エラー: " (message)
			     line-end)
		      (warning line-start
			       (file-name) ":" line ":" column ":" " 警告: " (message)
			       line-end))
    :modes (c-mode c++-mode)))

;; js2-mode
(add-hook 'js2-mode-hook
          (lambda ()
	    (require 'js)
	    (setq js-indent-level 4)
	    (set (make-local-variable 'indent-line-function) 'js-indent-line)
	    (setq show-trailing-whitespace t)
	    (define-key js2-mode-map "\M-n" 'next-error)
	    (define-key js2-mode-map "\M-p" 'previous-error)))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;c-mode-hook
(add-hook 'c-mode-common-hook
	  '(lambda ()
	    (define-key c-mode-base-map "\C-cc" 'compile)
	    (define-key c-mode-base-map "\C-ce" 'next-error)
	    (define-key c-mode-base-map "\M-p" 'flycheck-previous-error)
	    (define-key c-mode-base-map "\M-n" 'flycheck-next-error)
	    (define-key c-mode-base-map "\C-cf" 'flycheck-buffer)
	    (define-key c-mode-base-map "\M-t" 'ff-find-other-file)
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
	    (setq show-trailing-whitespace t)
	    (flycheck-mode t)
	    (flycheck-select-checker 'c/c++)
	    (auto-complete-mode t)
	    (c-set-style "k&r")
	    (setq c-basic-indent 4)
	    (setq c-basic-offset 4)
	    (c-toggle-hungry-state 1)))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))

;; python-mode
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map "\M-p" 'flycheck-previous-error)
	    (define-key python-mode-map "\M-n" 'flycheck-next-error)
	    (define-key python-mode-map "\C-cf" 'flycheck-buffer)
	    (auto-complete-mode t)
	    (setq show-trailing-whitespace t)
	    (flycheck-mode t)))

;; typescript-mode
(when (require 'typescript nil t)
  (add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode)))

;; view-mode
(add-hook 'view-mode-hook
	  (lambda ()
	    (define-key view-mode-map "j" 'View-scroll-line-forward)
	    (define-key view-mode-map "k" 'View-scroll-line-backward)))

;; latex-mode
(add-hook 'latex-mode-hook
	  (lambda ()
	    (define-key latex-mode-map "\C-cc" 'compile)))

;; text-mode
(add-hook 'text-mode-hook
	  (lambda ()
	    ;;行末のスペースを色づけ
	    (setq show-trailing-whitespace t)))

;;; GDB 関連
;;; 有用なバッファを開くモード
(setq gdb-many-windows t)

;;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook
	  (lambda ()
	    (gud-tooltip-mode t)))

;;; I/O バッファを表示
(setq gdb-use-separate-io-buffer t)

;;; t にすると mini buffer に値が表示される
(setq gud-tooltip-echo-area nil)


(iswitchb-mode 1)
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "選択している buffer を window に表示してみる。"
  (when (and
         (eq iswitchb-method iswitchb-default-method)
         iswitchb-matches)
    (select-window
     (get-buffer-window (cadr (buffer-list))))
    (let ((iswitchb-method 'samewindow))
      (iswitchb-visit-buffer
       (get-buffer (car iswitchb-matches))))
    (select-window (minibuffer-window))))
