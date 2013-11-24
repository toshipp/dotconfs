(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; package
(when (require 'package nil t)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/")
	       t)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

;; mozc
(when (require 'mozc nil t)
  (setq default-input-method "japanese-mozc")
  ;; (setq mozc-candidate-style 'overlay)
  )

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
  (setq migemo-coding-system 'utf-8-unix))

;;スタートアップを消す
(setq inhibit-startup-message t)

;;カーソル位置の色付け
(global-hl-line-mode)

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

;;flymake
(require 'flymake)
;;flymake colors
(set-face-background 'flymake-errline "dark red")
(set-face-background 'flymake-warnline "dark slate blue")
;; flymakeでエラー発生時に無視する
(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)
(defmacro flymake-init-maker (name command opt)
  `(defun ,name ()
     (let* ((temp-file   (flymake-init-create-temp-buffer-copy
			  'flymake-create-temp-inplace))
	    (local-file  (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
       (list ,command (append ,opt (list local-file))))))

;; linum
(require 'linum)
(global-linum-mode)

;;c-mode
(flymake-init-maker flymake-cc-init-no-makefile
		    "g++" '("-std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" "-fmax-errors=20"))
(defun flymake-cc-init()
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
    (flymake-cc-init-no-makefile)))
(push '("\\.hpp$"	flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.cpp$"	flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.h$"		flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.c$"		flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.cu$"	flymake-cc-init) flymake-allowed-file-name-masks)

;;mini-bufferにエラーを表示
(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; js2-mode
(add-hook 'js2-mode-hook
          (lambda ()
	    (require 'js)
	    (setq js-indent-level 2
		  indent-tabs-mode nil)
	    (set (make-local-variable 'indent-line-function) 'js-indent-line)
	    (setq show-trailing-whitespace t)
	    (set-face-background 'trailing-whitespace "Purple")
	    (define-key js2-mode-map "\M-n" 'next-error)
	    (define-key js2-mode-map "\M-p" 'previous-error)))
	    

;;c-mode-hook
(add-hook 'c-mode-common-hook
	  '(lambda ()
	    (define-key c-mode-base-map "\C-cc" 'compile)
	    (define-key c-mode-base-map "\C-ce" 'next-error)
	    (define-key c-mode-base-map "\C-cd" 'flymake-display-err-minibuf)
	    (define-key c-mode-base-map "\C-cf" 'flymake-start-syntax-check)
	    (define-key c-mode-base-map "\M-p" 'flymake-goto-prev-error)
	    (define-key c-mode-base-map "\M-n" 'flymake-goto-next-error)
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
	    ;;行末のスペースを色づけ
	    (setq show-trailing-whitespace t)
	    (set-face-background 'trailing-whitespace "Purple")
	    ;;flymake
	    (flymake-mode t)
	    (setq flymake-err-line-patterns
		  (cons
		   '("\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)" 1 2 3 4)
		   flymake-err-line-patterns))
	    ;;auto completion
	    (auto-complete-mode t)
	    (c-set-style "k&r")
	    (setq c-basic-indent 4)
	    (setq c-basic-offset 4)
	    (setq indent-tabs-mode nil)
	    (c-toggle-hungry-state 1)))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))

;; python-mode
(flymake-init-maker flymake-python-init
		    "epylint" '())
(push '("\\.py$"	flymake-python-init) flymake-allowed-file-name-masks)
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map "\M-p" 'flymake-goto-prev-error)
	    (define-key python-mode-map "\M-n" 'flymake-goto-next-error)
	    (define-key python-mode-map "\C-cd" 'flymake-display-err-minibuf)
	    (define-key python-mode-map "\C-cf" 'flymake-start-syntax-check)
	    (setq show-trailing-whitespace t)
	    (setq indent-tabs-mode nil)
	    (flymake-mode t)))

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
	    (setq show-trailing-whitespace t)
	    (set-face-background 'trailing-whitespace "Purple")))

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
