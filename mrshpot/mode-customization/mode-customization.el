
;; automatically reload externally changed files
(global-auto-revert-mode t)

;; automatically handle compressed files
(auto-compression-mode t)

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
(setq require-final-newline 'query)

;; shell
;; Fix shell-mode color special characters
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defalias 'qrr 'query-replace-regexp)

;; cc-mode
;; my preferred indentation style
(setq c-default-style "ellemtel")
(setq c-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; tabs
;; force 4-space tabs
(setq tab-width 4)
(setq default-tab-width 4)
(setq indent-tabs-mode t)
(add-hook 'c-mode-common-hook (lambda ()
				(setq tab-width 4)
				(setq indent-tabs-mode t)))

;; pretty Control-L
(require 'pp-c-l)
(setf pp^L-^L-string-pre "")

;; IDO, Interactively Do Things
;; for nicer autocompletion in minibuffer
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; dired
(load "my-dired.el")

;; FSF Info
(require 'info)
(add-to-list 'Info-additional-directory-list
			 (concat emacs-root "info"))

;; ediff
(setf ediff-window-setup-function 'ediff-setup-windows-plain)
(setf ediff-split-window-function 'split-window-horizontally)

;; Git
(add-site-lisp-dir "git")
(require 'git)
(require 'git-blame)

;; optional stuff that may or may not be present, as I don't use that for work anyway
(labels
	((optional-require (feature)
					   (let ((res (require feature nil t)))
						 (unless res
						   (message
							(format "Could not load optional feature %s. Skipping." feature)))
						 res)))
  ;; SuperCollider
  (optional-require 'sclang)

  ;; EMMS
  (when (optional-require 'emms-setup)
	(emms-standard)
	(emms-default-players)
	(load "my-emms"))

  ;; W3M pager browser
  (when (optional-require 'w3m)
	(setq browse-url-browser-function 'w3m-browse-url))

  ;; emacs-jabber
  (optional-require 'jabber)

  ;; StarDict Console Version
  (when (optional-require 'sdcv-mode)
	(global-set-key (kbd "C-c d") 'sdcv-search)))

; CUDA
(require 'cuda-mode)
(setq auto-mode-alist (append '(("/*.\.cuh$" . cuda-mode)) auto-mode-alist))

;; CEDET
(load "my-cedet.el")

;; SLIME
(load "my-slime.el")

;; CMake
(require 'cmake-mode)

;; OCaml
(add-site-lisp-dir "caml-mode")
(require 'caml)
(setq auto-mode-alist
	  (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(if window-system (require 'caml-font))

;; Haskell
(add-site-lisp-dir "haskell-mode")
(require 'haskell-mode)
(require 'inf-haskell)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))

;; PHP
(require 'php-mode)
(autoload 'php-mode "php-mode.el" "PHP mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

;; JavaScript: Steve Yegge's js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Arch Linux PKGBUILD mode
(autoload 'pkgbuild-mode  "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; DOS batch script mode
(require 'batch-mode)

;; Clojure
(add-site-lisp-dir "clojure-mode")
(require 'clojure-mode)

;; C#
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
	  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Sunrise Commander, a Midnight Commander look-alike
(require 'sunrise-commander)
(require 'sunrise-x-popviewer)

;; Rainbow Mode, a nice mode to display colors as colors in source files
(require 'rainbow-mode)

(provide 'mode-customization)
