
;; automatically reload externally changed files
(global-auto-revert-mode t)

;; automatically handle compressed files
(auto-compression-mode t)

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
(setq require-final-newline 'query)

;;; shell
;; Fix shell-mode color special characters
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; cc-mode
;; my preferred indentation style
(setq c-default-style "ellemtel")
(setq c-basic-offset 4)

;;; tabs
;; force 4-space tabs
(setq tab-width 4)
(setq default-tab-width 4)
(setq indent-tabs-mode t)
(add-hook 'c-mode-common-hook (lambda ()
				(setq tab-width 4)
				(setq indent-tabs-mode t)))

;; IDO, Interactively Do Things
;; for nicer autocompletion in minibuffer
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'info)
(add-to-list 'Info-additional-directory-list
			 (concat emacs-root "info"))

;; TODO: pull out optional stuff as stand-alone site-lisp packages
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
	(emms-default-players))

  ;; W3M pager browser
  (when (optional-require 'w3m)
	(setq browse-url-browser-function 'w3m-browse-url))

  ;; emacs-jabber
  (optional-require 'jabber)

  ;; SLIME with SBCL
  (when (and (file-exists-p "/usr/bin/sbcl") (file-exists-p "/usr/share/emacs/site-lisp/slime/"))
	(setf inferior-lisp-program "/usr/bin/sbcl")
	(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
	(require 'slime)
	(setf common-lisp-hyperspec-root (concat "file:" emacs-root "html/CL-HyperSpec/"))
	(slime-setup)))

; CUDA
(require 'cuda-mode)
(setq auto-mode-alist (append '(("/*.\.cuh$" . cuda-mode)) auto-mode-alist))

;; CEDET
(load "my-cedet.el")

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
(require 'haskell-mode)
(require 'inf-haskell)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))

;; PHP
(require 'php-mode)
(autoload 'php-mode "php-mode.el" "PHP mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

;; Arch Linux PKGBUILD mode
(autoload 'pkgbuild-mode  "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; Clojure
(add-site-lisp-dir "clojure-mode")
(require 'clojure-mode)


(provide 'mode-customization)
