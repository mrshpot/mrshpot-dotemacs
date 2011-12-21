
;; automatically reload externally changed files
(global-auto-revert-mode t)

;; automatically handle compressed files
(auto-compression-mode t)

;; default to unified diffs
(setq diff-switches "-u")

;; ediff
(require 'ediff)
(setf ediff-window-setup-function 'ediff-setup-windows-plain)
(setf ediff-split-window-function 'split-window-horizontally)

;; Diminish -- remove or rename modelines for minor modes
(require 'diminish)

;; CamelCase navigation
(global-subword-mode 1)

;; always end a file with a newline
(setq require-final-newline 'query)

;; Smart-Tab
;; TODO:
;;  - properly fall back to the default TAB behaviour in modes like
;;    shell-mode, org-mode, inferior-python-mode, SLIME REPL
;;    (disabling it for now as it breaks those modes and, I suspect, more)
;;  - in elisp-mode, do what M-TAB does instead of what C-/ does
;;  - in CEDET-enabled modes, try to do Semantic autocompletion,
;;    otherwise fall back to C-/
;; (add-site-lisp-dir "smart-tab")
;; (require 'smart-tab)
;; (global-smart-tab-mode 1)

;; yasnippet
(add-site-lisp-dir "yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory
 (path-join emacs-root "site-lisp" "yasnippet" "snippets"))
(eval-after-load 'yasnippet '(diminish 'yas/minor-mode " Y"))

;; auto-complete
(add-site-lisp-dir "auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "C-c SPC") 'auto-complete)
(setq ac-use-fuzzy t)
(add-hook 'c++-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))
(eval-after-load 'auto-complete-mode '(diminish 'auto-complete-mode))

;; elisp, emacs lisp
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(setq mode-name "el")))

;; Python + Pymacs + ropemacs + stuff
(load "my-python.el")

;; shell
;; Fix shell-mode color special characters
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defalias 'qrr 'query-replace-regexp)
(defalias 'rs 'replace-string)

;; move between windows with M-arrows
(windmove-default-keybindings 'meta)

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

(add-site-lisp-dir "highlight-indentation")
(require 'highlight-indentation)

;; IDO, Interactively Do Things
;; for nicer autocompletion in minibuffer
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(ido-everywhere 1)

;; dired
(load "my-dired")

;; FSF Info
(require 'info)
(add-to-list 'Info-additional-directory-list
			 (concat emacs-root "info"))

;; Python
(add-to-list 'auto-mode-alist '("\\.pyw$" . python-mode))

;; DocView
;; automatically switch to the next page
(require 'doc-view)
(setf doc-view-continuous t)

;; Git
(add-site-lisp-dir "git")
(require 'git)
(require 'git-blame)

;; Uniquify files with same name in different directories
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; optional stuff that may or may not be present, as I don't use that for work anyway
(labels
	((optional-require (feature)
					   (or (require feature nil t)
						   (progn (message "Could not load optional feature %s. Skipping." feature) nil))))
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

;; ElDoc mode -- display documentation about symbol under cursor
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; browse-kill-ring
(require 'browse-kill-ring)
(require 'browse-kill-ring+)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; CMake
(require 'cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))
(add-to-list 'auto-mode-alist '("/*.\.cmake$" . cmake-mode))

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
(autoload 'php-mode "php-mode.el" "PHP mode." t)
(add-to-list 'auto-mode-alist '("/*.\.php[345]?$" . php-mode))

;; JavaScript: Steve Yegge's js2-mode
(add-site-lisp-dir "js2-mode-git")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Arch Linux PKGBUILD mode
(autoload 'pkgbuild-mode  "pkgbuild-mode.el" "PKGBUILD mode." t)
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

;; DOS batch script mode
(autoload 'batch-mode  "batch-mode.el" "Batch script mode." t)
(add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))
(add-to-list 'auto-mode-alist '("\\.cmd\\'" . batch-mode))


;; PowerShell
(autoload 'powershell-mode "powershell-mode.el" "PowerShell mode." t)
(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))

;; Clojure
(add-site-lisp-dir "clojure-mode")
(autoload 'clojure-mode "clojure-mode.el" "Clojure mode." t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;; C#
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(text\\|md\\)" . markdown-mode))

;; hl-tags-mode
(autoload 'hl-tags-mode "hl-tags-mode" "Toggle hl-tags-mode." t)

;; nyan cat mode
(add-site-lisp-dir "nyan-mode-git")
(autoload 'nyan-mode "nyan-mode" "Use NyanCat to show buffer size and position in mode-line." t)

;; sml-modeline
(require 'sml-modeline)
(sml-modeline-mode 1)

;; Sunrise Commander, a Midnight Commander look-alike
(require 'sunrise-commander)
(require 'sunrise-x-popviewer)

;; Use cyrillic fonts in org-mode latex export
(require 'org)
(require 'org-latex)
(mapcar (lambda (x)
		  (setf (cadr x)
				(concat
				 (replace-regexp-in-string "\\[T1\\]" "[T2A]" (cadr x))
				 "\n\\usepackage{amsmath}")))
		org-export-latex-classes)

;; Rainbow Mode, a nice mode to display colors as colors in source files
(autoload 'rainbow-mode "rainbow-mode.el"
  "Color names with colored background." t)

(provide 'mode-customization)
;;;; mode-customization.el ends here
