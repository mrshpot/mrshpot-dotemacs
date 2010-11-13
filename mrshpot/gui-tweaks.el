
;; goodbye, splash screen
(setq inhibit-splash-screen t)

;; nice editing features
(global-hl-line-mode t)
(show-paren-mode t)
(setq transient-mark-mode t)

;; mode-line stuff
(column-number-mode nil)
(display-time-mode t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; turn off gui stuff I don't use anyway
(tool-bar-mode nil)
(menu-bar-mode nil)

(when (or (eq system-type 'gnu/linux)
	  (eq system-type 'linux)
	  (eq system-type 'cygwin))
  ;; nicer frame titles
  (setq frame-title-format
	(concat  "%b - " (getenv "USER") "@" (system-name))))

;; Color Theme
;; right now there seems to be a problem with my color themes.
;; either some default theme is overriding the defaults
;; or the themes I try to load are broken.
;; my custom theme selection
(let ((my-theme 'zenburn))
  (cond
   ; zenburn
   ((eq my-theme 'zenburn)
    (load-library
	 (if (eq system-type 'windows-nt)
		 "color-themes/zenburn-updated"
	   "color-themes/color-theme-zenburn"))
    (color-theme-zenburn))
   ;; tangotango, doesn't work
   ((eq my-theme 'tangotango)
    (load-library "color-themes/color-theme-tangotango")
    (setq color-theme-is-global t)
    (setq color-theme-is-cumulative t)
    (setq color-theme-load-all-themes nil)

    (color-theme-tangotango)

    (add-hook 'message-mode-hook 'color-theme-tangotango)
    (add-hook 'gnus-article-mode-hook 'color-theme-tangotango))
   ;; gruber-darker
   ((eq my-theme 'gruber-darker)
    (load-library "color-themes/color-theme-gruber-darker")
    (color-theme-gruber-darker))
   ;; almost-monokai
   ((eq my-theme 'almost-monokai)
    (load-library "color-themes/color-theme-almost-monokai")
    (color-theme-almost-monokai))))

