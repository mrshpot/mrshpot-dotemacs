
; Common Lisp
(require 'cl)

(global-set-key (kbd "C-x c") 'calculator)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

(defvar emacs-root
  (if (or (eq system-type 'gnu/linux)
	  (eq system-type 'linux)
	  (eq system-type 'cygwin))
      "/home/mrshpot/emacs/"
    "D:/emacs/"))

(labels ((add-path (p)
		   (add-to-list 'load-path
				(concat emacs-root p))))
  (add-path "mrshpot")
  (add-path "mrshpot/mode-customization")
  (add-path "site-lisp")
  (add-path "site-lisp/color-theme"))

(setq warning-suppress-types nil)

; mrshpot
(require 'mrshpot)
(load-library "mode-customization")

; Emacs customization
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("/home/mrshpot/projects/")))
 '(ecb-tip-of-the-day nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Dina"))))
 '(bold-italic ((t (:slant italic :weight bold))))
 '(italic ((t (:slant italic))))
 '(lazy-highlight ((t (:inherit isearch-lazy-highlight :background "paleturquoise4"))))
 '(link ((((class color) (min-colors 88) (background light)) (:foreground "#8cd0d3" :underline t))))
 '(link-visited ((default (:inherit link)) (((class color) (background light)) (:foreground "#dc8cc3")))))

