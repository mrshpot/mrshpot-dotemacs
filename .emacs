
;; Common Lisp
(require 'cl)

(global-set-key (kbd "C-x c") 'calculator)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

(defvar emacs-root
  (case system-type
	((gnu/linux linux cygwin) (format "/home/%s/emacs/" (getenv "USER")))
	(t "D:/emacs/")))

(labels ((add-path (p)
		   (add-to-list 'load-path
				(concat emacs-root p))))
  (add-path "mrshpot")
  (add-path "mrshpot/mode-customization")
  (add-path "site-lisp"))

(defun add-site-lisp-dir (d)
  (add-to-list 'load-path
			   (let
				   ((site-path (concat "/usr/share/emacs/site-lisp/" d))
					(local-path (concat emacs-root "site-lisp/" d)))
				 (cond
				  ((file-exists-p site-path) site-path)
				  ((file-exists-p local-path) local-path)
				  (t (error (format "directory %s not found" d)))))))

(setf warning-suppress-types nil)
(setf backup-by-copying t)

;; mrshpot
(require 'mrshpot)
(load-library "mode-customization")

; Emacs customization
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Cousine")))))
