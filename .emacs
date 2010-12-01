
;; Common Lisp
(require 'cl)

(global-set-key (kbd "C-x c") 'calculator)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

(defun normalize-slashes (dirname)
  (replace-regexp-in-string "//" "/" (replace-regexp-in-string "\\\\" "/" dirname)))

(defvar emacs-root
  (case system-type
	((gnu/linux linux cygwin) (format "/home/%s/emacs/" (getenv "USER")))
	(t (normalize-slashes (concat (getenv "HOME") "/")))
  "The root directory for my customizations.")

(labels ((add-path (p)
		   (add-to-list 'load-path
				(concat emacs-root p))))
  (add-path "mrshpot")
  (add-path "mrshpot/mode-customization")
  (add-path "site-lisp"))

(defun add-site-lisp-dir (dir)
  "Add DIR located in /usr/share/emacs/site-lisp/ or `emacs-root' to load-path.
If there was no DIR in those locations, signal an error."
  (add-to-list 'load-path
			   (let
				   ((site-path (concat "/usr/share/emacs/site-lisp/" dir))
					(local-path (concat emacs-root "site-lisp/" dir)))
				 (cond
				  ((file-exists-p site-path) site-path)
				  ((file-exists-p local-path) local-path)
				  (t (error (format "directory %s not found" dir)))))))

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
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
 '(gdb-max-frames 200)
 '(grep-command "grep -nHI -e ")
 '(ido-default-buffer-method (quote selected-window)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Dina"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "#4f4f4f"))))
 '(ecb-history-bucket-node-dir-soure-path-face ((((class color) (background dark)) (:inherit ecb-history-bucket-node-face :foreground "violet"))))
 '(ecb-method-non-semantic-face ((((class color) (background dark)) (:inherit ecb-methods-general-face :foreground "#9c6363"))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:background "paleturquoise4")))))
