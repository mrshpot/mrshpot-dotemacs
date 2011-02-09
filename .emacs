
;; Common Lisp
(require 'cl)

(global-set-key (kbd "C-x c") 'calculator)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "C-c <C-return>") 'redraw-display)

(defun normalize-slashes (pathname)
  "Reverse the Windows-backslashes in PATHNAME to be Unix-slashes; get rid of doubles"
  (replace-regexp-in-string "//" "/" (replace-regexp-in-string "\\\\" "/" pathname)))

(defun pathname-if-exists (pathname)
  "A convenience wrapper around `file-exists-p'

Return PATHNAME if it exists, nil otherwise"
  (if (file-exists-p pathname)
	  pathname
	nil))

(defun path-join (&rest args)
  (normalize-slashes
   (mapconcat 'identity args "/")))

;; Linux: ~/emacs/
;; Windows: %HOME%/, if %HOME%/.emacs exists, else D:/emacs
(defvar emacs-root
  (case system-type
	((gnu/linux linux cygwin) (format "/home/%s/emacs/" (getenv "USER")))
	(t (let ((home-path (normalize-slashes (concat (getenv "HOME") "/")))
			 (default-path "D:/emacs/"))
		 (if (file-exists-p (concat home-path ".emacs"))
			 home-path
		   default-path))))
  "The root directory for my customizations.")

(labels ((add-path (p)
		   (add-to-list 'load-path
				(concat emacs-root p))))
  (add-path "mrshpot")
  (add-path "mrshpot/mode-customization")
  (add-path "site-lisp"))

(byte-recompile-directory emacs-root) ; recursively recompile all out-of-date .elc files

(defun add-site-lisp-dir (dir)
  "Add DIR located in /usr/share/emacs/site-lisp/ or `emacs-root' to load-path.
If there was no DIR in those locations, signal an error."
  (add-to-list 'load-path
			   (let
				   ((site-path (concat "/usr/share/emacs/site-lisp/" dir))
					(local-path (concat emacs-root "site-lisp/" dir)))
				 (or
				  (pathname-if-exists site-path)
				  (pathname-if-exists local-path)
				  (t (error "site-lisp directory %s not found" dir))))))

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
 '(ansi-color-names-vector ["black" "#dca3a3" "#afd8af" "#f0dfaf" "#6ca0a3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(dired-listing-switches "-alGgh")
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
 '(gdb-max-frames 200)
 '(grep-command "grep -nHI -e ")
 '(ido-default-buffer-method (quote selected-window))
 '(yas/trigger-key "<C-tab>"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Andale Mono"))))
 '(dired-symlink ((t (:inherit font-lock-keyword-face :weight normal))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "#4f4f4f"))))
 '(ecb-history-bucket-node-dir-soure-path-face ((((class color) (background dark)) (:inherit ecb-history-bucket-node-face :foreground "violet"))))
 '(ecb-method-non-semantic-face ((((class color) (background dark)) (:inherit ecb-methods-general-face :foreground "#9c6363"))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:background "paleturquoise4"))))
 '(italic ((t (:inverse-video t :slant italic))))
 '(slime-repl-inputed-output-face ((((class color) (background dark)) (:foreground "#e37170"))))
 '(w3m-italic ((t (:inherit italic)))))
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
