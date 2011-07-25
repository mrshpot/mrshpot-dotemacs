;; SLIME, Superior Lisp Integration Mode
;; try a list of interpreters, use the first present
(dolist (lisp-command '("/usr/bin/sbcl" "/bin/clisp"))
  (when (file-exists-p lisp-command)
	(setf inferior-lisp-program lisp-command)
	(add-site-lisp-dir "slime")
	(require 'slime)
	(slime-setup '(slime-fancy))
	(setf common-lisp-hyperspec-root (concat "file://" emacs-root "html/CL-HyperSpec/"))
	(return)))

;; SLIME auto-complete integration
(if (and (featurep 'slime) (featurep 'auto-complete))
	(progn
	  (message "Loading SLIME backend for auto-complete..")
	  (add-site-lisp-dir "ac-slime-git")
	  (require 'ac-slime)
	  (add-hook 'slime-mode-hook 'set-up-slime-ac)
	  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
	  (add-to-list 'ac-modes 'slime-repl-mode)
	  (add-to-list 'ac-modes 'lisp-mode))
  (message "Skipping SLIME backend for auto-complete"))
