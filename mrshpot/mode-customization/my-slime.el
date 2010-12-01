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
