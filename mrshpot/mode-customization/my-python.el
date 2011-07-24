(require 'python)
(require 'auto-complete)

(defvar python-interpreter
  (or (pathname-if-exists "C:/Python27/python.exe")
	  (pathname-if-exists "C:/Python26/python.exe")
	  (pathname-if-exists "/usr/bin/python2")
	  (pathname-if-exists "/usr/bin/python27")
	  (pathname-if-exists "/usr/bin/python2.7")
	  (pathname-if-exists "/usr/bin/python26")
	  (pathname-if-exists "/usr/bin/python2.6"))
  "Full path to Python2 interpreter")

(defun add-to-pythonpath (new-directory)
  "Add NEW-DIRECTORY to $PYTHONPATH"
  (add-path-to-env new-directory "PYTHONPATH"))
  
(defun mrshpot-load-pymacs (python-interpreter)
  (setenv "PYMACS_PYTHON" python-interpreter)
  (add-to-pythonpath (path-join emacs-root "site-lisp" "pymacs" "build" "lib"))
  (add-site-lisp-dir "pymacs")

  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  ;; (eval-after-load "pymacs"
  ;; 	'(add-to-list 'pymacs-load-path "YOUR-PYMACS-DIRECTORY"))
  )

(defun mrshpot-python-autocomplete-hook ()
  (add-to-list 'ac-sources 'ac-source-ropemacs))

(defun mrshpot-load-ropemacs ()
  (add-to-pythonpath (path-join emacs-root "etc" "rope" "build" "lib"))
  (add-to-pythonpath (path-join emacs-root "etc" "ropemode" "build" "lib"))
  (add-to-pythonpath (path-join emacs-root "etc" "ropemacs" "build" "lib"))

  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-guess-project t)
  (ac-ropemacs-setup))

(if python-interpreter
	(progn
	  (mrshpot-load-pymacs python-interpreter)
	  (mrshpot-load-ropemacs))
  (message "Python not found, skipping"))

