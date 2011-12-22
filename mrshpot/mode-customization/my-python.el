(require 'python)
(require 'auto-complete)
(require 'flymake)

(add-hook 'python-mode-hook
		  (lambda ()
			(setq mode-name "Py")))

(defvar python-interpreter
  (or (pathname-if-exists "C:/Python27/python.exe")
	  (pathname-if-exists "C:/Python26/python.exe")
	  (pathname-if-exists "/usr/bin/python27")
	  (pathname-if-exists "/usr/bin/python2.7")
	  (pathname-if-exists "/usr/bin/python26")
	  (pathname-if-exists "/usr/bin/python2.6")
	  (pathname-if-exists "/usr/bin/python2"))
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
  ;; ropemacs
  (add-to-pythonpath (path-join emacs-root "etc" "rope" "build" "lib"))
  (add-to-pythonpath (path-join emacs-root "etc" "ropemode" "build" "lib"))
  (add-to-pythonpath (path-join emacs-root "etc" "ropemacs" "build" "lib"))

  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-guess-project t)
  (ac-ropemacs-setup))

(defun flymake-get-pylint-cmdline (source base-dir)
  `(,python-interpreter ("-c" "import mrshpot_epylint; mrshpot_epylint.Run();" ,source)))
    
(defun flymake-pylint-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-inplace nil nil buffer-file-name 'flymake-get-pylint-cmdline))

(defun mrshpot-setup-flymake ()
  ;; pylint
  (add-to-pythonpath (path-join emacs-root "etc" "pylint-deps"))
  (add-to-pythonpath (path-join emacs-root "etc" "pylint-hack"))
  (add-to-pythonpath (path-join emacs-root "etc" "pylint-0.25.1" "build" "lib"))

  (add-to-list 'flymake-allowed-file-name-masks
			   '("\\.py\\'" flymake-pylint-init)))

(if python-interpreter
	(progn
	  (mrshpot-load-pymacs python-interpreter)
	  (mrshpot-load-ropemacs)
	  (mrshpot-setup-flymake))
  (message "Python not found, skipping"))
