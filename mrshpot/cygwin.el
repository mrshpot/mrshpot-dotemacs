  ;;;;
  ;;;; cygwin support
  ;;;;

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
(defconst cygwin-root (or
					   (pathname-if-exists "C:/cygwin")
					   (pathname-if-exists "D:/cygwin")
					   (pathname-if-exists "F:/cygwin")
					   (pathname-if-exists (concat emacs-root "../cygwin"))
					   (error "Could not locate cygwin")))

(let* ((cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))
    
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
    
    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
    
    ;; NT-emacs assumes a Windows shell. Change to baash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name) 
    (setq explicit-shell-file-name shell-file-name) 
    
    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

(load (concat emacs-root "site-lisp/cygwin-mount"))
(load (concat emacs-root "site-lisp/cygwin-setup"))
