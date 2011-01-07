(defvar integrated-cedet-p
  (or
   (and (= emacs-major-version 23) ;; CEDET is built-in since GNU Emacs 23.2
	(>= emacs-minor-version 2))
   (> emacs-major-version 23)))

(if integrated-cedet-p
    (progn
      (message "Loading integrated CEDET..")
      (setq load-path (remove-if (lambda (x)
				   (or (search "site-lisp/cedet" x)
				       (search "site-lisp/ecb" x)))
				 load-path))
      (require 'cedet)
      (require 'semantic/sb)
      (require 'semantic/ia)
	  (require 'semantic/analyze/refs)
	  (require 'semantic/bovine/c))
  (progn
    (message "Loading external CEDET..")
    ;; CEDET
    (load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
    (semantic-load-enable-code-helpers)
    ;; Semantic configuration
    (require 'semantic-ia)
    (require 'semantic-gcc)
    ;; CEDET EDE
    (require 'ede)
    (put 'dired-find-alternate-file 'disabled nil)))

(semantic-mode 1)
(global-ede-mode t)

;; from http://navaneethkn.wordpress.com/2009/10/11/getting-smart-completion-wxwidgets-cedet-emacs/
;; Function to load all include files in the specified directory
(defun DE-imply-includes-in-directory (dir)
  "Add all header files in DIR to `semanticdb-implied-include-tags'."
  (let ((files (directory-files dir t "^.+\\.h[hp]*$" t)))
    (defvar-mode-local c++-mode semanticdb-implied-include-tags
      (mapcar (lambda (header)
                (semantic-tag-new-include
                 header
                 nil
                 :filename header))
              files))))

(defun mrshpot-setup-wxwidgets ()
  (interactive)
  (semantic-add-system-include "/usr/include/wx-2.8")
  
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("WXDLLEXPORT" . ""))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("WXDLLIMPEXP_CORE" . ""))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("WXDLLIMPEXP_FWD_CORE" . ""))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("WXDLLIMPEXP_BASE" . ""))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("WXDLLIMPEXP_FWD_BASE" . ""))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("WXDLLIMPEXP_FWD_XML" . ""))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("WXDLLIMPEXP_ADV" . ""))
  
  (DE-imply-includes-in-directory "/usr/lib/wx/include/gtk2-unicode-release-2.8/"))

;; ECB (Emacs Code Browser)
(add-site-lisp-dir "ecb-cvs")
(require 'ecb)

(global-semantic-idle-summary-mode)
(setq semantic-complete-inline-analyzer-displayor-class 'semantic-displayor-ghost)

(defun my-cedet-hook ()
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump))
(add-hook 'c-mode-common-hook 'my-cedet-hook)
