;;; helpers.el --- Various stuff that doesn't deserve its own file

;; Filename: helpers.el
;; Description: Convenience functions
;; Author: Taras Shpot
;; Copyright (C) 2011, Taras Shpot, all rights reserved.
;; Created: Jan 04 2011
;; Version: 0.1

;;; Code:


(require 'uuid)


(defun insert-command-output (command)
  "Execute string COMMAND via \\[shell-command] and insert the output into the current buffer"
  (interactive
   ;; copied from \\[shell-command]
   (list (read-shell-command "Shell command: " nil nil
							 (let ((filename
									(cond
									 (buffer-file-name)
									 ((eq major-mode 'dired-mode)
									  (dired-get-filename nil t)))))
							   (and filename (file-relative-name filename))))))
  (let ((shell-output nil))
	(with-temp-buffer
	  (shell-command command (current-buffer))
	  (setf shell-output (buffer-string)))
	(insert shell-output)))


(defun insert-c-header-guard-internal (guard)
  "Insert C header guard consisting of the ifdef/define/endif pattern."
  (save-excursion
	(goto-char (point-min))
	(insert "#ifndef " guard)
	(newline)
	(insert "#define " guard)
	(newline 2)
	(goto-char (point-max))
	(newline)
	(insert "#endif // " guard)
	(newline)))

(defun insert-c-header-guard-simple ()
  "Insert a C header guard in the form of FILE_NAME__INCLUDED"
  (interactive)
  (let ((guard
		 (concat ""
				 (replace-regexp-in-string "\\.\\|-" "_" (buffer-name))
				 "__INCLUDED")))
	(insert-c-header-guard-internal (upcase guard))))

(defun insert-c-header-guard-uuid ()
  "Insert C header guard in the form of _FILE_NAME_UUID_"
  (interactive)
  (let* ((stripped-fname (replace-regexp-in-string "\\.[^.]*$" "" (buffer-name)))
		 (guard-base (concat stripped-fname "_" (uuid-1)))
		 (guard (concat "_"
						(replace-regexp-in-string "\\.\\|-" "_" guard-base)
						"_")))
	(insert-c-header-guard-internal (upcase guard))))
