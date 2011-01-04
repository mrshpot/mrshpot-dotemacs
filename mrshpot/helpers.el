;;; helpers.el --- Various stuff that doesn't deserve its own file

;; Filename: helpers.el
;; Description: Convenience functions
;; Author: Taras Shpot
;; Copyright (C) 2011, Taras Shpot, all rights reserved.
;; Created: Jan 04 2011
;; Version: 0.1

;;; Code:

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
