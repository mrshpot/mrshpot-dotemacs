;;; shell-n.el --- Convenience function for spawning multiple shells.

;; Filename: shell-n.el
;; Description: Convenience function for spawning multiple shells.
;; Author: Taras Shpot
;; Copyright (C) 2010, Taras Shpot, all rights reserved.
;; Created: Nov 14 2010
;; Version: 0.1

;;; Code:

(defun get-free-buffer-number (format-string)
  "Return the least N, for which buffer `(\\[format] FORMAT-STRING N) doesn't exist.
FORMAT-STRING will be passed to \\[format] and must contain a `%d' parameter."
  (interactive "sFormat-string (must contain %%d): ")
  (let ((i 1)
		(buffer-names (mapcar 'buffer-name (buffer-list))))
    (while (find (format format-string i) buffer-names :test 'string-equal)
      (setq i (1+ i)))
    i))

(setq shell-n-format "*shell-%d*")

(defun shell-n (&optional number)
  "Create a shell with name specified by `numbered-shell-format' (default \"*shell-%d*\")
Identical to \\[shell] with a prefix arg.
If NUMBER if provided, use it; otherwise use the first free number.

Query for NUMBER if a prefix arg present."
  (interactive
   (list (and current-prefix-arg
			  (read-number "Number: " (get-free-buffer-number shell-n-format)))))
  (let ((new-buffer-name
		 (format shell-n-format
				 (or number
					 (get-free-buffer-number shell-n-format)))))
    (shell (get-buffer-create new-buffer-name))))

(provide 'shell-n)

;;; shell-n.el ends here
