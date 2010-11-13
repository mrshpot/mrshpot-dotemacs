
(defun get-free-buffer-number (format-string)
  "Return the least N, for which buffer `(\\[format] FORMAT-STRING N) doesn't exist.
FORMAT-STRING will be passed to \\[format] and must contain a `%d' parameter."
  (interactive "sFormat-string (must contain %%d): ")
  (let ((i 1)
	(buffer-names (mapcar 'buffer-name (buffer-list))))
    (while (find (format format-string i) buffer-names :test 'string-equal)
      (setq i (1+ i)))
    i))

(setq numbered-shell-format "*shell-%d*")

(defun shell-n (&optional number)
  "Create a shell with name specified by `numbered-shell-format' (default `*shell-n*')
Call the \\[shell] function with buffer name specified by
numbered-shell-format (default `*shell-n'), where n is NUMBER if provided,
or the first free number.

Query for NUMBER if a prefix arg present."
  (interactive
   (list (and current-prefix-arg
	      (read-number "Number: " (get-free-buffer-number numbered-shell-format)))))
  (let ((new-buffer-name
	 (format numbered-shell-format
		 (or number
		     (get-free-buffer-number numbered-shell-format)))))
    (shell (get-buffer-create new-buffer-name))))
