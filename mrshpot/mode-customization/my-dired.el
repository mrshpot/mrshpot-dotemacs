(defvar dired-buffer-prefix "dired-"
  "Prefix for the dired advice `dired-add-prefix'")

(defadvice dired (after dired-add-prefix activate)
  "Prefix the dired buffer name with `dired-buffer-prefix' if it's non-nil"
  (when dired-buffer-prefix
	(with-current-buffer ad-return-value
	  (unless (string-match
			   (concat "^" dired-buffer-prefix ".*")
			   (buffer-name))
		(rename-buffer (concat dired-buffer-prefix (buffer-name)))))))

(put 'dired-find-alternate-file 'disabled t)
