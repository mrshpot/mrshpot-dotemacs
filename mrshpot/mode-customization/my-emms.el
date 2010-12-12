(defadvice emms-start (after emms-start-notify activate)
  "Send a notification when a track starts playing"
  (let ((fname (cdr (assoc 'name (emms-playlist-current-selected-track)))))
	(notify "Currently playing:" fname)))
