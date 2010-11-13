;; central file to mrshpot's configuration.
;; everything else is loaded from here.

(load "buffer-utils")
(load "gui-tweaks")
(load "unicode-customization")
(when (eq system-type 'windows-nt)
  (load "cygwin"))

(provide 'mrshpot)
