;; central file to mrshpot's configuration.
;; everything else is loaded from here.

(require 'shell-n)
(load "helpers")
(load "gui-tweaks")
(load "unicode-customization")
(when (eq system-type 'windows-nt)
  (load "cygwin"))

(provide 'mrshpot)
