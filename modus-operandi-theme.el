;;; modus-operandi-theme.el --- Testing -*- lexical-binding:t -*-
;; URL: https://www.protesilaos.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:
;; -
;;; Code:

(eval-when-compile
  (require 'modus-themes-core))
(require 'modus-operandi)
(require 'modus-themes-faces)

(modus-themes-core-with-colors
 modus-operandi-colors-alist-default
 (custom-theme-set-faces
  'modus-operandi
  (modus-themes-faces)))

(provide-theme 'modus-operandi)

(provide 'modus-operandi-theme)
;;; modus-operandi-theme.el ends here
