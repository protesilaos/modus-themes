;;; modus-vivendi-theme.el --- Testing -*- lexical-binding:t -*-
;; URL: https://www.protesilaos.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:
;; -
;;; Code:

(eval-when-compile
  (require 'modus-themes-core))
(require 'modus-vivendi)
(require 'modus-themes-faces)

(modus-themes-core-with-colors
 modus-vivendi-colors-alist-default
 (custom-theme-set-faces
  'modus-vivendi
  (modus-themes-faces)))

(provide 'modus-vivendi-theme)
;;; modus-vivendi-theme.el ends here
