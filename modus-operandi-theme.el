;;; modus-operandi-theme.el --- Testing -*- lexical-binding:t -*-
;; URL: https://www.protesilaos.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:
;; -
;;; Code:

(eval-and-compile
  (require 'modus-themes))

(eval-when-compile
  (require 'modus-themes-core))

(modus-themes-core-theme
 'modus-operandi)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'modus-operandi)

(provide 'modus-operandi-theme)

;;; modus-operandi-theme.el ends here
