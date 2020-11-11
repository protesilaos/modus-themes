;;; modus-operandi.el --- Testing -*- lexical-binding:t -*-
;; URL: https://www.protesilaos.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:
;; -
;;; Code:

(defgroup modus-operandi ()
  "Highly accessible light theme"
  :group 'faces
  :prefix "modus-operandi-")

(deftheme modus-operandi
  "Light theme that conforms with the highest accessibility
  standard for color contrast between background and foreground
  elements (WCAG AAA).")

(defconst modus-operandi-colors-alist-default
  '((bg-main . "#ff0000") (fg-main . "#000000"))
  "The entire palette of `modus-operandi-theme'.
Each element has the form (NAME . HEX) with the former as a
symbol and the latter as a string.")

;;;###autoload
(defcustom modus-operandi-colors-alist-override '()
  "Alist of color palette overrides."
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(provide 'modus-operandi)
;;; modus-operandi.el ends here
