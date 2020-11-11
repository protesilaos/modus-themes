;;; modus-vivendi.el --- Testing -*- lexical-binding:t -*-
;; URL: https://www.protesilaos.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:
;; -
;;; Code:

(defgroup modus-vivendi ()
  "Highly accessible light theme"
  :group 'faces
  :prefix "modus-vivendi-")

(deftheme modus-vivendi
  "Light theme that conforms with the highest accessibility
  standard for color contrast between background and foreground
  elements (WCAG AAA).")

(defconst modus-vivendi-colors-alist-default
  '((bg-main . "#000000") (fg-main . "#ffffff"))
  "The entire palette of `modus-vivendi-theme'.
Each element has the form (NAME . HEX) with the former as a
symbol and the latter as a string.")

;;;###autoload
(defcustom modus-vivendi-colors-alist-override '()
  "Alist of color palette overrides."
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(provide 'modus-vivendi)
;;; modus-vivendi.el ends here
