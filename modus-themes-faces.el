;;; modus-themes-faces.el --- Testing -*- lexical-binding:t -*-
;; URL: https://www.protesilaos.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:
;; -
;;; Code:

;; (defmacro modus-themes-faces ()
;;   "Return a backquote which defines a list of face specs.
;; It expects to be evaluated in a scope in which the various color
;; names to which it refers are bound."
;;   (quote
;;    (mapcar
;;     (lambda (entry)
;;       (list (car entry) `((,class ,@(cdr entry)))))
;;     `(
;;       (default :background ,bg-main :foreground ,fg-main)))))

(defun modus-themes-faces ()
  "Face specs for use with `modus-themes-core-with-colors'."
  (quote
   `(default ((,class :background ,bg-main :foreground ,fg-main)))))

(provide 'modus-themes-faces)
;;; modus-themes-faces.el ends here
