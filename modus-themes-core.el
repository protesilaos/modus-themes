;;; modus-themes-core.el --- Testing -*- lexical-binding:t -*-
;; URL: https://www.protesilaos.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:
;; -
;;; Code:

(defmacro modus-themes-core-with-colors (alist &rest body)
  "`let' bind ALIST elements around BODY."
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     `(,(car cons) ,(cdr cons)))
                   (symbol-value alist)))
     ,@body))

(provide 'modus-themes-core)
;;; modus-themes-core.el ends here
