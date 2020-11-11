;;; modus-themes-faces.el --- Testing -*- lexical-binding:t -*-
;; URL: https://www.protesilaos.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:
;; -
;;; Code:

;;; Custom faces

(defgroup modus-theme ()
  "Custom faces for the Modus themes."
  :group 'faces
  :prefix "modus-theme-"
  :link '(url-link :tag "GitLab" "https://gitlab.com/protesilaos/modus-themes")
  :tag "Modus Operandi")

(defface modus-theme-subtle-red nil nil)
(defface modus-theme-subtle-green nil nil)
(defface modus-theme-subtle-yellow nil nil)
(defface modus-theme-subtle-blue nil nil)
(defface modus-theme-subtle-magenta nil nil)
(defface modus-theme-subtle-cyan nil nil)
(defface modus-theme-subtle-neutral nil nil)
(defface modus-theme-intense-red nil nil)
(defface modus-theme-intense-green nil nil)
(defface modus-theme-intense-yellow nil nil)
(defface modus-theme-intense-blue nil nil)
(defface modus-theme-intense-magenta nil nil)
(defface modus-theme-intense-cyan nil nil)
(defface modus-theme-intense-neutral nil nil)
(defface modus-theme-refine-red nil nil)
(defface modus-theme-refine-green nil nil)
(defface modus-theme-refine-yellow nil nil)
(defface modus-theme-refine-blue nil nil)
(defface modus-theme-refine-magenta nil nil)
(defface modus-theme-refine-cyan nil nil)
(defface modus-theme-active-red nil nil)
(defface modus-theme-active-green nil nil)
(defface modus-theme-active-yellow nil nil)
(defface modus-theme-active-blue nil nil)
(defface modus-theme-active-magenta nil nil)
(defface modus-theme-active-cyan nil nil)
(defface modus-theme-fringe-red nil nil)
(defface modus-theme-fringe-green nil nil)
(defface modus-theme-fringe-yellow nil nil)
(defface modus-theme-fringe-blue nil nil)
(defface modus-theme-fringe-magenta nil nil)
(defface modus-theme-fringe-cyan nil nil)
(defface modus-theme-nuanced-red nil nil)
(defface modus-theme-nuanced-green nil nil)
(defface modus-theme-nuanced-yellow nil nil)
(defface modus-theme-nuanced-blue nil nil)
(defface modus-theme-nuanced-magenta nil nil)
(defface modus-theme-nuanced-cyan nil nil)
(defface modus-theme-special-cold nil nil)
(defface modus-theme-special-mild nil nil)
(defface modus-theme-special-warm nil nil)
(defface modus-theme-special-calm nil nil)
(defface modus-theme-diff-added nil nil)
(defface modus-theme-diff-changed nil nil)
(defface modus-theme-diff-removed nil nil)
(defface modus-theme-diff-refine-added nil nil)
(defface modus-theme-diff-refine-changed nil nil)
(defface modus-theme-diff-refine-removed nil nil)
(defface modus-theme-diff-focus-added nil nil)
(defface modus-theme-diff-focus-changed nil nil)
(defface modus-theme-diff-focus-removed nil nil)
(defface modus-theme-diff-heading nil nil)
(defface modus-theme-pseudo-header nil nil)
(defface modus-theme-mark-alt nil nil)
(defface modus-theme-mark-del nil nil)
(defface modus-theme-mark-sel nil nil)
(defface modus-theme-mark-symbol nil nil)
(defface modus-theme-heading-1 nil nil)
(defface modus-theme-heading-2 nil nil)
(defface modus-theme-heading-3 nil nil)
(defface modus-theme-heading-4 nil nil)
(defface modus-theme-heading-5 nil nil)
(defface modus-theme-heading-6 nil nil)
(defface modus-theme-heading-7 nil nil)
(defface modus-theme-heading-8 nil nil)
(defface modus-theme-hl-line nil nil)
(defface modus-theme-bold nil nil)
(defface modus-theme-slant nil nil)
(defface modus-theme-variable-pitch nil nil)

;;;; Face specifications

(defun modus-themes-faces ()
  "Face specs for use with `modus-themes-core-with-colors'."
  (quote
   `(default ((,class :background ,bg-main :foreground ,fg-main)))))

(provide 'modus-themes-faces)
;;; modus-themes-faces.el ends here
