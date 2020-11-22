;;; modus-themes-core.el --- Highly accessible themes (WCAG AAA) -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/modus-themes
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The Modus themes conform with the highest standard for color-contrast
;; accessibility between background and foreground values (WCAG AAA).
;; This file contains all customisation options, helper functions,
;; interactive commands, and face specifications.
;;
;; For a complete view of the project, also refer to the following files
;; (should be distributed in the same repository/directory as the
;; current item):
;;
;; - modus-themes.el            (Main code shared between the themes)
;; - modus-operandi-theme.el    (Light theme)
;; - modus-vivendi-theme.el     (Dark theme)

;;; Code:



(defvar modus-themes-colors-operandi)
(defvar modus-themes-colors-vivendi)
(defvar modus-themes-faces)
(defvar modus-themes-custom-variables)

(defun modus-themes-core-theme-variables (name)
  "Return correct variable for Modus theme NAME."
  (pcase name
    (''modus-operandi modus-themes-colors-operandi)
    (''modus-vivendi modus-themes-colors-vivendi)
    (_ (user-error "<< %s >> is not a valid Modus theme" name))))

(defmacro modus-themes-core-theme (name)
  "Bind NAME's color palette around face specifications.

NAME should be the proper name of a Modus theme, either
'modus-operandi or 'modus-vivendi.

Face specifications are those passed to `custom-theme-set-faces'.
They are extracted directly from variables defined in the
`modus-themes' library.  For example, `modus-themes-faces'."
  (let ((faces modus-themes-faces)
        (cus modus-themes-custom-variables))
    `(let ((class '((class color) (min-colors 89)))
           ,@(mapcar (lambda (cons)
                       `(,(car cons) ,(cdr cons)))
                     (modus-themes-core-theme-variables name)))
       (custom-theme-set-faces
        ,name
        ,@faces)
       (custom-theme-set-variables
        ,name
        ,@cus))))

(provide 'modus-themes-core)
;;; modus-themes-core.el ends here
