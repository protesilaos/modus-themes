;;; modus-themes-core.el --- Core code of the Modus themes -*- lexical-binding:t -*-

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



(defvar modus-themes-operandi-colors)
(defvar modus-themes-vivendi-colors)
(defvar modus-themes)

(defun modus-themes-core-theme-variables (name)
  "Return correct variable for Modus theme NAME."
  (pcase name
    (''modus-operandi modus-themes-operandi-colors)
    (''modus-vivendi modus-themes-vivendi-colors)
    (_ (user-error "<< %s >> is not a valid Modus theme" name))))

(defmacro modus-themes-core-theme (name &rest body)
  "Bind NAME's color palette around BODY of face specifications.

NAME should be the proper name of a Modus theme, either
'modus-operandi or 'modus-vivendi.

BODY consists of face specs as interpreted by
`custom-theme-set-faces'.  For concrete examples, refer to
`modus-themes'."
  (let ((specs (or `,body modus-themes)))
    `(let ((class '((class color) (min-colors 89)))
           ,@(mapcar (lambda (cons)
                       `(,(car cons) ,(cdr cons)))
                     (modus-themes-core-theme-variables name)))
       (custom-theme-set-faces
        ,name
        ,@specs))))

(provide 'modus-themes-core)
;;; modus-themes-core.el ends here
