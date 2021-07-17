;;; modus-vivendi-theme.el --- Accessible dark theme (WCAG AAA) [DEPRECATED] -*- lexical-binding:t -*-

;; Copyright (c) 2019-2021  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/modus-themes
;; Version: 0.13.2
;; Package-Requires: ((emacs "26.1") (modus-themes "1.2.4"))
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package is obsolete.  It has been replaced by `modus-themes',
;; which includes the modus-operandi and modus-vivendi themes.  Users
;; who were tracking the `master' branch must switch to `main'.
;;
;; An Info manual should be distributed with the Modus themes.
;; Evaluate this form to access it directly:
;;
;;    (info "(modus-themes) Top")
;;
;; Else check the web page: <https://protesilaos.com/modus-themes/>.
;;
;; A summary of all new customisation options is available here:
;; <https://protesilaos.com/modus-themes/#h:bf1c82f2-46c7-4eb2-ad00-dd11fdd8b53f>.
;; Or evaluate this form to read the Info manual:
;;
;;    (info "(modus-themes) Customization Options")

;;; Code:



(if t (require 'modus-themes)) ;; Don't require it during compilation!

(eval-and-compile
  (let ((msg "Package `modus-vivendi-theme' is obsolete; use the new `modus-themes' or switch to the `main' branch"))
    (if (and noninteractive (fboundp 'byte-compile-warn))
        (byte-compile-warn msg)
      (message "%s" msg))))

(provide 'modus-vivendi-theme)

;;; modus-vivendi-theme.el ends here
