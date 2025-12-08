;;; modus-themes-test.el --- Unit tests for the Modus themes -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Rudolf Adamkovič <rudolf@adamkovic.org>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/modus-themes

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; Tests for the Modus themes.

;;; Code:

(require 'ert)
(require 'modus-themes)

;; Third-party packages, loaded if possible.
(require 'font-latex nil t)

(ert-deftest modus-themes-test-inheritance ()
  "Ensure all faces inherit from valid faces."
  (modus-themes-select 'modus-operandi)
  (should-not (seq-filter
               (lambda (face)
                 ;; The face either has no parent ...
                 (if-let* ((parent (face-attribute face :inherit)))
                     (and (symbolp parent)
                          (not (eq parent 'unspecified))
                          ;; ... or its parent is a valid face.
                          (not (facep parent)))))
               (face-list))))

(provide 'modus-themes-test)
;;; modus-themes-test.el ends here
