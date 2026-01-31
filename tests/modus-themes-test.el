;;; modus-themes-test.el --- Unit tests for the Modus themes -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Rudolf Adamkovič <rudolf@adamkovic.org>,
;;         Protesilaos Stavrou <info@protesilaos.com>
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

;; Tests for the Modus themes.  Note that we are using Shorthands in
;; this file, so the "mtt-" prefix really is "modus-themes-test-".
;; Evaluate the following to learn more:
;;
;;    (info "(elisp) Shorthands")

;;; Code:

(require 'ert)
(require 'modus-themes)

(ert-deftest mtt-inheritance ()
  "Ensure all faces inherit from valid faces."
  ;; Third-party packages, loaded if possible to better test face inheritance.
  (require 'font-latex nil t)
  (let ((current-theme (modus-themes-get-current-theme)))
    (unwind-protect
        (progn
          (modus-themes-load-theme 'modus-operandi)
          (should-not (seq-filter
                       (lambda (face)
                         ;; The face either has no parent ...
                         (if-let* ((parent (face-attribute face :inherit)))
                             (and (symbolp parent)
                                  (not (eq parent 'unspecified))
                                  ;; ... or its parent is a valid face.
                                  (not (facep parent)))))
                       (face-list))))
      (modus-themes-load-theme current-theme))))


(provide 'modus-themes-test)
;;; modus-themes-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mtt" . "modus-themes-test-"))
;; End:
