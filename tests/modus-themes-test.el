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

(ert-deftest mtt-color-dark-p ()
  "Test `modus-themes-color-dark-p'."
  (let ((modus-operandi-sample-foregrounds
         '("#a60000"
           "#972500"
           "#a0132f"
           "#7f0000"
           "#006800"
           "#316500"
           "#00663f"
           "#2a5045"
           "#6f5500"
           "#884900"
           "#7a4f2f"
           "#624416"
           "#0031a9"
           "#3548cf"
           "#0000b0"
           "#003497"
           "#721045"
           "#8f0075"
           "#531ab6"
           "#7c318f"
           "#005e8b"
           "#3f578f"
           "#005f5f"
           "#005077"))
         (modus-vivendi-sample-foregrounds
          '("#ff5f59"
            "#ff6b55"
            "#ff7f86"
            "#ff9580"
            "#44bc44"
            "#70b900"
            "#00c06f"
            "#88ca9f"
            "#d0bc00"
            "#fec43f"
            "#dfaf7a"
            "#d2b580"
            "#2fafff"
            "#79a8ff"
            "#00bcff"
            "#82b0ec"
            "#feacd0"
            "#f78fe7"
            "#b6a0ff"
            "#caa6df"
            "#00d3d0"
            "#4ae2f0"
            "#6ae4b9"
            "#9ac8e0")))
    (should (seq-every-p #'modus-themes-color-dark-p modus-operandi-sample-foregrounds))
    (should-not (seq-every-p #'modus-themes-color-dark-p modus-vivendi-sample-foregrounds))))

(ert-deftest mtt-get-readable-foreground ()
  "Test `modus-themes-get-readable-foreground'."
  (let ((modus-operandi-sample-foregrounds
         '("#a60000"
           "#972500"
           "#a0132f"
           "#7f0000"
           "#006800"
           "#316500"
           "#00663f"
           "#2a5045"
           "#6f5500"
           "#884900"
           "#7a4f2f"
           "#624416"
           "#0031a9"
           "#3548cf"
           "#0000b0"
           "#003497"
           "#721045"
           "#8f0075"
           "#531ab6"
           "#7c318f"
           "#005e8b"
           "#3f578f"
           "#005f5f"
           "#005077"))
        (modus-vivendi-sample-foregrounds
         '("#ff5f59"
           "#ff6b55"
           "#ff7f86"
           "#ff9580"
           "#44bc44"
           "#70b900"
           "#00c06f"
           "#88ca9f"
           "#d0bc00"
           "#fec43f"
           "#dfaf7a"
           "#d2b580"
           "#2fafff"
           "#79a8ff"
           "#00bcff"
           "#82b0ec"
           "#feacd0"
           "#f78fe7"
           "#b6a0ff"
           "#caa6df"
           "#00d3d0"
           "#4ae2f0"
           "#6ae4b9"
           "#9ac8e0")))
    (should (seq-every-p
             (lambda (value)
               (string= value "#ffffff"))
             (mapcar #'modus-themes-get-readable-foreground modus-operandi-sample-foregrounds)))
    (should (seq-every-p
             (lambda (value)
               (string= value "#000000"))
             (mapcar #'modus-themes-get-readable-foreground modus-vivendi-sample-foregrounds)))))

(provide 'modus-themes-test)
;;; modus-themes-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mtt" . "modus-themes-test-"))
;; End:
