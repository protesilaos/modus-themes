;;; modus-themes-test.el --- Unit tests for the Modus themes -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>,
;;         Rudolf Adamkovič <rudolf@adamkovic.org>
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

(defmacro mtt-define-test (symbol &rest args)
  "Write test for SYMBOL with DOCSTRING that runs BODY.
If DOCSTRING is nil, use a generic snippet of text.

\(fn NAME [DOCSTRING] BODY...)"
  (declare (doc-string 2) (indent defun))
  (let* ((has-docstring (stringp (car args)))
         (docstring (if has-docstring
                        (car args)
                      (format "Test that `%s' does the right thing." symbol)))
         (body (if has-docstring (cdr args) args)))
    `(ert-deftest ,(intern (format "mtt-%s" symbol)) ()
       ,docstring
       ,@body)))

(mtt-define-test modus-themes--hex-to-rgb
  (should (equal (modus-themes--hex-to-rgb "#fff") (list 1.0 1.0 1.0)))
  (should (equal (modus-themes--hex-to-rgb "#000") (list 0.0 0.0 0.0)))
  (should (equal (modus-themes--hex-to-rgb "#f00") (list 1.0 0.0 0.0)))
  (should (equal (modus-themes--hex-to-rgb "#0f0") (list 0.0 1.0 0.0)))
  (should (equal (modus-themes--hex-to-rgb "#00f") (list 0.0 0.0 1.0)))
  (should (equal (modus-themes--hex-to-rgb "#ffffff") (list 1.0 1.0 1.0)))
  (should (equal (modus-themes--hex-to-rgb "#000000") (list 0.0 0.0 0.0)))
  (should (equal (modus-themes--hex-to-rgb "#ff0000") (list 1.0 0.0 0.0)))
  (should (equal (modus-themes--hex-to-rgb "#00ff00") (list 0.0 1.0 0.0)))
  (let ((rgb-rounded-fn
         (lambda (hex)
           (let ((rgb (modus-themes--hex-to-rgb hex)))
             (mapcar (lambda (float) (string-to-number (format "%.2f" float))) rgb)))))
    (should (equal (funcall rgb-rounded-fn "#800000") (list 0.5 0.0 0.0)))
    (should (equal (funcall rgb-rounded-fn "#008000") (list 0.0 0.5 0.0)))
    (should (equal (funcall rgb-rounded-fn "#000080") (list 0.0 0.0 0.5))))
  (should-not (modus-themes--hex-to-rgb ""))
  (should-not (modus-themes--hex-to-rgb "#"))
  (should-not (modus-themes--hex-to-rgb "#1"))
  (should-not (modus-themes--hex-to-rgb "#12"))
  (should-not (modus-themes--hex-to-rgb "#1234"))
  (should-not (modus-themes--hex-to-rgb "#12345"))
  (should-not (modus-themes--hex-to-rgb "#gggggg"))
  (should-error (modus-themes--hex-to-rgb (list 1.0 1.0 1.0))))

(mtt-define-test modus-themes-wcag-formula
  "Test that `modus-themes-wcag-formula' does the right thing.
Also see `modus-themes-test--modus-themes--hex-to-rgb'."
  (should (= (modus-themes-wcag-formula "#ffffff") 1.0))
  (should (= (modus-themes-wcag-formula "#000000") 0.0))
  (should-not (modus-themes-wcag-formula "#00000")))

(mtt-define-test modus-themes-contrast
  "Test that `modus-themes-contrast' works as intended.
Also see `modus-themes-test--modus-themes--hex-to-rgb'."
  (should (= (modus-themes-contrast "#ffffff" "#000000") 21.0))
  (should (= (modus-themes-contrast "#000000" "#ffffff") 21.0))
  (let ((float-2-fn (lambda (hex1 hex2)
                      (string-to-number (format "%.2f" (modus-themes-contrast hex1 hex2))))))
    (should (= (funcall float-2-fn "#ff0000" "#ffffff")  4.0))
    (should (= (funcall float-2-fn "#00ff00" "#ffffff") 1.37))
    (should (= (funcall float-2-fn "#0000ff" "#ffffff") 8.59))
    (should (= (funcall float-2-fn "#ffff00" "#ffffff") 1.07))
    (should (= (funcall float-2-fn "#00ffff" "#ffffff") 1.25))
    (should (= (funcall float-2-fn "#ff00ff" "#ffffff") 3.14)))
  (should-error (modus-themes-contrast "#ffffff" "#00000"))
  (should-error (modus-themes-contrast "#fffff" "#00000")))

(mtt-define-test modus-themes--color-eight-to-six-digits
  (should (string= (modus-themes--color-eight-to-six-digits "#f00") "#f00"))
  (should (string= (modus-themes--color-eight-to-six-digits "#ff1919") "#ff1919"))
  (should (string= (modus-themes--color-eight-to-six-digits "#ffff19991999") "#ff1919")))

(mtt-define-test modus-themes-adjust-value
  "Test that `modus-themes-adjust-value' does the right thing.
Also see `modus-themes-test--modus-themes--hex-to-rgb'."
  (should (string= (modus-themes-adjust-value "#ff0000" 10) "#ff1919"))
  (should (string= (modus-themes-adjust-value "#505050" 100) "#ffffff"))
  (should (string= (modus-themes-adjust-value "#505050" -100) "#000000"))
  (should (string= (modus-themes-adjust-value "#505050" 0) "#505050"))
  (should-error (modus-themes-adjust-value "#ff00" 10)))

(mtt-define-test modus-themes-activate
  (if (custom-theme-p 'modus-operandi-tritanopia)
      (should-not (modus-themes-activate 'modus-operandi-tritanopia))
    (should (custom-theme-p 'modus-operandi-tritanopia))))

(mtt-define-test modus-themes--belongs-to-family-p
  (should (modus-themes--belongs-to-family-p 'modus-operandi 'modus-themes))
  (should-not (modus-themes--belongs-to-family-p 'my-fancy-theme 'modus-themes))
  (should-not (modus-themes--belongs-to-family-p 'modus-operandi 'my-fancy-themes)))

(mtt-define-test modus-themes-get-all-known-themes
  (should (equal (modus-themes-get-all-known-themes) modus-themes-items))
  (should-not (modus-themes-get-all-known-themes 'my-fancy-themes)))

(mtt-define-test modus-themes--background-p
  (should (modus-themes--background-p 'modus-operandi 'light))
  (should-not (modus-themes--background-p 'modus-operandi 'dark))
  (should-not (modus-themes--background-p 'modus-operandi t))
  (should-not (modus-themes--background-p 'modus-operandi :light)))

(mtt-define-test modus-themes-sort
  (let ((first-has-prefix-fn (lambda (themes prefix)
                               (when-let* ((first (car themes))
                                           (name (symbol-name first)))
                                 (string-match-p prefix name)))))
    (should (funcall first-has-prefix-fn (modus-themes-sort (reverse modus-themes-items) 'light) "modus-operandi"))
    (should (funcall first-has-prefix-fn (modus-themes-sort modus-themes-items 'dark) "modus-vivendi"))))

(mtt-define-test inheritance
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

(mtt-define-test color-dark-p
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

(mtt-define-test get-readable-foreground
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
