;;; modus-themes.el --- Elegant, highly legible and customizable themes -*- lexical-binding:t -*-

;; Copyright (C) 2019-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Modus-Themes Development <~protesilaos/modus-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/modus-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/modus-themes
;; Version: 4.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The Modus themes conform with the highest standard for color-contrast
;; accessibility between background and foreground values (WCAG AAA).
;; This file contains all customization variables, helper functions,
;; interactive commands, and face specifications.  Please refer to the
;; official Info manual for further documentation (distributed with the
;; themes, or available at: <https://protesilaos.com/emacs/modus-themes>).
;;
;; The themes share the following customization variables:
;;
;;     modus-themes-completions                    (alist)
;;     modus-themes-headings                       (alist)
;;     modus-themes-org-agenda                     (alist)
;;     modus-themes-bold-constructs                (boolean)
;;     modus-themes-deuteranopia                   (boolean)
;;     modus-themes-inhibit-reload                 (boolean)
;;     modus-themes-intense-mouseovers             (boolean)
;;     modus-themes-italic-constructs              (boolean)
;;     modus-themes-mixed-fonts                    (boolean)
;;     modus-themes-subtle-line-numbers            (boolean)
;;     modus-themes-variable-pitch-ui              (boolean)
;;     modus-themes-box-buttons                    (choice)
;;     modus-themes-diffs                          (choice)
;;     modus-themes-fringes                        (choice)
;;     modus-themes-hl-line                        (choice)
;;     modus-themes-links                          (choice)
;;     modus-themes-mail-citations                 (choice)
;;     modus-themes-mode-line                      (choice)
;;     modus-themes-org-blocks                     (choice)
;;     modus-themes-paren-match                    (choice)
;;     modus-themes-prompts                        (choice)
;;     modus-themes-region                         (choice)
;;
;; There also exist two unique customization variables for overriding
;; color palette values.  The specifics are documented in the manual.
;; The symbols are:
;;
;;     modus-themes-operandi-color-overrides       (alist)
;;     modus-themes-vivendi-color-overrides        (alist)
;;
;; Check the manual for all supported packages (there are hundreds of
;; them).
;;
;; For a complete view of the project, also refer to the following files
;; (should be distributed in the same repository/directory as the
;; current item):
;;
;; - modus-operandi-theme.el    (Light theme)
;; - modus-vivendi-theme.el     (Dark theme)

;;; Code:



(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup modus-themes ()
  "Options for `modus-operandi', `modus-vivendi' themes.
The Modus themes conform with the WCAG AAA standard for color
contrast between background and foreground combinations (a
minimum contrast of 7:1---the highest standard of its kind).  The
themes also strive to empower users with red-green color
deficiency: this is achieved through customization variables that
replace all relevant instances of green with blue, as well as the
overall design of the themes which relies mostly on colors that
cover the blue-cyan-magenta side of the spectrum."
  :group 'faces
  :link '(info-link "(modus-themes) Top")
  :prefix "modus-themes-"
  :tag "Modus Themes")

(defgroup modus-themes-faces ()
  "Faces defined by `modus-operandi' and `modus-vivendi' themes."
  :group 'modus-themes
  :link '(info-link "(modus-themes) Top")
  :prefix "modus-themes-"
  :tag "Modus Themes Faces")

(defvar modus-themes--version "4.0.0-dev"
  "Current version of the Modus themes.

The version either is the last tagged release, such as '1.0.0',
or an in-development version like '1.1.0-dev'.  As we use
semantic versioning, tags of the '1.0.1' sort are not reported:
those would count as part of '1.1.0-dev'.")

;;;###autoload
(defun modus-themes-version (&optional insert)
  "Print `modus-themes--version' in the echo area.
If optional INSERT argument is provided from Lisp or as a prefix
argument, insert the `modus-themes--version' at point."
  (interactive "P")
  (funcall (if insert 'insert 'message) modus-themes--version))

;;;###autoload
(defun modus-themes-report-bug ()
  "Submit a bug report or issue to the Modus themes developers."
  (interactive)
  (reporter-submit-bug-report
   "~protesilaos/modus-themes@lists.sr.ht"
   (format "modus-themes (%s)\n" modus-themes--version)
   ;; I am just getting started with this.  Let's first see what people
   ;; think about it.
   nil nil nil nil))



;;; Custom faces

;; These faces are used internally to ensure consistency between various
;; groups and to streamline the evaluation of relevant customization
;; options.

(dolist (color '(red green yellow blue magenta cyan))
  (custom-declare-face
   (intern (format "modus-themes-subtle-%s" color))
   nil (format "Subtle %s background." color)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (color '(red green yellow blue magenta cyan))
  (custom-declare-face
   (intern (format "modus-themes-intense-%s" color))
   nil (format "Intense %s background." color)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (scope '(alt del sel))
  (custom-declare-face
   (intern (format "modus-themes-mark-%s" scope))
   nil (format "Mark of type %s." scope)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (scope '(note warning error))
  (custom-declare-face
   (intern (format "modus-themes-lang-%s" scope))
   nil (format "Linter or spell check of type %s." scope)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (scope '(current lazy))
  (custom-declare-face
   (intern (format "modus-themes-search-%s" scope))
   nil (format "Search of type %s." scope)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(define-obsolete-variable-alias
  'modus-themes-search-success
  'modus-themes-search-current
  "4.0.0")

(define-obsolete-variable-alias
  'modus-themes-search-success-lazy
  'modus-themes-search-lazy
  "4.0.0")

(dolist (scope '(code macro verbatim))
  (custom-declare-face
   (intern (format "modus-themes-prose-%s" scope))
   nil (format "Construct of type %s for prose." scope)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(define-obsolete-variable-alias
  'modus-themes-markup-code
  'modus-themes-prose-code
  "4.0.0")

(define-obsolete-variable-alias
  'modus-themes-markup-macro
  'modus-themes-prose-macro
  "4.0.0")

(define-obsolete-variable-alias
  'modus-themes-markup-verbatim
  'modus-themes-prose-verbatim
  "4.0.0")

(dotimes (n 9)
  (custom-declare-face
   (intern (format "modus-themes-heading-%d" n))
   nil (format "Level %d heading." n)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(defface modus-themes-bold nil
  "Generic face for applying a conditional bold weight.
This behaves in accordance with `modus-themes-bold-constructs'."
  :package-version '(modus-themes . "4.0.0")
   :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-slant nil
  "Generic face for applying a conditional slant (italics).
This behaves in accordance with `modus-themes-italic-constructs'."
  :package-version '(modus-themes . "4.0.0")
   :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-key-binding nil
  "Face for key bindings."
  :package-version '(modus-themes . "4.0.0")
   :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-fixed-pitch nil
  "Face for `fixed-pitch' if `modus-themes-mixed-fonts' is non-nil."
  :package-version '(modus-themes . "4.0.0")
   :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-ui-variable-pitch nil
  "Face for `variable-pitch' if `modus-themes-variable-pitch-ui' is non-nil."
  :package-version '(modus-themes . "4.0.0")
   :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-reset-soft nil
  "Generic face to set most face properties to nil.

This is intended to be inherited by faces that should not retain
properties from their context (e.g. an overlay over an underlined
text should not be underlined as well) yet still blend in.  Also
see `modus-themes-reset-hard'."
  :group 'modus-themes-faces)

(defface modus-themes-reset-hard nil
  "Generic face to set all face properties to nil.

This is intended to be inherited by faces that should not retain
properties from their context (e.g. an overlay over an underlined
text should not be underlined as well) and not blend in.  Also
see `modus-themes-reset-soft'."
  :group 'modus-themes-faces)

(defface modus-themes-prompt nil
  "Generic face for command prompts."
  :group 'modus-themes-faces)

;; "Grue" is "green" and "blue".
(defface modus-themes-grue nil
  "Generic face for `modus-themes-deuteranopia' foreground."
  :group 'modus-themes-faces)

(defface modus-themes-completion-selected nil
  "Face for current selection in completion UIs."
  :group 'modus-themes-faces)

(dotimes (n 4)
  (custom-declare-face
   (intern (format "modus-themes-completion-match-%d" n))
   nil (format "Completions match level %d." n)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(make-obsolete 'modus-themes-subtle-neutral nil "4.0.0")
(make-obsolete 'modus-themes-intense-neutral nil "4.0.0")
(make-obsolete 'modus-themes-refine-red nil "4.0.0")
(make-obsolete 'modus-themes-refine-green nil "4.0.0")
(make-obsolete 'modus-themes-refine-yellow nil "4.0.0")
(make-obsolete 'modus-themes-refine-blue nil "4.0.0")
(make-obsolete 'modus-themes-refine-magenta nil "4.0.0")
(make-obsolete 'modus-themes-refine-cyan nil "4.0.0")
(make-obsolete 'modus-themes-active-red nil "4.0.0")
(make-obsolete 'modus-themes-active-green nil "4.0.0")
(make-obsolete 'modus-themes-active-yellow nil "4.0.0")
(make-obsolete 'modus-themes-active-blue nil "4.0.0")
(make-obsolete 'modus-themes-active-magenta nil "4.0.0")
(make-obsolete 'modus-themes-active-cyan nil "4.0.0")
(make-obsolete 'modus-themes-fringe-red nil "4.0.0")
(make-obsolete 'modus-themes-fringe-green nil "4.0.0")
(make-obsolete 'modus-themes-fringe-yellow nil "4.0.0")
(make-obsolete 'modus-themes-fringe-blue nil "4.0.0")
(make-obsolete 'modus-themes-fringe-magenta nil "4.0.0")
(make-obsolete 'modus-themes-fringe-cyan nil "4.0.0")
(make-obsolete 'modus-themes-grue-nuanced nil "4.0.0")
(make-obsolete 'modus-themes-red-nuanced nil "4.0.0")
(make-obsolete 'modus-themes-green-nuanced nil "4.0.0")
(make-obsolete 'modus-themes-yellow-nuanced nil "4.0.0")
(make-obsolete 'modus-themes-blue-nuanced nil "4.0.0")
(make-obsolete 'modus-themes-magenta-nuanced nil "4.0.0")
(make-obsolete 'modus-themes-cyan-nuanced nil "4.0.0")
(make-obsolete 'modus-themes-special-calm nil "4.0.0")
(make-obsolete 'modus-themes-special-cold nil "4.0.0")
(make-obsolete 'modus-themes-special-mild nil "4.0.0")
(make-obsolete 'modus-themes-special-warm nil "4.0.0")
(make-obsolete 'modus-themes-diff-added nil "4.0.0")
(make-obsolete 'modus-themes-diff-changed nil "4.0.0")
(make-obsolete 'modus-themes-diff-removed nil "4.0.0")
(make-obsolete 'modus-themes-diff-refine-added nil "4.0.0")
(make-obsolete 'modus-themes-diff-refine-changed nil "4.0.0")
(make-obsolete 'modus-themes-diff-refine-removed nil "4.0.0")
(make-obsolete 'modus-themes-diff-focus-added nil "4.0.0")
(make-obsolete 'modus-themes-diff-focus-changed nil "4.0.0")
(make-obsolete 'modus-themes-diff-focus-removed nil "4.0.0")
(make-obsolete 'modus-themes-diff-heading nil "4.0.0")
(make-obsolete 'modus-themes-pseudo-header nil "4.0.0")
(make-obsolete 'modus-themes-mark-symbol nil "4.0.0")
(make-obsolete 'modus-themes-hl-line nil "4.0.0")
(make-obsolete 'modus-themes-search-success-modeline nil "4.0.0")
(make-obsolete 'modus-themes-grue-active nil "4.0.0")
(make-obsolete 'modus-themes-grue-background-active nil "4.0.0")
(make-obsolete 'modus-themes-grue-background-intense nil "4.0.0")
(make-obsolete 'modus-themes-grue-background-subtle nil "4.0.0")
(make-obsolete 'modus-themes-grue-background-refine nil "4.0.0")
(make-obsolete 'modus-themes-link-broken nil "4.0.0")
(make-obsolete 'modus-themes-link-symlink nil "4.0.0")
(make-obsolete 'modus-themes-tab-backdrop nil "4.0.0")
(make-obsolete 'modus-themes-tab-active nil "4.0.0")
(make-obsolete 'modus-themes-tab-inactive nil "4.0.0")
(make-obsolete 'modus-themes-completion-selected-popup nil "4.0.0")
(make-obsolete 'modus-themes-box-button nil "4.0.0")
(make-obsolete 'modus-themes-box-button-pressed nil "4.0.0")



;;; Customization variables

(defcustom modus-themes-inhibit-reload t
  "Control theme reload when setting options with Customize.

By default, customizing a theme-related user option through the
Custom interfaces or with `customize-set-variable' will not
reload the currently active Modus theme.

Enable this behavior by setting this variable to nil."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Custom reload theme"))

(defun modus-themes--set-option (sym val)
  "Custom setter for theme related user options.
Will set SYM to VAL, and reload the current theme, unless
`modus-themes-inhibit-reload' is non-nil."
  (set-default sym val)
  (unless (or modus-themes-inhibit-reload
              ;; Check if a theme is being loaded, in which case we
              ;; don't want to reload a theme if the setter is
              ;; invoked. `custom--inhibit-theme-enable' is set to nil
              ;; by `enable-theme'.
              (null (bound-and-true-p custom--inhibit-theme-enable)))
    (let ((modus-themes-inhibit-reload t))
      (pcase (modus-themes--current-theme)
        ('modus-operandi (modus-themes-load-operandi))
        ('modus-vivendi (modus-themes-load-vivendi))))))

(make-obsolete 'modus-themes-operandi-color-overrides nil "4.0.0")
(make-obsolete 'modus-themes-vivendi-color-overrides nil "4.0.0")

(defvaralias 'modus-themes-slanted-constructs 'modus-themes-italic-constructs)

(defcustom modus-themes-italic-constructs nil
  "Use italic font forms in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Italic constructs"))

(defcustom modus-themes-bold-constructs nil
  "Use bold text in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Bold constructs"))

(defcustom modus-themes-variable-pitch-ui nil
  "Use proportional fonts (variable-pitch) in UI elements.
This includes the mode line, header line, tab bar, and tab line."
  :group 'modus-themes
  :package-version '(modus-themes . "1.1.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) UI typeface"))

(defcustom modus-themes-mixed-fonts nil
  "Non-nil to enable inheritance from `fixed-pitch' in some faces.

This is done to allow spacing-sensitive constructs, such as Org
tables and code blocks, to remain monospaced when users opt for
something like the command `variable-pitch-mode'.

Users may need to explicitly configure the font family of
`fixed-pitch' in order to get a consistent experience."
  :group 'modus-themes
  :package-version '(modus-themes . "1.7.0")
  :version "29.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Mixed fonts"))

(defcustom modus-themes-intense-mouseovers nil
  "When non-nil use more intense style for mouse hover effects.

This affects the generic `highlight' face which, strictly
speaking, is not limited to mouse usage."
  :group 'modus-themes
  :package-version '(modus-themes . "2.3.0")
  :version "29.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Mouse hover effects"))

(defconst modus-themes--headings-choice
  '(set :tag "Properties" :greedy t
        (const :tag "Proportionately spaced font (variable-pitch)" variable-pitch)
        (choice :tag "Font weight (must be supported by the typeface)"
                (const :tag "Bold (default)" nil)
                (const :tag "Thin" thin)
                (const :tag "Ultra-light" ultralight)
                (const :tag "Extra-light" extralight)
                (const :tag "Light" light)
                (const :tag "Semi-light" semilight)
                (const :tag "Regular" regular)
                (const :tag "Medium" medium)
                (const :tag "Semi-bold" semibold)
                (const :tag "Extra-bold" extrabold)
                (const :tag "Ultra-bold" ultrabold))
        (radio :tag "Height"
               (float :tag "Floating point to adjust height by")
               (cons :tag "Cons cell of `(height . FLOAT)'"
                     (const :tag "The `height' key (constant)" height)
                     (float :tag "Floating point")))
        (choice :tag "Colors"
                (const :tag "Subtle colors" nil)
                (const :tag "Rainbow colors" rainbow)
                (const :tag "Monochrome" monochrome)))
  "Refer to the doc string of `modus-themes-headings'.
This is a helper variable intended for internal use.")

(defcustom modus-themes-headings nil
  "Heading styles with optional list of values for levels 0-8.

This is an alist that accepts a (key . list-of-values)
combination.  The key is either a number, representing the
heading's level (0-8) or t, which pertains to the fallback style.

Level 0 is a special heading: it is used for what counts as a
document title or equivalent, such as the #+title construct we
find in Org files.  Levels 1-8 are regular headings.

The list of values covers symbols that refer to properties, as
described below.  Here is a complete sample, followed by a
presentation of all available properties:

    (setq modus-themes-headings
          (quote ((1 . (variable-pitch 1.5))
                  (2 . (rainbow 1.3))
                  (3 . (1.1))
                  (t . (monochrome)))))

By default (a nil value for this variable), all headings have a
bold typographic weight, use a desaturated text color, have a
font family that is the same as the `default' face (typically
monospaced), and a height that is equal to the `default' face's
height.

A `rainbow' property makes the text color more saturated.

A `monochrome' property makes the heading the same as the base
color, which is that of the `default' face's foreground.  When
`background' is also set, `monochrome' changes its color to gray.
If both `monochrome' and `rainbow' are set, the former takes
precedence.

A `variable-pitch' property changes the font family of the
heading to that of the `variable-pitch' face (normally a
proportionately spaced typeface).

The symbol of a weight attribute adjusts the font of the heading
accordingly, such as `light', `semibold', etc.  Valid symbols are
defined in the variable `modus-themes-weights'.  The absence of a
weight means that bold will be used by virtue of inheriting the
`bold' face (check the manual for tweaking bold and italic
faces).  For backward compatibility, the `no-bold' value is
accepted, though users are encouraged to specify a `regular'
weight instead.

A number, expressed as a floating point (e.g. 1.5), adjusts the
height of the heading to that many times the base font size.  The
default height is the same as 1.0, though it need not be
explicitly stated.  Instead of a floating point, an acceptable
value can be in the form of a cons cell like (height . FLOAT)
or (height FLOAT), where FLOAT is the given number.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (semibold)
    (rainbow background)
    (monochrome semibold 1.3)
    (monochrome semibold (height 1.3)) ; same as above
    (monochrome semibold (height . 1.3)) ; same as above

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-headings
          (quote ((1 . (rainbow 1.5))
                  (2 . (1.3))
                  (t . (semibold)))))

When defining the styles per heading level, it is possible to
pass a non-nil value (t) instead of a list of properties.  This
will retain the original aesthetic for that level.  For example:

    (setq modus-themes-headings
          (quote ((1 . t)           ; keep the default style
                  (2 . (semibold rainbow))
                  (t . (rainbow))))) ; style for all other headings

    (setq modus-themes-headings
          (quote ((1 . (variable-pitch extrabold 1.5))
                  (2 . (rainbow semibold))
                  (t . t)))) ; default style for all other levels"
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type `(alist
          :options ,(mapcar (lambda (el)
                              (list el modus-themes--headings-choice))
                            '(0 1 2 3 4 5 6 7 8 t))
          :key-type symbol
          :value-type ,modus-themes--headings-choice)
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Heading styles"))

(defcustom modus-themes-org-agenda nil
  "Control the style of individual Org agenda constructs.

This is an alist that accepts a (key . value) combination.  Here
is a sample, followed by a description of all possible
combinations:

    (setq modus-themes-org-agenda
          (quote ((header-block . (variable-pitch 1.5 semibold))
                  (header-date . (grayscale workaholic bold-today 1.2))
                  (event . (accented italic varied))
                  (scheduled . uniform)
                  (habit . traffic-light))))

A `header-block' key applies to elements that concern the
headings which demarcate blocks in the structure of the agenda.
By default (a nil value) those are rendered in a bold typographic
weight, plus a height that is slightly taller than the default
font size.  Acceptable values come in the form of a list that can
include either or both of those properties:

- `variable-pitch' to use a proportionately spaced typeface;

- A number as a floating point (e.g. 1.5) to set the height of
  the text to that many times the default font height.  A float
  of 1.0 or the symbol `no-scale' have the same effect of making
  the font the same height as the rest of the buffer.  When
  neither a number nor `no-scale' are present, the default is a
  small increase in height (a value of 1.15).

  Instead of a floating point, an acceptable value can be in the
  form of a cons cell like (height . FLOAT) or (height FLOAT),
  where FLOAT is the given number.

- The symbol of a weight attribute adjusts the font of the
  heading accordingly, such as `light', `semibold', etc.  Valid
  symbols are defined in the variable `modus-themes-weights'.
  The absence of a weight means that bold will be used by virtue
  of inheriting the `bold' face (check the manual for tweaking
  bold and italic faces).

In case both a number and `no-scale' are in the list, the latter
takes precedence.  If two numbers are specified, the first one is
applied.

Example usage:

    (header-block . nil)
    (header-block . (1.5))
    (header-block . (no-scale))
    (header-block . (variable-pitch 1.5))
    (header-block . (variable-pitch 1.5 semibold))

A `header-date' key covers date headings.  Dates use only a
foreground color by default (a nil value), with weekdays and
weekends having a slight difference in hueness.  The current date
has an added gray background.  This key accepts a list of values
that can include any of the following properties:

- `grayscale' to make weekdays use the main foreground color and
  weekends a more subtle gray;

- `workaholic' to make weekdays and weekends look the same in
  terms of color;

- `bold-today' to apply a bold typographic weight to the current
  date;

- `bold-all' to render all date headings in a bold weight;

- `underline-today' applies an underline to the current date
  while removing the background it has by default;

- A number as a floating point (e.g. 1.2) to set the height of
  the text to that many times the default font height.  The
  default is the same as the base font height (the equivalent of
  1.0).  Instead of a floating point, an acceptable value can be
  in the form of a cons cell like (height . FLOAT) or (height
  FLOAT), where FLOAT is the given number.

For example:

    (header-date . nil)
    (header-date . (workaholic))
    (header-date . (grayscale bold-all))
    (header-date . (grayscale workaholic))
    (header-date . (grayscale workaholic bold-today))
    (header-date . (grayscale workaholic bold-today 1.2))

An `event' key covers (i) headings with a plain time stamp that
are shown on the agenda, also known as events, (ii) entries
imported from the diary, and (iii) other items that derive from a
symbolic expression or sexp (phases of the moon, holidays, etc.).
By default all those look the same and have a subtle foreground
color (the default is a nil value or an empty list).  This key
accepts a list of properties.  Those are:

- `accented' applies an accent value to the event's foreground,
  replacing the original gray.  It makes all entries stand out more.
- `italic' adds a slant to the font's forms (italic or oblique
  forms, depending on the typeface).
- `varied' differentiates between events with a plain time stamp
  and entries that are generated from either the diary or a
  symbolic expression.  It generally puts more emphasis on
  events.  When `varied' is combined with `accented', it makes
  only events use an accent color, while diary/sexp entries
  retain their original subtle foreground.  When `varied' is used
  in tandem with `italic', it applies a slant only to diary and
  sexp entries, not events.  And when `varied' is the sole
  property passed to the `event' key, it has the same meaning as
  the list (italic varied).  The combination of `varied',
  `accented', `italic' covers all of the aforementioned cases.

For example:

    (event . nil)
    (event . (italic))
    (event . (accented italic))
    (event . (accented italic varied))

A `scheduled' key applies to tasks with a scheduled date.  By
default (a nil value), these use varying shades of yellow to
denote (i) a past or current date and (ii) a future date.  Valid
values are symbols:

- nil (default);
- `uniform' to make all scheduled dates the same color;
- `rainbow' to use contrasting colors for past, present, future
  scheduled dates.

For example:

    (scheduled . nil)
    (scheduled . uniform)
    (scheduled . rainbow)

A `habit' key applies to the `org-habit' graph.  All possible
value are passed as a symbol.  Those are:

- The default (nil) is meant to conform with the original
  aesthetic of `org-habit'.  It employs all four color codes that
  correspond to the org-habit states---clear, ready, alert, and
  overdue---while distinguishing between their present and future
  variants.  This results in a total of eight colors in use: red,
  yellow, green, blue, in tinted and shaded versions.  They cover
  the full set of information provided by the `org-habit'
  consistency graph.

- `simplified' is like the default except that it removes the
  dichotomy between current and future variants by applying
  uniform color-coded values.  It applies a total of four colors:
  red, yellow, green, blue.  They produce a simplified
  consistency graph that is more legible (or less \"busy\") than
  the default.  The intent is to shift focus towards the
  distinction between the four states of a habit task, rather
  than each state's present/future outlook.

- `traffic-light' further reduces the available colors to red,
  yellow, and green.  As in `simplified', present and future
  variants appear uniformly, but differently from it, the CLEAR
  state is rendered in a green hue, instead of the original blue.
  This is meant to capture the use-case where a habit task being
  too early is less important than it being too late.  The
  difference between READY and CLEAR states is attenuated by
  painting both of them using shades of green.  This option thus
  highlights the alert and overdue states.

- When `modus-themes-deuteranopia' is non-nil the exact style of
  the habit graph adapts to the needs of users with red-green
  color deficiency by substituting every instance of green with
  blue or cyan (depending on the specifics).

For example:

    (habit . nil)
    (habit . simplified)
    (habit . traffic-light)"
  :group 'modus-themes
  :package-version '(modus-themes . "2.3.0")
  :version "29.1"
  :type '(set
          (cons :tag "Block header"
                (const header-block)
                (set :tag "Header presentation" :greedy t
                     (choice :tag "Font style"
                             (const :tag "Use the original typeface (default)" nil)
                             (const :tag "Use `variable-pitch' font" variable-pitch))
                     (choice :tag "Font weight (must be supported by the typeface)"
                             (const :tag "Bold (default)" nil)
                             (const :tag "Thin" thin)
                             (const :tag "Ultra-light" ultralight)
                             (const :tag "Extra-light" extralight)
                             (const :tag "Light" light)
                             (const :tag "Semi-light" semilight)
                             (const :tag "Regular" regular)
                             (const :tag "Medium" medium)
                             (const :tag "Semi-bold" semibold)
                             (const :tag "Extra-bold" extrabold)
                             (const :tag "Ultra-bold" ultrabold))
                     (radio :tag "Scaling"
                             (const :tag "Slight increase in height (default)" nil)
                             (const :tag "Do not scale" no-scale)
                             (radio :tag "Number (float) to adjust height by"
                                    (float :tag "Just the number")
                                    (cons :tag "Cons cell of `(height . FLOAT)'"
                                          (const :tag "The `height' key (constant)" height)
                                          (float :tag "Floating point"))))))
          (cons :tag "Date header" :greedy t
                (const header-date)
                (set :tag "Header presentation" :greedy t
                     (const :tag "Use grayscale for date headers" grayscale)
                     (const :tag "Do not differentiate weekdays from weekends" workaholic)
                     (const :tag "Make today bold" bold-today)
                     (const :tag "Make all dates bold" bold-all)
                     (const :tag "Make today underlined; remove the background" underline-today)
                     (radio :tag "Number (float) to adjust height by"
                                    (float :tag "Just the number")
                                    (cons :tag "Cons cell of `(height . FLOAT)'"
                                          (const :tag "The `height' key (constant)" height)
                                          (float :tag "Floating point")))))
          (cons :tag "Event entry" :greedy t
                (const event)
                (set :tag "Text presentation" :greedy t
                     (const :tag "Apply an accent color" accented)
                     (const :tag "Italic font slant (oblique forms)" italic)
                     (const :tag "Differentiate events from diary/sexp entries" varied)))
          (cons :tag "Scheduled tasks"
                (const scheduled)
                (choice (const :tag "Yellow colors to distinguish current and future tasks (default)" nil)
                        (const :tag "Uniform subtle warm color for all scheduled tasks" uniform)
                        (const :tag "Rainbow-colored scheduled tasks" rainbow)))
          (cons :tag "Habit graph"
                (const habit)
                (choice (const :tag "Follow the original design of `org-habit' (default)" nil)
                        (const :tag "Do not distinguish between present and future variants" simplified)
                        (const :tag "Use only red, yellow, green" traffic-light))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Org agenda"))

(defcustom modus-themes-fringes 'subtle
  "Control the visibility of fringes.

When the value is nil, do not apply a distinct background color.

With a value of `subtle' use a gray background color that is
visible yet close to the main background color.

With `intense' use a more pronounced gray background color."
  :group 'modus-themes
  :package-version '(modus-themes . "3.0.0")
  :version "29.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "No visible fringes" nil)
          (const :format "[%v] %t\n" :tag "Subtle gray background" subtle)
          (const :format "[%v] %t\n" :tag "Intense gray background" intense))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Fringes"))

(make-obsolete 'modus-themes-lang-checkers nil "4.0.0")

(defcustom modus-themes-org-blocks nil
  "Set the overall style of Org code blocks, quotes, and the like.

Nil (the default) means that the block has no background of its
own: it uses the one that applies to the rest of the buffer.  In
this case, the delimiter lines have a gray color for their text,
making them look exactly like all other Org properties.

Option `gray-background' applies a subtle gray background to the
block's contents.  It also affects the begin and end lines of the
block as they get another shade of gray as their background,
which differentiates them from the contents of the block.  All
background colors extend to the edge of the window, giving the
area a rectangular, \"blocky\" presentation.

Option `tinted-background' uses a slightly colored background for
the contents of the block.  The exact color will depend on the
programming language and is controlled by the variable
`org-src-block-faces' (refer to the theme's source code for the
current association list).  For this to take effect, the Org
buffer needs to be restarted with `org-mode-restart'.  In this
scenario, it may be better to inhibit the extension of the
delimiter lines' background to the edge of the window because Org
does not provide a mechanism to update their colors depending on
the contents of the block.  Disable the extension of such
backgrounds by setting `org-fontify-whole-block-delimiter-line'
to nil.

Code blocks use their major mode's colors only when the variable
`org-src-fontify-natively' is non-nil.  While quote/verse blocks
require setting `org-fontify-quote-and-verse-blocks' to a non-nil
value.

Older versions of the themes provided options `grayscale' (or
`greyscale') and `rainbow'.  Those will continue to work as they
are aliases for `gray-background' and `tinted-background',
respectively."
  :group 'modus-themes
  :package-version '(modus-themes . "2.1.0")
  :version "28.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "No Org block background (default)" nil)
          (const :format "[%v] %t\n" :tag "Subtle gray block background" gray-background)
          (const :format "[%v] %t\n" :tag "Alias for `gray-background'" grayscale) ; for backward compatibility
          (const :format "[%v] %t\n" :tag "Alias for `gray-background'" greyscale)
          (const :format "[%v] %t\n" :tag "Color-coded background per programming language" tinted-background)
          (const :format "[%v] %t\n" :tag "Alias for `tinted-background'" rainbow)) ; back compat
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Org mode blocks"))

(defcustom modus-themes-mode-line nil
  "Control the overall style of the mode line.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a two-dimensional
rectangle with a border around it.  The active and the inactive
mode lines use different shades of grayscale values for the
background, foreground, border.

The `3d' property applies a three-dimensional effect to the
active mode line.  The inactive mode lines remain two-dimensional
and are toned down a bit, relative to the default style.

The `moody' property optimizes the mode line for use with the
library of the same name (hereinafter referred to as Moody).
In practice, it removes the box effect and replaces it with
underline and overline properties.  It also tones down the
inactive mode lines.  Despite its intended purpose, this option
can also be used without the Moody library (please consult the
themes' manual on this point for more details).  If both `3d' and
`moody' properties are set, the latter takes precedence.

The `borderless' property removes the color of the borders.  It
does not actually remove the borders, but only makes their color
the same as the background, effectively creating some padding.

The `accented' property ensures that the active mode line uses a
colored background instead of the standard shade of gray.

A positive integer (natural number or natnum) applies a padding
effect of NATNUM pixels at the boundaries of the mode lines.  The
default value is 1 and does not need to be specified explicitly.
The padding has no effect when the `moody' property is also used,
because Moody already applies its own tweaks.  To ensure that the
underline is placed at the bottom of the mode line, set
`x-underline-at-descent-line' to non-nil (this is not needed when
the `borderless' property is also set).  For users on Emacs 29,
the `x-use-underline-position-properties' variable must also be
set to nil.

The padding can also be expressed as a cons cell in the form
of (padding . NATNUM) or (padding NATNUM) where the key is
constant and NATNUM is the desired natural number.

A floating point (e.g. 0.9) applies an adjusted height to the
mode line's text as a multiple of the main font size.  The
default rate is 1.0 and does not need to be specified.  Apart
from a floating point, the height may also be expressed as a cons
cell in the form of (height . FLOAT) or (height FLOAT) where the
key is constant and the FLOAT is the desired number.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (accented)
    (borderless 3d)
    (moody accented borderless)

Same as above, using the padding and height as an example (these
all yield the same result):

    (accented borderless 4 0.9)
    (accented borderless (padding . 4) (height . 0.9))
    (accented borderless (padding 4) (height 0.9))

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-mode-line (quote (borderless accented)))

Note that Moody does not expose any faces that the themes could
style directly.  Instead it re-purposes existing ones to render
its tabs and ribbons.  As such, there may be cases where the
contrast ratio falls below the 7:1 target that the themes conform
with (WCAG AAA).  To hedge against this, we configure a fallback
foreground for the `moody' property, which will come into effect
when the background of the mode line changes to something less
accessible, such as Moody ribbons (read the doc string of
`set-face-attribute', specifically `:distant-foreground').  This
fallback is activated when Emacs determines that the background
and foreground of the given construct are too close to each other
in terms of color distance.  In practice, users will need to
experiment with the variable `face-near-same-color-threshold' to
trigger the effect.  We find that a value of 45000 shall suffice,
contrary to the default 30000.  Though for the combinations that
involve the `accented' and `moody' properties, as mentioned
above, that should be raised up to 70000.  Do not set it too
high, because it has the adverse effect of always overriding the
default colors (which have been carefully designed to be highly
accessible).

Furthermore, because Moody expects an underline and overline
instead of a box style, it is strongly advised to set
`x-underline-at-descent-line' to a non-nil value."
  :group 'modus-themes
  :package-version '(modus-themes . "2.3.0")
  :version "29.1"
  :type '(set :tag "Properties" :greedy t
              (choice :tag "Overall style"
                      (const :tag "Rectangular Border" nil)
                      (const :tag "3d borders" 3d)
                      (const :tag "No box effects (Moody-compatible)" moody))
              (const :tag "Colored background" accented)
              (const :tag "Without border color" borderless)
              (radio :tag "Padding"
               (natnum :tag "Natural number (e.g. 4)")
               (cons :tag "Cons cell of `(padding . NATNUM)'"
                     (const :tag "The `padding' key (constant)" padding)
                     (natnum :tag "Natural number")))
              (radio :tag "Height"
               (float :tag "Floating point (e.g. 0.9)")
               (cons :tag "Cons cell of `(height . FLOAT)'"
                     (const :tag "The `height' key (constant)" height)
                     (float :tag "Floating point"))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Mode line"))

(defcustom modus-themes-diffs nil
  "Adjust the overall style of diffs.

The default (nil) uses fairly intense color combinations for
diffs, by applying prominently colored backgrounds, with
appropriately tinted foregrounds.

Option `desaturated' follows the same principles as with the
default (nil), though it tones down all relevant colors.

Option `bg-only' applies a background but does not override the
text's foreground.  This makes it suitable for a non-nil value
passed to `diff-font-lock-syntax' (note: Magit does not support
syntax highlighting in diffs---last checked on 2021-12-02).

When the user option `modus-themes-deuteranopia' is non-nil, all
diffs will use a red/blue color-coding system instead of the
standard red/green.  Other stylistic changes are made in the
interest of optimizing for such a use-case."
  :group 'modus-themes
  :package-version '(modus-themes . "2.0.0")
  :version "29.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "Intensely colored backgrounds (default)" nil)
          (const :format "[%v] %t\n" :tag "Slightly accented backgrounds with tinted text" desaturated)
          (const :format "[%v] %t\n" :tag "Apply color-coded backgrounds; keep syntax colors intact" bg-only))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Diffs"))

(defcustom modus-themes-completions
  '((selection . (intense))
    (popup . (intense)))
  "Control the style of completion user interfaces.

This affects Company, Corfu, Flx, Helm, Icomplete/Fido, Ido, Ivy,
Orderless, Vertico.  The value is an alist that takes the form of
a (KEY . PROPERTIES) combination.  KEY is a symbol, while
PROPERTIES is a list.  Here is a sample, followed by a
description of the particularities:

    (setq modus-themes-completions
          (quote ((matches . (extrabold background intense))
                  (selection . (semibold accented intense))
                  (popup . (accented)))))

The `matches' key refers to the highlighted characters that
correspond to the user's input.  When its properties are nil or
an empty list, matching characters in the user interface will
have a bold weight and a colored foreground.  The list of
properties may include any of the following symbols regardless of
the order they may appear in:

- `background' to add a background color;

- `intense' to increase the overall coloration (also amplifies
  the `background', if present);

- `underline' to draw a line below the characters;

- `italic' to use a slanted font (italic or oblique forms);

- The symbol of a font weight attribute such as `light',
  `semibold', et cetera.  Valid symbols are defined in the
  variable `modus-themes-weights'.  The absence of a weight means
  that bold will be used.

The `selection' key applies to the current line or currently
matched candidate, depending on the specifics of the user
interface.  When its properties are nil or an empty list, it has
a subtle gray background, a bold weight, and the base foreground
value for the text.  The list of properties it accepts is as
follows (order is not significant):

- `accented' to make the background colorful instead of gray;

- `text-also' to apply extra color to the text of the selected
  line;

- `intense' to increase the overall coloration;

- `underline' to draw a line below the characters;

- `italic' to use a slanted font (italic or oblique forms);

- The symbol of a font weight attribute such as `light',
  `semibold', et cetera.  Valid symbols are defined in the
  variable `modus-themes-weights'.  The absence of a weight means
  that bold will be used.

The `popup' key takes the same values as `selection'.  The only
difference is that it applies specifically to user interfaces
that display an inline popup and thus have slightly different
styling requirements than the minibuffer.  The two prominent
packages are `company' and `corfu'.

Apart from specifying each key separately, a fallback list is
accepted.  This is only useful when the desired aesthetic is the
same across all keys that are not explicitly referenced.  For
example, this:

    (setq modus-themes-completions
          (quote ((t . (extrabold intense)))))

Is the same as:

    (setq modus-themes-completions
          (quote ((matches . (extrabold intense))
                  (selection . (extrabold intense))
                  (popup . (extrabold intense)))))

In the case of the fallback, any property that does not apply to
the corresponding key is simply ignored (`matches' does not have
`accented' and `text-also', while `selection' and `popup' do not
have `background').

Check the manual for tweaking `bold' and `italic' faces: Info
node `(modus-themes) Configure bold and italic faces'.

Also refer to the documentation of the `orderless' package for
its intersection with `company' (if you choose to use those in
tandem)."
  :group 'modus-themes
  :package-version '(modus-themes . "3.0.0")
  :version "29.1"
  :type `(set
          (cons :tag "Matches"
                (const matches)
                (set :tag "Style of matches" :greedy t
                     (choice :tag "Font weight (must be supported by the typeface)"
                             (const :tag "Bold (default)" nil)
                             (const :tag "Thin" thin)
                             (const :tag "Ultra-light" ultralight)
                             (const :tag "Extra-light" extralight)
                             (const :tag "Light" light)
                             (const :tag "Semi-light" semilight)
                             (const :tag "Regular" regular)
                             (const :tag "Medium" medium)
                             (const :tag "Semi-bold" semibold)
                             (const :tag "Extra-bold" extrabold)
                             (const :tag "Ultra-bold" ultrabold))
                     (const :tag "With added background" background)
                     (const :tag "Increased coloration" intense)
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline)))
          (cons :tag "Selection"
                (const selection)
                (set :tag "Style of selection" :greedy t
                     (choice :tag "Font weight (must be supported by the typeface)"
                             (const :tag "Bold (default)" nil)
                             (const :tag "Thin" thin)
                             (const :tag "Ultra-light" ultralight)
                             (const :tag "Extra-light" extralight)
                             (const :tag "Light" light)
                             (const :tag "Semi-light" semilight)
                             (const :tag "Regular" regular)
                             (const :tag "Medium" medium)
                             (const :tag "Semi-bold" semibold)
                             (const :tag "Extra-bold" extrabold)
                             (const :tag "Ultra-bold" ultrabold))
                     (const :tag "Apply color to the line's text" text-also)
                     (const :tag "With accented background" accented)
                     (const :tag "Increased coloration" intense)
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline)))
          (cons :tag "Popup"
                (const popup)
                (set :tag "Style of completion pop-ups" :greedy t
                     (choice :tag "Font weight (must be supported by the typeface)"
                             (const :tag "Bold (default)" nil)
                             (const :tag "Thin" thin)
                             (const :tag "Ultra-light" ultralight)
                             (const :tag "Extra-light" extralight)
                             (const :tag "Light" light)
                             (const :tag "Semi-light" semilight)
                             (const :tag "Regular" regular)
                             (const :tag "Medium" medium)
                             (const :tag "Semi-bold" semibold)
                             (const :tag "Extra-bold" extrabold)
                             (const :tag "Ultra-bold" ultrabold))
                     (const :tag "Apply color to the line's text" text-also)
                     (const :tag "With accented background" accented)
                     (const :tag "Increased coloration" intense)
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Completion UIs"))

(defcustom modus-themes-prompts nil
  "Use subtle or intense styles for minibuffer and REPL prompts.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) means to only use a
subtle accented foreground color.

The property `background' applies a background color to the
prompt's text.  By default, this is a subtle accented value.

The property `bold' makes the text use a bold typographic weight.
Similarly, `italic' adds a slant to the font's forms (italic or
oblique forms, depending on the typeface).

Combinations of any of those properties are expressed as a list,
like in these examples:

    (background)
    (bold italic)
    (italic bold background)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-prompts (quote (background italic)))"
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "With Background" background)
              (const :tag "Bold font weight" bold)
              (const :tag "Italic font slant" italic))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Command prompts"))

(defcustom modus-themes-hl-line '(intense)
  "Control the current line highlight of `hl-line-mode'.

The value is a list of properties, each designated by a symbol.
With a nil value, or an empty list, the style is a subtle gray
background color.

The property `accented' changes the background to a colored
variant.

An `underline' property draws a line below the highlighted area.
Its color is similar to the background, so gray by default or an
accent color when `accented' is also set.

An `intense' property amplifies the colors in use, which may be
both the background and the underline.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (intense)
    (underline intense)
    (accented intense underline)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-hl-line (quote (underline accented)))

Set `x-underline-at-descent-line' to a non-nil value so that the
placement of the underline coincides with the lower boundary of
the colored background."
  :group 'modus-themes
  :package-version '(modus-themes . "3.0.0")
  :version "29.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Colored background" accented)
              (const :tag "Underline" underline)
              (const :tag "Intense style" intense))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Line highlighting"))

(defcustom modus-themes-subtle-line-numbers nil
  "Use more subtle style for command `display-line-numbers-mode'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.2.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Line numbers"))

(make-obsolete 'modus-themes-markup nil "4.0.0")

(defcustom modus-themes-paren-match nil
  "Control the style of matching parentheses or delimiters.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a subtle background
color.

The `bold' property adds a bold weight to the characters of the
matching delimiters.

The `intense' property applies a more prominent background color
to the delimiters.

The `underline' property draws a straight line under the affected
text.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (bold)
    (underline intense)
    (bold intense underline)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-paren-match (quote (bold intense)))"
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Bold weight" bold)
              (const :tag "Intense background color" intense)
              (const :tag "Underline" underline))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Matching parentheses"))

(make-obsolete 'modus-themes-syntax nil "4.0.0")

(defcustom modus-themes-links nil
  "Set the style of links.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a prominent text
color, typically blue, with an underline of the same color.

For the style of the underline, a `neutral-underline' property
turns the color of the line into a subtle gray, while the
`no-underline' property removes the line altogether.  If both of
those are set, the latter takes precedence.

For text coloration, a `faint' property desaturates the color of
the text and the underline, unless the underline is affected by
the aforementioned properties.  While a `no-color' property
removes the color from the text.  If both of those are set, the
latter takes precedence.

A `bold' property applies a heavy typographic weight to the text
of the link.

An `italic' property adds a slant to the link's text (italic or
oblique forms, depending on the typeface).

A `background' property applies a subtle tinted background color.

In case both `no-underline' and `no-color' are set, then a subtle
gray background is applied to all links.  This can still be
combined with the `bold' and `italic' properties.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (faint)
    (no-underline faint)
    (no-color no-underline bold)
    (italic bold background no-color no-underline)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-links (quote (neutral-underline background)))

The placement of the underline, meaning its proximity to the
text, is controlled by `x-use-underline-position-properties',
`x-underline-at-descent-line', `underline-minimum-offset'.
Please refer to their documentation strings."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (choice :tag "Text coloration"
                      (const :tag "Saturared color (default)" nil)
                      (const :tag "Faint coloration" faint)
                      (const :tag "No color (use main black/white)" no-color))
              (choice :tag "Underline"
                      (const :tag "Same color as text (default)" nil)
                      (const :tag "Neutral (gray) underline color" neutral-underline)
                      (const :tag "No underline" no-underline))
              (const :tag "Bold font weight" bold)
              (const :tag "Italic font slant" italic)
              (const :tag "Subtle background color" background))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Link styles"))

(defcustom modus-themes-region nil
  "Control the overall style of the active region.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a prominent gray
background that overrides all foreground colors in the area it
encompasses.  Its reach extends to the edge of the window.

The `no-extend' property limits the region to the end of the
line, so that it does not reach the edge of the window.

The `bg-only' property makes the region's background color more
subtle to allow the underlying text to retain its foreground
colors.

The `accented' property applies a more colorful background to the
region.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (no-extend)
    (bg-only accented)
    (accented bg-only no-extend)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-region (quote (bg-only no-extend)))"
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Do not extend to the edge of the window" no-extend)
              (const :tag "Background only (preserve underlying colors)" bg-only)
              (const :tag "Accented background" accented))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Active region"))

(defcustom modus-themes-deuteranopia nil
  "When non-nil use red/blue color-coding instead of red/green.

This is to account for red-green color deficiency, also know as
deuteranopia and variants.  It applies to all contexts where
there can be a color-coded distinction between failure or
success, a to-do or done state, a mark for deletion versus a mark
for selection (e.g. in Dired), current and lazily highlighted
search matches, removed lines in diffs as opposed to added ones,
and so on.

Note that this does not change all colors throughout the active
theme, but only applies to cases that have color-coding
significance.  For example, regular code syntax highlighting is
not affected.  There is no such need because of the themes'
overarching commitment to the highest legibility standard, which
ensures that text is readable regardless of hue, as well as the
predominance of colors on the blue-cyan-magenta-purple side of
the spectrum."
  :group 'modus-themes
  :package-version '(modus-themes . "2.0.0")
  :version "29.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Deuteranopia style"))

(make-obsolete 'modus-themes-mail-citations nil "4.0.0")
(make-obsolete 'modus-themes-tabs-accented nil "4.0.0")
(make-obsolete 'modus-themes-box-buttons nil "4.0.0")



;;; Internal functions

(defun modus-themes--warn (option)
  "Warn that OPTION has changed."
  (prog1 nil
    (display-warning
     'modus-themes
     (format "`%s' has changed; please read the updated documentation" option)
     :warning)))

(defun modus-themes--list-or-warn (option)
  "Return list or nil value of OPTION, else `modus-themes--warn'."
  (let* ((value (symbol-value option)))
    (if (or (null value) (listp value))
        value
      (modus-themes--warn option))))

(defun modus-themes--property-lookup (properties alist-key list-pred default)
  "Return value from property alist or list.
Check PROPERTIES for an alist value that corresponds to
ALIST-KEY.  If no alist is present, search the PROPERTIES
list given LIST-PRED, using DEFAULT as a fallback."
  (if-let* ((val (or (alist-get alist-key properties)
                     (cl-loop for x in properties
                              if (funcall list-pred x) return x)
                     default))
            ((listp val)))
      (car val)
    val))

(defun modus-themes--palette (theme)
  "Return THEME palette as a symbol."
  (when theme
    (intern (format "%s-palette" theme))))

(defun modus-themes--current-theme-palette ()
  "Return palette of active Ef theme, else produce `user-error'."
  (if-let* ((palette (modus-themes--palette (modus-themes--current-theme))))
      (symbol-value palette)
    (user-error "No enabled Modus theme could be found")))

(defun modus-themes--current-theme ()
  "Return current modus theme."
  (car
   (seq-filter
    (lambda (theme)
      (string-match-p "^modus" (symbol-name theme)))
    custom-enabled-themes)))

;; Helper functions that are meant to ease the implementation of the
;; above customization variables.
(defun modus-themes--bold-weight ()
  "Conditional use of a heavier text weight."
  (when modus-themes-bold-constructs
    (list :inherit 'bold)))

(defun modus-themes--slant ()
  "Conditional use of italics for slant attribute."
  (when modus-themes-italic-constructs
    (list :inherit 'italic)))

(defun modus-themes--fixed-pitch ()
  "Conditional application of `fixed-pitch' inheritance."
  (when modus-themes-mixed-fonts
    (list :inherit 'fixed-pitch)))

(defun modus-themes--variable-pitch-ui ()
  "Conditional use of `variable-pitch' in UI elements."
  (when modus-themes-variable-pitch-ui
    (list :inherit 'variable-pitch)))

(defun modus-themes--fringe (mainbg subtlebg intensebg)
  "Conditional use of background colors for fringes.
MAINBG is the default.  SUBTLEBG should be a subtle grayscale
value.  INTENSEBG must be a more pronounced grayscale color."
  (pcase modus-themes-fringes
    ('intense (list :background intensebg))
    ('subtle (list :background subtlebg))
    (_ (list :background mainbg))))

(defun modus-themes--line-numbers (mainfg mainbg altfg &optional altbg)
  "Conditional use of colors for line numbers.
MAINBG and MAINFG are the default colors.  ALTFG is a color that
combines with the theme's primary background (white/black)."
  (if modus-themes-subtle-line-numbers
      (list :background (or altbg 'unspecified) :foreground altfg)
    (list :background mainbg :foreground mainfg)))

(defun modus-themes--prompt (fg bg)
  "Conditional use of colors for text prompt faces.
FG is the prompt's standard foreground.  BG is a background
color that is combined with FG-FOR-BG."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-prompts)))
    (list :background
          (if (memq 'background properties) bg 'unspecified)
          :foreground
          (if (memq 'background properties) 'unspecified fg)
          :inherit
          (cond
           ((and (memq 'bold properties)
                 (memq 'italic properties))
            'bold-italic)
           ((memq 'italic properties)
            'italic)
           ((memq 'bold properties)
            'bold)
           ('unspecified)))))

(defun modus-themes--paren (normalbg intensebg)
  "Conditional use of intense colors for matching parentheses.
NORMALBG should be the special palette color bg-paren-match or
something similar.  INTENSEBG must be easier to discern next to
other backgrounds, such as the special palette color
bg-paren-match-intense."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-paren-match)))
    (list :inherit
          (if (memq 'bold properties)
              'bold
            'unspecified)
          :background
          (if (memq 'intense properties)
              intensebg
            normalbg)
          :underline
          (if (memq 'underline properties)
              t
            nil))))

(defun modus-themes--key-cdr (key alist)
  "Get cdr of KEY in ALIST."
  (cdr (assoc key alist)))

(defconst modus-themes-weights
  '( thin ultralight extralight light semilight regular medium
     semibold bold heavy extrabold ultrabold)
  "List of font weights.")

(defun modus-themes--weight (list)
  "Search for `modus-themes--heading' weight in LIST."
  (catch 'found
    (dolist (elt list)
      (when (memq elt modus-themes-weights)
        (throw 'found elt)))))

(defun modus-themes--heading (level fg fg-alt)
  "Conditional styles for `modus-themes-headings'.

LEVEL is the heading's position in their order.  FG is the
default text color.  FG-ALT is an accented, more saturated value
than the default."
  (let* ((key (modus-themes--key-cdr level modus-themes-headings))
         (style (or key (modus-themes--key-cdr t modus-themes-headings)))
         (style-listp (listp style))
         (properties style)
         (var (when (memq 'variable-pitch properties) 'variable-pitch))
         (varbold (if var
                      (append (list 'bold) (list var))
                    'bold))
         (weight (when style-listp (modus-themes--weight style))))
    (list :inherit
          (cond
           ;; `no-bold' is for backward compatibility because we cannot
           ;; deprecate a variable's value.
           ((or weight (memq 'no-bold properties))
            var)
           (varbold))
          :foreground
          (cond
           ((memq 'monochrome properties)
            'unspecified)
           ((memq 'rainbow properties)
            fg-alt)
           (fg))
          :height
          (modus-themes--property-lookup properties 'height #'floatp 'unspecified)
          :weight
          (or weight 'unspecified))))

(defun modus-themes--agenda-structure (fg)
  "Control the style of the Org agenda structure.
FG is the foreground color to use."
  (let* ((properties (modus-themes--key-cdr 'header-block modus-themes-org-agenda))
         (weight (modus-themes--weight properties)))
    (list :inherit
          (cond
           ((and weight (memq 'variable-pitch properties))
            'variable-pitch)
           (weight 'unspecified)
           ((memq 'variable-pitch properties)
            (list 'bold 'variable-pitch))
           ('bold))
          :weight
          (or weight 'unspecified)
          :height
          (cond ((memq 'no-scale properties) 'unspecified)
                ((modus-themes--property-lookup properties 'height #'floatp 1.15)))
          :foreground fg)))

(defun modus-themes--agenda-date (defaultfg grayscalefg &optional workaholicfg grayscaleworkaholicfg bg bold ul)
  "Control the style of date headings in Org agenda buffers.
DEFAULTFG is the original accent color for the foreground.
GRAYSCALEFG is a neutral color.  Optional WORKAHOLICFG and
GRAYSCALEWORKAHOLICFG are alternative foreground colors.
Optional BG is a background color.  Optional BOLD applies a bold
weight.  Optional UL applies an underline."
  (let ((properties (modus-themes--key-cdr 'header-date modus-themes-org-agenda)))
    (list :inherit
          (cond
           ((or (memq 'bold-all properties)
                (and bold (memq 'bold-today properties)))
            'bold)
           (t
            'unspecified))
          :background
          (cond
           ((memq 'underline-today properties)
            'unspecified)
           ((or bg 'unspecified)))
          :foreground
          (cond
           ((and (memq 'grayscale properties)
                 (memq 'workaholic properties))
            (or grayscaleworkaholicfg grayscalefg))
           ((memq 'grayscale properties)
            grayscalefg)
           ((memq 'workaholic properties)
            (or workaholicfg defaultfg))
           (t
            defaultfg))
          :height
          (modus-themes--property-lookup properties 'height #'floatp 'unspecified)
          :underline
          (if (and ul (memq 'underline-today properties))
              t
            'unspecified))))

(defun modus-themes--agenda-event (fg-accent &optional varied)
  "Control the style of the Org agenda events.
FG-ACCENT is the accent color to use.  Optional VARIED is a
toggle to behave in accordance with the semantics of the `varied'
property that the `event' key accepts in
`modus-themes-org-agenda'."
  (let ((properties (modus-themes--key-cdr 'event modus-themes-org-agenda)))
    (list :foreground
          (cond
           ((or (and (memq 'varied properties) varied)
                (and (memq 'accented properties)
                     (memq 'varied properties)
                     varied))
            'unspecified)
           ((memq 'accented properties)
            fg-accent)
           ('unspecified))
          :inherit
          (cond
           ((and (memq 'italic properties)
                 (memq 'varied properties)
                 varied)
            '(shadow italic))
           ((and (memq 'accented properties)
                 (memq 'varied properties)
                 varied)
            'shadow)
           ((or (and (memq 'varied properties) varied)
                (and (memq 'italic properties) varied))
            '(shadow italic))
           ((and (memq 'italic properties)
                 (not (memq 'varied properties)))
            '(shadow italic))
           ('shadow)))))

(defun modus-themes--agenda-scheduled (defaultfg uniformfg rainbowfg)
  "Control the style of the Org agenda scheduled tasks.
DEFAULTFG is an accented foreground color that is meant to
differentiate between past or present and future tasks.
UNIFORMFG is a more subtle color that eliminates the color coding
for scheduled tasks.  RAINBOWFG is a prominent accent value that
clearly distinguishes past, present, future tasks."
  (pcase (modus-themes--key-cdr 'scheduled modus-themes-org-agenda)
    ('uniform (list :foreground uniformfg))
    ('rainbow (list :foreground rainbowfg))
    (_ (list :foreground defaultfg))))

(defun modus-themes--agenda-habit (default traffic simple &optional default-d traffic-d simple-d)
  "Specify background values for `modus-themes-org-agenda' habits.
DEFAULT is the original foregrounc color.  TRAFFIC is to be used
when the traffic-light style is applied, while SIMPLE corresponds
to the simplified style.

Optional DEFAULT-D, TRAFFIC-D, SIMPLE-D are alternatives to the
main colors, meant for dopia when `modus-themes-deuteranopia' is
non-nil."
  (let ((habit (modus-themes--key-cdr 'habit modus-themes-org-agenda)))
    (cond
     ((and modus-themes-deuteranopia (null habit))
      (list :background (or default-d default)))
     ((and modus-themes-deuteranopia (eq habit 'traffic-light))
      (list :background (or traffic-d traffic)))
     ((and modus-themes-deuteranopia (eq habit 'simplified))
      (list :background (or simple-d simple)))
     (t
      (pcase habit
        ('traffic-light (list :background traffic))
        ('simplified (list :background simple))
        (_ (list :background default)))))))

(defun modus-themes--org-block (bgblk fgdefault &optional fgblk)
  "Conditionally set the background of Org blocks.
BGBLK applies to a distinct neutral background.  Else blocks have
no background of their own (the default), so they look the same
as the rest of the buffer.  FGDEFAULT is used when no distinct
background is present.  While optional FGBLK specifies a
foreground value that can be combined with BGBLK.

`modus-themes-org-blocks' also accepts `tinted-background' (alias
`rainbow') as a value which applies to `org-src-block-faces' (see
the theme's source code)."
  (if (or (eq modus-themes-org-blocks 'gray-background)
          (eq modus-themes-org-blocks 'grayscale)
          (eq modus-themes-org-blocks 'greyscale))
      (list :background bgblk :foreground (or fgblk fgdefault) :extend t)
    (list :background 'unspecified :foreground fgdefault)))

(defun modus-themes--org-block-delim (bgaccent fgaccent bg fg)
  "Conditionally set the styles of Org block delimiters.
BG, FG, BGACCENT, FGACCENT apply a background and foreground
color respectively.

The former pair is a grayscale combination that should be more
distinct than the background of the block.  It is applied to the
default styles or when `modus-themes-org-blocks' is set
to `grayscale' (or `greyscale').

The latter pair should be more subtle than the background of the
block, as it is used when `modus-themes-org-blocks' is
set to `rainbow'."
  (pcase modus-themes-org-blocks
    ('gray-background (list :background bg :foreground fg :extend t))
    ('grayscale (list :background bg :foreground fg :extend t))
    ('greyscale (list :background bg :foreground fg :extend t))
    ('tinted-background (list :background bgaccent :foreground fgaccent :extend nil))
    ('rainbow (list :background bgaccent :foreground fgaccent :extend nil))
    (_ (list :foreground fg :extend nil))))

(defun modus-themes--mode-line-attrs
    (fg bg fg-alt bg-alt fg-accent bg-accent border border-3d &optional alt-style fg-distant)
  "Color combinations for `modus-themes-mode-line'.

FG and BG are the default colors.  FG-ALT and BG-ALT are meant to
accommodate the options for a 3D mode line or a `moody' compliant
one.  FG-ACCENT and BG-ACCENT are used for all variants.  BORDER
applies to all permutations of the mode line, except the
three-dimensional effect, where BORDER-3D is used instead.

Optional ALT-STYLE applies an appropriate style to the mode
line's box property.

Optional FG-DISTANT should be close to the main background
values.  It is intended to be used as a distant-foreground
property."
  (let* ((properties (modus-themes--list-or-warn 'modus-themes-mode-line))
         (padding (modus-themes--property-lookup properties 'padding #'natnump 1))
         (height (modus-themes--property-lookup properties 'height #'floatp 'unspecified))
         (padded (> padding 1))
         (base (cond ((memq 'accented properties)
                      (cons fg-accent bg-accent))
                     ((and (or (memq 'moody properties)
                               (memq '3d properties))
                           (not (memq 'borderless properties)))
                      (cons fg-alt bg-alt))
                     ((cons fg bg))))
         (line (cond ((not (or (memq 'moody properties) padded))
                      'unspecified)
                     ((and (not (memq 'moody properties))
                           padded
                           (memq 'borderless properties))
                      'unspecified)
                     ((and (memq 'borderless properties)
                           (memq 'accented properties))
                      bg-accent)
                     ((memq 'borderless properties)
                      bg)
                     (border))))
    (list :foreground (car base)
          :background (cdr base)
          :height height
          :box
          (cond ((memq 'moody properties)
                 'unspecified)
                ((and (memq '3d properties) padded)
                 (list :line-width padding
                       :color
                       (cond ((and (memq 'accented properties)
                                   (memq 'borderless properties))
                              bg-accent)
                             ((or (memq 'accented properties)
                                  (memq 'borderless properties))
                              bg)
                             (bg-alt))
                       :style (when alt-style 'released-button)))
                ((and (memq 'accented properties) padded)
                 (list :line-width padding :color bg-accent))
                ((memq '3d properties)
                 (list :line-width padding
                       :color
                       (cond ((and (memq 'accented properties)
                                   (memq 'borderless properties))
                              bg-accent)
                             ((memq 'borderless properties) bg)
                             (border-3d))
                       :style (when alt-style 'released-button)))
                ((and (memq 'accented properties)
                      (memq 'borderless properties))
                 (list :line-width padding :color bg-accent))
                ((or (memq 'borderless properties) padded)
                 (list :line-width padding :color bg))
                (border))
          :overline line
          :underline line
          :distant-foreground
          (if (memq 'moody properties)
              fg-distant
            'unspecified))))

;; Basically this is just for the keycast key indicator.
(defun modus-themes--mode-line-padded-box (color)
  "Set padding of mode line box attribute with given COLOR."
  (list :box (list :color color
                   :line-width
                   (or (cl-loop
                        for x in modus-themes-mode-line
                        if (natnump x) return x)
                       1))))

(defun modus-themes--diff (mainbg mainfg altbg altfg &optional deubg deufg deualtbg deualtfg bg-only-fg)
  "Color combinations for `modus-themes-diffs'.

MAINBG must be one of the dedicated backgrounds for diffs while
MAINFG must be the same for the foreground.

ALTBG needs to be a slightly accented background that is meant to
be combined with ALTFG.  Both must be less intense than MAINBG
and MAINFG respectively.

DEUBG and DEUFG must be combinations of colors that account for
red-green color defficiency (deuteranopia).  They are the
equivalent of MAINBG and MAINFG.

DEUALTBG and DEUALTFG are the equivalent of ALTBG and ALTFG for
deuteranopia.

Optional non-nil BG-ONLY-FG applies ALTFG else leaves the
foreground unspecified."
  (if modus-themes-deuteranopia
      (pcase modus-themes-diffs
        ('desaturated (list :background (or deualtbg altbg) :foreground (or deualtfg altfg)))
        ('bg-only (list :background (or deualtbg altbg) :foreground (if bg-only-fg (or deualtfg altfg) 'unspecified)))
        (_ (list :background (or deubg mainbg) :foreground (or deufg mainfg))))
    (pcase modus-themes-diffs
      ('desaturated (list :background altbg :foreground altfg))
      ('bg-only (list :background altbg :foreground (if bg-only-fg altfg 'unspecified)))
      (_ (list :background mainbg :foreground mainfg)))))

(defun modus-themes--deuteran (deuteran main)
  "Determine whether to color-code success as DEUTERAN or MAIN."
  (if modus-themes-deuteranopia
      (list deuteran)
    (list main)))

(defun modus-themes--completion-line (key bg fg bgintense fgintense &optional bgaccent bgaccentintense)
  "Styles for `modus-themes-completions'.
KEY is the key of a cons cell.  BG and FG are the main colors.
BGINTENSE works with the main foreground.  FGINTENSE works on its
own.  BGACCENT and BGACCENTINTENSE are colorful variants of the
other backgrounds."
  (let* ((var (modus-themes--list-or-warn 'modus-themes-completions))
         (properties (or (alist-get key var) (alist-get t var)))
         (popup (eq key 'popup))
         (selection (eq key 'selection))
         (line (or popup selection))
         (text (memq 'text-also properties))
         (accented (memq 'accented properties))
         (intense (memq 'intense properties))
         (italic (memq 'italic properties))
         (weight (modus-themes--weight properties))
         (bold (when (and weight (eq weight 'bold)) 'bold)))
    (list
     :inherit
     (cond
      ((and italic weight (not (eq weight 'bold)))
       'italic)
      ((and weight (not (eq weight 'bold)))
       'unspecified)
      (italic 'bold-italic)
      ('bold))
     :background
     (cond
      ((and accented intense line)
       bgaccentintense)
      ((and accented line)
       bgaccent)
      (intense bgintense)
      (bg))
     :foreground
     (cond
      ((and line text intense)
       fgintense)
      ((and line text)
       fg)
      ('unspecified))
     :underline
     (if (memq 'underline properties) t 'unspecified)
     :weight
     (if (and weight (null bold)) weight 'unspecified))))

(defun modus-themes--completion-match (key bg fg bgintense fgintense)
  "Styles for `modus-themes-completions'.
KEY is the key of a cons cell.  BG and FG are the main colors.
BGINTENSE works with the main foreground.  FGINTENSE works on its
own."
  (let* ((var (modus-themes--list-or-warn 'modus-themes-completions))
         (properties (or (alist-get key var) (alist-get t var)))
         (background (memq 'background properties))
         (intense (memq 'intense properties))
         (italic (memq 'italic properties))
         (weight (modus-themes--weight properties))
         (bold (when (and weight (eq weight 'bold)) 'bold)))
    (list
     :inherit
     (cond
      ((and italic weight (not (eq weight 'bold)))
       'italic)
      ((and weight (not (eq weight 'bold)))
       'unspecified)
      (italic 'bold-italic)
      ('bold))
     :background
     (cond
      ((and background intense)
       bgintense)
      (background bg)
      ('unspecified))
     :foreground
     (cond
      ((and background intense)
       'unspecified)
      (background fg)
      (intense fgintense)
      (fg))
     :underline
     (if (memq 'underline properties) t 'unspecified)
     :weight
     (if (and weight (null bold)) weight 'unspecified))))

(defun modus-themes--link (fg fgfaint underline bg bgneutral)
  "Conditional application of link styles.
FG is the link's default color for its text and underline
property.  FGFAINT is a desaturated color for the text and
underline.  UNDERLINE is a gray color only for the undeline.  BG
is a background color and BGNEUTRAL is its fallback value."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-links)))
    (list :inherit
          (cond
           ((and (memq 'bold properties)
                 (memq 'italic properties))
            'bold-italic)
           ((memq 'italic properties)
            'italic)
           ((memq 'bold properties)
            'bold)
           ('unspecified))
          :background
          (cond
           ((and (memq 'no-color properties)
                 (memq 'no-underline properties))
            bgneutral)
           ((memq 'background properties)
            bg)
           ('unspecified))
          :foreground
          (cond
           ((memq 'no-color properties)
            'unspecified)
           ((memq 'faint properties)
            fgfaint)
           (fg))
          :underline
          (cond
           ((memq 'no-underline properties)
            'unspecified)
           ((memq 'neutral-underline properties)
            underline)
           (t)))))

(defun modus-themes--link-color (fg fgfaint &optional neutralfg)
  "Extend `modus-themes--link'.
FG is the main accented foreground.  FGFAINT is also accented,
yet desaturated.  Optional NEUTRALFG is a gray value."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-links)))
    (list :foreground
          (cond
           ((memq 'no-color properties)
            (or neutralfg 'unspecified))
           ((memq 'faint properties)
            fgfaint)
           (fg))
          :underline
          (cond
           ((memq 'no-underline properties)
            'unspecified)
           ((memq 'neutral-underline properties)
            (or neutralfg 'unspecified))
           (t)))))

(defun modus-themes--region (bg fg bgsubtle bgaccent bgaccentsubtle)
  "Apply `modus-themes-region' styles.

BG and FG are the main values that are used by default.  BGSUBTLE
is a subtle background value that can be combined with all colors
used to fontify text and code syntax.  BGACCENT is a colored
background that combines well with FG.  BGACCENTSUBTLE can be
combined with all colors used to fontify text."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-region)))
    (list :background
          (cond
           ((and (memq 'accented properties)
                 (memq 'bg-only properties))
            bgaccentsubtle)
           ((memq 'accented properties)
            bgaccent)
           ((memq 'bg-only properties)
            bgsubtle)
           (bg))
          :foreground
          (cond
           ((and (memq 'accented properties)
                 (memq 'bg-only properties))
            'unspecified)
           ((memq 'bg-only properties)
            'unspecified)
           (fg))
          :extend
          (cond
           ((memq 'no-extend properties)
            nil)
           (t)))))

(defun modus-themes--hl-line
    (bgdefault bgintense bgaccent bgaccentsubtle lineneutral lineaccent lineneutralintense lineaccentintense)
  "Apply `modus-themes-hl-line' styles.

BGDEFAULT is a subtle neutral background.  BGINTENSE is like the
default, but more prominent.  BGACCENT is a prominent accented
background, while BGACCENTSUBTLE is more subtle.  LINENEUTRAL and
LINEACCENT are color values that can remain distinct against the
buffer's possible backgrounds: the former is neutral, the latter
is accented.  LINENEUTRALINTENSE and LINEACCENTINTENSE are their
more prominent alternatives."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-hl-line)))
    (list :background
          (cond
           ((and (memq 'intense properties)
                 (memq 'accented properties))
            bgaccent)
           ((memq 'accented properties)
            bgaccentsubtle)
           ((memq 'intense properties)
            bgintense)
           (bgdefault))
          :underline
          (cond
           ((and (memq 'intense properties)
                 (memq 'accented properties)
                 (memq 'underline properties))
            lineaccentintense)
           ((and (memq 'accented properties)
                 (memq 'underline properties))
            lineaccent)
           ((and (memq 'intense properties)
                 (memq 'underline properties))
            lineneutralintense)
           ((or (memq 'no-background properties)
                (memq 'underline properties))
            lineneutral)
           ('unspecified)))))

(defun modus-themes--button (bg bgfaint bgaccent bgaccentfaint border &optional pressed-button-p)
  "Apply `modus-themes-box-buttons' styles.

BG is the main background.  BGFAINT is its subtle alternative.
BGACCENT is its accented variant and BGACCENTFAINT is the same
but less intense.  BORDER is the color around the box.

When optional PRESSED-BUTTON-P is non-nil, the box uses the
pressed button style, else the released button."
  (let* ((properties modus-themes-box-buttons)
         (weight (modus-themes--weight properties)))
    (list :inherit
          (cond
           ((and (memq 'variable-pitch properties)
                 (eq weight 'bold))
            (list 'bold 'variable-pitch))
           ((memq 'variable-pitch properties)
            'variable-pitch)
           ((eq weight 'bold)
            'bold)
           ('unspecified))
          :background
          (cond
           ((and (memq 'accented properties)
                 (memq 'faint properties)
                 bgaccentfaint))
           ((memq 'faint properties)
            bgfaint)
           ((memq 'accented properties)
            bgaccent)
           (bg))
          :box
          (cond
           ((memq 'underline properties)
            'unspecified)
           ((memq 'flat properties)
            (list :line-width -1 :color border))
           ((list :line-width -1
                  :style (if pressed-button-p
                             'pressed-button
                           'released-button)
                  :color border)))
          :weight
          (cond
           ((eq weight 'bold)
            'unspecified) ; we :inherit the `bold' face above
           (weight weight)
           ('unspecified))
          :height
          (modus-themes--property-lookup properties 'height #'floatp 'unspecified)
          :underline
          (if (memq 'underline properties)
              t
            'unspecified))))



;;;; Utilities for DIY users

;;;;; List colors (a variant of M-x list-colors-display)

(defun modus-themes--list-colors-render (buffer theme &rest _)
  "Render colors in BUFFER from THEME.
Routine for `modus-themes-list-colors'."
  (let ((palette (seq-uniq (modus-themes--palette theme)
                           (lambda (x y)
                             (eq (car x) (car y)))))
        (current-buffer buffer)
        (current-theme theme))
    (with-help-window buffer
      (with-current-buffer standard-output
        (erase-buffer)
        (when (<= (display-color-cells) 256)
          (insert (concat "Your display terminal may not render all color previews!\n"
                          "It seems to only support <= 256 colors.\n\n"))
          (put-text-property (point-min) (point) 'face 'warning))
        ;; We need this to properly render the first line.
        (insert " ")
        (dolist (cell palette)
          (let* ((name (car cell))
                 (color (cdr cell))
                 (fg (readable-foreground-color color))
                 (pad (make-string 5 ?\s)))
            (let ((old-point (point)))
              (insert (format "%s %s" color pad))
              (put-text-property old-point (point) 'face `( :foreground ,color)))
            (let ((old-point (point)))
              (insert (format " %s %s %s\n" color pad name))
              (put-text-property old-point (point)
                                 'face `( :background ,color
                                          :foreground ,fg
                                          :extend t)))
            ;; We need this to properly render the last line.
            (insert " ")))
        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                       (modus-themes--list-colors-render current-buffer current-theme)))))))

(defvar modus-themes--list-colors-prompt-history '()
  "Minibuffer history for `modus-themes--list-colors-prompt'.")

(defun modus-themes--list-colors-prompt ()
  "Prompt for Modus theme.
Helper function for `modus-themes-list-colors'."
  (let ((def (format "%s" (modus-themes--current-theme))))
    (completing-read
     (format "Use palette from theme [%s]: " def)
     '(modus-operandi modus-vivendi) nil t nil
     'modus-themes--list-colors-prompt-history def)))

(defun modus-themes-list-colors (theme)
  "Preview palette of the Modus THEME of choice."
  (interactive (list (intern (modus-themes--list-colors-prompt))))
  (modus-themes--list-colors-render
   (format "*%s-list-colors*" theme)
   theme))

(defun modus-themes-list-colors-current ()
  "Call `modus-themes-list-colors' for the current Modus theme."
  (interactive)
  (modus-themes-list-colors (modus-themes--current-theme)))

;;;;; Formula to measure relative luminance

;; This is the WCAG formula: https://www.w3.org/TR/WCAG20-TECHS/G18.html
(defun modus-themes-wcag-formula (hex)
  "Get WCAG value of color value HEX.
The value is defined in hexadecimal RGB notation, such as those in
`modus-themes-operandi-colors' and `modus-themes-vivendi-colors'."
  (cl-loop for k in '(0.2126 0.7152 0.0722)
           for x in (color-name-to-rgb hex)
           sum (* k (if (<= x 0.03928)
                        (/ x 12.92)
                      (expt (/ (+ x 0.055) 1.055) 2.4)))))

;;;###autoload
(defun modus-themes-contrast (c1 c2)
  "Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB."
  (let ((ct (/ (+ (modus-themes-wcag-formula c1) 0.05)
               (+ (modus-themes-wcag-formula c2) 0.05))))
    (max ct (/ ct))))

(make-obsolete 'modus-themes-color nil "4.0.0")
(make-obsolete 'modus-themes-color-alts nil "4.0.0")



;;;; Commands

;;;###autoload
(defun modus-themes-load-themes ()
  "Ensure that the Modus themes are in `custom-enabled-themes'.

This function is intended for use in package declarations such as
those defined with the help of `use-package'.  The idea is to add
this function to the `:init' stage of the package's loading, so
that subsequent calls that assume the presence of a loaded theme,
like `modus-themes-toggle' or `modus-themes-load-operandi', will
continue to work as intended even if they are lazy-loaded (such
as when they are declared in the `:config' phase)."
  (unless (or (custom-theme-p 'modus-operandi)
              (custom-theme-p 'modus-vivendi))
    (load-theme 'modus-operandi t t)
    (load-theme 'modus-vivendi t t)))

(defvar modus-themes-after-load-theme-hook nil
  "Hook that runs after the `modus-themes-toggle' routines.")

;;;###autoload
(defun modus-themes-load-operandi ()
  "Load `modus-operandi' and disable `modus-vivendi'.
Also run `modus-themes-after-load-theme-hook'."
  (interactive)
  (disable-theme 'modus-vivendi)
  (load-theme 'modus-operandi t)
  (run-hooks 'modus-themes-after-load-theme-hook))

;;;###autoload
(defun modus-themes-load-vivendi ()
  "Load `modus-vivendi' and disable `modus-operandi'.
Also run `modus-themes-after-load-theme-hook'."
  (interactive)
  (disable-theme 'modus-operandi)
  (load-theme 'modus-vivendi t)
  (run-hooks 'modus-themes-after-load-theme-hook))

(defun modus-themes--load-prompt ()
  "Helper for `modus-themes-toggle'."
  (let ((theme
         (intern
          (completing-read "Load Modus theme (will disable all others): "
                           '(modus-operandi modus-vivendi) nil t))))
    (mapc #'disable-theme custom-enabled-themes)
    (pcase theme
      ('modus-operandi (modus-themes-load-operandi))
      ('modus-vivendi (modus-themes-load-vivendi)))))

;;;###autoload
(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes.
Also runs `modus-themes-after-load-theme-hook' at its last stage
by virtue of calling either of `modus-themes-load-operandi' and
`modus-themes-load-vivendi' functions."
  (interactive)
  (modus-themes-load-themes)
  (pcase (modus-themes--current-theme)
    ('modus-operandi (modus-themes-load-vivendi))
    ('modus-vivendi (modus-themes-load-operandi))
    (_ (modus-themes--load-prompt))))



;;;; Face specifications

(defconst modus-themes-faces
  '(
;;;; custom faces
    ;; these bespoke faces are inherited by other constructs below
;;;;; subtle colored backgrounds
    `(modus-themes-subtle-red ((,c :background ,red-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-green ((,c :background ,green-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-yellow ((,c :background ,yellow-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-blue ((,c :background ,blue-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-magenta ((,c :background ,magenta-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-cyan ((,c :background ,cyan-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-neutral ((,c :background ,bg-inactive :foreground ,fg-inactive)))
;;;;; intense colored backgrounds
    `(modus-themes-intense-red ((,c :background ,red-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-green ((,c :background ,green-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-yellow ((,c :background ,yellow-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-blue ((,c :background ,blue-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-magenta ((,c :background ,magenta-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-cyan ((,c :background ,cyan-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-neutral ((,c :background ,bg-active :foreground ,fg-main)))
;;;;; refined background and foreground combinations
    ;; general purpose styles that use an accented foreground against an
    ;; accented background
    `(modus-themes-refine-red ((,c :background ,red-refine-bg :foreground ,red-refine-fg)))
    `(modus-themes-refine-green ((,c :background ,green-refine-bg :foreground ,green-refine-fg)))
    `(modus-themes-refine-yellow ((,c :background ,yellow-refine-bg :foreground ,yellow-refine-fg)))
    `(modus-themes-refine-blue ((,c :background ,blue-refine-bg :foreground ,blue-refine-fg)))
    `(modus-themes-refine-magenta ((,c :background ,magenta-refine-bg :foreground ,magenta-refine-fg)))
    `(modus-themes-refine-cyan ((,c :background ,cyan-refine-bg :foreground ,cyan-refine-fg)))
;;;;; nuanced backgrounds
    ;; useful for adding an accented background that is suitable for all
    ;; main foreground colors (intended for use in Org source blocks)
    `(modus-themes-nuanced-red ((,c :background ,red-nuanced-bg :extend t)))
    `(modus-themes-nuanced-green ((,c :background ,green-nuanced-bg :extend t)))
    `(modus-themes-nuanced-yellow ((,c :background ,yellow-nuanced-bg :extend t)))
    `(modus-themes-nuanced-blue ((,c :background ,blue-nuanced-bg :extend t)))
    `(modus-themes-nuanced-magenta ((,c :background ,magenta-nuanced-bg :extend t)))
    `(modus-themes-nuanced-cyan ((,c :background ,cyan-nuanced-bg :extend t)))
;;;;; fringe-specific combinations
    `(modus-themes-fringe-red ((,c :background ,red-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-green ((,c :background ,green-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-yellow ((,c :background ,yellow-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-blue ((,c :background ,blue-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-magenta ((,c :background ,magenta-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-cyan ((,c :background ,cyan-fringe-bg :foreground ,fg-main)))
;;;;; special base values
    ;; these are closer to the grayscale than the accents defined above
    ;; and should only be used when the next closest alternative would be
    ;; a grayscale value than an accented one
    `(modus-themes-special-cold ((,c :background ,bg-special-cold :foreground ,fg-special-cold)))
    `(modus-themes-special-mild ((,c :background ,bg-special-mild :foreground ,fg-special-mild)))
    `(modus-themes-special-warm ((,c :background ,bg-special-warm :foreground ,fg-special-warm)))
    `(modus-themes-special-calm ((,c :background ,bg-special-calm :foreground ,fg-special-calm)))
;;;;; diff-specific combinations
    ;; intended for `diff-mode' or equivalent
    `(modus-themes-diff-added
      ((,c ,@(modus-themes--diff
                  bg-diff-focus-added fg-diff-focus-added
                  green-nuanced-bg fg-diff-added
                  bg-diff-focus-added-deuteran fg-diff-focus-added-deuteran
                  blue-nuanced-bg fg-diff-added-deuteran))))
    `(modus-themes-diff-changed
      ((,c ,@(modus-themes--diff
                  bg-diff-focus-changed fg-diff-focus-changed
                  yellow-nuanced-bg fg-diff-changed))))
    `(modus-themes-diff-removed
      ((,c ,@(modus-themes--diff
                  bg-diff-focus-removed fg-diff-focus-removed
                  red-nuanced-bg fg-diff-removed))))
    `(modus-themes-diff-refine-added
      ((,c ,@(modus-themes--diff
                  bg-diff-refine-added fg-diff-refine-added
                  bg-diff-focus-added fg-diff-focus-added
                  bg-diff-refine-added-deuteran fg-diff-refine-added-deuteran
                  bg-diff-focus-added-deuteran fg-diff-focus-added-deuteran))))
    `(modus-themes-diff-refine-changed
      ((,c ,@(modus-themes--diff
                  bg-diff-refine-changed fg-diff-refine-changed
                  bg-diff-focus-changed fg-diff-focus-changed))))
    `(modus-themes-diff-refine-removed
      ((,c ,@(modus-themes--diff
                  bg-diff-refine-removed fg-diff-refine-removed
                  bg-diff-focus-removed fg-diff-focus-removed))))
    `(modus-themes-diff-focus-added
      ((,c ,@(modus-themes--diff
                  bg-diff-focus-added fg-diff-focus-added
                  bg-diff-added fg-diff-added
                  bg-diff-focus-added-deuteran fg-diff-focus-added-deuteran
                  bg-diff-added-deuteran fg-diff-added-deuteran))))
    `(modus-themes-diff-focus-changed
      ((,c ,@(modus-themes--diff
                  bg-diff-focus-changed fg-diff-focus-changed
                  bg-diff-changed fg-diff-changed))))
    `(modus-themes-diff-focus-removed
      ((,c ,@(modus-themes--diff
                  bg-diff-focus-removed fg-diff-focus-removed
                  bg-diff-removed fg-diff-removed))))
    `(modus-themes-diff-heading
      ((,c ,@(modus-themes--diff
                  bg-diff-heading fg-diff-heading
                  cyan-nuanced-bg cyan-nuanced-fg
                  bg-active fg-main
                  bg-active fg-main
                  t))))
;;;;; deuteranopia-specific
    `(modus-themes-grue ((,c :foreground ,@(modus-themes--deuteran blue success))))
    `(modus-themes-grue-active ((,c :foreground ,@(modus-themes--deuteran blue green))))
    `(modus-themes-grue-nuanced ((,c :foreground ,@(modus-themes--deuteran blue-nuanced-fg green-nuanced-fg))))
    `(modus-themes-grue-background-active ((,c :inherit ,@(modus-themes--deuteran
                                                               'modus-themes-fringe-blue
                                                               'modus-themes-fringe-green))))
    `(modus-themes-grue-background-intense ((,c :inherit ,@(modus-themes--deuteran
                                                                'modus-themes-intense-blue
                                                                'modus-themes-intense-green))))
    `(modus-themes-grue-background-subtle ((,c :inherit ,@(modus-themes--deuteran
                                                               'modus-themes-subtle-blue
                                                               'modus-themes-subtle-green))))
    `(modus-themes-grue-background-subtle ((,c :inherit ,@(modus-themes--deuteran
                                                               'modus-themes-refine-blue
                                                               'modus-themes-refine-green))))
;;;;; mark indicators
    ;; color combinations intended for Dired, Ibuffer, or equivalent
    `(modus-themes-pseudo-header ((,c :inherit bold :foreground ,fg-main)))
    `(modus-themes-mark-alt ((,c :inherit bold :background ,yellow-refine-bg :foreground ,yellow-refine-fg)))
    `(modus-themes-mark-del ((,c :inherit bold :background ,red-refine-bg :foreground ,red-refine-fg)))
    `(modus-themes-mark-sel ((,c :inherit bold :background ,cyan-refine-bg :foreground ,cyan-refine-fg)))
    `(modus-themes-mark-symbol ((,c :inherit bold :foreground ,blue-warmer)))
;;;;; heading levels
    ;; styles for regular headings used in Org, Markdown, Info, etc.
    `(modus-themes-heading-0 ((,c ,@(modus-themes--heading 0 heading-0 heading-rainbow-0))))
    `(modus-themes-heading-1 ((,c ,@(modus-themes--heading 1 heading-1 heading-rainbow-1))))
    `(modus-themes-heading-2 ((,c ,@(modus-themes--heading 2 heading-2 heading-rainbow-2))))
    `(modus-themes-heading-3 ((,c ,@(modus-themes--heading 3 heading-3 heading-rainbow-3))))
    `(modus-themes-heading-4 ((,c ,@(modus-themes--heading 4 heading-4 heading-rainbow-4))))
    `(modus-themes-heading-5 ((,c ,@(modus-themes--heading 5 heading-5 heading-rainbow-5))))
    `(modus-themes-heading-6 ((,c ,@(modus-themes--heading 6 heading-6 heading-rainbow-6))))
    `(modus-themes-heading-7 ((,c ,@(modus-themes--heading 7 heading-7 heading-rainbow-7))))
    `(modus-themes-heading-8 ((,c ,@(modus-themes--heading 8 heading-8 heading-rainbow-8))))
;;;;; language checkers
    `(modus-themes-lang-error ((,c :foreground ,err)))
    `(modus-themes-lang-note ((,c :foreground ,note)))
    `(modus-themes-lang-warning ((,c :foreground ,warning)))
;;;;; links
    `(modus-themes-link-broken ((,c :inherit button ,@(modus-themes--link-color red red-faint))))
    `(modus-themes-link-symlink ((,c :inherit button ,@(modus-themes--link-color cyan cyan-faint))))
;;;;; markup
    `(modus-themes-markup-code ((,c :foreground ,cyan-cooler)))
    `(modus-themes-markup-macro ((,c :foreground ,magenta-cooler)))
    `(modus-themes-markup-verbatim ((,c :foreground ,magenta-warmer)))
;;;;; search
    `(modus-themes-search-success ((,c :inherit modus-themes-intense-yellow)))
    `(modus-themes-search-success-lazy ((,c :inherit modus-themes-subtle-cyan)))
    `(modus-themes-search-success-modeline ((,c :foreground ,@(modus-themes--deuteran
                                                                   blue
                                                                   green))))
;;;;; completion frameworks
    `(modus-themes-completion-match-0
      ((,c ,@(modus-themes--completion-match
                  'matches magenta-refine-bg magenta-warmer
                  magenta-subtle-bg magenta-intense))))
    `(modus-themes-completion-match-1
      ((,c ,@(modus-themes--completion-match
                  'matches blue-refine-bg blue
                  blue-subtle-bg blue-intense))))
    `(modus-themes-completion-match-2
      ((,c ,@(modus-themes--completion-match
                  'matches green-refine-bg green
                  green-subtle-bg green-intense))))
    `(modus-themes-completion-match-3
      ((,c ,@(modus-themes--completion-match
                  'matches yellow-refine-bg yellow
                  yellow-subtle-bg yellow-intense))))
    `(modus-themes-completion-selected
      ((,c ,@(modus-themes--completion-line
                  'selection bg-inactive blue-warmer
                  bg-active blue
                  bg-completion-subtle bg-completion))))
    `(modus-themes-completion-selected-popup
      ((,c ,@(modus-themes--completion-line
                  'popup bg-active blue-warmer
                  bg-region blue
                  cyan-subtle-bg cyan-refine-bg))))
;;;;; buttons
    `(modus-themes-box-button
      ((,c ,@(modus-themes--button bg-active bg-main bg-active-accent
                                       bg-special-cold bg-region))))
    `(modus-themes-box-button-pressed
      ((,c ,@(modus-themes--button bg-active bg-main bg-active-accent
                                       bg-special-cold bg-region t))))
;;;;; typography
    `(modus-themes-bold ((,c ,@(modus-themes--bold-weight))))
    `(modus-themes-fixed-pitch ((,c ,@(modus-themes--fixed-pitch))))
    `(modus-themes-slant ((,c ,@(modus-themes--slant))))
    `(modus-themes-ui-variable-pitch ((,c ,@(modus-themes--variable-pitch-ui))))
;;;;; other custom faces
    `(modus-themes-hl-line ((,c ,@(modus-themes--hl-line
                                       bg-hl-line bg-hl-line-intense
                                       bg-hl-line-intense-accent blue-nuanced-bg
                                       bg-region blue-intense-bg
                                       fg-alt blue-intense)
                                    :extend t)))
    `(modus-themes-key-binding ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(modus-themes-prompt ((,c ,@(modus-themes--prompt prompt bg-prompt))))
    `(modus-themes-reset-hard ((,c :inherit (fixed-pitch modus-themes-reset-soft)
                                       :family ,(face-attribute 'default :family))))
    `(modus-themes-reset-soft ((,c :background ,bg-main :foreground ,fg-main
                                       :weight normal :slant normal :strike-through nil
                                       :box nil :underline nil :overline nil :extend nil)))
;;;; standard faces
;;;;; absolute essentials
    `(default ((,c :background ,bg-main :foreground ,fg-main)))
    `(cursor ((,c :background ,fg-main)))
    `(fringe ((,c ,@(modus-themes--fringe bg-main bg-inactive bg-active)
                      :foreground ,fg-main)))
    `(vertical-border ((,c :foreground ,border)))
;;;;; basic and/or ungrouped styles
    `(bold ((,c :weight bold)))
    `(bold-italic ((,c :inherit (bold italic))))
    `(underline ((,c :underline ,fg-alt)))
    `(buffer-menu-buffer ((,c :inherit bold)))
    `(child-frame-border ((,c :background ,border)))
    `(comint-highlight-input ((,c :inherit bold)))
    `(comint-highlight-prompt ((,c :inherit modus-themes-prompt)))
    `(confusingly-reordered ((,c :inherit modus-themes-lang-error)))
    `(edmacro-label ((,c :inherit bold :foreground ,cyan)))
    `(elisp-shorthand-font-lock-face ((,c :inherit font-lock-variable-name-face)))
    `(error ((,c :inherit bold :foreground ,err)))
    `(escape-glyph ((,c :foreground ,err)))
    `(file-name-shadow ((,c :inherit shadow)))
    `(header-line ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-active)))
    `(header-line-highlight ((,c :inherit highlight)))
    `(help-argument-name ((,c :inherit modus-themes-slant :foreground ,cyan)))
    `(help-key-binding ((,c :inherit modus-themes-key-binding)))
    `(homoglyph ((,c :foreground ,warning)))
    `(ibuffer-locked-buffer ((,c :foreground ,warning)))
    `(icon-button ((,c :inherit modus-themes-box-button)))
    `(italic ((,c :slant italic)))
    `(nobreak-hyphen ((,c :foreground ,err)))
    `(nobreak-space ((,c :foreground ,err :underline t)))
    `(menu ((,c :inverse-video unspecified :inherit modus-themes-intense-neutral)))
    `(minibuffer-prompt ((,c :inherit modus-themes-prompt)))
    `(mm-command-output ((,c :foreground ,red-cooler)))
    `(mm-uu-extract ((,c :background ,bg-dim :foreground ,fg-special-mild)))
    `(next-error ((,c :inherit modus-themes-subtle-red :extend t)))
    `(pgtk-im-0 ((,c :inherit modus-themes-refine-cyan)))
    `(read-multiple-choice-face ((,c :inherit (bold modus-themes-mark-alt))))
    `(rectangle-preview ((,c :inherit modus-themes-special-warm)))
    `(region ((,c ,@(modus-themes--region bg-region fg-main
                                          bg-active bg-region-accent
                                          bg-region-accent-subtle))))
    `(secondary-selection ((,c :inherit modus-themes-special-cold)))
    `(separator-line ((,c :underline ,bg-region)))
    `(shadow ((,c :foreground "gray50")))
    `(success ((,c :inherit (bold modus-themes-grue))))
    `(trailing-whitespace ((,c :background ,red-intense-bg)))
    `(warning ((,c :inherit bold :foreground ,yellow)))
;;;;; buttons, links, widgets
    `(button ((,c ,@(modus-themes--link
                     link blue-faint
                     bg-region blue-nuanced-bg bg-alt))))
    `(link ((,c :inherit button)))
    `(link-visited ((,c :inherit button
                        ,@(modus-themes--link-color
                           link-visited magenta-faint fg-alt))))
    `(tooltip ((,c :background ,bg-special-cold :foreground ,fg-main)))
    `(widget-button ((,c ,@(if (memq 'all-buttons modus-themes-box-buttons)
                                   (list :inherit 'modus-themes-box-button)
                                 (list :inherit 'bold :foreground blue-warmer)))))
    `(widget-button-pressed ((,c ,@(if (memq 'all-buttons modus-themes-box-buttons)
                                           (list :inherit 'modus-themes-box-button-pressed)
                                         (list :inherit 'bold :foreground magenta-warmer)))))
    `(widget-documentation ((,c :foreground ,green)))
    `(widget-field ((,c :background ,bg-alt :foreground ,fg-main :extend nil)))
    `(widget-inactive ((,c :inherit shadow :background ,bg-dim)))
    `(widget-single-line-field ((,c :inherit widget-field)))
;;;;; agda2-mode
    `(agda2-highlight-bound-variable-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-catchall-clause-face ((,c :background ,bg-alt)))
    `(agda2-highlight-coinductive-constructor-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-coverage-problem-face ((,c :inherit modus-themes-lang-error)))
    `(agda2-highlight-datatype-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-deadcode-face ((,c :background ,bg-active)))
    `(agda2-highlight-dotted-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-error-face ((,c :inherit modus-themes-lang-error)))
    `(agda2-highlight-field-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-function-face ((,c :inherit font-lock-function-name-face)))
    `(agda2-highlight-generalizable-variable-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-incomplete-pattern-face ((,c :inherit modus-themes-lang-warning)))
    `(agda2-highlight-inductive-constructor-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-keyword-face ((,c :inherit font-lock-keyword-face)))
    `(agda2-highlight-macro-face ((,c :inherit font-lock-keyword-face)))
    `(agda2-highlight-module-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-number-face ((,c :foreground ,fg-special-warm)))
    `(agda2-highlight-operator-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-positivity-problem-face ((,c :inherit modus-themes-lang-warning)))
    `(agda2-highlight-postulate-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-pragma-face ((,c :inherit font-lock-preprocessor-face)))
    `(agda2-highlight-primitive-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-primitive-type-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-record-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-string-face ((,c :inherit font-lock-string-face)))
    `(agda2-highlight-symbol-face ((,c :inherit font-lock-constant-face)))
    `(agda2-highlight-termination-problem-face ((,c :inherit modus-themes-lang-warning)))
    `(agda2-highlight-typechecks-face ((,c :inherit font-lock-warning-face)))
    `(agda2-highlight-unsolved-constraint-face ((,c :inherit modus-themes-lang-warning)))
    `(agda2-highlight-unsolved-meta-face ((,c :inherit modus-themes-lang-warning)))
;;;;; alert
    `(alert-high-face ((,c :inherit bold :foreground ,red-warmer)))
    `(alert-low-face ((,c :foreground ,fg-special-mild)))
    `(alert-moderate-face ((,c :inherit bold :foreground ,yellow)))
    `(alert-trivial-face ((,c :foreground ,fg-special-calm)))
    `(alert-urgent-face ((,c :inherit bold :foreground ,red-intense)))
;;;;; all-the-icons
    `(all-the-icons-blue ((,c :foreground ,blue-cooler)))
    `(all-the-icons-blue-warmer ((,c :foreground ,blue-warmer)))
    `(all-the-icons-cyan ((,c :foreground ,cyan-intense)))
    `(all-the-icons-cyan-warmer ((,c :foreground ,cyan-warmer)))
    `(all-the-icons-dblue ((,c :foreground ,blue-faint)))
    `(all-the-icons-dcyan ((,c :foreground ,cyan-faint)))
    `(all-the-icons-dgreen ((,c :foreground ,green)))
    `(all-the-icons-dmaroon ((,c :foreground ,magenta-faint)))
    `(all-the-icons-dorange ((,c :foreground ,red-faint)))
    `(all-the-icons-dpink ((,c :foreground ,magenta-faint)))
    `(all-the-icons-dpurple ((,c :foreground ,magenta-faint)))
    `(all-the-icons-dred ((,c :foreground ,red-faint)))
    `(all-the-icons-dsilver ((,c :foreground ,cyan-faint)))
    `(all-the-icons-dyellow ((,c :foreground ,yellow-faint)))
    `(all-the-icons-green ((,c :foreground ,green-intense)))
    `(all-the-icons-lblue ((,c :foreground ,blue-cooler)))
    `(all-the-icons-lcyan ((,c :foreground ,cyan)))
    `(all-the-icons-lgreen ((,c :foreground ,green-cooler)))
    `(all-the-icons-lmaroon ((,c :foreground ,magenta-warmer)))
    `(all-the-icons-lorange ((,c :foreground ,red-warmer)))
    `(all-the-icons-lpink ((,c :foreground ,magenta)))
    `(all-the-icons-lpurple ((,c :foreground ,magenta-faint)))
    `(all-the-icons-lred ((,c :foreground ,red)))
    `(all-the-icons-lyellow ((,c :foreground ,yellow-warmer)))
    `(all-the-icons-maroon ((,c :foreground ,magenta-intense)))
    `(all-the-icons-pink ((,c :foreground ,fg-special-calm)))
    `(all-the-icons-red ((,c :foreground ,red-intense)))
    `(all-the-icons-red-warmer ((,c :foreground ,red-cooler)))
    `(all-the-icons-silver ((,c :foreground ,fg-special-cold)))
    `(all-the-icons-yellow ((,c :foreground ,yellow)))
;;;;; all-the-icons-dired
    `(all-the-icons-dired-dir-face ((,c :foreground ,cyan-faint)))
;;;;; all-the-icons-ibuffer
    `(all-the-icons-ibuffer-dir-face ((,c :foreground ,cyan-faint)))
    `(all-the-icons-ibuffer-file-face ((,c :foreground ,blue-faint)))
    `(all-the-icons-ibuffer-mode-face ((,c :foreground ,cyan)))
    `(all-the-icons-ibuffer-size-face ((,c :foreground ,cyan-cooler)))
;;;;; annotate
    `(annotate-annotation ((,c :inherit modus-themes-subtle-blue)))
    `(annotate-annotation-secondary ((,c :inherit modus-themes-subtle-green)))
    `(annotate-highlight ((,c :background ,blue-nuanced-bg :underline ,blue-intense)))
    `(annotate-highlight-secondary ((,c :background ,green-nuanced-bg :underline ,green-intense)))
;;;;; ansi-color
    ;; Those are in Emacs28.
    `(ansi-color-black ((,c :background "black" :foreground "black")))
    `(ansi-color-blue ((,c :background ,blue :foreground ,blue)))
    `(ansi-color-bold ((,c :inherit bold)))
    `(ansi-color-bright-black ((,c :background "gray35" :foreground "gray35")))
    `(ansi-color-bright-blue ((,c :background ,blue-warmer :foreground ,blue-warmer)))
    `(ansi-color-bright-cyan ((,c :background ,cyan-cooler :foreground ,cyan-cooler)))
    `(ansi-color-bright-green ((,c :background ,green-cooler :foreground ,green-cooler)))
    `(ansi-color-bright-magenta ((,c :background ,magenta-cooler :foreground ,magenta-cooler)))
    `(ansi-color-bright-red ((,c :background ,red-warmer :foreground ,red-warmer)))
    `(ansi-color-bright-white ((,c :background "white" :foreground "white")))
    `(ansi-color-bright-yellow ((,c :background ,yellow-warmer :foreground ,yellow-warmer)))
    `(ansi-color-cyan ((,c :background ,cyan :foreground ,cyan)))
    `(ansi-color-green ((,c :background ,green :foreground ,green)))
    `(ansi-color-magenta ((,c :background ,magenta :foreground ,magenta)))
    `(ansi-color-red ((,c :background ,red :foreground ,red)))
    `(ansi-color-white ((,c :background "gray65" :foreground "gray65")))
    `(ansi-color-yellow ((,c :background ,yellow :foreground ,yellow)))
;;;;; anzu
    `(anzu-match-1 ((,c :inherit modus-themes-subtle-cyan)))
    `(anzu-match-2 ((,c :inherit modus-themes-search-success)))
    `(anzu-match-3 ((,c :inherit modus-themes-subtle-yellow)))
    `(anzu-mode-line ((,c :inherit (bold modus-themes-search-success-modeline))))
    `(anzu-mode-line-no-match ((,c :inherit bold :foreground ,red)))
    `(anzu-replace-highlight ((,c :inherit modus-themes-refine-red :underline t)))
    `(anzu-replace-to ((,c :inherit modus-themes-search-success)))
;;;;; apropos
    `(apropos-button ((,c :foreground ,magenta-cooler)))
    `(apropos-function-button ((,c :foreground ,magenta)))
    `(apropos-keybinding ((,c :inherit modus-themes-key-binding)))
    `(apropos-misc-button ((,c :foreground ,green-cooler)))
    `(apropos-property ((,c :inherit modus-themes-bold :foreground ,magenta-warmer)))
    `(apropos-symbol ((,c :inherit modus-themes-pseudo-header)))
    `(apropos-user-option-button ((,c :foreground ,cyan)))
    `(apropos-variable-button ((,c :foreground ,blue-warmer)))
;;;;; artbollocks-mode
    `(artbollocks-face ((,c :inherit modus-themes-lang-note)))
    `(artbollocks-lexical-illusions-face ((,c :background ,bg-alt :foreground ,red-warmer :underline t)))
    `(artbollocks-passive-voice-face ((,c :inherit modus-themes-lang-warning)))
    `(artbollocks-weasel-words-face ((,c :inherit modus-themes-lang-error)))
;;;;; auctex and Tex
    `(font-latex-bold-face ((,c :inherit bold)))
    `(font-latex-doctex-documentation-face ((,c :inherit font-lock-doc-face)))
    `(font-latex-doctex-preprocessor-face ((,c :inherit font-lock-preprocessor-face)))
    `(font-latex-italic-face ((,c :inherit italic)))
    `(font-latex-math-face ((,c :inherit font-lock-constant-face)))
    `(font-latex-script-char-face ((,c :inherit font-lock-builtin-face)))
    `(font-latex-sectioning-5-face ((,c :inherit (bold modus-themes-variable-pitch) :foreground ,blue-nuanced-fg)))
    `(font-latex-sedate-face ((,c :inherit font-lock-keyword-face)))
    `(font-latex-slide-title-face ((,c :inherit modus-themes-heading-1)))
    `(font-latex-string-face ((,c :inherit font-lock-string-face)))
    `(font-latex-subscript-face ((,c :height 0.95)))
    `(font-latex-superscript-face ((,c :height 0.95)))
    `(font-latex-underline-face ((,c :inherit underline)))
    `(font-latex-verbatim-face ((,c :inherit modus-themes-markup-verbatim)))
    `(font-latex-warning-face ((,c :inherit font-lock-warning-face)))
    `(tex-verbatim ((,c :inherit modus-themes-markup-verbatim)))
    `(texinfo-heading ((,c :foreground ,magenta)))
    `(TeX-error-description-error ((,c :inherit error)))
    `(TeX-error-description-help ((,c :inherit success)))
    `(TeX-error-description-tex-said ((,c :inherit success)))
    `(TeX-error-description-warning ((,c :inherit warning)))
;;;;; auto-dim-other-buffers
    `(auto-dim-other-buffers-face ((,c :background ,bg-alt)))
;;;;; avy
    `(avy-background-face ((,c :background ,bg-dim :foreground ,fg-dim :extend t)))
    `(avy-goto-char-timer-face ((,c :inherit (modus-themes-intense-neutral bold))))
    `(avy-lead-face ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-0)))
    `(avy-lead-face-0 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-1)))
    `(avy-lead-face-1 ((,c :inherit (modus-themes-special-warm modus-themes-reset-soft))))
    `(avy-lead-face-2 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-2)))
;;;;; aw (ace-window)
    `(aw-background-face ((,c :foreground "gray50")))
    `(aw-key-face ((,c :inherit modus-themes-key-binding)))
    `(aw-leading-char-face ((,c :inherit (bold modus-themes-reset-soft) :height 1.5
                                    :foreground ,red-intense)))
    `(aw-minibuffer-leading-char-face ((,c :inherit (modus-themes-intense-red bold))))
    `(aw-mode-line-face ((,c :inherit bold)))
;;;;; awesome-tray
    `(awesome-tray-module-awesome-tab-face ((,c :inherit bold :foreground ,red-cooler)))
    `(awesome-tray-module-battery-face ((,c :inherit bold :foreground ,cyan-cooler)))
    `(awesome-tray-module-buffer-name-face ((,c :inherit bold :foreground ,yellow-cooler)))
    `(awesome-tray-module-circe-face ((,c :inherit bold :foreground ,blue-warmer)))
    `(awesome-tray-module-date-face ((,c :inherit bold :foreground ,fg-dim)))
    `(awesome-tray-module-evil-face ((,c :inherit bold :foreground ,green-warmer)))
    `(awesome-tray-module-git-face ((,c :inherit bold :foreground ,magenta)))
    `(awesome-tray-module-last-command-face ((,c :inherit bold :foreground ,blue-cooler)))
    `(awesome-tray-module-location-face ((,c :inherit bold :foreground ,yellow)))
    `(awesome-tray-module-mode-name-face ((,c :inherit bold :foreground ,green)))
    `(awesome-tray-module-parent-dir-face ((,c :inherit bold :foreground ,cyan)))
    `(awesome-tray-module-rvm-face ((,c :inherit bold :foreground ,magenta-cooler)))
;;;;; bbdb
    `(bbdb-name ((,c :foreground ,magenta-cooler)))
    `(bbdb-organization ((,c :foreground ,red-cooler)))
    `(bbdb-field-name ((,c :foreground ,cyan-cooler)))
;;;;; binder
    `(binder-sidebar-highlight ((,c :inherit modus-themes-subtle-cyan)))
    `(binder-sidebar-marked ((,c :inherit modus-themes-mark-sel)))
    `(binder-sidebar-missing ((,c :inherit modus-themes-subtle-red)))
    `(binder-sidebar-tags ((,c :foreground ,cyan)))
;;;;; bm
    `(bm-face ((,c :inherit modus-themes-subtle-yellow :extend t)))
    `(bm-fringe-face ((,c :inherit modus-themes-fringe-yellow)))
    `(bm-fringe-persistent-face ((,c :inherit modus-themes-fringe-blue)))
    `(bm-persistent-face ((,c :inherit modus-themes-intense-blue :extend t)))
;;;;; bongo
    `(bongo-album-title ((,c :foreground ,fg-active)))
    `(bongo-artist ((,c :foreground ,magenta)))
    `(bongo-currently-playing-track ((,c :inherit bold)))
    `(bongo-elapsed-track-part ((,c :inherit modus-themes-subtle-magenta :underline t)))
    `(bongo-filled-seek-bar ((,c :background ,blue-intense-bg :foreground ,fg-main)))
    `(bongo-marked-track ((,c :foreground ,note)))
    `(bongo-marked-track-line ((,c :inherit modus-themes-mark-sel)))
    `(bongo-played-track ((,c :inherit shadow :strike-through t)))
    `(bongo-track-length ((,c :inherit shadow)))
    `(bongo-track-title ((,c :foreground ,blue)))
    `(bongo-unfilled-seek-bar ((,c :background ,bg-special-cold :foreground ,fg-main)))
;;;;; boon
    `(boon-modeline-cmd ((,c :inherit modus-themes-intense-blue)))
    `(boon-modeline-ins ((,c :inherit modus-themes-intense-red)))
    `(boon-modeline-off ((,c :inherit modus-themes-intense-yellow)))
    `(boon-modeline-spc ((,c :inherit modus-themes-intense-green)))
;;;;; bookmark
    `(bookmark-face ((,c :inherit modus-themes-fringe-cyan)))
    `(bookmark-menu-bookmark ((,c :inherit bold)))
;;;;; breakpoint (built-in gdb-mi.el)
    `(breakpoint-disabled ((,c :inherit shadow)))
    `(breakpoint-enabled ((,c :inherit bold :foreground ,red)))
;;;;; calendar and diary
    `(calendar-month-header ((,c :inherit modus-themes-pseudo-header)))
    `(calendar-today ((,c :inherit bold :underline t)))
    `(calendar-weekday-header ((,c :inherit shadow)))
    `(calendar-weekend-header ((,c :foreground ,red-faint)))
    `(diary ((,c :background ,blue-nuanced-bg :foreground ,blue-cooler)))
    `(diary-anniversary ((,c :foreground ,red-cooler)))
    `(diary-time ((,c :foreground ,cyan)))
    `(holiday ((,c :background ,magenta-nuanced-bg :foreground ,magenta-warmer)))
;;;;; calfw
    `(cfw:face-annotation ((,c :foreground ,fg-special-warm)))
    `(cfw:face-day-title ((,c :foreground ,fg-main)))
    `(cfw:face-default-content ((,c :foreground ,green-warmer)))
    `(cfw:face-default-day ((,c :inherit (cfw:face-day-title bold))))
    `(cfw:face-disable ((,c :inherit shadow)))
    `(cfw:face-grid ((,c :foreground "gray50")))
    `(cfw:face-header ((,c :inherit bold :foreground ,fg-main)))
    `(cfw:face-holiday ((,c :foreground ,magenta-cooler)))
    `(cfw:face-periods ((,c :foreground ,cyan-cooler)))
    `(cfw:face-saturday ((,c :inherit bold :foreground ,cyan-cooler)))
    `(cfw:face-select ((,c :inherit modus-themes-intense-blue)))
    `(cfw:face-sunday ((,c :inherit bold :foreground ,cyan-cooler)))
    `(cfw:face-title ((,c :inherit modus-themes-heading-1 :background ,bg-main :overline nil :foreground ,fg-special-cold)))
    `(cfw:face-today ((,c :background ,bg-inactive)))
    `(cfw:face-today-title ((,c :background ,bg-active)))
    `(cfw:face-toolbar ((,c :background ,bg-alt :foreground ,bg-alt)))
    `(cfw:face-toolbar-button-off ((,c :inherit shadow)))
    `(cfw:face-toolbar-button-on ((,c :inherit bold :background ,blue-nuanced-bg
                                          :foreground ,blue-warmer)))
;;;;; calibredb
    `(calibredb-archive-face ((,c :foreground ,magenta-faint)))
    `(calibredb-author-face ((,c :foreground ,blue-faint)))
    `(calibredb-comment-face ((,c :inherit shadow)))
    `(calibredb-date-face ((,c :foreground ,cyan)))
    `(calibredb-edit-annotation-header-title-face ((,c :inherit bold)))
    `(calibredb-favorite-face ((,c :foreground ,red-warmer)))
    `(calibredb-file-face (( )))
    `(calibredb-format-face ((,c :foreground ,cyan-faint)))
    `(calibredb-highlight-face ((,c :inherit success)))
    `(calibredb-id-face (( )))
    `(calibredb-ids-face (( )))
    `(calibredb-search-header-highlight-face ((,c :inherit modus-themes-hl-line)))
    `(calibredb-search-header-library-name-face ((,c :foreground ,blue)))
    `(calibredb-search-header-library-path-face ((,c :inherit bold)))
    `(calibredb-search-header-sort-face ((,c :inherit bold :foreground ,magenta)))
    `(calibredb-search-header-total-face ((,c :inherit bold :foreground ,cyan)))
    `(calibredb-search-header-filter-face ((,c :inherit bold)))
    `(calibredb-mark-face ((,c :inherit modus-themes-mark-sel)))
    `(calibredb-size-face (( )))
    `(calibredb-tag-face ((,c :foreground ,magenta-faint)))
;;;;; cfrs
    `(cfrs-border-color ((,c :background ,border)))
;;;;; change-log and log-view (`vc-print-log' and `vc-print-root-log')
    `(change-log-acknowledgment ((,c :inherit shadow)))
    `(change-log-conditionals ((,c :foreground ,yellow)))
    `(change-log-date ((,c :foreground ,cyan)))
    `(change-log-email ((,c :foreground ,cyan-cooler)))
    `(change-log-file ((,c :inherit bold :foreground ,fg-special-cold)))
    `(change-log-function ((,c :foreground ,green-cooler)))
    `(change-log-list ((,c :foreground ,magenta-warmer)))
    `(change-log-name ((,c :foreground ,magenta-cooler)))
    `(log-edit-header ((,c :foreground ,fg-special-warm)))
    `(log-edit-headers-separator ((,c :height 1 :background ,border :extend t)))
    `(log-edit-summary ((,c :inherit bold :foreground ,blue)))
    `(log-edit-unknown-header ((,c :inherit shadow)))
    `(log-view-commit-body ((,c :foreground ,blue-nuanced-fg)))
    `(log-view-file ((,c :inherit bold :foreground ,fg-special-cold)))
    `(log-view-message ((,c :background ,bg-alt :foreground ,fg-alt)))
;;;;; cider
    `(cider-debug-code-overlay-face ((,c :background ,bg-alt)))
    `(cider-debug-prompt-face ((,c :foreground ,magenta-warmer :underline t)))
    `(cider-deprecated-face ((,c :inherit modus-themes-refine-yellow)))
    `(cider-docview-emphasis-face ((,c :inherit italic :foreground ,fg-special-cold)))
    `(cider-docview-literal-face ((,c :foreground ,blue-warmer)))
    `(cider-docview-strong-face ((,c :inherit bold :foreground ,fg-special-cold)))
    `(cider-docview-table-border-face ((,c :inherit shadow)))
    `(cider-enlightened-face ((,c :box (:line-width -1 :color ,yellow-warmer :style nil) :background ,bg-dim)))
    `(cider-enlightened-local-face ((,c :inherit bold :foreground ,yellow-cooler)))
    `(cider-error-highlight-face ((,c :foreground ,red :underline t)))
    `(cider-fragile-button-face ((,c :box (:line-width 3 :color ,fg-alt :style released-button) :foreground ,yellow)))
    `(cider-fringe-good-face ((,c :foreground ,green)))
    `(cider-instrumented-face ((,c :box (:line-width -1 :color ,red :style nil) :background ,bg-dim)))
    `(cider-reader-conditional-face ((,c :inherit italic :foreground ,fg-special-warm)))
    `(cider-repl-input-face ((,c :inherit bold)))
    `(cider-repl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(cider-repl-stderr-face ((,c :inherit bold :foreground ,red)))
    `(cider-repl-stdout-face ((,c :foreground ,blue)))
    `(cider-result-overlay-face ((,c :box (:line-width -1 :color ,blue :style nil) :background ,bg-dim)))
    `(cider-stacktrace-error-class-face ((,c :inherit bold :foreground ,red)))
    `(cider-stacktrace-error-message-face ((,c :inherit italic :foreground ,red-cooler)))
    `(cider-stacktrace-face ((,c :foreground ,fg-main)))
    `(cider-stacktrace-filter-active-face ((,c :foreground ,cyan-warmer :underline t)))
    `(cider-stacktrace-filter-inactive-face ((,c :foreground ,cyan-warmer)))
    `(cider-stacktrace-fn-face ((,c :inherit bold :foreground ,fg-main)))
    `(cider-stacktrace-ns-face ((,c :inherit (shadow italic))))
    `(cider-stacktrace-promoted-button-face ((,c :box (:line-width 3 :color ,fg-alt :style released-button) :foreground ,red)))
    `(cider-stacktrace-suppressed-button-face ((,c :box (:line-width 3 :color ,fg-alt :style pressed-button)
                                                       :background ,bg-alt :foreground ,fg-alt)))
    `(cider-test-error-face ((,c :inherit modus-themes-subtle-red)))
    `(cider-test-failure-face ((,c :inherit (modus-themes-intense-red bold))))
    `(cider-test-success-face ((,c :inherit modus-themes-grue-background-intense)))
    `(cider-traced-face ((,c :box (:line-width -1 :color ,cyan :style nil) :background ,bg-dim)))
    `(cider-warning-highlight-face ((,c :foreground ,yellow :underline t)))
;;;;; circe (and lui)
    `(circe-fool-face ((,c :inherit shadow)))
    `(circe-highlight-nick-face ((,c :inherit bold :foreground ,blue)))
    `(circe-prompt-face ((,c :inherit modus-themes-prompt)))
    `(circe-server-face ((,c :inherit shadow)))
    `(lui-button-face ((,c :inherit button)))
    `(lui-highlight-face ((,c :foreground ,magenta-warmer)))
    `(lui-time-stamp-face ((,c :foreground ,blue-nuanced-fg)))
;;;;; citar
    `(citar ((,c :inherit shadow)))
    `(citar-highlight (( )))
;;;;; clojure-mode
    `(clojure-keyword-face ((,c :inherit font-lock-builtin-face)))
;;;;; color-rg
    `(color-rg-font-lock-column-number ((,c :foreground ,magenta-cooler)))
    `(color-rg-font-lock-command ((,c :inherit bold :foreground ,fg-main)))
    `(color-rg-font-lock-file ((,c :inherit bold :foreground ,fg-special-cold)))
    `(color-rg-font-lock-flash ((,c :inherit modus-themes-intense-blue)))
    `(color-rg-font-lock-function-location ((,c :inherit modus-themes-special-calm)))
    `(color-rg-font-lock-header-line-directory ((,c :foreground ,blue)))
    `(color-rg-font-lock-header-line-edit-mode ((,c :foreground ,magenta)))
    `(color-rg-font-lock-header-line-keyword ((,c :foreground ,green)))
    `(color-rg-font-lock-header-line-text ((,c :foreground ,fg-active)))
    `(color-rg-font-lock-line-number ((,c :foreground ,fg-special-warm)))
    `(color-rg-font-lock-mark-changed ((,c :inherit bold :foreground ,blue)))
    `(color-rg-font-lock-mark-deleted ((,c :inherit bold :foreground ,red)))
    `(color-rg-font-lock-match ((,c :inherit modus-themes-special-calm)))
    `(color-rg-font-lock-position-splitter ((,c :inherit shadow)))
;;;;; column-enforce-mode
    `(column-enforce-face ((,c :inherit modus-themes-refine-yellow)))
;;;;; company-mode
    `(company-echo-common ((,c :inherit modus-themes-completion-match-0)))
    `(company-preview ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(company-preview-common ((,c :inherit company-echo-common)))
    `(company-preview-search ((,c :inherit modus-themes-special-calm)))
    `(company-template-field ((,c :inherit modus-themes-intense-magenta)))
    `(company-scrollbar-bg ((,c :background ,bg-active)))
    `(company-scrollbar-fg ((,c :background ,fg-active)))
    `(company-tooltip ((,c :background ,bg-alt)))
    `(company-tooltip-annotation ((,c :inherit completions-annotations)))
    `(company-tooltip-common ((,c :inherit company-echo-common)))
    `(company-tooltip-deprecated ((,c :inherit company-tooltip :strike-through t)))
    `(company-tooltip-mouse ((,c :inherit highlight)))
    `(company-tooltip-scrollbar-thumb ((,c :background ,fg-active)))
    `(company-tooltip-scrollbar-track ((,c :background ,bg-active)))
    `(company-tooltip-search ((,c :inherit (modus-themes-search-success-lazy bold))))
    `(company-tooltip-search-selection ((,c :inherit modus-themes-search-success :underline t)))
    `(company-tooltip-selection ((,c :inherit modus-themes-completion-selected-popup)))
;;;;; company-posframe
    `(company-posframe-active-backend-name ((,c :inherit bold :background ,bg-active :foreground ,blue)))
    `(company-posframe-inactive-backend-name ((,c :background ,bg-active :foreground ,fg-active)))
    `(company-posframe-metadata ((,c :background ,bg-inactive :foreground ,fg-inactive)))
;;;;; compilation
    `(compilation-column-number ((,c :inherit compilation-line-number)))
    `(compilation-error ((,c :inherit modus-themes-bold :foreground ,red)))
    `(compilation-info ((,c :inherit modus-themes-bold :foreground ,fg-special-cold)))
    `(compilation-line-number ((,c :foreground ,fg-special-warm)))
    `(compilation-mode-line-exit ((,c :inherit bold)))
    `(compilation-mode-line-fail ((,c :inherit modus-themes-bold :foreground ,red)))
    `(compilation-mode-line-run ((,c :inherit modus-themes-bold :foreground ,cyan)))
    `(compilation-warning ((,c :inherit modus-themes-bold :foreground ,yellow-warmer)))
;;;;; completions
    `(completions-annotations ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(completions-common-part ((,c :inherit modus-themes-completion-match-0)))
    `(completions-first-difference ((,c :inherit modus-themes-completion-match-1)))
;;;;; consult
    `(consult-async-running ((,c :inherit bold :foreground ,blue)))
    `(consult-async-split ((,c :foreground ,magenta-warmer)))
    `(consult-bookmark ((,c :foreground ,blue)))
    `(consult-file ((,c :foreground ,fg-special-cold)))
    `(consult-imenu-prefix ((,c :inherit shadow)))
    `(consult-key ((,c :inherit modus-themes-key-binding)))
    `(consult-line-number ((,c :foreground ,fg-special-warm)))
    `(consult-line-number-prefix ((,c :inherit shadow)))
    `(consult-narrow-indicator ((,c :foreground ,magenta-warmer)))
    `(consult-preview-cursor ((,c :inherit modus-themes-intense-blue)))
    `(consult-preview-insertion ((,c :inherit modus-themes-special-warm)))
;;;;; corfu
    `(corfu-current ((,c :inherit modus-themes-completion-selected-popup)))
    `(corfu-bar ((,c :background ,fg-alt)))
    `(corfu-border ((,c :background ,bg-active)))
    `(corfu-default ((,c :background ,bg-alt)))
;;;;; corfu-quick
    `(corfu-quick1 ((,c :inherit bold :background ,bg-char-0)))
    `(corfu-quick2 ((,c :inherit bold :background ,bg-char-1)))
;;;;; counsel
    `(counsel-active-mode ((,c :foreground ,magenta-cooler)))
    `(counsel-application-name ((,c :foreground ,red-cooler)))
    `(counsel-key-binding ((,c :inherit modus-themes-key-binding)))
    `(counsel-outline-1 ((,c :inherit org-level-1)))
    `(counsel-outline-2 ((,c :inherit org-level-2)))
    `(counsel-outline-3 ((,c :inherit org-level-3)))
    `(counsel-outline-4 ((,c :inherit org-level-4)))
    `(counsel-outline-5 ((,c :inherit org-level-5)))
    `(counsel-outline-6 ((,c :inherit org-level-6)))
    `(counsel-outline-7 ((,c :inherit org-level-7)))
    `(counsel-outline-8 ((,c :inherit org-level-8)))
    `(counsel-outline-default ((,c :foreground ,fg-main)))
    `(counsel-variable-documentation ((,c :inherit modus-themes-slant :foreground ,yellow-cooler)))
;;;;; counsel-css
    `(counsel-css-selector-depth-face-1 ((,c :foreground ,blue)))
    `(counsel-css-selector-depth-face-2 ((,c :foreground ,cyan)))
    `(counsel-css-selector-depth-face-3 ((,c :foreground ,green)))
    `(counsel-css-selector-depth-face-4 ((,c :foreground ,yellow)))
    `(counsel-css-selector-depth-face-5 ((,c :foreground ,magenta)))
    `(counsel-css-selector-depth-face-6 ((,c :foreground ,red)))
;;;;; cov
    `(cov-coverage-not-run-face ((,c :foreground ,red-intense)))
    `(cov-coverage-run-face ((,c :foreground ,green-intense)))
    `(cov-heavy-face ((,c :foreground ,magenta-intense)))
    `(cov-light-face ((,c :foreground ,blue-intense)))
    `(cov-med-face ((,c :foreground ,yellow-intense)))
    `(cov-none-face ((,c :foreground ,cyan-intense)))
;;;;; cperl-mode
    `(cperl-nonoverridable-face ((,c :foreground unspecified)))
    `(cperl-array-face ((,c :inherit font-lock-keyword-face)))
    `(cperl-hash-face ((,c :inherit font-lock-variable-name-face)))
;;;;; crontab-mode
    `(crontab-minute ((,c :foreground ,blue-warmer)))
    `(crontab-hour ((,c :foreground ,magenta-cooler)))
    `(crontab-month-day ((,c :foreground ,magenta-warmer)))
    `(crontab-month ((,c :foreground ,blue)))
    `(crontab-week-day ((,c :foreground ,cyan)))
    `(crontab-predefined ((,c :foreground ,blue-warmer)))
;;;;; css-mode
    `(css-property ((,c :inherit font-lock-type-face)))
    `(css-selector ((,c :inherit font-lock-keyword-face)))
;;;;; csv-mode
    `(csv-separator-face ((,c :foreground ,red-intense)))
;;;;; ctrlf
    `(ctrlf-highlight-active ((,c :inherit modus-themes-search-success)))
    `(ctrlf-highlight-line ((,c :inherit modus-themes-hl-line)))
    `(ctrlf-highlight-passive ((,c :inherit modus-themes-search-success-lazy)))
;;;;; custom (M-x customize)
    `(custom-button ((,c :inherit modus-themes-box-button)))
    `(custom-button-mouse ((,c :inherit (highlight custom-button))))
    `(custom-button-pressed ((,c :inherit modus-themes-box-button-pressed)))
    `(custom-changed ((,c :inherit modus-themes-subtle-cyan)))
    `(custom-comment ((,c :inherit shadow)))
    `(custom-comment-tag ((,c :background ,bg-alt :foreground ,yellow-cooler)))
    `(custom-face-tag ((,c :inherit bold :foreground ,blue-intense)))
    `(custom-group-tag ((,c :inherit modus-themes-pseudo-header :foreground ,magenta-warmer)))
    `(custom-group-tag-1 ((,c :inherit modus-themes-special-warm)))
    `(custom-invalid ((,c :inherit (modus-themes-intense-red bold))))
    `(custom-modified ((,c :inherit modus-themes-subtle-cyan)))
    `(custom-rogue ((,c :inherit modus-themes-refine-magenta)))
    `(custom-set ((,c :foreground ,blue-warmer)))
    `(custom-state ((,c :foreground ,red-faint)))
    `(custom-themed ((,c :inherit modus-themes-subtle-blue)))
    `(custom-variable-obsolete ((,c :inherit shadow)))
    `(custom-variable-tag ((,c :foreground ,cyan)))
;;;;; dap-mode
    `(dap-mouse-eval-thing-face ((,c :box (:line-width -1 :color ,blue :style nil)
                                         :background ,bg-active :foreground ,fg-main)))
    `(dap-result-overlay-face ((,c :box (:line-width -1 :color ,bg-active :style nil)
                                       :background ,bg-active :foreground ,fg-main)))
    `(dap-ui-breakpoint-verified-fringe ((,c :inherit bold :foreground ,green)))
    `(dap-ui-compile-errline ((,c :inherit bold :foreground ,red-intense)))
    `(dap-ui-locals-scope-face ((,c :inherit bold :foreground ,magenta :underline t)))
    `(dap-ui-locals-variable-face ((,c :inherit bold :foreground ,cyan)))
    `(dap-ui-locals-variable-leaf-face ((,c :inherit italic :foreground ,cyan-cooler)))
    `(dap-ui-marker-face ((,c :inherit modus-themes-subtle-blue)))
    `(dap-ui-sessions-stack-frame-face ((,c :inherit bold :foreground ,magenta-warmer)))
    `(dap-ui-sessions-terminated-active-face ((,c :inherit bold :foreground ,fg-alt)))
    `(dap-ui-sessions-terminated-face ((,c :inherit shadow)))
;;;;; deadgrep
    `(deadgrep-filename-face ((,c :inherit bold :foreground ,fg-special-cold)))
    `(deadgrep-match-face ((,c :inherit modus-themes-special-calm)))
    `(deadgrep-meta-face ((,c :inherit shadow)))
    `(deadgrep-regexp-metachar-face ((,c :inherit bold :foreground ,yellow-intense)))
    `(deadgrep-search-term-face ((,c :inherit bold :foreground ,green-intense)))
;;;;; debbugs
    `(debbugs-gnu-archived ((,c :inverse-video t)))
    `(debbugs-gnu-done ((,c :inherit shadow)))
    `(debbugs-gnu-forwarded ((,c :foreground ,fg-special-warm)))
    `(debbugs-gnu-handled ((,c :foreground ,blue)))
    `(debbugs-gnu-new ((,c :foreground ,red)))
    `(debbugs-gnu-pending ((,c :foreground ,cyan)))
    `(debbugs-gnu-stale-1 ((,c :foreground ,yellow-nuanced-fg)))
    `(debbugs-gnu-stale-2 ((,c :foreground ,yellow)))
    `(debbugs-gnu-stale-3 ((,c :foreground ,yellow-warmer)))
    `(debbugs-gnu-stale-4 ((,c :foreground ,yellow-cooler)))
    `(debbugs-gnu-stale-5 ((,c :foreground ,red-warmer)))
    `(debbugs-gnu-tagged ((,c :foreground ,magenta-warmer)))
;;;;; deft
    `(deft-filter-string-face ((,c :inherit bold :foreground ,blue)))
    `(deft-header-face ((,c :foreground ,fg-special-warm)))
    `(deft-separator-face ((,c :foreground "gray50")))
    `(deft-summary-face ((,c :inherit (shadow modus-themes-slant))))
    `(deft-time-face ((,c :foreground ,cyan)))
    `(deft-title-face ((,c :inherit bold)))
;;;;; denote
    `(denote-faces-date ((,c :foreground ,cyan)))
    `(denote-faces-keywords ((,c :inherit modus-themes-bold :foreground ,magenta-warmer)))
;;;;; devdocs
    `(devdocs-code-block ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))
;;;;; dictionary
    `(dictionary-button-face ((,c :inherit bold :foreground ,fg-special-cold)))
    `(dictionary-reference-face ((,c :inherit button)))
    `(dictionary-word-definition-face (()))
    `(dictionary-word-entry-face ((,c :inherit font-lock-comment-face)))
;;;;; diff-hl
    `(diff-hl-change ((,c :inherit modus-themes-fringe-yellow)))
    `(diff-hl-delete ((,c :inherit modus-themes-fringe-red)))
    `(diff-hl-insert ((,c :inherit modus-themes-grue-background-active)))
    `(diff-hl-reverted-hunk-highlight ((,c :background ,fg-main :foreground ,bg-main)))
;;;;; diff-mode
    `(diff-added ((,c :inherit modus-themes-diff-added)))
    `(diff-changed ((,c :inherit modus-themes-diff-changed :extend t)))
    `(diff-changed-unspecified ((,c :inherit diff-changed)))
    `(diff-context ((,c ,@(unless (eq modus-themes-diffs 'bg-only) (list :foreground "gray50")))))
    `(diff-error ((,c :inherit modus-themes-intense-red)))
    `(diff-file-header ((,c :inherit (bold diff-header))))
    `(diff-function ((,c :inherit modus-themes-diff-heading)))
    `(diff-header ((,c :foreground ,fg-main)))
    `(diff-hunk-header ((,c :inherit (bold modus-themes-diff-heading))))
    `(diff-index ((,c :inherit bold :foreground ,blue-warmer)))
    `(diff-indicator-added ((,c :inherit (modus-themes-grue diff-added bold))))
    `(diff-indicator-changed ((,c :inherit (diff-changed bold) :foreground ,yellow)))
    `(diff-indicator-removed ((,c :inherit (diff-removed bold) :foreground ,red)))
    `(diff-nonexistent ((,c :inherit (modus-themes-neutral bold))))
    `(diff-refine-added ((,c :inherit modus-themes-diff-refine-added)))
    `(diff-refine-changed ((,c :inherit modus-themes-diff-refine-changed)))
    `(diff-refine-removed ((,c :inherit modus-themes-diff-refine-removed)))
    `(diff-removed ((,c :inherit modus-themes-diff-removed)))
;;;;; dim-autoload
    `(dim-autoload-cookie-line ((,c :inherit font-lock-comment-face)))
;;;;; dir-treeview
    `(dir-treeview-archive-face ((,c :foreground ,fg-special-warm)))
    `(dir-treeview-archive-icon-face ((,c :inherit dir-treeview-default-icon-face :foreground ,yellow)))
    `(dir-treeview-audio-face ((,c :foreground ,magenta)))
    `(dir-treeview-audio-icon-face ((,c :inherit dir-treeview-default-icon-face :foreground ,magenta-warmer)))
    `(dir-treeview-control-face ((,c :inherit shadow)))
    `(dir-treeview-control-mouse-face ((,c :inherit highlight)))
    `(dir-treeview-default-icon-face ((,c :inherit (shadow bold) :family "Font Awesome")))
    `(dir-treeview-default-filename-face ((,c :foreground ,fg-main)))
    `(dir-treeview-directory-face ((,c :foreground ,blue)))
    `(dir-treeview-directory-icon-face ((,c :inherit dir-treeview-default-icon-face :foreground ,blue-warmer)))
    `(dir-treeview-executable-face ((,c :foreground ,red-warmer)))
    `(dir-treeview-executable-icon-face ((,c :inherit dir-treeview-default-icon-face :foreground ,red-cooler)))
    `(dir-treeview-image-face ((,c :foreground ,green-cooler)))
    `(dir-treeview-image-icon-face ((,c :inherit dir-treeview-default-icon-face :foreground ,green-warmer)))
    `(dir-treeview-indent-face ((,c :inherit shadow)))
    `(dir-treeview-label-mouse-face ((,c :inherit highlight)))
    `(dir-treeview-start-dir-face ((,c :inherit modus-themes-pseudo-header)))
    `(dir-treeview-symlink-face ((,c :inherit modus-themes-link-symlink)))
    `(dir-treeview-video-face ((,c :foreground ,magenta-cooler)))
    `(dir-treeview-video-icon-face ((,c :inherit dir-treeview-default-icon-face :foreground ,magenta-cooler)))
;;;;; dired
    `(dired-broken-symlink ((,c :inherit modus-themes-link-broken)))
    `(dired-directory ((,c :foreground ,blue)))
    `(dired-flagged ((,c :inherit modus-themes-mark-del)))
    `(dired-header ((,c :inherit modus-themes-pseudo-header)))
    `(dired-ignored ((,c :inherit shadow)))
    `(dired-mark ((,c :inherit modus-themes-mark-symbol)))
    `(dired-marked ((,c :inherit modus-themes-mark-sel)))
    `(dired-perm-write ((,c :foreground ,fg-special-warm)))
    `(dired-symlink ((,c :inherit modus-themes-link-symlink)))
    `(dired-warning ((,c :inherit bold :foreground ,yellow)))
;;;;; dired-async
    `(dired-async-failures ((,c :inherit bold :foreground ,red)))
    `(dired-async-message ((,c :inherit bold :foreground ,blue)))
    `(dired-async-mode-message ((,c :inherit bold :foreground ,cyan)))
;;;;; dired-git
    `(dired-git-branch-else ((,c :inherit bold :foreground ,magenta-warmer)))
    `(dired-git-branch-master ((,c :inherit bold :foreground ,magenta-cooler)))
;;;;; dired-git-info
    `(dgi-commit-message-face ((,c :foreground ,cyan-cooler)))
;;;;; dired-narrow
    `(dired-narrow-blink ((,c :inherit (modus-themes-subtle-cyan bold))))
;;;;; dired-subtree
    ;; remove backgrounds from dired-subtree faces, else they break
    ;; dired-{flagged,marked} and any other face that sets a background
    ;; such as hl-line.  Also, denoting depth by varying shades of gray
    ;; is not good for accessibility.
    `(dired-subtree-depth-1-face (()))
    `(dired-subtree-depth-2-face (()))
    `(dired-subtree-depth-3-face (()))
    `(dired-subtree-depth-4-face (()))
    `(dired-subtree-depth-5-face (()))
    `(dired-subtree-depth-6-face (()))
;;;;; diredfl
    `(diredfl-autofile-name ((,c :background ,bg-alt)))
    `(diredfl-compressed-file-name ((,c :foreground ,yellow-cooler)))
    `(diredfl-compressed-file-suffix ((,c :foreground ,red)))
    `(diredfl-date-time ((,c :foreground ,date)))
    `(diredfl-deletion ((,c :inherit dired-flagged)))
    `(diredfl-deletion-file-name ((,c :inherit diredfl-deletion)))
    `(diredfl-dir-heading ((,c :inherit bold)))
    `(diredfl-dir-name ((,c :inherit dired-directory)))
    `(diredfl-dir-priv ((,c :inherit dired-directory)))
    `(diredfl-exec-priv ((,c :foreground ,rainbow-3)))
    `(diredfl-executable-tag ((,c :inherit diredfl-exec-priv)))
    `(diredfl-file-name ((,c :foreground ,fg-main)))
    `(diredfl-file-suffix ((,c :foreground ,variable)))
    `(diredfl-flag-mark ((,c :inherit dired-marked)))
    `(diredfl-flag-mark-line ((,c :inherit dired-marked)))
    `(diredfl-ignored-file-name ((,c :inherit shadow)))
    `(diredfl-link-priv ((,c :foreground ,link)))
    `(diredfl-no-priv ((,c :inherit shadow)))
    `(diredfl-number ((,c :inherit shadow)))
    `(diredfl-other-priv ((,c :foreground ,rainbow-0)))
    `(diredfl-rare-priv ((,c :foreground ,rainbow-0)))
    `(diredfl-read-priv ((,c :foreground ,rainbow-1)))
    `(diredfl-symlink ((,c :inherit dired-symlink)))
    `(diredfl-tagged-autofile-name ((,c :inherit (diredfl-autofile-name dired-marked))))
    `(diredfl-write-priv ((,c :foreground ,rainbow-2)))
;;;;; display-fill-column-indicator-mode
    `(fill-column-indicator ((,c :height 1 :background ,bg-region :foreground ,bg-region)))
;;;;; doom-modeline
    `(doom-modeline-bar ((,c :inherit modus-themes-intense-blue)))
    `(doom-modeline-bar-inactive ((,c :background ,bg-inactive)))
    `(doom-modeline-battery-charging ((,c :foreground ,success)))
    `(doom-modeline-battery-critical ((,c :underline t :foreground ,err)))
    `(doom-modeline-battery-error ((,c :underline t :foreground ,err)))
    `(doom-modeline-battery-full (( )))
    `(doom-modeline-battery-warning ((,c :inherit warning)))
    `(doom-modeline-buffer-file ((,c :inherit bold)))
    `(doom-modeline-buffer-major-mode (( )))
    `(doom-modeline-buffer-minor-mode (( )))
    `(doom-modeline-buffer-modified ((,c :foreground ,err)))
    `(doom-modeline-buffer-path (( )))
    `(doom-modeline-evil-emacs-state ((,c :inherit italic)))
    `(doom-modeline-evil-insert-state ((,c :foreground ,note)))
    `(doom-modeline-evil-motion-state (( )))
    `(doom-modeline-evil-normal-state (( )))
    `(doom-modeline-evil-operator-state ((,c :inherit bold)))
    `(doom-modeline-evil-replace-state ((,c :inherit error)))
    `(doom-modeline-evil-visual-state ((,c :inherit warning)))
    `(doom-modeline-info ((,c :inherit success)))
    `(doom-modeline-input-method (( )))
    `(doom-modeline-lsp-error ((,c :inherit bold-italic)))
    `(doom-modeline-lsp-running (( )))
    `(doom-modeline-lsp-success ((,c :inherit success)))
    `(doom-modeline-lsp-warning ((,c :inherit warning)))
    `(doom-modeline-notification ((,c :inherit error)))
    `(doom-modeline-project-dir (( )))
    `(doom-modeline-project-parent-dir (( )))
    `(doom-modeline-project-root-dir (( )))
    `(doom-modeline-repl-success ((,c :inherit success)))
    `(doom-modeline-repl-warning ((,c :inherit warning)))
    `(doom-modeline-time (( )))
    `(doom-modeline-urgent ((,c :inherit bold-italic :foreground ,err)))
    `(doom-modeline-warning ((,c :inherit warning)))
;;;;; easy-jekyll
    `(easy-jekyll-help-face ((,c :background ,bg-dim :foreground ,blue-cooler)))
;;;;; ebdb
    `(ebdb-address-default ((,c :foreground ,fg-special-calm)))
    `(ebdb-defunct ((,c :inherit shadow)))
    `(ebdb-field-hidden ((,c :foreground ,magenta)))
    `(ebdb-label ((,c :foreground ,cyan-cooler)))
    `(ebdb-mail-default ((,c :foreground ,fg-main)))
    `(ebdb-mail-primary ((,c :foreground ,magenta-warmer)))
    `(ebdb-marked ((,c :background ,cyan-intense-bg)))
    `(ebdb-organization-name ((,c :foreground ,red-cooler)))
    `(ebdb-person-name ((,c :foreground ,magenta-cooler)))
    `(ebdb-phone-default ((,c :foreground ,cyan)))
    `(eieio-custom-slot-tag-face ((,c :foreground ,red-warmer)))
;;;;; ediff
    `(ediff-current-diff-A ((,c :inherit modus-themes-diff-removed)))
    `(ediff-current-diff-Ancestor ((,c ,@(modus-themes--diff
                                              bg-special-cold fg-special-cold
                                              blue-nuanced-bg blue))))
    `(ediff-current-diff-B ((,c :inherit modus-themes-diff-added)))
    `(ediff-current-diff-C ((,c :inherit modus-themes-diff-changed)))
    `(ediff-even-diff-A ((,c :background ,bg-alt)))
    `(ediff-even-diff-Ancestor ((,c :background ,bg-alt)))
    `(ediff-even-diff-B ((,c :background ,bg-alt)))
    `(ediff-even-diff-C ((,c :background ,bg-alt)))
    `(ediff-fine-diff-A ((,c :inherit modus-themes-diff-refine-removed)))
    `(ediff-fine-diff-Ancestor ((,c :inherit modus-themes-refine-cyan)))
    `(ediff-fine-diff-B ((,c :inherit modus-themes-diff-refine-added)))
    `(ediff-fine-diff-C ((,c :inherit modus-themes-diff-refine-changed)))
    `(ediff-odd-diff-A ((,c :inherit ediff-even-diff-A)))
    `(ediff-odd-diff-Ancestor ((,c :inherit ediff-even-diff-Ancestor)))
    `(ediff-odd-diff-B ((,c :inherit ediff-even-diff-B)))
    `(ediff-odd-diff-C ((,c :inherit ediff-even-diff-C)))
;;;;; ein (Emacs IPython Notebook)
    `(ein:basecell-input-area-face ((,c :background ,bg-dim :extend t)))
    `(ein:cell-output-area (( )))
    `(ein:cell-output-area-error ((,c :background ,red-nuanced-bg :extend t)))
    `(ein:cell-output-stderr ((,c :background ,red-nuanced-bg :extend t)))
    `(ein:markdowncell-input-area-face (( )))
    `(ein:notification-tab-normal ((,c :underline t)))
;;;;; eglot
    `(eglot-mode-line ((,c :inherit modus-themes-bold :foreground ,magenta)))
;;;;; el-search
    `(el-search-highlight-in-prompt-face ((,c :inherit bold :foreground ,magenta-warmer)))
    `(el-search-match ((,c :inherit modus-themes-search-success)))
    `(el-search-other-match ((,c :inherit modus-themes-special-mild)))
    `(el-search-occur-match ((,c :inherit modus-themes-special-calm)))
;;;;; eldoc
    ;; NOTE: see https://github.com/purcell/package-lint/issues/187
    (list 'eldoc-highlight-function-argument `((,c :inherit bold
                                                       :background ,yellow-nuanced-bg
                                                       :foreground ,yellow-cooler)))
;;;;; eldoc-box
    `(eldoc-box-body ((,c :background ,bg-alt :foreground ,fg-main)))
    `(eldoc-box-border ((,c :background ,fg-alt)))
;;;;; elfeed
    `(elfeed-log-date-face ((,c :inherit elfeed-search-date-face)))
    `(elfeed-log-debug-level-face ((,c :inherit elfeed-search-filter-face)))
    `(elfeed-log-error-level-face ((,c :inherit error)))
    `(elfeed-log-info-level-face ((,c :inherit success)))
    `(elfeed-log-warn-level-face ((,c :inherit warning)))
    `(elfeed-search-date-face ((,c :foreground ,cyan)))
    `(elfeed-search-feed-face ((,c :foreground ,blue-faint)))
    `(elfeed-search-filter-face ((,c :inherit bold :foreground ,magenta)))
    `(elfeed-search-last-update-face ((,c :inherit bold :foreground ,cyan)))
    `(elfeed-search-tag-face ((,c :foreground ,magenta-faint)))
    `(elfeed-search-title-face ((,c :foreground ,fg-dim)))
    `(elfeed-search-unread-count-face ((,c :inherit bold :foreground ,fg-active)))
    `(elfeed-search-unread-title-face ((,c :inherit bold :foreground ,fg-main)))
;;;;; elfeed-score
    `(elfeed-score-date-face ((,c :foreground ,blue)))
    `(elfeed-score-debug-level-face ((,c :foreground ,magenta-cooler)))
    `(elfeed-score-error-level-face ((,c :foreground ,red)))
    `(elfeed-score-info-level-face ((,c :foreground ,cyan)))
    `(elfeed-score-warn-level-face ((,c :foreground ,yellow)))
;;;;; elpher
    `(elpher-gemini-heading1 ((,c :inherit modus-themes-heading-1)))
    `(elpher-gemini-heading2 ((,c :inherit modus-themes-heading-2)))
    `(elpher-gemini-heading3 ((,c :inherit modus-themes-heading-3)))
;;;;; embark
    `(embark-keybinding ((,c :inherit modus-themes-key-binding)))
    `(embark-collect-marked ((,c :inherit modus-themes-mark-sel)))
;;;;; ement (ement.el)
    `(ement-room-fully-read-marker ((,c :background ,cyan-subtle-bg)))
    `(ement-room-membership ((,c :inherit shadow)))
    `(ement-room-mention ((,c :inherit highlight)))
    `(ement-room-name ((,c :inherit bold)))
    `(ement-room-reactions ((,c :inherit shadow)))
    `(ement-room-read-receipt-marker ((,c :background ,yellow-subtle-bg)))
    `(ement-room-self ((,c :inherit bold :foreground ,magenta)))
    `(ement-room-self-message ((,c :foreground ,magenta-faint)))
    `(ement-room-timestamp ((,c :inherit shadow)))
    `(ement-room-timestamp-header ((,c :inherit bold :foreground ,cyan)))
    `(ement-room-user ((,c :inherit bold :foreground ,blue)))
;;;;; emms
    `(emms-browser-album-face ((,c :foreground ,magenta-cooler)))
    `(emms-browser-artist-face ((,c :foreground ,cyan)))
    `(emms-browser-composer-face ((,c :foreground ,magenta-warmer)))
    `(emms-browser-performer-face ((,c :inherit emms-browser-artist-face)))
    `(emms-browser-track-face ((,c :inherit emms-playlist-track-face)))
    `(emms-browser-year/genre-face ((,c :foreground ,cyan-cooler)))
    `(emms-playlist-track-face ((,c :foreground ,blue-warmer)))
    `(emms-playlist-selected-face ((,c :inherit bold :foreground ,blue-cooler)))
    `(emms-metaplaylist-mode-current-face ((,c :inherit emms-playlist-selected-face)))
    `(emms-metaplaylist-mode-face ((,c :foreground ,cyan)))
;;;;; enh-ruby-mode (enhanced-ruby-mode)
    `(enh-ruby-heredoc-delimiter-face ((,c :inherit font-lock-constant-face)))
    `(enh-ruby-op-face ((,c :foreground ,fg-main)))
    `(enh-ruby-regexp-delimiter-face ((,c :inherit font-lock-regexp-grouping-construct)))
    `(enh-ruby-regexp-face ((,c :inherit font-lock-string-face)))
    `(enh-ruby-string-delimiter-face ((,c :inherit font-lock-string-face)))
    `(erm-syn-errline ((,c :inherit modus-themes-lang-error)))
    `(erm-syn-warnline ((,c :inherit modus-themes-lang-warning)))
;;;;; epa
    `(epa-field-body ((,c :foreground ,fg-main)))
    `(epa-field-name ((,c :inherit bold :foreground ,fg-dim)))
    `(epa-mark ((,c :inherit bold :foreground ,magenta)))
    `(epa-string ((,c :foreground ,blue-warmer)))
    `(epa-validity-disabled ((,c :foreground ,red)))
    `(epa-validity-high ((,c :inherit bold :foreground ,cyan)))
    `(epa-validity-low ((,c :inherit shadow)))
    `(epa-validity-medium ((,c :foreground ,green-warmer)))
;;;;; equake
    `(equake-buffer-face ((,c :background ,bg-main :foreground ,fg-main)))
    `(equake-shell-type-eshell ((,c :background ,bg-inactive :foreground ,blue)))
    `(equake-shell-type-rash ((,c :background ,bg-inactive :foreground ,red)))
    `(equake-shell-type-shell ((,c :background ,bg-inactive :foreground ,cyan)))
    `(equake-shell-type-term ((,c :background ,bg-inactive :foreground ,yellow)))
    `(equake-shell-type-vterm ((,c :background ,bg-inactive :foreground ,magenta)))
    `(equake-tab-active ((,c :background ,fg-alt :foreground ,bg-alt)))
    `(equake-tab-inactive ((,c :foreground ,fg-inactive)))
;;;;; erc
    `(erc-action-face ((,c :foreground ,cyan-cooler)))
    `(erc-bold-face ((,c :inherit bold)))
    `(erc-button ((,c :inherit button)))
    `(erc-command-indicator-face ((,c :inherit bold :foreground ,cyan-warmer)))
    `(erc-current-nick-face ((,c :inherit bold :foreground ,red-warmer)))
    `(erc-dangerous-host-face ((,c :inherit modus-themes-intense-red)))
    `(erc-direct-msg-face ((,c :foreground ,fg-special-warm)))
    `(erc-error-face ((,c :inherit bold :foreground ,red)))
    `(erc-fool-face ((,c :inherit shadow)))
    `(erc-header-line ((,c :background ,bg-active)))
    `(erc-input-face ((,c :foreground ,magenta)))
    `(erc-inverse-face ((,c :inherit erc-default-face :inverse-video t)))
    `(erc-keyword-face ((,c :inherit bold :foreground ,magenta-cooler)))
    `(erc-my-nick-face ((,c :inherit bold :foreground ,magenta)))
    `(erc-my-nick-prefix-face ((,c :inherit erc-my-nick-face)))
    `(erc-nick-default-face ((,c :inherit bold :foreground ,blue)))
    `(erc-nick-msg-face ((,c :inherit warning)))
    `(erc-nick-prefix-face ((,c :inherit erc-nick-default-face)))
    `(erc-notice-face ((,c :inherit font-lock-comment-face)))
    `(erc-pal-face ((,c :inherit bold :foreground ,magenta-warmer)))
    `(erc-prompt-face ((,c :inherit modus-themes-prompt)))
    `(erc-timestamp-face ((,c :foreground ,cyan)))
    `(erc-underline-face ((,c :underline t)))
    `(bg:erc-color-face0 ((,c :background "white")))
    `(bg:erc-color-face1 ((,c :background "black")))
    `(bg:erc-color-face10 ((,c :background ,cyan-subtle-bg)))
    `(bg:erc-color-face11 ((,c :background ,cyan-intense-bg)))
    `(bg:erc-color-face12 ((,c :background ,blue-subtle-bg)))
    `(bg:erc-color-face13 ((,c :background ,magenta-subtle-bg)))
    `(bg:erc-color-face14 ((,c :background "gray60")))
    `(bg:erc-color-face15 ((,c :background "gray80")))
    `(bg:erc-color-face2 ((,c :background ,blue-intense-bg)))
    `(bg:erc-color-face3 ((,c :background ,green-intense-bg)))
    `(bg:erc-color-face4 ((,c :background ,red-subtle-bg)))
    `(bg:erc-color-face5 ((,c :background ,red-intense-bg)))
    `(bg:erc-color-face6 ((,c :background ,magenta-refine-bg)))
    `(bg:erc-color-face7 ((,c :background ,yellow-subtle-bg)))
    `(bg:erc-color-face8 ((,c :background ,yellow-refine-bg)))
    `(bg:erc-color-face9 ((,c :background ,green-subtle-bg)))
    `(fg:erc-color-face0 ((,c :foreground "white")))
    `(fg:erc-color-face1 ((,c :foreground "black")))
    `(fg:erc-color-face10 ((,c :foreground ,cyan)))
    `(fg:erc-color-face11 ((,c :foreground ,cyan-cooler)))
    `(fg:erc-color-face12 ((,c :foreground ,blue)))
    `(fg:erc-color-face13 ((,c :foreground ,magenta-warmer)))
    `(fg:erc-color-face14 ((,c :foreground "gray60")))
    `(fg:erc-color-face15 ((,c :foreground "gray80")))
    `(fg:erc-color-face2 ((,c :foreground ,blue-cooler)))
    `(fg:erc-color-face3 ((,c :foreground ,green)))
    `(fg:erc-color-face4 ((,c :foreground ,red)))
    `(fg:erc-color-face5 ((,c :foreground ,red-warmer)))
    `(fg:erc-color-face6 ((,c :foreground ,magenta-cooler)))
    `(fg:erc-color-face7 ((,c :foreground ,yellow-cooler)))
    `(fg:erc-color-face8 ((,c :foreground ,yellow-warmer)))
    `(fg:erc-color-face9 ((,c :foreground ,green-cooler)))
;;;;; eros
    `(eros-result-overlay-face ((,c :box (:line-width -1 :color ,blue)
                                        :background ,bg-dim :foreground ,fg-dim)))
;;;;; ert
    `(ert-test-result-expected ((,c :inherit modus-themes-intense-green)))
    `(ert-test-result-unexpected ((,c :inherit modus-themes-intense-red)))
;;;;; eshell
    `(eshell-ls-archive ((,c :foreground ,cyan-warmer)))
    `(eshell-ls-backup ((,c :inherit shadow)))
    `(eshell-ls-clutter ((,c :foreground ,red-warmer)))
    `(eshell-ls-directory ((,c :foreground ,blue-warmer)))
    `(eshell-ls-executable ((,c :foreground ,magenta-warmer)))
    `(eshell-ls-missing ((,c :inherit modus-themes-intense-red)))
    `(eshell-ls-product ((,c :inherit shadow)))
    `(eshell-ls-readonly ((,c :foreground ,yellow-faint)))
    `(eshell-ls-special ((,c :foreground ,magenta)))
    `(eshell-ls-symlink ((,c :inherit modus-themes-link-symlink)))
    `(eshell-ls-unreadable ((,c :background ,bg-inactive :foreground ,fg-inactive)))
    `(eshell-prompt ((,c :inherit modus-themes-prompt)))
;;;;; eshell-fringe-status
    `(eshell-fringe-status-failure ((,c :inherit error)))
    `(eshell-fringe-status-success ((,c :inherit success)))
;;;;; eshell-git-prompt
    `(eshell-git-prompt-add-face ((,c :foreground ,magenta-cooler)))
    `(eshell-git-prompt-branch-face ((,c :foreground ,magenta-warmer)))
    `(eshell-git-prompt-directory-face ((,c :inherit bold :foreground ,blue)))
    `(eshell-git-prompt-exit-fail-face ((,c :inherit error)))
    `(eshell-git-prompt-exit-success-face ((,c :inherit success)))
    `(eshell-git-prompt-modified-face ((,c :foreground ,yellow)))
    `(eshell-git-prompt-powerline-clean-face ((,c :background ,green-refine-bg)))
    `(eshell-git-prompt-powerline-dir-face ((,c :background ,blue-refine-bg)))
    `(eshell-git-prompt-powerline-not-clean-face ((,c :background ,yellow-fringe-bg)))
    `(eshell-git-prompt-robyrussell-branch-face ((,c :foreground ,magenta-warmer)))
    `(eshell-git-prompt-robyrussell-git-dirty-face ((,c :foreground ,yellow)))
    `(eshell-git-prompt-robyrussell-git-face ((,c :foreground ,magenta-cooler)))
;;;;; eshell-prompt-extras (epe)
    `(epe-dir-face ((,c :inherit bold :foreground ,blue)))
    `(epe-git-dir-face ((,c :foreground ,red-cooler)))
    `(epe-git-face ((,c :foreground ,magenta-warmer)))
    `(epe-pipeline-delimiter-face ((,c :inherit shadow)))
    `(epe-pipeline-host-face ((,c :foreground ,fg-main)))
    `(epe-pipeline-time-face ((,c :foreground ,fg-main)))
    `(epe-pipeline-user-face ((,c :foreground ,magenta-cooler)))
    `(epe-remote-face ((,c :inherit (shadow modus-themes-slant))))
    `(epe-status-face ((,c :foreground ,magenta-cooler)))
    `(epe-venv-face ((,c :inherit (shadow modus-themes-slant))))
;;;;; eshell-syntax-highlighting
    `(eshell-syntax-highlighting-directory-face ((,c :inherit eshell-ls-directory)))
    `(eshell-syntax-highlighting-invalid-face ((,c :foreground ,red)))
    `(eshell-syntax-highlighting-shell-command-face ((,c :foreground ,fg-main)))
;;;;; evil-mode
    `(evil-ex-commands ((,c :foreground ,magenta-cooler)))
    `(evil-ex-info ((,c :foreground ,cyan-cooler)))
    `(evil-ex-lazy-highlight ((,c :inherit modus-themes-search-success-lazy)))
    `(evil-ex-search ((,c :inherit modus-themes-search-success)))
    `(evil-ex-substitute-matches ((,c :inherit modus-themes-refine-yellow :underline t)))
    `(evil-ex-substitute-replacement ((,c :inherit modus-themes-search-success)))
;;;;; evil-goggles
    `(evil-goggles-change-face ((,c :inherit modus-themes-refine-yellow)))
    `(evil-goggles-commentary-face ((,c :inherit (modus-themes-subtle-neutral modus-themes-slant))))
    `(evil-goggles-default-face ((,c :inherit modus-themes-subtle-neutral)))
    `(evil-goggles-delete-face ((,c :inherit modus-themes-refine-red)))
    `(evil-goggles-fill-and-move-face ((,c :inherit evil-goggles-default-face)))
    `(evil-goggles-indent-face ((,c :inherit evil-goggles-default-face)))
    `(evil-goggles-join-face ((,c :inherit modus-themes-subtle-green)))
    `(evil-goggles-nerd-commenter-face ((,c :inherit evil-goggles-commentary-face)))
    `(evil-goggles-paste-face ((,c :inherit modus-themes-subtle-cyan)))
    `(evil-goggles-record-macro-face ((,c :inherit modus-themes-special-cold)))
    `(evil-goggles-replace-with-register-face ((,c :inherit modus-themes-refine-magenta)))
    `(evil-goggles-set-marker-face ((,c :inherit modus-themes-intense-magenta)))
    `(evil-goggles-shift-face ((,c :inherit evil-goggles-default-face)))
    `(evil-goggles-surround-face ((,c :inherit evil-goggles-default-face)))
    `(evil-goggles-yank-face ((,c :inherit modus-themes-subtle-blue)))
;;;;; evil-snipe
    `(evil-snipe-first-match-face ((,c :inherit (bold modus-themes-intense-blue))))
    `(evil-snipe-matches-face ((,c :inherit modus-themes-refine-magenta)))
;;;;; evil-visual-mark-mode
    `(evil-visual-mark-face ((,c :inherit modus-themes-intense-magenta)))
;;;;; eww
    `(eww-invalid-certificate ((,c :foreground ,red-faint)))
    `(eww-valid-certificate ((,c :foreground ,blue-faint)))
    `(eww-form-checkbox ((,c :inherit eww-form-text)))
    `(eww-form-file ((,c :inherit eww-form-submit)))
    `(eww-form-select ((,c :inherit eww-form-submit)))
    `(eww-form-submit ((,c :inherit modus-themes-box-button)))
    `(eww-form-text ((,c :inherit widget-field)))
    `(eww-form-textarea ((,c :inherit eww-form-text)))
;;;;; eyebrowse
    `(eyebrowse-mode-line-active ((,c :inherit bold :foreground ,blue)))
;;;;; fancy-dabbrev
    `(fancy-dabbrev-menu-face ((,c :background ,bg-alt :foreground ,fg-alt)))
    `(fancy-dabbrev-preview-face ((,c :inherit shadow :underline t)))
    `(fancy-dabbrev-selection-face ((,c :inherit (modus-themes-intense-cyan bold))))
;;;;; flycheck
    `(flycheck-error ((,c :inherit modus-themes-lang-error)))
    `(flycheck-error-list-checker-name ((,c :foreground ,magenta)))
    `(flycheck-error-list-column-number ((,c :foreground ,fg-special-cold)))
    `(flycheck-error-list-error ((,c :inherit modus-themes-bold :foreground ,red)))
    `(flycheck-error-list-filename ((,c :foreground ,blue)))
    `(flycheck-error-list-highlight ((,c :inherit modus-themes-hl-line)))
    `(flycheck-error-list-id ((,c :foreground ,magenta-cooler)))
    `(flycheck-error-list-id-with-explainer ((,c :inherit flycheck-error-list-id :box t)))
    `(flycheck-error-list-info ((,c :foreground ,cyan)))
    `(flycheck-error-list-line-number ((,c :foreground ,fg-special-warm)))
    `(flycheck-error-list-warning ((,c :foreground ,yellow)))
    `(flycheck-fringe-error ((,c :inherit modus-themes-fringe-red)))
    `(flycheck-fringe-info ((,c :inherit modus-themes-fringe-cyan)))
    `(flycheck-fringe-warning ((,c :inherit modus-themes-fringe-yellow)))
    `(flycheck-info ((,c :inherit modus-themes-lang-note)))
    `(flycheck-verify-select-checker ((,c :box (:line-width 1 :color nil :style released-button))))
    `(flycheck-warning ((,c :inherit modus-themes-lang-warning)))
;;;;; flycheck-color-mode-line
    `(flycheck-color-mode-line-error-face ((,c :inherit flycheck-fringe-error)))
    `(flycheck-color-mode-line-info-face ((,c :inherit flycheck-fringe-info)))
    `(flycheck-color-mode-line-running-face ((,c :inherit italic :foreground ,fg-inactive)))
    `(flycheck-color-mode-line-info-face ((,c :inherit flycheck-fringe-warning)))
;;;;; flycheck-indicator
    `(flycheck-indicator-disabled ((,c :inherit modus-themes-slant :foreground ,fg-inactive)))
    `(flycheck-indicator-error ((,c :inherit modus-themes-bold :foreground ,red)))
    `(flycheck-indicator-info ((,c :inherit modus-themes-bold :foreground ,blue)))
    `(flycheck-indicator-running ((,c :inherit modus-themes-bold :foreground ,magenta)))
    `(flycheck-indicator-success ((,c :inherit (modus-themes-bold modus-themes-grue-active))))
    `(flycheck-indicator-warning ((,c :inherit modus-themes-bold :foreground ,yellow)))
;;;;; flycheck-posframe
    `(flycheck-posframe-background-face ((,c :background ,bg-alt)))
    `(flycheck-posframe-border-face ((,c :inherit shadow)))
    `(flycheck-posframe-error-face ((,c :inherit bold :foreground ,red)))
    `(flycheck-posframe-face ((,c :inherit modus-themes-slant :foreground ,fg-main)))
    `(flycheck-posframe-info-face ((,c :inherit bold :foreground ,cyan)))
    `(flycheck-posframe-warning-face ((,c :inherit bold :foreground ,yellow)))
;;;;; flymake
    `(flymake-error ((,c :inherit modus-themes-lang-error)))
    `(flymake-note ((,c :inherit modus-themes-lang-note)))
    `(flymake-warning ((,c :inherit modus-themes-lang-warning)))
;;;;; flyspell
    `(flyspell-duplicate ((,c :inherit modus-themes-lang-warning)))
    `(flyspell-incorrect ((,c :inherit modus-themes-lang-error)))
;;;;; flx
    `(flx-highlight-face ((,c :inherit modus-themes-completion-match-0)))
;;;;; freeze-it
    `(freeze-it-show ((,c :background ,bg-dim :foreground ,fg-special-warm)))
;;;;; focus
    `(focus-unfocused ((,c :inherit shadow)))
;;;;; fold-this
    `(fold-this-overlay ((,c :inherit modus-themes-special-mild)))
;;;;; font-lock
    `(font-lock-builtin-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(font-lock-comment-delimiter-face ((,c :inherit font-lock-comment-face)))
    `(font-lock-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(font-lock-constant-face ((,c :foreground ,constant)))
    `(font-lock-doc-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(font-lock-function-name-face ((,c :foreground ,fnname)))
    `(font-lock-keyword-face ((,c :inherit modus-themes-bold :foreground ,magenta-cooler)))
    `(font-lock-negation-char-face ((,c :inherit modus-themes-bold :foreground ,err)))
    `(font-lock-preprocessor-face ((,c :foreground ,preprocessor)))
    `(font-lock-regexp-grouping-backslash ((,c :inherit modus-themes-bold :foreground ,rx-backslash)))
    `(font-lock-regexp-grouping-construct ((,c :inherit modus-themes-bold :foreground ,rx-construct)))
    `(font-lock-string-face ((,c :foreground ,string)))
    `(font-lock-type-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(font-lock-variable-name-face ((,c :foreground ,variable)))
    `(font-lock-warning-face ((,c :inherit modus-themes-bold :foreground ,warning)))
;;;;; forge
    `(forge-post-author ((,c :inherit bold :foreground ,fg-main)))
    `(forge-post-date ((,c :foreground ,fg-special-cold)))
    `(forge-topic-closed ((,c :inherit shadow)))
    `(forge-topic-merged ((,c :inherit shadow)))
    `(forge-topic-open ((,c :foreground ,fg-special-mild)))
    `(forge-topic-unmerged ((,c :inherit modus-themes-slant :foreground ,magenta)))
    `(forge-topic-unread ((,c :inherit bold :foreground ,fg-main)))
;;;;; fountain-mode
    `(fountain-character ((,c :foreground ,blue-cooler)))
    `(fountain-comment ((,c :inherit font-lock-comment-face)))
    `(fountain-dialog ((,c :foreground ,blue-warmer)))
    `(fountain-metadata-key ((,c :foreground ,green-cooler)))
    `(fountain-metadata-value ((,c :foreground ,blue)))
    `(fountain-non-printing ((,c :inherit shadow)))
    `(fountain-note ((,c :inherit modus-themes-slant :foreground ,yellow)))
    `(fountain-page-break ((,c :inherit bold :foreground ,red-warmer)))
    `(fountain-page-number ((,c :inherit bold :foreground ,red-cooler)))
    `(fountain-paren ((,c :foreground ,cyan)))
    `(fountain-scene-heading ((,c :inherit bold :foreground ,blue-nuanced-fg)))
    `(fountain-section-heading ((,c :inherit modus-themes-heading-1)))
    `(fountain-section-heading-1 ((,c :inherit modus-themes-heading-1)))
    `(fountain-section-heading-2 ((,c :inherit modus-themes-heading-2)))
    `(fountain-section-heading-3 ((,c :inherit modus-themes-heading-3)))
    `(fountain-section-heading-4 ((,c :inherit modus-themes-heading-4)))
    `(fountain-section-heading-5 ((,c :inherit modus-themes-heading-5)))
    `(fountain-synopsis ((,c :foreground ,cyan-warmer)))
    `(fountain-trans ((,c :foreground ,yellow-cooler)))
;;;;; geiser
    `(geiser-font-lock-autodoc-current-arg ((,c :inherit bold
                                                    :background ,yellow-nuanced-bg
                                                    :foreground ,yellow-cooler)))
    `(geiser-font-lock-autodoc-identifier ((,c :foreground ,cyan)))
    `(geiser-font-lock-doc-button ((,c :inherit button :foreground ,fg-special-cold)))
    `(geiser-font-lock-doc-link ((,c :inherit button)))
    `(geiser-font-lock-error-link ((,c :inherit button :foreground ,red)))
    `(geiser-font-lock-image-button ((,c :inherit button :foreground ,green-warmer)))
    `(geiser-font-lock-repl-input ((,c :inherit bold)))
    `(geiser-font-lock-repl-output ((,c :inherit font-lock-keyword-face)))
    `(geiser-font-lock-repl-prompt ((,c :inherit modus-themes-prompt)))
    `(geiser-font-lock-xref-header ((,c :inherit bold)))
    `(geiser-font-lock-xref-link ((,c :inherit button)))
;;;;; git-commit
    `(git-commit-comment-action ((,c :inherit font-lock-comment-face)))
    `(git-commit-comment-branch-local ((,c :inherit font-lock-comment-face :foreground ,blue-warmer)))
    `(git-commit-comment-branch-remote ((,c :inherit font-lock-comment-face :foreground ,magenta-warmer)))
    `(git-commit-comment-detached ((,c :inherit font-lock-comment-face :foreground ,cyan-warmer)))
    `(git-commit-comment-file ((,c :inherit font-lock-comment-face :foreground ,cyan)))
    `(git-commit-comment-heading ((,c :inherit (bold font-lock-comment-face))))
    `(git-commit-keyword ((,c :foreground ,magenta)))
    `(git-commit-known-pseudo-header ((,c :foreground ,cyan-cooler)))
    `(git-commit-nonempty-second-line ((,c :inherit error)))
    `(git-commit-overlong-summary ((,c :inherit warning)))
    `(git-commit-pseudo-header ((,c :foreground ,blue)))
    `(git-commit-summary ((,c :inherit bold :foreground ,blue)))
;;;;; git-gutter
    `(git-gutter:added ((,c :inherit modus-themes-grue-background-active)))
    `(git-gutter:deleted ((,c :inherit modus-themes-fringe-red)))
    `(git-gutter:modified ((,c :inherit modus-themes-fringe-yellow)))
    `(git-gutter:separator ((,c :inherit modus-themes-fringe-cyan)))
    `(git-gutter:unchanged ((,c :inherit modus-themes-fringe-magenta)))
;;;;; git-gutter-fr
    `(git-gutter-fr:added ((,c :inherit modus-themes-grue-background-active)))
    `(git-gutter-fr:deleted ((,c :inherit modus-themes-fringe-red)))
    `(git-gutter-fr:modified ((,c :inherit modus-themes-fringe-yellow)))
;;;;; git-rebase
    `(git-rebase-comment-hash ((,c :inherit font-lock-comment-face :foreground ,cyan)))
    `(git-rebase-comment-heading  ((,c :inherit (bold font-lock-comment-face))))
    `(git-rebase-description ((,c :foreground ,fg-main)))
    `(git-rebase-hash ((,c :foreground ,cyan-cooler)))
;;;;; git-timemachine
    `(git-timemachine-commit ((,c :inherit bold :foreground ,yellow)))
    `(git-timemachine-minibuffer-author-face ((,c :foreground ,fg-special-warm)))
    `(git-timemachine-minibuffer-detail-face ((,c :foreground ,red-warmer)))
;;;;; gnus
    `(gnus-button ((,c :inherit button)))
    `(gnus-cite-1 ((,c :inherit message-cited-text-1)))
    `(gnus-cite-2 ((,c :inherit message-cited-text-2)))
    `(gnus-cite-3 ((,c :inherit message-cited-text-3)))
    `(gnus-cite-4 ((,c :inherit message-cited-text-4)))
    `(gnus-cite-5 ((,c :inherit gnus-cite-1)))
    `(gnus-cite-6 ((,c :inherit gnus-cite-2)))
    `(gnus-cite-7 ((,c :inherit gnus-cite-3)))
    `(gnus-cite-8 ((,c :inherit gnus-cite-4)))
    `(gnus-cite-9 ((,c :inherit gnus-cite-1)))
    `(gnus-cite-10 ((,c :inherit gnus-cite-2)))
    `(gnus-cite-11 ((,c :inherit gnus-cite-3)))
    `(gnus-cite-attribution ((,c :inherit italic :foreground ,fg-main)))
    `(gnus-emphasis-bold ((,c :inherit bold)))
    `(gnus-emphasis-bold-italic ((,c :inherit bold-italic)))
    `(gnus-emphasis-highlight-words ((,c :inherit modus-themes-refine-yellow)))
    `(gnus-emphasis-italic ((,c :inherit italic)))
    `(gnus-emphasis-underline-bold ((,c :inherit gnus-emphasis-bold :underline t)))
    `(gnus-emphasis-underline-bold-italic ((,c :inherit gnus-emphasis-bold-italic :underline t)))
    `(gnus-emphasis-underline-italic ((,c :inherit gnus-emphasis-italic :underline t)))
    `(gnus-group-mail-1 ((,c :inherit bold :foreground ,magenta-warmer)))
    `(gnus-group-mail-1-empty ((,c :foreground ,magenta-warmer)))
    `(gnus-group-mail-2 ((,c :inherit bold :foreground ,magenta)))
    `(gnus-group-mail-2-empty ((,c :foreground ,magenta)))
    `(gnus-group-mail-3 ((,c :inherit bold :foreground ,magenta-cooler)))
    `(gnus-group-mail-3-empty ((,c :foreground ,magenta-cooler)))
    `(gnus-group-mail-low ((,c :inherit bold :foreground ,magenta-nuanced-fg)))
    `(gnus-group-mail-low-empty ((,c :foreground ,magenta-nuanced-fg)))
    `(gnus-group-news-1 ((,c :inherit bold :foreground ,green)))
    `(gnus-group-news-1-empty ((,c :foreground ,green)))
    `(gnus-group-news-2 ((,c :inherit bold :foreground ,cyan)))
    `(gnus-group-news-2-empty ((,c :foreground ,cyan)))
    `(gnus-group-news-3 ((,c :inherit bold :foreground ,yellow-nuanced-fg)))
    `(gnus-group-news-3-empty ((,c :foreground ,yellow-nuanced-fg)))
    `(gnus-group-news-4 ((,c :inherit bold :foreground ,cyan-nuanced-fg)))
    `(gnus-group-news-4-empty ((,c :foreground ,cyan-nuanced-fg)))
    `(gnus-group-news-5 ((,c :inherit bold :foreground ,red-nuanced-fg)))
    `(gnus-group-news-5-empty ((,c :foreground ,red-nuanced-fg)))
    `(gnus-group-news-6 ((,c :inherit (shadow bold))))
    `(gnus-group-news-6-empty ((,c :inherit shadow)))
    `(gnus-group-news-low ((,c :inherit bold :foreground ,green-nuanced-fg)))
    `(gnus-group-news-low-empty ((,c :foreground ,green-nuanced-fg)))
    `(gnus-header-content ((,c :inherit message-header-other)))
    `(gnus-header-from ((,c :inherit message-header-to :underline nil)))
    `(gnus-header-name ((,c :inherit message-header-name)))
    `(gnus-header-newsgroups ((,c :inherit message-header-newsgroups)))
    `(gnus-header-subject ((,c :inherit message-header-subject)))
    `(gnus-server-agent ((,c :inherit bold :foreground ,cyan)))
    `(gnus-server-closed ((,c :inherit bold :foreground ,magenta)))
    `(gnus-server-cloud ((,c :inherit bold :foreground ,cyan-warmer)))
    `(gnus-server-cloud-host ((,c :inherit modus-themes-refine-cyan)))
    `(gnus-server-denied ((,c :inherit bold :foreground ,red)))
    `(gnus-server-offline ((,c :inherit bold :foreground ,yellow)))
    `(gnus-server-opened ((,c :inherit bold :foreground ,green)))
    `(gnus-signature ((,c :inherit italic :foreground ,fg-special-cold)))
    `(gnus-splash ((,c :inherit shadow)))
    `(gnus-summary-cancelled ((,c :inherit modus-themes-mark-alt :extend t)))
    `(gnus-summary-high-ancient ((,c :inherit bold :foreground ,fg-alt)))
    `(gnus-summary-high-read ((,c :inherit bold :foreground ,fg-special-cold)))
    `(gnus-summary-high-ticked ((,c :inherit bold :foreground ,red-cooler)))
    `(gnus-summary-high-undownloaded ((,c :inherit bold :foreground ,yellow)))
    `(gnus-summary-high-unread ((,c :inherit bold :foreground ,fg-main)))
    `(gnus-summary-low-ancient ((,c :inherit italic :foreground ,fg-alt)))
    `(gnus-summary-low-read ((,c :inherit italic :foreground ,fg-alt)))
    `(gnus-summary-low-ticked ((,c :inherit italic :foreground ,red-refine-fg)))
    `(gnus-summary-low-undownloaded ((,c :inherit italic :foreground ,yellow-refine-fg)))
    `(gnus-summary-low-unread ((,c :inherit italic :foreground ,fg-special-cold)))
    `(gnus-summary-normal-ancient ((,c :foreground ,fg-special-calm)))
    `(gnus-summary-normal-read ((,c :inherit shadow)))
    `(gnus-summary-normal-ticked ((,c :foreground ,red-cooler)))
    `(gnus-summary-normal-undownloaded ((,c :foreground ,yellow)))
    `(gnus-summary-normal-unread ((,c :foreground ,fg-main)))
    `(gnus-summary-selected ((,c :inherit highlight :extend t)))
;;;;; gotest
    `(go-test--ok-face ((,c :inherit success)))
    `(go-test--error-face ((,c :inherit error)))
    `(go-test--warning-face ((,c :inherit warning)))
    `(go-test--pointer-face ((,c :foreground ,magenta-cooler)))
    `(go-test--standard-face ((,c :foreground ,fg-special-cold)))
;;;;; golden-ratio-scroll-screen
    `(golden-ratio-scroll-highlight-line-face ((,c :background ,cyan-subtle-bg :foreground ,fg-main)))
;;;;; helm
    `(helm-M-x-key ((,c :inherit modus-themes-key-binding)))
    `(helm-action ((,c :underline t)))
    `(helm-bookmark-addressbook ((,c :foreground ,green-warmer)))
    `(helm-bookmark-directory ((,c :inherit bold :foreground ,blue)))
    `(helm-bookmark-file ((,c :foreground ,fg-main)))
    `(helm-bookmark-file-not-found ((,c :background ,bg-alt :foreground ,fg-alt)))
    `(helm-bookmark-gnus ((,c :foreground ,magenta)))
    `(helm-bookmark-info ((,c :foreground ,cyan-warmer)))
    `(helm-bookmark-man ((,c :foreground ,yellow-warmer)))
    `(helm-bookmark-w3m ((,c :foreground ,blue-warmer)))
    `(helm-buffer-archive ((,c :inherit bold :foreground ,cyan)))
    `(helm-buffer-directory ((,c :inherit bold :foreground ,blue)))
    `(helm-buffer-file ((,c :foreground ,fg-main)))
    `(helm-buffer-modified ((,c :foreground ,yellow-warmer)))
    `(helm-buffer-not-saved ((,c :foreground ,red-warmer)))
    `(helm-buffer-process ((,c :foreground ,magenta)))
    `(helm-buffer-saved-out ((,c :inherit bold :background ,bg-alt :foreground ,red)))
    `(helm-buffer-size ((,c :inherit shadow)))
    `(helm-candidate-number ((,c :foreground ,cyan)))
    `(helm-candidate-number-suspended ((,c :foreground ,yellow)))
    `(helm-comint-prompts-buffer-name ((,c :foreground ,green)))
    `(helm-comint-prompts-promptidx ((,c :foreground ,cyan)))
    `(helm-delete-async-message ((,c :inherit bold :foreground ,magenta)))
    `(helm-eob-line ((,c :background ,bg-main :foreground ,fg-main)))
    `(helm-eshell-prompts-buffer-name ((,c :foreground ,green)))
    `(helm-eshell-prompts-promptidx ((,c :foreground ,cyan)))
    `(helm-etags-file ((,c :foreground ,fg-dim :underline t)))
    `(helm-ff-backup-file ((,c :inherit shadow)))
    `(helm-ff-denied ((,c :inherit modus-themes-intense-red)))
    `(helm-ff-directory ((,c :inherit helm-buffer-directory)))
    `(helm-ff-dirs ((,c :inherit bold :foreground ,blue-cooler)))
    `(helm-ff-dotted-directory ((,c :inherit bold :background ,bg-alt :foreground ,fg-alt)))
    `(helm-ff-dotted-symlink-directory ((,c :inherit (button helm-ff-dotted-directory))))
    `(helm-ff-executable ((,c :foreground ,magenta-warmer)))
    `(helm-ff-file ((,c :foreground ,fg-main)))
    `(helm-ff-file-extension ((,c :foreground ,fg-special-warm)))
    `(helm-ff-invalid-symlink ((,c :inherit modus-themes-link-broken)))
    `(helm-ff-pipe ((,c :inherit modus-themes-special-calm)))
    `(helm-ff-prefix ((,c :inherit modus-themes-special-warm)))
    `(helm-ff-socket ((,c :foreground ,red-cooler)))
    `(helm-ff-suid ((,c :inherit modus-themes-special-warm)))
    `(helm-ff-symlink ((,c :inherit modus-themes-link-symlink)))
    `(helm-ff-truename ((,c :foreground ,blue-cooler)))
    `(helm-fd-finish ((,c :inherit success)))
    `(helm-grep-cmd-line ((,c :foreground ,yellow-cooler)))
    `(helm-grep-file ((,c :inherit bold :foreground ,fg-special-cold)))
    `(helm-grep-finish ((,c :inherit bold)))
    `(helm-grep-lineno ((,c :foreground ,fg-special-warm)))
    `(helm-grep-match ((,c :inherit modus-themes-special-calm)))
    `(helm-header ((,c :inherit bold :foreground ,fg-special-cold)))
    `(helm-header-line-left-margin ((,c :inherit bold :foreground ,yellow-intense)))
    `(helm-history-deleted ((,c :inherit modus-themes-special-warm)))
    `(helm-history-remote ((,c :foreground ,red-cooler)))
    `(helm-lisp-completion-info ((,c :inherit modus-themes-bold :foreground ,fg-special-cold)))
    `(helm-lisp-show-completion ((,c :inherit modus-themes-special-warm)))
    `(helm-locate-finish ((,c :inherit success)))
    `(helm-match ((,c :inherit modus-themes-completion-match-0)))
    `(helm-match-item ((,c :inherit helm-match)))
    `(helm-minibuffer-prompt ((,c :inherit modus-themes-prompt)))
    `(helm-moccur-buffer ((,c :inherit button :foreground ,cyan-cooler)))
    `(helm-mode-prefix ((,c :inherit modus-themes-special-calm)))
    `(helm-non-file-buffer ((,c :inherit shadow)))
    `(helm-prefarg ((,c :foreground ,red)))
    `(helm-resume-need-update ((,c :inherit modus-themes-special-calm)))
    `(helm-selection ((,c :inherit modus-themes-completion-selected)))
    `(helm-selection-line ((,c :inherit highlight)))
    `(helm-separator ((,c :foreground ,fg-special-mild)))
    `(helm-time-zone-current ((,c :foreground ,green)))
    `(helm-time-zone-home ((,c :foreground ,magenta)))
    `(helm-source-header ((,c :inherit modus-themes-pseudo-header :foreground ,fg-special-warm)))
    `(helm-top-columns ((,c :inherit helm-header)))
    `(helm-ucs-char ((,c :foreground ,yellow-cooler)))
    `(helm-visible-mark ((,c :inherit modus-themes-subtle-cyan)))
;;;;; helm-ls-git
    `(helm-ls-git-added-copied-face ((,c :foreground ,green-intense)))
    `(helm-ls-git-added-modified-face ((,c :foreground ,yellow-intense)))
    `(helm-ls-git-conflict-face ((,c :inherit bold :foreground ,red-intense)))
    `(helm-ls-git-deleted-and-staged-face ((,c :foreground ,red-nuanced-fg)))
    `(helm-ls-git-deleted-not-staged-face ((,c :foreground ,red)))
    `(helm-ls-git-modified-and-staged-face ((,c :foreground ,yellow-nuanced-fg)))
    `(helm-ls-git-modified-not-staged-face ((,c :foreground ,yellow)))
    `(helm-ls-git-renamed-modified-face ((,c :foreground ,magenta)))
    `(helm-ls-git-untracked-face ((,c :foreground ,fg-special-cold)))
;;;;; helm-switch-shell
    `(helm-switch-shell-new-shell-face ((,c :inherit modus-themes-completion-match-0)))
;;;;; helm-xref
    `(helm-xref-file-name ((,c :inherit modus-themes-bold :foreground ,fg-special-cold)))
;;;;; helpful
    `(helpful-heading ((,c :inherit modus-themes-heading-1)))
;;;;; highlight region or ad-hoc regexp
    ;; HACK 2022-06-23: The :inverse-video prevents hl-line-mode from
    ;; overriding the background.  Such an override really defeats the
    ;; purpose of setting those highlights.
    ;;
    ;; NOTE 2022-10-04: We do not use the ,c here but instead
    ;; hardcode color values.  We have to do this as the themes lack
    ;; entries in their palette for such an edge case.  Defining those
    ;; entries is not appropriate.
    `(hi-aquamarine ((((class color) (min-colors 88) (background light))
                      :background "white" :foreground "#227f9f" :inverse-video t)
                     (((class color) (min-colors 88) (background dark))
                      :background "black" :foreground "#66cbdc" :inverse-video t)))
    `(hi-black-b ((,c :inverse-video t)))
    `(hi-black-hb ((,c :background ,bg-main :foreground ,fg-alt :inverse-video t)))
    `(hi-blue ((((class color) (min-colors 88) (background light))
                :background "white" :foreground "#3366dd" :inverse-video t)
               (((class color) (min-colors 88) (background dark))
                :background "black" :foreground "#aaccff" :inverse-video t)))
    `(hi-blue-b ((,c :inherit (bold hi-blue))))
    `(hi-green ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#008a00" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#66dd66" :inverse-video t)))
    `(hi-green-b ((,c :inherit (bold hi-green))))
    `(hi-pink ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#bd30aa" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#ff88ee" :inverse-video t)))
    `(hi-red-b ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#dd0000" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#f06666" :inverse-video t)))
    `(hi-salmon ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#bf555a" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#e08a50" :inverse-video t)))
    `(hi-yellow ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#af6400" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#faea00" :inverse-video t)))
    `(highlight ((,c ,@(if modus-themes-intense-mouseovers
                               (list :background blue-intense-bg :foreground fg-main)
                             (list :background cyan-subtle-bg :foreground fg-main)))))
    `(highlight-changes ((,c :foreground ,red-warmer :underline nil)))
    `(highlight-changes-delete ((,c :background ,red-nuanced-bg
                                        :foreground ,red :underline t)))
    `(hl-line ((,c :inherit modus-themes-hl-line)))
;;;;; highlight-indentation
    `(highlight-indentation-face ((,c :inherit modus-themes-hl-line)))
    `(highlight-indentation-current-column-face ((,c :background ,bg-active)))
;;;;; highlight-numbers
    `(highlight-numbers-number ((,c :foreground ,blue-cooler)))
;;;;; highlight-thing
    `(highlight-thing ((,c :inherit modus-themes-special-calm)))
;;;;; hl-defined
    `(hdefd-functions ((,c :foreground ,blue)))
    `(hdefd-undefined ((,c :foreground ,red-warmer)))
    `(hdefd-variables ((,c :foreground ,cyan-warmer)))
;;;;; hl-fill-column
    `(hl-fill-column-face ((,c :background ,bg-active :foreground ,fg-active)))
;;;;; hl-todo
    `(hl-todo ((,c :inherit (bold modus-themes-slant) :foreground ,red-cooler)))
;;;;; hydra
    `(hydra-face-amaranth ((,c :inherit bold :foreground ,yellow-warmer)))
    `(hydra-face-blue ((,c :inherit bold :foreground ,blue)))
    `(hydra-face-pink ((,c :inherit bold :foreground ,magenta-faint)))
    `(hydra-face-red ((,c :inherit bold :foreground ,red-faint)))
    `(hydra-face-teal ((,c :inherit bold :foreground ,cyan-cooler)))
;;;;; icomplete
    `(icomplete-first-match ((,c :inherit modus-themes-completion-match-0)))
    `(icomplete-selected-match ((,c :inherit modus-themes-completion-selected)))
;;;;; icomplete-vertical
    `(icomplete-vertical-separator ((,c :inherit shadow)))
;;;;; ido-mode
    `(ido-first-match ((,c :inherit modus-themes-completion-match-0)))
    `(ido-incomplete-regexp ((,c :inherit error)))
    `(ido-indicator ((,c :inherit modus-themes-subtle-yellow)))
    `(ido-only-match ((,c :inherit ido-first-match)))
    `(ido-subdir ((,c :foreground ,blue)))
    `(ido-virtual ((,c :foreground ,magenta-cooler)))
;;;;; iedit
    `(iedit-occurrence ((,c :inherit modus-themes-refine-blue)))
    `(iedit-read-only-occurrence ((,c :inherit modus-themes-intense-yellow)))
;;;;; iflipb
    `(iflipb-current-buffer-face ((,c :inherit bold :foreground ,cyan-warmer)))
    `(iflipb-other-buffer-face ((,c :inherit shadow)))
;;;;; image-dired
    `(image-dired-thumb-flagged ((,c :background ,red-intense-bg)))
    `(image-dired-thumb-header-file-name ((,c :inherit bold)))
    `(image-dired-thumb-header-file-size ((,c :foreground ,blue)))
    `(image-dired-thumb-mark ((,c :inherit modus-themes-grue-background-intense)))
;;;;; imenu-list
    `(imenu-list-entry-face-0 ((,c :foreground ,cyan)))
    `(imenu-list-entry-face-1 ((,c :foreground ,blue)))
    `(imenu-list-entry-face-2 ((,c :foreground ,cyan-cooler)))
    `(imenu-list-entry-face-3 ((,c :foreground ,blue-warmer)))
    `(imenu-list-entry-subalist-face-0 ((,c :inherit bold :foreground ,magenta-cooler :underline t)))
    `(imenu-list-entry-subalist-face-1 ((,c :inherit bold :foreground ,magenta :underline t)))
    `(imenu-list-entry-subalist-face-2 ((,c :inherit bold :foreground ,green-cooler :underline t)))
    `(imenu-list-entry-subalist-face-3 ((,c :inherit bold :foreground ,red-cooler :underline t)))
;;;;; indium
    `(indium-breakpoint-face ((,c :foreground ,red)))
    `(indium-frame-url-face ((,c :inherit (shadow button))))
    `(indium-keyword-face ((,c :inherit font-lock-keyword-face)))
    `(indium-litable-face ((,c :inherit modus-themes-slant :foreground ,fg-special-warm)))
    `(indium-repl-error-face ((,c :inherit error)))
    `(indium-repl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(indium-repl-stdout-face ((,c :foreground ,fg-main)))
;;;;; info
    `(Info-quoted ((,c :inherit modus-themes-markup-verbatim))) ; the capitalization is canonical
    `(info-header-node ((,c :inherit (shadow bold))))
    `(info-header-xref ((,c :foreground ,blue)))
    `(info-index-match ((,c :inherit match)))
    `(info-menu-header ((,c :inherit modus-themes-pseudo-header)))
    `(info-menu-star ((,c :foreground ,red)))
    `(info-node ((,c :inherit bold)))
    `(info-title-1 ((,c :inherit modus-themes-heading-1)))
    `(info-title-2 ((,c :inherit modus-themes-heading-2)))
    `(info-title-3 ((,c :inherit modus-themes-heading-3)))
    `(info-title-4 ((,c :inherit modus-themes-heading-4)))
;;;;; info+ (info-plus)
    `(info-command-ref-item ((,c :inherit font-lock-function-name-face)))
    `(info-constant-ref-item ((,c :inherit font-lock-constant-face)))
    `(info-custom-delimited ((,c :inherit modus-themes-markup-verbatim)))
    `(info-double-quoted-name ((,c :inherit font-lock-string-face)))
    `(info-file (( )))
    `(info-function-ref-item ((,c :inherit font-lock-function-name-face)))
    `(info-glossary-word ((,c :inherit modus-themes-box-button)))
    `(info-indented-text (( )))
    `(info-isolated-backquote (( )))
    `(info-isolated-quote (( )))
    `(info-macro-ref-item ((,c :inherit font-lock-keyword-face)))
    `(info-menu ((,c :inherit bold)))
    `(info-quoted-name ((,c :inherit modus-themes-markup-verbatim)))
    `(info-reference-item ((,c :inherit bold)))
    `(info-special-form-ref-item ((,c :inherit warning)))
    `(info-string ((,c :inherit font-lock-string-face)))
    `(info-syntax-class-item ((,c :inherit modus-themes-markup-code)))
    `(info-user-option-ref-item ((,c :inherit font-lock-variable-name-face)))
    `(info-variable-ref-item ((,c :inherit font-lock-variable-name-face)))
;;;;; info-colors
    `(info-colors-lisp-code-block ((,c :inherit modus-themes-fixed-pitch)))
    `(info-colors-ref-item-command ((,c :inherit font-lock-function-name-face)))
    `(info-colors-ref-item-constant ((,c :inherit font-lock-constant-face)))
    `(info-colors-ref-item-function ((,c :inherit font-lock-function-name-face)))
    `(info-colors-ref-item-macro ((,c :inherit font-lock-keyword-face)))
    `(info-colors-ref-item-other ((,c :inherit font-lock-doc-face)))
    `(info-colors-ref-item-special-form ((,c :inherit font-lock-keyword-face)))
    `(info-colors-ref-item-syntax-class ((,c :inherit font-lock-builtin-face)))
    `(info-colors-ref-item-type ((,c :inherit font-lock-type-face)))
    `(info-colors-ref-item-user-option ((,c :inherit font-lock-variable-name-face)))
    `(info-colors-ref-item-variable ((,c :inherit font-lock-variable-name-face)))
;;;;; interaction-log
    `(ilog-buffer-face ((,c :foreground ,magenta-cooler)))
    `(ilog-change-face ((,c :foreground ,magenta-warmer)))
    `(ilog-echo-face ((,c :foreground ,yellow-cooler)))
    `(ilog-load-face ((,c :foreground ,green)))
    `(ilog-message-face ((,c :inherit shadow)))
    `(ilog-non-change-face ((,c :foreground ,blue)))
;;;;; ioccur
    `(ioccur-cursor ((,c :foreground ,fg-main)))
    `(ioccur-invalid-regexp ((,c :foreground ,red)))
    `(ioccur-match-face ((,c :inherit modus-themes-special-calm)))
    `(ioccur-match-overlay-face ((,c :inherit modus-themes-special-cold :extend t)))
    `(ioccur-num-line-face ((,c :foreground ,fg-special-warm)))
    `(ioccur-overlay-face ((,c :inherit modus-themes-refine-blue :extend t)))
    `(ioccur-regexp-face ((,c :inherit (modus-themes-intense-magenta bold))))
    `(ioccur-title-face ((,c :inherit modus-themes-pseudo-header :foreground ,fg-special-cold)))
;;;;; isearch, occur, and the like
    `(isearch ((,c :inherit modus-themes-search-success)))
    `(isearch-fail ((,c :inherit modus-themes-refine-red)))
    `(isearch-group-1 ((,c :inherit modus-themes-refine-blue)))
    `(isearch-group-2 ((,c :inherit modus-themes-refine-magenta)))
    `(lazy-highlight ((,c :inherit modus-themes-search-success-lazy)))
    `(match ((,c :inherit modus-themes-special-calm)))
    `(query-replace ((,c :inherit modus-themes-intense-red)))
;;;;; ivy
    `(ivy-action ((,c :inherit modus-themes-key-binding)))
    `(ivy-confirm-face ((,c :inherit success)))
    `(ivy-current-match ((,c :inherit modus-themes-completion-selected)))
    `(ivy-cursor ((,c :background ,fg-main :foreground ,bg-main)))
    `(ivy-highlight-face ((,c :foreground ,magenta)))
    `(ivy-match-required-face ((,c :inherit error)))
    `(ivy-minibuffer-match-face-1 (( )))
    `(ivy-minibuffer-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(ivy-minibuffer-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(ivy-minibuffer-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
    `(ivy-org ((,c :foreground ,cyan-cooler)))
    `(ivy-remote ((,c :foreground ,magenta)))
    `(ivy-separator ((,c :inherit shadow)))
    `(ivy-subdir ((,c :foreground ,blue)))
    `(ivy-virtual ((,c :foreground ,magenta-cooler)))
;;;;; ivy-posframe
    `(ivy-posframe-border ((,c :background ,border)))
    `(ivy-posframe-cursor ((,c :background ,fg-main :foreground ,bg-main)))
;;;;; jira (org-jira)
    `(jiralib-comment-face ((,c :background ,bg-alt)))
    `(jiralib-comment-header-face ((,c :inherit bold)))
    `(jiralib-issue-info-face ((,c :inherit modus-themes-special-warm)))
    `(jiralib-issue-info-header-face ((,c :inherit (modus-themes-special-warm bold))))
    `(jiralib-issue-summary-face ((,c :inherit bold)))
    `(jiralib-link-filter-face ((,c :underline t)))
    `(jiralib-link-issue-face ((,c :underline t)))
    `(jiralib-link-project-face ((,c :underline t)))
;;;;; journalctl-mode
    `(journalctl-error-face ((,c :inherit error)))
    `(journalctl-finished-face ((,c :inherit success)))
    `(journalctl-host-face ((,c :foreground ,blue)))
    `(journalctl-process-face ((,c :foreground ,cyan-cooler)))
    `(journalctl-starting-face ((,c :foreground ,green)))
    `(journalctl-timestamp-face ((,c :foreground ,fg-special-cold)))
    `(journalctl-warning-face ((,c :inherit warning)))
;;;;; js2-mode
    `(js2-error ((,c :inherit modus-themes-lang-error)))
    `(js2-external-variable ((,c :inherit font-lock-variable-name-face)))
    `(js2-function-call ((,c :inherit font-lock-function-name-face)))
    `(js2-function-param ((,c :inherit font-lock-constant-face)))
    `(js2-instance-member ((,c :inherit font-lock-keyword-face)))
    `(js2-jsdoc-html-tag-delimiter ((,c :foreground ,fg-main)))
    `(js2-jsdoc-html-tag-name ((,c :inherit font-lock-function-name-face)))
    `(js2-jsdoc-tag ((,c :inherit (font-lock-builtin-face font-lock-comment-face) :weight normal)))
    `(js2-jsdoc-type ((,c :inherit (font-lock-type-face font-lock-comment-face) :weight normal)))
    `(js2-jsdoc-value ((,c :inherit (font-lock-constant-face font-lock-comment-face) :weight normal)))
    `(js2-object-property ((,c :foreground ,fg-main)))
    `(js2-object-property-access ((,c :foreground ,fg-main)))
    `(js2-private-function-call ((,c :inherit font-lock-preprocessor-face)))
    `(js2-private-member ((,c :inherit font-lock-warning-face)))
    `(js2-warning ((,c :inherit modus-themes-lang-warning)))
;;;;; julia
    `(julia-macro-face ((,c :inherit font-lock-builtin-face)))
    `(julia-quoted-symbol-face ((,c :inherit font-lock-constant-face)))
;;;;; jupyter
    `(jupyter-eval-overlay ((,c :inherit bold :foreground ,blue)))
    `(jupyter-repl-input-prompt ((,c :foreground ,cyan-cooler)))
    `(jupyter-repl-output-prompt ((,c :foreground ,magenta-cooler)))
    `(jupyter-repl-traceback ((,c :inherit modus-themes-intense-red)))
;;;;; kaocha-runner
    `(kaocha-runner-error-face ((,c :inherit error)))
    `(kaocha-runner-success-face ((,c :inherit success)))
    `(kaocha-runner-warning-face ((,c :inherit warning)))
;;;;; keycast
    `(keycast-command ((,c :inherit bold :foreground ,blue)))
    ;; FIXME 2022-05-03: The padding breaks `keycast-tab-bar-mode'
    `(keycast-key ((,c ;; ,@(modus-themes--mode-line-padded-box blue)
                           :background ,blue :foreground ,bg-main)))
;;;;; ledger-mode
    `(ledger-font-auto-xact-face ((,c :foreground ,magenta)))
    `(ledger-font-account-name-face ((,c :foreground ,fg-special-cold)))
    `(ledger-font-directive-face ((,c :foreground ,magenta-cooler)))
    `(ledger-font-posting-date-face ((,c :inherit bold :foreground ,fg-main)))
    `(ledger-font-periodic-xact-face ((,c :foreground ,cyan-cooler)))
    `(ledger-font-posting-amount-face ((,c :foreground ,fg-special-mild)))
    `(ledger-font-payee-cleared-face ((,c :foreground ,blue-warmer)))
    `(ledger-font-payee-pending-face ((,c :foreground ,yellow)))
    `(ledger-font-payee-uncleared-face ((,c :foreground ,red-cooler)))
    `(ledger-font-xact-highlight-face ((,c :inherit highlight)))
;;;;; leerzeichen
    `(leerzeichen ((,c :background ,bg-alt)))
;;;;; line numbers (display-line-numbers-mode and global variant)
    ;; Here we cannot inherit `modus-themes-fixed-pitch'.  We need to
    ;; fall back to `default' otherwise line numbers do not scale when
    ;; using `text-scale-adjust'.
    `(line-number
      ((,c :inherit ,(if modus-themes-mixed-fonts '(fixed-pitch default) 'default)
               ,@(modus-themes--line-numbers
                  fg-alt bg-dim
                  "gray50"))))
    `(line-number-current-line
      ((,c :inherit (bold line-number)
               ,@(modus-themes--line-numbers
                  fg-main bg-active
                  blue-cooler))))
    `(line-number-major-tick
      ((,c :inherit (bold line-number)
               ,@(modus-themes--line-numbers
                  yellow-nuanced-fg yellow-nuanced-bg
                  red-warmer))))
    `(line-number-minor-tick
      ((,c :inherit (bold line-number)
               ,@(modus-themes--line-numbers
                  fg-alt bg-inactive
                  fg-inactive))))
;;;;; lsp-mode
    `(lsp-face-highlight-read ((,c :inherit modus-themes-subtle-blue :underline t)))
    `(lsp-face-highlight-textual ((,c :inherit modus-themes-subtle-blue)))
    `(lsp-face-highlight-write ((,c :inherit (modus-themes-refine-blue bold))))
    `(lsp-face-semhl-constant ((,c :foreground ,blue-cooler)))
    `(lsp-face-semhl-deprecated ((,c :inherit modus-themes-lang-warning)))
    `(lsp-face-semhl-enummember ((,c :foreground ,blue-cooler)))
    `(lsp-face-semhl-field ((,c :foreground ,cyan-warmer)))
    `(lsp-face-semhl-field-static ((,c :inherit modus-themes-slant :foreground ,cyan-warmer)))
    `(lsp-face-semhl-function ((,c :foreground ,magenta)))
    `(lsp-face-semhl-method ((,c :foreground ,magenta)))
    `(lsp-face-semhl-namespace ((,c :inherit modus-themes-bold :foreground ,magenta-warmer)))
    `(lsp-face-semhl-preprocessor ((,c :foreground ,red-cooler)))
    `(lsp-face-semhl-static-method ((,c :inherit modus-themes-slant :foreground ,magenta)))
    `(lsp-face-semhl-type-class ((,c :foreground ,magenta-warmer)))
    `(lsp-face-semhl-type-enum ((,c :foreground ,magenta-warmer)))
    `(lsp-face-semhl-type-primitive ((,c :inherit modus-themes-slant :foreground ,magenta-warmer)))
    `(lsp-face-semhl-type-template ((,c :inherit modus-themes-slant :foreground ,magenta-warmer)))
    `(lsp-face-semhl-type-typedef ((,c :inherit modus-themes-slant :foreground ,magenta-warmer)))
    `(lsp-face-semhl-variable ((,c :foreground ,cyan)))
    `(lsp-face-semhl-variable-local ((,c :foreground ,cyan)))
    `(lsp-face-semhl-variable-parameter ((,c :foreground ,cyan-cooler)))
    `(lsp-lens-face ((,c  :inherit shadow :height 0.8)))
    `(lsp-lens-mouse-face ((,c :height 0.8 :foreground ,blue-cooler :underline t)))
    `(lsp-ui-doc-background ((,c :background ,bg-alt)))
    `(lsp-ui-doc-header ((,c :background ,bg-active)))
    `(lsp-ui-doc-url ((,c :inherit button)))
    `(lsp-ui-peek-filename ((,c :foreground ,fg-special-warm)))
    `(lsp-ui-peek-footer ((,c :background ,bg-active)))
    `(lsp-ui-peek-header ((,c :background ,bg-active)))
    `(lsp-ui-peek-highlight ((,c :inherit modus-themes-subtle-blue)))
    `(lsp-ui-peek-line-number ((,c :inherit shadow)))
    `(lsp-ui-peek-list ((,c :background ,bg-dim)))
    `(lsp-ui-peek-peek ((,c :background ,bg-alt)))
    `(lsp-ui-peek-selection ((,c :inherit modus-themes-subtle-cyan)))
    `(lsp-ui-sideline-code-action ((,c :foreground ,yellow)))
    `(lsp-ui-sideline-current-symbol ((,c :inherit bold :height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-main)))
    `(lsp-ui-sideline-symbol ((,c :inherit bold :height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-alt)))
    `(lsp-ui-sideline-symbol-info ((,c :inherit italic :height 0.99)))
;;;;; macrostep
    `(macrostep-compiler-macro-face ((,c :inherit italic)))
    `(macrostep-expansion-highlight-face ((,c :background ,blue-nuanced-bg)))
    `(macrostep-gensym-1 ((,c :inherit bold :foreground ,blue :box t)))
    `(macrostep-gensym-2 ((,c :inherit bold :foreground ,green :box t)))
    `(macrostep-gensym-3 ((,c :inherit bold :foreground ,yellow :box t)))
    `(macrostep-gensym-4 ((,c :inherit bold :foreground ,red :box t)))
    `(macrostep-gensym-5 ((,c :inherit bold :foreground ,magenta :box t)))
    `(macrostep-macro-face ((,c :inherit button :foreground ,green-warmer)))
;;;;; magit
    `(magit-bisect-bad ((,c :inherit error)))
    `(magit-bisect-good ((,c :inherit success)))
    `(magit-bisect-skip ((,c :inherit warning)))
    `(magit-blame-date ((,c :foreground ,blue)))
    `(magit-blame-dimmed ((,c :inherit (shadow modus-themes-reset-hard))))
    `(magit-blame-hash ((,c :foreground ,fg-special-warm)))
    `(magit-blame-heading ((,c :inherit modus-themes-reset-hard :background ,bg-alt :extend t)))
    `(magit-blame-highlight ((,c :inherit modus-themes-nuanced-cyan)))
    `(magit-blame-margin ((,c :inherit (magit-blame-highlight modus-themes-reset-hard))))
    `(magit-blame-name ((,c :foreground ,magenta-cooler)))
    `(magit-blame-summary ((,c :foreground ,cyan-cooler)))
    ;; ;; NOTE 2021-11-23: we do not set the `magit-branch-current'
    ;; ;; because its definition checks if the :box attribute can be set
    ;; ;; and if not, it uses :inverse-video.  Useful for terminal
    ;; ;; emulators.
    ;;
    ;; `(magit-branch-current ((,c :foreground ,blue-cooler :box t)))
    `(magit-branch-local ((,c :foreground ,blue-warmer)))
    `(magit-branch-remote ((,c :foreground ,magenta-warmer)))
    `(magit-branch-remote-head ((,c :foreground ,magenta-cooler :box t)))
    `(magit-branch-upstream ((,c :inherit italic)))
    `(magit-branch-warning ((,c :inherit warning)))
    `(magit-cherry-equivalent ((,c :background ,bg-main :foreground ,magenta-intense)))
    `(magit-cherry-unmatched ((,c :background ,bg-main :foreground ,cyan-intense)))
    ;; NOTE: here we break from the pattern of inheriting from the
    ;; modus-themes-diff-* faces, though only for the standard actions,
    ;; not the highlighted ones.  This is because Magit's interaction
    ;; model relies on highlighting the current diff hunk.
    `(magit-diff-added ((,c ,@(modus-themes--diff
                                   bg-diff-added fg-diff-added
                                   green-nuanced-bg fg-diff-added
                                   bg-diff-added-deuteran fg-diff-added-deuteran
                                   blue-nuanced-bg fg-diff-added-deuteran))))
    `(magit-diff-added-highlight ((,c :inherit modus-themes-diff-focus-added)))
    `(magit-diff-base ((,c ,@(modus-themes--diff
                                  bg-diff-changed fg-diff-changed
                                  yellow-nuanced-bg fg-diff-changed))))
    `(magit-diff-base-highlight ((,c :inherit modus-themes-diff-focus-changed)))
    `(magit-diff-context ((,c ,@(unless (eq modus-themes-diffs 'bg-only) (list :foreground "gray50")))))
    `(magit-diff-context-highlight ((,c ,@(modus-themes--diff
                                               bg-inactive fg-inactive
                                               bg-dim fg-alt
                                               bg-dim fg-alt))))
    `(magit-diff-file-heading ((,c :inherit bold :foreground ,fg-special-cold)))
    `(magit-diff-file-heading-highlight ((,c :inherit (modus-themes-special-cold bold))))
    `(magit-diff-file-heading-selection ((,c :inherit modus-themes-refine-cyan)))
    ;; NOTE: here we break from the pattern of inheriting from the
    ;; modus-themes-diff-* faces.
    `(magit-diff-hunk-heading ((,c :inherit bold
                                       ,@(modus-themes--diff
                                          bg-active fg-inactive
                                          bg-inactive fg-inactive
                                          bg-inactive fg-inactive
                                          nil nil
                                          t))))
    ;; NOTE: we do not follow the pattern of inheriting from
    ;; modus-themes-grue-* faces, as this is a special case.
    `(magit-diff-hunk-heading-highlight
      ((,c :inherit bold
               :background ,@(modus-themes--deuteran bg-active bg-diff-heading)
               :foreground ,@(modus-themes--deuteran fg-main fg-diff-heading))))
    `(magit-diff-hunk-heading-selection ((,c :inherit modus-themes-refine-blue)))
    `(magit-diff-hunk-region ((,c :inherit bold)))
    `(magit-diff-lines-boundary ((,c :background ,fg-main)))
    `(magit-diff-lines-heading ((,c :inherit modus-themes-refine-magenta)))
    `(magit-diff-removed ((,c ,@(modus-themes--diff
                                     bg-diff-removed fg-diff-removed
                                     red-nuanced-bg fg-diff-removed))))
    `(magit-diff-removed-highlight ((,c :inherit modus-themes-diff-focus-removed)))
    `(magit-diffstat-added ((,c :inherit modus-themes-grue)))
    `(magit-diffstat-removed ((,c :foreground ,red)))
    `(magit-dimmed ((,c :inherit shadow)))
    `(magit-filename ((,c :foreground ,fg-special-cold)))
    `(magit-hash ((,c :inherit shadow)))
    `(magit-head ((,c :inherit magit-branch-local)))
    `(magit-header-line ((,c :inherit bold :foreground ,magenta)))
    `(magit-header-line-key ((,c :inherit modus-themes-key-binding)))
    `(magit-header-line-log-select ((,c :inherit bold :foreground ,fg-main)))
    `(magit-keyword ((,c :foreground ,magenta)))
    `(magit-keyword-squash ((,c :inherit bold :foreground ,yellow-cooler)))
    `(magit-log-author ((,c :foreground ,cyan)))
    `(magit-log-date ((,c :inherit shadow)))
    `(magit-log-graph ((,c :foreground ,fg-dim)))
    `(magit-mode-line-process ((,c :inherit bold :foreground ,cyan)))
    `(magit-mode-line-process-error ((,c :inherit bold :foreground ,red)))
    `(magit-process-ng ((,c :inherit error)))
    `(magit-process-ok ((,c :inherit success)))
    `(magit-reflog-amend ((,c :inherit warning)))
    `(magit-reflog-checkout ((,c :inherit bold :foreground ,blue-warmer)))
    `(magit-reflog-cherry-pick ((,c :inherit success)))
    `(magit-reflog-commit ((,c :inherit bold)))
    `(magit-reflog-merge ((,c :inherit success)))
    `(magit-reflog-other ((,c :inherit bold :foreground ,cyan)))
    `(magit-reflog-rebase ((,c :inherit bold :foreground ,magenta)))
    `(magit-reflog-remote ((,c :inherit bold :foreground ,magenta-cooler)))
    `(magit-reflog-reset ((,c :inherit error)))
    `(magit-refname ((,c :inherit shadow)))
    `(magit-refname-pullreq ((,c :inherit shadow)))
    `(magit-refname-stash ((,c :inherit shadow)))
    `(magit-refname-wip ((,c :inherit shadow)))
    `(magit-section ((,c :background ,bg-dim :foreground ,fg-main)))
    `(magit-section-heading ((,c :inherit bold :foreground ,cyan)))
    `(magit-section-heading-selection ((,c :inherit (modus-themes-refine-cyan bold))))
    `(magit-section-highlight ((,c :background ,bg-alt)))
    `(magit-sequence-done ((,c :inherit success)))
    `(magit-sequence-drop ((,c :inherit error)))
    `(magit-sequence-exec ((,c :inherit bold :foreground ,magenta-warmer)))
    `(magit-sequence-head ((,c :inherit bold :foreground ,cyan-warmer)))
    `(magit-sequence-onto ((,c :inherit (bold shadow))))
    `(magit-sequence-part ((,c :inherit warning)))
    `(magit-sequence-pick ((,c :inherit bold)))
    `(magit-sequence-stop ((,c :inherit error)))
    `(magit-signature-bad ((,c :inherit error)))
    `(magit-signature-error ((,c :inherit error)))
    `(magit-signature-expired ((,c :inherit warning)))
    `(magit-signature-expired-key ((,c :foreground ,yellow)))
    `(magit-signature-good ((,c :inherit success)))
    `(magit-signature-revoked ((,c :inherit bold :foreground ,magenta)))
    `(magit-signature-untrusted ((,c :inherit (bold shadow))))
    `(magit-tag ((,c :foreground ,yellow-cooler)))
;;;;; magit-imerge
    `(magit-imerge-overriding-value ((,c :inherit bold :foreground ,red-warmer)))
;;;;; make-mode (makefiles)
    `(makefile-makepp-perl ((,c :background ,cyan-nuanced-bg)))
    `(makefile-space ((,c :background ,magenta-nuanced-bg)))
;;;;; man
    `(Man-overstrike ((,c :inherit bold :foreground ,magenta-warmer)))
    `(Man-reverse ((,c :inherit modus-themes-subtle-magenta)))
    `(Man-underline ((,c :foreground ,cyan-cooler :underline t)))
;;;;; marginalia
    `(marginalia-archive ((,c :foreground ,cyan-cooler)))
    `(marginalia-char ((,c :foreground ,magenta)))
    `(marginalia-date ((,c :foreground ,cyan)))
    `(marginalia-documentation ((,c :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(marginalia-file-name ((,c :foreground ,blue-faint)))
    `(marginalia-file-owner ((,c :foreground ,red-faint)))
    `(marginalia-file-priv-dir ((,c :foreground ,blue-warmer)))
    `(marginalia-file-priv-exec ((,c :foreground ,magenta-warmer)))
    `(marginalia-file-priv-link ((,c :foreground ,blue-cooler)))
    `(marginalia-file-priv-no ((,c :foreground "gray50")))
    `(marginalia-file-priv-other ((,c :foreground ,yellow)))
    `(marginalia-file-priv-rare ((,c :foreground ,red)))
    `(marginalia-file-priv-read ((,c :foreground ,fg-main)))
    `(marginalia-file-priv-write ((,c :foreground ,cyan)))
    `(marginalia-function ((,c :foreground ,magenta-faint)))
    `(marginalia-key ((,c :inherit modus-themes-key-binding)))
    `(marginalia-lighter ((,c :foreground ,blue-warmer)))
    `(marginalia-list ((,c :foreground ,magenta-faint)))
    `(marginalia-mode ((,c :foreground ,cyan)))
    `(marginalia-modified ((,c :foreground ,magenta-faint)))
    `(marginalia-null ((,c :inherit shadow)))
    `(marginalia-number ((,c :foreground ,cyan)))
    `(marginalia-size ((,c :foreground ,cyan-faint)))
    `(marginalia-string ((,c :foreground ,blue-warmer)))
    `(marginalia-symbol ((,c :foreground ,blue-faint)))
    `(marginalia-true ((,c :foreground ,fg-main)))
    `(marginalia-type ((,c :foreground ,cyan-cooler)))
    `(marginalia-value ((,c :foreground ,cyan)))
    `(marginalia-version ((,c :foreground ,cyan)))
;;;;; markdown-mode
    `(markdown-blockquote-face ((,c :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(markdown-bold-face ((,c :inherit bold)))
    `(markdown-code-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))
    `(markdown-comment-face ((,c :inherit font-lock-comment-face)))
    `(markdown-footnote-marker-face ((,c :inherit bold :foreground ,cyan-warmer)))
    `(markdown-footnote-text-face ((,c :inherit modus-themes-slant :foreground ,fg-main)))
    `(markdown-gfm-checkbox-face ((,c :foreground ,yellow-cooler)))
    `(markdown-header-delimiter-face ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(markdown-header-face ((t nil)))
    `(markdown-header-face-1 ((,c :inherit modus-themes-heading-1)))
    `(markdown-header-face-2 ((,c :inherit modus-themes-heading-2)))
    `(markdown-header-face-3 ((,c :inherit modus-themes-heading-3)))
    `(markdown-header-face-4 ((,c :inherit modus-themes-heading-4)))
    `(markdown-header-face-5 ((,c :inherit modus-themes-heading-5)))
    `(markdown-header-face-6 ((,c :inherit modus-themes-heading-6)))
    `(markdown-header-rule-face ((,c :inherit bold :foreground ,fg-special-warm)))
    `(markdown-highlighting-face ((,c :inherit modus-themes-refine-yellow)))
    `(markdown-hr-face ((,c :inherit bold :foreground ,fg-special-warm)))
    `(markdown-html-attr-name-face ((,c :inherit modus-themes-fixed-pitch
                                            :foreground ,cyan)))
    `(markdown-html-attr-value-face ((,c :inherit modus-themes-fixed-pitch
                                             :foreground ,blue)))
    `(markdown-html-entity-face ((,c :inherit modus-themes-fixed-pitch
                                         :foreground ,cyan)))
    `(markdown-html-tag-delimiter-face ((,c :inherit modus-themes-fixed-pitch
                                                :foreground ,fg-special-mild)))
    `(markdown-html-tag-name-face ((,c :inherit modus-themes-fixed-pitch
                                           :foreground ,magenta-warmer)))
    `(markdown-inline-code-face ((,c :inherit modus-themes-markup-verbatim)))
    `(markdown-italic-face ((,c :inherit italic)))
    `(markdown-language-info-face ((,c :inherit modus-themes-fixed-pitch
                                           :foreground ,fg-special-cold)))
    `(markdown-language-keyword-face ((,c :inherit modus-themes-fixed-pitch
                                              :background ,bg-alt
                                              :foreground ,fg-alt)))
    `(markdown-line-break-face ((,c :inherit modus-themes-refine-cyan :underline t)))
    `(markdown-link-face ((,c :inherit button)))
    `(markdown-link-title-face ((,c :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(markdown-list-face ((,c :foreground ,fg-dim)))
    `(markdown-markup-face ((,c :inherit shadow)))
    `(markdown-math-face ((,c :foreground ,magenta-cooler)))
    `(markdown-metadata-key-face ((,c :foreground ,cyan-cooler)))
    `(markdown-metadata-value-face ((,c :foreground ,blue-warmer)))
    `(markdown-missing-link-face ((,c :inherit bold :foreground ,yellow)))
    `(markdown-plain-url-face ((,c :inherit markdown-link-face)))
    `(markdown-pre-face ((,c :inherit markdown-code-face :foreground ,fg-special-mild)))
    `(markdown-reference-face ((,c :inherit markdown-markup-face)))
    `(markdown-strike-through-face ((,c :strike-through t)))
    `(markdown-table-face ((,c :inherit modus-themes-fixed-pitch
                                   :foreground ,fg-special-cold)))
    `(markdown-url-face ((,c :foreground ,blue-warmer)))
;;;;; markup-faces (`adoc-mode')
    `(markup-attribute-face ((,c :inherit (italic markup-meta-face))))
    `(markup-bold-face ((,c :inherit bold :foreground ,red-nuanced-fg)))
    `(markup-code-face ((,c :foreground ,magenta)))
    `(markup-comment-face ((,c :inherit font-lock-comment-face)))
    `(markup-complex-replacement-face ((,c :background ,magenta-nuanced-bg :foreground ,magenta-cooler)))
    `(markup-emphasis-face ((,c :inherit markup-italic-face)))
    `(markup-error-face ((,c :inherit error)))
    `(markup-gen-face ((,c :foreground ,magenta-warmer)))
    `(markup-internal-reference-face ((,c :inherit modus-themes-slant :foreground ,fg-alt)))
    `(markup-italic-face ((,c :inherit italic)))
    `(markup-list-face ((,c :inherit modus-themes-special-cold)))
    `(markup-meta-face ((,c :inherit (modus-themes-fixed-pitch shadow))))
    `(markup-meta-hide-face ((,c :foreground "gray50")))
    `(markup-reference-face ((,c :inherit modus-themes-slant :foreground ,blue-warmer)))
    `(markup-replacement-face ((,c :inherit modus-themes-fixed-pitch :foreground ,red-warmer)))
    `(markup-secondary-text-face ((,c :height 0.9 :foreground ,cyan-cooler)))
    `(markup-small-face ((,c :inherit markup-gen-face :height 0.9)))
    `(markup-strong-face ((,c :inherit markup-bold-face)))
    `(markup-subscript-face ((,c :height 0.9 :foreground ,magenta-cooler)))
    `(markup-superscript-face ((,c :height 0.9 :foreground ,magenta-cooler)))
    `(markup-table-cell-face ((,c :inherit modus-themes-subtle-neutral)))
    `(markup-table-face ((,c :inherit modus-themes-subtle-neutral)))
    `(markup-table-row-face ((,c :inherit modus-themes-special-cold)))
    `(markup-title-0-face ((,c :inherit modus-themes-heading-1)))
    `(markup-title-1-face ((,c :inherit modus-themes-heading-2)))
    `(markup-title-2-face ((,c :inherit modus-themes-heading-3)))
    `(markup-title-3-face ((,c :inherit modus-themes-heading-4)))
    `(markup-title-4-face ((,c :inherit modus-themes-heading-5)))
    `(markup-title-5-face ((,c :inherit modus-themes-heading-6)))
    `(markup-verbatim-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-alt)))
;;;;; mentor
    `(mentor-download-message ((,c :foreground ,fg-special-warm)))
    `(mentor-download-name ((,c :foreground ,fg-special-cold)))
    `(mentor-download-progress ((,c :foreground ,blue-cooler)))
    `(mentor-download-size ((,c :foreground ,magenta-cooler)))
    `(mentor-download-speed-down ((,c :foreground ,cyan-warmer)))
    `(mentor-download-speed-up ((,c :foreground ,red-warmer)))
    `(mentor-download-state ((,c :foreground ,yellow-warmer)))
    `(mentor-highlight-face ((,c :inherit modus-themes-subtle-blue)))
    `(mentor-tracker-name ((,c :foreground ,magenta-warmer)))
;;;;; messages
    `(message-cited-text-1 ((,c :foreground ,mail-0)))
    `(message-cited-text-2 ((,c :foreground ,mail-1)))
    `(message-cited-text-3 ((,c :foreground ,mail-2)))
    `(message-cited-text-4 ((,c :foreground ,mail-3)))
    `(message-header-name ((,c :inherit bold)))
    `(message-header-newsgroups ((,c :inherit message-header-other)))
    `(message-header-to ((,c :inherit bold :foreground ,mail-recipient)))
    `(message-header-cc ((,c :foreground ,mail-recipient)))
    `(message-header-subject ((,c :inherit bold :foreground ,mail-subject)))
    `(message-header-xheader ((,c :inherit message-header-other)))
    `(message-header-other ((,c :foreground ,mail-other)))
    `(message-mml ((,c :foreground ,mail-4)))
    `(message-separator ((,c :background ,bg-active)))
;;;;; mini-modeline
    `(mini-modeline-mode-line ((,c :background ,blue-intense :height 0.14)))
    `(mini-modeline-mode-line-inactive ((,c :background ,border :height 0.1)))
;;;;; minimap
    `(minimap-active-region-background ((,c :background ,bg-active)))
    `(minimap-current-line-face ((,c :background ,cyan-intense-bg :foreground ,fg-main)))
;;;;; mmm-mode
    `(mmm-cleanup-submode-face ((,c :background ,yellow-nuanced-bg)))
    `(mmm-code-submode-face ((,c :background ,bg-alt)))
    `(mmm-comment-submode-face ((,c :background ,blue-nuanced-bg)))
    `(mmm-declaration-submode-face ((,c :background ,cyan-nuanced-bg)))
    `(mmm-default-submode-face ((,c :background ,bg-dim)))
    `(mmm-init-submode-face ((,c :background ,magenta-nuanced-bg)))
    `(mmm-output-submode-face ((,c :background ,red-nuanced-bg)))
    `(mmm-special-submode-face ((,c :background ,green-nuanced-bg)))
;;;;; mode-line
    `(mode-line ((,c :inherit modus-themes-ui-variable-pitch
                         ,@(modus-themes--mode-line-attrs
                            fg-active bg-active
                            fg-dim bg-active
                            fg-main bg-active-accent
                            fg-alt bg-active
                            'alt-style bg-main))))
    `(mode-line-active ((,c :inherit mode-line)))
    `(mode-line-buffer-id ((,c :inherit bold)))
    `(mode-line-emphasis ((,c :inherit bold :foreground ,magenta)))
    `(mode-line-highlight ((,c ,@(if modus-themes-intense-mouseovers
                                         (list :inherit 'modus-themes-intense-blue)
                                       (list :inherit 'highlight)))))
    `(mode-line-inactive ((,c :inherit modus-themes-ui-variable-pitch
                                  ,@(modus-themes--mode-line-attrs
                                     fg-inactive bg-inactive
                                     fg-alt bg-dim
                                     fg-inactive bg-inactive
                                     bg-region bg-active))))
;;;;; mood-line
    `(mood-line-modified ((,c :foreground ,magenta)))
    `(mood-line-status-error ((,c :inherit bold :foreground ,red)))
    `(mood-line-status-info ((,c :foreground ,cyan)))
    `(mood-line-status-neutral ((,c :foreground ,blue)))
    `(mood-line-status-success ((,c :inherit modus-themes-grue-active)))
    `(mood-line-status-warning ((,c :inherit bold :foreground ,yellow)))
    `(mood-line-unimportant ((,c :foreground ,fg-inactive)))
;;;;; mpdel
    `(mpdel-browser-directory-face ((,c :foreground ,blue)))
    `(mpdel-playlist-current-song-face ((,c :inherit bold :foreground ,blue-cooler)))
;;;;; mu4e
    `(mu4e-attach-number-face ((,c :inherit bold :foreground ,fg-dim)))
    `(mu4e-cited-1-face ((,c :inherit message-cited-text-1)))
    `(mu4e-cited-2-face ((,c :inherit message-cited-text-2)))
    `(mu4e-cited-3-face ((,c :inherit message-cited-text-3)))
    `(mu4e-cited-4-face ((,c :inherit message-cited-text-4)))
    `(mu4e-cited-5-face ((,c :inherit message-cited-text-1)))
    `(mu4e-cited-6-face ((,c :inherit message-cited-text-2)))
    `(mu4e-cited-7-face ((,c :inherit message-cited-text-3)))
    `(mu4e-compose-header-face ((,c :inherit mu4e-compose-separator-face)))
    `(mu4e-compose-separator-face ((,c :inherit modus-themes-intense-neutral)))
    `(mu4e-contact-face ((,c :inherit message-header-to)))
    `(mu4e-context-face ((,c :foreground ,blue)))
    `(mu4e-draft-face ((,c :foreground ,magenta-warmer)))
    `(mu4e-flagged-face ((,c :foreground ,red-cooler)))
    `(mu4e-footer-face ((,c :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(mu4e-forwarded-face ((,c :foreground ,magenta-cooler)))
    `(mu4e-header-face ((,c :inherit shadow)))
    `(mu4e-header-highlight-face ((,c :inherit modus-themes-hl-line)))
    `(mu4e-header-key-face ((,c :inherit message-header-name)))
    `(mu4e-header-marks-face ((,c :inherit mu4e-special-header-value-face)))
    `(mu4e-header-title-face ((,c :foreground ,fg-special-mild)))
    `(mu4e-header-value-face ((,c :inherit message-header-other)))
    `(mu4e-highlight-face ((,c :inherit modus-themes-key-binding)))
    `(mu4e-link-face ((,c :inherit button)))
    `(mu4e-modeline-face ((,c :foreground ,magenta)))
    `(mu4e-moved-face ((,c :inherit modus-themes-slant :foreground ,yellow)))
    `(mu4e-ok-face ((,c :inherit bold :foreground ,green)))
    `(mu4e-region-code ((,c :inherit modus-themes-special-calm)))
    `(mu4e-related-face ((,c :inherit (italic shadow))))
    `(mu4e-replied-face ((,c :foreground ,blue)))
    `(mu4e-special-header-value-face ((,c :inherit message-header-subject)))
    `(mu4e-system-face ((,c :inherit modus-themes-slant :foreground ,err)))
    `(mu4e-title-face ((,c :foreground ,fg-main)))
    `(mu4e-trashed-face ((,c :foreground ,red)))
    `(mu4e-unread-face ((,c :inherit bold)))
    `(mu4e-url-number-face ((,c :inherit shadow)))
    `(mu4e-view-body-face ((,c :foreground ,fg-main)))
    `(mu4e-warning-face ((,c :inherit warning)))
;;;;; multiple-cursors
    `(mc/cursor-bar-face ((,c :height 1 :foreground ,fg-main :background ,bg-main)))
    `(mc/cursor-face ((,c :inverse-video t)))
    `(mc/region-face ((,c :inherit region)))
;;;;; nano-modeline
    `(nano-modeline-active-primary ((,c :inherit mode-line :foreground ,fg-special-mild)))
    `(nano-modeline-active-secondary ((,c :inherit mode-line :foreground ,fg-special-cold)))
    `(nano-modeline-active-status-** ((,c :inherit mode-line :background ,yellow-subtle-bg)))
    `(nano-modeline-active-status-RO ((,c :inherit mode-line :background ,red-subtle-bg)))
    `(nano-modeline-active-status-RW ((,c :inherit mode-line :background ,cyan-subtle-bg)))
    `(nano-modeline-inactive-primary ((,c :inherit mode-line-inactive :foreground ,fg-inactive)))
    `(nano-modeline-inactive-secondary ((,c :inherit mode-line-inactive :foreground ,fg-inactive)))
    `(nano-modeline-inactive-status-** ((,c :inherit mode-line-inactive :foreground ,yellow)))
    `(nano-modeline-inactive-status-RO ((,c :inherit mode-line-inactive :foreground ,red)))
    `(nano-modeline-inactive-status-RW ((,c :inherit mode-line-inactive :foreground ,cyan)))
;;;;; neotree
    `(neo-banner-face ((,c :foreground ,magenta)))
    `(neo-button-face ((,c :inherit button)))
    `(neo-dir-link-face ((,c :inherit bold :foreground ,blue)))
    `(neo-expand-btn-face ((,c :foreground ,cyan)))
    `(neo-file-link-face ((,c :foreground ,fg-main)))
    `(neo-header-face ((,c :inherit bold :foreground ,fg-main)))
    `(neo-root-dir-face ((,c :inherit bold :foreground ,cyan-warmer)))
    `(neo-vc-added-face ((,c :inherit modus-themes-grue)))
    `(neo-vc-conflict-face ((,c :inherit error)))
    `(neo-vc-default-face ((,c :foreground ,fg-main)))
    `(neo-vc-edited-face ((,c :foreground ,yellow)))
    `(neo-vc-ignored-face ((,c :foreground ,fg-inactive)))
    `(neo-vc-missing-face ((,c :foreground ,red-warmer)))
    `(neo-vc-needs-merge-face ((,c :foreground ,magenta-warmer)))
    `(neo-vc-needs-update-face ((,c :underline t)))
    `(neo-vc-removed-face ((,c :strike-through t)))
    `(neo-vc-unlocked-changes-face ((,c :inherit modus-themes-refine-blue)))
    `(neo-vc-up-to-date-face ((,c :inherit shadow)))
    `(neo-vc-user-face ((,c :foreground ,magenta)))
;;;;; notmuch
    `(notmuch-crypto-decryption ((,c :inherit (shadow bold))))
    `(notmuch-crypto-part-header ((,c :foreground ,magenta-cooler)))
    `(notmuch-crypto-signature-bad ((,c :inherit error)))
    `(notmuch-crypto-signature-good ((,c :inherit success)))
    `(notmuch-crypto-signature-good-key ((,c :inherit bold :foreground ,cyan)))
    `(notmuch-crypto-signature-unknown ((,c :inherit warning)))
    `(notmuch-hello-logo-background ((,c :background "gray50")))
    `(notmuch-jump-key ((,c :inherit modus-themes-key-binding)))
    `(notmuch-message-summary-face ((,c :inherit (bold modus-themes-nuanced-cyan))))
    `(notmuch-search-count ((,c :inherit shadow)))
    `(notmuch-search-date ((,c :foreground ,cyan)))
    `(notmuch-search-flagged-face ((,c :foreground ,red-cooler)))
    `(notmuch-search-matching-authors ((,c :foreground ,fg-special-cold)))
    `(notmuch-search-non-matching-authors ((,c :inherit shadow)))
    `(notmuch-search-subject ((,c :foreground ,fg-main)))
    `(notmuch-search-unread-face ((,c :inherit bold)))
    `(notmuch-tag-added ((,c :underline ,blue)))
    `(notmuch-tag-deleted ((,c :strike-through ,red)))
    `(notmuch-tag-face ((,c :foreground ,blue)))
    `(notmuch-tag-flagged ((,c :foreground ,red-warmer)))
    `(notmuch-tag-unread ((,c :foreground ,magenta-warmer)))
    `(notmuch-tree-match-author-face ((,c :inherit notmuch-search-matching-authors)))
    `(notmuch-tree-match-date-face ((,c :inherit notmuch-search-date)))
    `(notmuch-tree-match-face ((,c :foreground ,fg-main)))
    `(notmuch-tree-match-tag-face ((,c :inherit notmuch-tag-face)))
    `(notmuch-tree-no-match-face ((,c :inherit shadow)))
    `(notmuch-tree-no-match-date-face ((,c :inherit shadow)))
    `(notmuch-wash-cited-text ((,c :inherit message-cited-text-1)))
    `(notmuch-wash-toggle-button ((,c :background ,bg-alt :foreground ,fg-alt)))
;;;;; num3-mode
    `(num3-face-even ((,c :inherit bold :background ,bg-alt)))
;;;;; nxml-mode
    `(nxml-attribute-colon ((,c :foreground ,fg-main)))
    `(nxml-attribute-local-name ((,c :inherit font-lock-variable-name-face)))
    `(nxml-attribute-prefix ((,c  :inherit font-lock-type-face)))
    `(nxml-attribute-value ((,c :inherit font-lock-constant-face)))
    `(nxml-cdata-section-CDATA ((,c :inherit error)))
    `(nxml-cdata-section-delimiter ((,c :inherit error)))
    `(nxml-char-ref-delimiter ((,c :foreground ,fg-special-mild)))
    `(nxml-char-ref-number ((,c :inherit modus-themes-bold :foreground ,fg-special-mild)))
    `(nxml-delimited-data ((,c :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(nxml-delimiter ((,c :foreground ,fg-dim)))
    `(nxml-element-colon ((,c :foreground ,fg-main)))
    `(nxml-element-local-name ((,c :inherit font-lock-function-name-face)))
    `(nxml-element-prefix ((,c :inherit font-lock-builtin-face)))
    `(nxml-entity-ref-delimiter ((,c :foreground ,fg-special-mild)))
    `(nxml-entity-ref-name ((,c :inherit modus-themes-bold :foreground ,fg-special-mild)))
    `(nxml-glyph ((,c :inherit modus-themes-intense-neutral)))
    `(nxml-hash ((,c :inherit (bold font-lock-string-face))))
    `(nxml-heading ((,c :inherit bold)))
    `(nxml-name ((,c :inherit font-lock-builtin-face)))
    `(nxml-namespace-attribute-colon ((,c :foreground ,fg-main)))
    `(nxml-namespace-attribute-prefix ((,c :inherit font-lock-variable-name-face)))
    `(nxml-processing-instruction-target ((,c :inherit font-lock-keyword-face)))
    `(nxml-prolog-keyword ((,c :inherit font-lock-keyword-face)))
    `(nxml-ref ((,c :inherit modus-themes-bold :foreground ,fg-special-mild)))
    `(rng-error ((,c :inherit error)))
;;;;; olivetti
    `(olivetti-fringe ((,c :background ,bg-main)))
;;;;; orderless
    `(orderless-match-face-0 ((,c :inherit modus-themes-completion-match-0)))
    `(orderless-match-face-1 ((,c :inherit modus-themes-completion-match-1)))
    `(orderless-match-face-2 ((,c :inherit modus-themes-completion-match-2)))
    `(orderless-match-face-3 ((,c :inherit modus-themes-completion-match-3)))
;;;;; org
    `(org-agenda-calendar-event ((,c ,@(modus-themes--agenda-event blue-warmer))))
    `(org-agenda-calendar-sexp ((,c ,@(modus-themes--agenda-event blue-warmer t))))
    `(org-agenda-clocking ((,c :background ,yellow-nuanced-bg :foreground ,red-warmer)))
    `(org-agenda-column-dateline ((,c :background ,bg-alt)))
    `(org-agenda-current-time ((,c :foreground ,blue-faint)))
    `(org-agenda-date ((,c ,@(modus-themes--agenda-date cyan fg-main))))
    `(org-agenda-date-today
      ((,c ,@(modus-themes--agenda-date cyan fg-main nil nil bg-special-cold t t))))
    `(org-agenda-date-weekend
      ((,c ,@(modus-themes--agenda-date cyan-faint fg-alt cyan fg-main))))
    `(org-agenda-date-weekend-today
      ((,c ,@(modus-themes--agenda-date cyan-faint fg-alt cyan fg-main bg-special-cold t t))))
    `(org-agenda-diary ((,c :inherit org-agenda-calendar-sexp)))
    `(org-agenda-dimmed-todo-face ((,c :inherit shadow)))
    `(org-agenda-done ((,c :inherit modus-themes-grue-nuanced)))
    `(org-agenda-filter-category ((,c :inherit bold :foreground ,cyan)))
    `(org-agenda-filter-effort ((,c :inherit bold :foreground ,cyan)))
    `(org-agenda-filter-regexp ((,c :inherit bold :foreground ,cyan)))
    `(org-agenda-filter-tags ((,c :inherit bold :foreground ,cyan)))
    `(org-agenda-restriction-lock ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(org-agenda-structure ((,c ,@(modus-themes--agenda-structure blue-warmer))))
    `(org-agenda-structure-filter ((,c :inherit org-agenda-structure :foreground ,yellow)))
    `(org-agenda-structure-secondary ((,c :foreground ,cyan)))
    `(org-archived ((,c :background ,bg-alt :foreground ,fg-alt)))
    `(org-block ((,c :inherit modus-themes-fixed-pitch
                         ,@(modus-themes--org-block bg-dim fg-main))))
    `(org-block-begin-line ((,c :inherit modus-themes-fixed-pitch
                                    ,@(modus-themes--org-block-delim
                                       bg-dim fg-special-cold
                                       bg-alt fg-alt))))
    `(org-block-end-line ((,c :inherit org-block-begin-line)))
    `(org-checkbox ((,c :foreground ,yellow-cooler)))
    `(org-checkbox-statistics-done ((,c :inherit org-done)))
    `(org-checkbox-statistics-todo ((,c :inherit org-todo)))
    `(org-clock-overlay ((,c :background ,yellow-nuanced-bg :foreground ,red-faint)))
    `(org-code ((,c :inherit modus-themes-markup-code :extend t)))
    `(org-column ((,c :inherit (modus-themes-fixed-pitch default)
                          :background ,bg-alt)))
    `(org-column-title ((,c :inherit (bold modus-themes-fixed-pitch default)
                                :underline t :background ,bg-alt)))
    `(org-date ((,c :inherit (modus-themes-link-symlink modus-themes-fixed-pitch))))
    `(org-date-selected ((,c :foreground ,blue-warmer :inverse-video t)))
    `(org-dispatcher-highlight ((,c :inherit (bold modus-themes-mark-alt))))
    `(org-document-info ((,c :foreground ,fg-special-cold)))
    `(org-document-info-keyword ((,c :inherit (shadow modus-themes-fixed-pitch))))
    `(org-document-title ((,c :inherit modus-themes-heading-0)))
    `(org-done ((,c :inherit modus-themes-grue)))
    `(org-drawer ((,c :inherit (shadow modus-themes-fixed-pitch))))
    `(org-ellipsis (())) ; inherits from the heading's color
    `(org-footnote ((,c :inherit button
                            ,@(modus-themes--link-color
                               blue blue-faint))))
    `(org-formula ((,c :inherit modus-themes-fixed-pitch :foreground ,red-warmer)))
    `(org-habit-alert-face ((,c ,@(modus-themes--agenda-habit
                                       yellow-graph-0-bg
                                       yellow-graph-0-bg
                                       yellow-graph-1-bg)
                                    :foreground "black"))) ; special case
    `(org-habit-alert-future-face ((,c ,@(modus-themes--agenda-habit
                                              yellow-graph-1-bg
                                              yellow-graph-0-bg
                                              yellow-graph-1-bg))))
    `(org-habit-clear-face ((,c ,@(modus-themes--agenda-habit
                                       blue-graph-0-bg
                                       green-graph-1-bg
                                       blue-graph-1-bg
                                       blue-graph-1-bg
                                       blue-graph-1-bg)
                                    :foreground "black"))) ; special case
    `(org-habit-clear-future-face ((,c ,@(modus-themes--agenda-habit
                                              blue-graph-1-bg
                                              green-graph-1-bg
                                              blue-graph-1-bg
                                              blue-graph-1-bg
                                              blue-graph-1-bg))))
    `(org-habit-overdue-face ((,c ,@(modus-themes--agenda-habit
                                         red-graph-0-bg
                                         red-graph-0-bg
                                         red-graph-1-bg))))
    `(org-habit-overdue-future-face ((,c ,@(modus-themes--agenda-habit
                                                red-graph-1-bg
                                                red-graph-0-bg
                                                red-graph-1-bg))))
    `(org-habit-ready-face ((,c ,@(modus-themes--agenda-habit
                                       green-graph-0-bg
                                       green-graph-0-bg
                                       green-graph-1-bg
                                       cyan-graph-0-bg
                                       blue-graph-0-bg
                                       cyan-graph-1-bg)
                                    :foreground "black"))) ; special case
    `(org-habit-ready-future-face ((,c ,@(modus-themes--agenda-habit
                                              green-graph-1-bg
                                              green-graph-0-bg
                                              green-graph-1-bg
                                              cyan-graph-1-bg
                                              blue-graph-0-bg
                                              cyan-graph-1-bg))))
    `(org-headline-done ((,c :inherit (modus-themes-variable-pitch modus-themes-grue-nuanced))))
    `(org-headline-todo ((,c :inherit modus-themes-variable-pitch :foreground ,red-nuanced-fg)))
    `(org-hide ((,c :foreground ,bg-main)))
    `(org-indent ((,c :inherit (fixed-pitch org-hide))))
    `(org-imminent-deadline ((,c :foreground ,red-intense)))
    `(org-latex-and-related ((,c :foreground ,magenta-faint)))
    `(org-level-1 ((,c :inherit modus-themes-heading-1)))
    `(org-level-2 ((,c :inherit modus-themes-heading-2)))
    `(org-level-3 ((,c :inherit modus-themes-heading-3)))
    `(org-level-4 ((,c :inherit modus-themes-heading-4)))
    `(org-level-5 ((,c :inherit modus-themes-heading-5)))
    `(org-level-6 ((,c :inherit modus-themes-heading-6)))
    `(org-level-7 ((,c :inherit modus-themes-heading-7)))
    `(org-level-8 ((,c :inherit modus-themes-heading-8)))
    `(org-link ((,c :inherit button)))
    `(org-list-dt ((,c :inherit bold)))
    `(org-macro ((,c :inherit modus-themes-markup-macro)))
    `(org-meta-line ((,c :inherit (shadow modus-themes-fixed-pitch))))
    `(org-mode-line-clock ((,c :foreground ,fg-main)))
    `(org-mode-line-clock-overrun ((,c :inherit bold :foreground ,red)))
    `(org-priority ((,c :foreground ,magenta)))
    `(org-property-value ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-special-cold)))
    `(org-quote ((,c ,@(modus-themes--org-block bg-dim fg-special-cold fg-main))))
    `(org-scheduled ((,c ,@(modus-themes--agenda-scheduled yellow-faint fg-special-warm magenta-warmer))))
    `(org-scheduled-previously ((,c ,@(modus-themes--agenda-scheduled yellow fg-special-warm yellow-cooler))))
    `(org-scheduled-today ((,c ,@(modus-themes--agenda-scheduled yellow fg-special-warm magenta-cooler))))
    `(org-sexp-date ((,c :foreground ,cyan-cooler)))
    `(org-special-keyword ((,c :inherit (shadow modus-themes-fixed-pitch))))
    `(org-table ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-special-cold)))
    `(org-table-header ((,c :inherit (fixed-pitch modus-themes-special-cold))))
    `(org-tag ((,c :foreground ,magenta-nuanced-fg)))
    `(org-tag-group ((,c :inherit bold :foreground ,cyan-nuanced-fg)))
    `(org-target ((,c :underline t)))
    `(org-time-grid ((,c :inherit shadow)))
    `(org-todo ((,c :foreground ,red)))
    `(org-upcoming-deadline ((,c :foreground ,red-cooler)))
    `(org-upcoming-distant-deadline ((,c :foreground ,red-faint)))
    `(org-verbatim ((,c :inherit modus-themes-markup-verbatim)))
    `(org-verse ((,c :inherit org-quote)))
    `(org-warning ((,c :inherit bold :foreground ,red-cooler)))
;;;;; org-journal
    `(org-journal-calendar-entry-face ((,c :inherit modus-themes-slant :foreground ,yellow-cooler)))
    `(org-journal-calendar-scheduled-face ((,c :inherit modus-themes-slant :foreground ,red-cooler)))
    `(org-journal-highlight ((,c :foreground ,magenta-warmer)))
;;;;; org-noter
    `(org-noter-no-notes-exist-face ((,c :inherit error)))
    `(org-noter-notes-exist-face ((,c :inherit success)))
;;;;; org-pomodoro
    `(org-pomodoro-mode-line ((,c :foreground ,red)))
    `(org-pomodoro-mode-line-break ((,c :foreground ,cyan)))
    `(org-pomodoro-mode-line-overtime ((,c :inherit bold :foreground ,red)))
;;;;; org-recur
    `(org-recur ((,c :foreground ,magenta)))
;;;;; org-roam
    `(org-roam-dim ((,c :foreground "gray50")))
    `(org-roam-header-line ((,c :inherit bold :foreground ,magenta)))
    `(org-roam-olp ((,c :inherit shadow)))
    `(org-roam-preview-heading ((,c :inherit modus-themes-subtle-neutral)))
    `(org-roam-preview-heading-highlight ((,c :inherit modus-themes-intense-neutral)))
    `(org-roam-preview-heading-selection ((,c :inherit modus-themes-special-cold)))
    `(org-roam-preview-region ((,c :inherit bold)))
    `(org-roam-title ((,c :inherit modus-themes-pseudo-header)))
;;;;; org-superstar
    `(org-superstar-item ((,c :foreground ,fg-main)))
;;;;; org-table-sticky-header
    `(org-table-sticky-header-face ((,c :inherit modus-themes-special-cold)))
;;;;; org-tree-slide
    `(org-tree-slide-header-overlay-face ((,c :inherit org-document-title)))
;;;;; origami
    `(origami-fold-header-face ((,c :background ,bg-dim :foreground ,fg-dim :box t)))
    `(origami-fold-replacement-face ((,c :background ,bg-alt :foreground ,fg-alt)))
;;;;; outline-mode
    `(outline-1 ((,c :inherit modus-themes-heading-1)))
    `(outline-2 ((,c :inherit modus-themes-heading-2)))
    `(outline-3 ((,c :inherit modus-themes-heading-3)))
    `(outline-4 ((,c :inherit modus-themes-heading-4)))
    `(outline-5 ((,c :inherit modus-themes-heading-5)))
    `(outline-6 ((,c :inherit modus-themes-heading-6)))
    `(outline-7 ((,c :inherit modus-themes-heading-7)))
    `(outline-8 ((,c :inherit modus-themes-heading-8)))
;;;;; outline-minor-faces
    `(outline-minor-0 (()))
;;;;; package (M-x list-packages)
    `(package-description ((,c :foreground ,fg-special-cold)))
    `(package-help-section-name ((,c :inherit bold :foreground ,cyan)))
    `(package-name ((,c :inherit button)))
    `(package-status-available ((,c :foreground ,cyan-cooler)))
    `(package-status-avail-obso ((,c :inherit error)))
    `(package-status-built-in ((,c :foreground ,magenta)))
    `(package-status-dependency ((,c :foreground ,magenta-cooler)))
    `(package-status-disabled ((,c :inherit modus-themes-subtle-red)))
    `(package-status-external ((,c :foreground ,cyan-cooler)))
    `(package-status-held ((,c :foreground ,yellow-warmer)))
    `(package-status-incompat ((,c :inherit warning)))
    `(package-status-installed ((,c :foreground ,fg-special-warm)))
    `(package-status-new ((,c :inherit success)))
    `(package-status-unsigned ((,c :inherit error)))
;;;;; page-break-lines
    `(page-break-lines ((,c :inherit default :foreground "gray50")))
;;;;; pandoc-mode
    `(pandoc-citation-key-face ((,c :background ,bg-dim :foreground ,magenta-warmer)))
    `(pandoc-directive-@@-face ((,c :background ,bg-dim :foreground ,blue-cooler)))
    `(pandoc-directive-braces-face ((,c :foreground ,blue-cooler)))
    `(pandoc-directive-contents-face ((,c :foreground ,cyan-cooler)))
    `(pandoc-directive-type-face ((,c :foreground ,magenta)))
;;;;; paren-face
    `(parenthesis ((,c :inherit shadow)))
;;;;; pass
    `(pass-mode-directory-face ((,c :inherit bold :foreground ,fg-special-cold)))
    `(pass-mode-entry-face ((,c :background ,bg-main :foreground ,fg-main)))
    `(pass-mode-header-face ((,c :foreground ,fg-special-warm)))
;;;;; pdf-tools
    `(pdf-links-read-link ((,c :background ,fg-main :foreground ,magenta-intense-bg :inherit bold))) ; Foreground is background and vice versa
    `(pdf-occur-document-face ((,c :inherit shadow)))
    `(pdf-occur-page-face ((,c :inherit shadow)))
;;;;; persp-mode
    `(persp-face-lighter-buffer-not-in-persp ((,c :inherit modus-themes-intense-red)))
    `(persp-face-lighter-default ((,c :inherit bold :foreground ,blue)))
    `(persp-face-lighter-nil-persp ((,c :inherit bold :foreground ,fg-active)))
;;;;; perspective
    `(persp-selected-face ((,c :inherit bold :foreground ,blue)))
;;;;; phi-grep
    `(phi-grep-heading-face ((,c :inherit modus-themes-pseudo-header :foreground ,fg-special-cold)))
    `(phi-grep-line-number-face ((,c :foreground ,fg-special-warm)))
    `(phi-grep-match-face ((,c :inherit modus-themes-special-calm)))
    `(phi-grep-modified-face ((,c :inherit modus-themes-refine-yellow)))
    `(phi-grep-overlay-face ((,c :inherit modus-themes-refine-blue)))
;;;;; pomidor
    `(pomidor-break-face ((,c :foreground ,blue-cooler)))
    `(pomidor-overwork-face ((,c :foreground ,red-cooler)))
    `(pomidor-skip-face ((,c :inherit (shadow modus-themes-slant))))
    `(pomidor-work-face ((,c :inherit modus-themes-grue)))
;;;;; popup
    `(popup-face ((,c :background ,bg-alt :foreground ,fg-main)))
    `(popup-isearch-match ((,c :inherit modus-themes-search-success)))
    `(popup-menu-mouse-face ((,c :inherit highlight)))
    `(popup-menu-selection-face ((,c :inherit modus-themes-completion-selected-popup)))
    `(popup-scroll-bar-background-face ((,c :background ,bg-active)))
    `(popup-scroll-bar-foreground-face ((,c :foreground ,fg-active)))
    `(popup-summary-face ((,c :background ,bg-active :foreground ,fg-inactive)))
    `(popup-tip-face ((,c :inherit modus-themes-refine-yellow)))
;;;;; powerline
    `(powerline-active0 ((,c :background "gray50" :foreground ,bg-main)))
    `(powerline-active1 ((,c :inherit mode-line-active)))
    `(powerline-active2 ((,c :inherit mode-line-inactive)))
    `(powerline-inactive0 ((,c :background ,bg-active :foreground ,fg-alt)))
    `(powerline-inactive1 ((,c :background ,bg-main :foreground ,fg-alt)))
    `(powerline-inactive2 ((,c :inherit mode-line-inactive)))
;;;;; powerline-evil
    `(powerline-evil-base-face ((,c :background ,fg-main :foreground ,bg-main)))
    `(powerline-evil-emacs-face ((,c :inherit modus-themes-intense-magenta)))
    `(powerline-evil-insert-face ((,c :inherit modus-themes-intense-green)))
    `(powerline-evil-motion-face ((,c :inherit modus-themes-intense-blue)))
    `(powerline-evil-normal-face ((,c :background ,fg-alt :foreground ,bg-main)))
    `(powerline-evil-operator-face ((,c :inherit modus-themes-intense-yellow)))
    `(powerline-evil-replace-face ((,c :inherit modus-themes-intense-red)))
    `(powerline-evil-visual-face ((,c :inherit modus-themes-intense-cyan)))
;;;;; prescient
    `(prescient-primary-highlight ((,c :inherit modus-themes-completion-match-0)))
    `(prescient-secondary-highlight ((,c :inherit modus-themes-completion-match-1)))
;;;;; proced
    `(proced-mark ((,c :inherit modus-themes-mark-symbol)))
    `(proced-marked ((,c :inherit modus-themes-mark-alt)))
    `(proced-sort-header ((,c :inherit bold :foreground ,fg-special-calm :underline t)))
;;;;; prodigy
    `(prodigy-green-face ((,c :inherit success)))
    `(prodigy-red-face ((,c :inherit error)))
    `(prodigy-yellow-face ((,c :inherit warning)))
;;;;; pulse
    `(pulse-highlight-start-face ((,c :background ,bg-active-accent :extend t)))
;;;;; pyim
    `(pyim-page ((,c :background ,bg-active :foreground ,fg-active)))
    `(pyim-page-selection ((,c :inherit bold :background ,bg-active :foreground ,blue)))
    `(pyim-page-subword ((,c :background ,bg-inactive)))
;;;;; quick-peek
    `(quick-peek-background-face ((,c :background ,bg-alt)))
    `(quick-peek-border-face ((,c :background ,border :height 1)))
    `(quick-peek-padding-face ((,c :background ,bg-alt :height 0.15)))
;;;;; racket-mode
    `(racket-debug-break-face ((,c :inherit modus-themes-intense-red)))
    `(racket-debug-locals-face ((,c :box (:line-width -1 :color nil)
                                        :foreground ,green-cooler)))
    `(racket-debug-result-face ((,c :inherit bold :box (:line-width -1 :color nil)
                                        :foreground ,green)))
    `(racket-here-string-face ((,c :foreground ,blue-warmer)))
    `(racket-keyword-argument-face ((,c :foreground ,red-warmer)))
    `(racket-logger-config-face ((,c :inherit (shadow modus-themes-slant))))
    `(racket-logger-debug-face ((,c :foreground ,blue-cooler)))
    `(racket-logger-info-face ((,c :foreground ,note)))
    `(racket-logger-topic-face ((,c :inherit modus-themes-slant :foreground ,magenta)))
    `(racket-selfeval-face ((,c :foreground ,green-warmer)))
    `(racket-xp-error-face ((,c :inherit modus-themes-lang-error)))
;;;;; rainbow-blocks
    `(rainbow-blocks-depth-1-face ((,c :foreground ,rainbow-0)))
    `(rainbow-blocks-depth-2-face ((,c :foreground ,rainbow-1)))
    `(rainbow-blocks-depth-3-face ((,c :foreground ,rainbow-2)))
    `(rainbow-blocks-depth-4-face ((,c :foreground ,rainbow-3)))
    `(rainbow-blocks-depth-5-face ((,c :foreground ,rainbow-4)))
    `(rainbow-blocks-depth-6-face ((,c :foreground ,rainbow-5)))
    `(rainbow-blocks-depth-7-face ((,c :foreground ,rainbow-6)))
    `(rainbow-blocks-depth-8-face ((,c :foreground ,rainbow-7)))
    `(rainbow-blocks-depth-9-face ((,c :foreground ,rainbow-8)))
    `(rainbow-blocks-unmatched-face ((,c :foreground ,red)))
;;;;; rainbow-delimiters
    `(rainbow-delimiters-base-error-face ((,c :background ,red-subtle-bg :foreground ,fg-main)))
    `(rainbow-delimiters-base-face ((,c :foreground ,fg-main)))
    `(rainbow-delimiters-depth-1-face ((,c :foreground ,rainbow-0)))
    `(rainbow-delimiters-depth-2-face ((,c :foreground ,rainbow-1)))
    `(rainbow-delimiters-depth-3-face ((,c :foreground ,rainbow-2)))
    `(rainbow-delimiters-depth-4-face ((,c :foreground ,rainbow-3)))
    `(rainbow-delimiters-depth-5-face ((,c :foreground ,rainbow-4)))
    `(rainbow-delimiters-depth-6-face ((,c :foreground ,rainbow-5)))
    `(rainbow-delimiters-depth-7-face ((,c :foreground ,rainbow-6)))
    `(rainbow-delimiters-depth-8-face ((,c :foreground ,rainbow-7)))
    `(rainbow-delimiters-depth-9-face ((,c :foreground ,rainbow-8)))
    `(rainbow-delimiters-mismatched-face ((,c :inherit (bold modus-themes-refine-yellow))))
    `(rainbow-delimiters-unmatched-face ((,c :inherit (bold modus-themes-refine-red))))
;;;;; rcirc
    `(rcirc-bright-nick ((,c :inherit bold :foreground ,magenta-intense)))
    `(rcirc-dim-nick ((,c :inherit shadow)))
    `(rcirc-monospace-text ((,c :inherit fixed-pitch)))
    `(rcirc-my-nick ((,c :inherit bold :foreground ,magenta)))
    `(rcirc-nick-in-message ((,c :inherit bold :foreground ,red-warmer)))
    `(rcirc-nick-in-message-full-line ((,c :inherit bold :foreground ,cyan-cooler)))
    `(rcirc-other-nick ((,c :inherit bold :foreground ,blue)))
    `(rcirc-prompt ((,c :inherit modus-themes-prompt)))
    `(rcirc-server ((,c :inherit shadow)))
    `(rcirc-timestamp ((,c :foreground ,cyan)))
    `(rcirc-track-keyword ((,c :inherit bold)))
    `(rcirc-track-nick ((,c :inherit bold :foreground ,red)))
    `(rcirc-url ((,c :inherit link)))
;;;;; recursion-indicator
    `(recursion-indicator-general ((,c :foreground ,blue)))
    `(recursion-indicator-minibuffer ((,c :foreground ,red)))
;;;;; regexp-builder (re-builder)
    `(reb-match-0 ((,c :inherit modus-themes-refine-cyan)))
    `(reb-match-1 ((,c :inherit modus-themes-subtle-magenta)))
    `(reb-match-2 ((,c :inherit modus-themes-subtle-green)))
    `(reb-match-3 ((,c :inherit modus-themes-refine-yellow)))
    `(reb-regexp-grouping-backslash ((,c :inherit font-lock-regexp-grouping-backslash)))
    `(reb-regexp-grouping-construct ((,c :inherit font-lock-regexp-grouping-construct)))
;;;;; rg (rg.el)
    `(rg-column-number-face ((,c :foreground ,magenta-cooler)))
    `(rg-context-face ((,c :inherit shadow)))
    `(rg-error-face ((,c :inherit bold :foreground ,red)))
    `(rg-file-tag-face ((,c :foreground ,fg-special-cold)))
    `(rg-filename-face ((,c :inherit bold :foreground ,fg-special-cold)))
    `(rg-line-number-face ((,c :foreground ,fg-special-warm)))
    `(rg-literal-face ((,c :foreground ,blue-warmer)))
    `(rg-match-face ((,c :inherit modus-themes-special-calm)))
    `(rg-regexp-face ((,c :foreground ,magenta)))
    `(rg-toggle-off-face ((,c :inherit bold :foreground ,fg-inactive)))
    `(rg-toggle-on-face ((,c :inherit bold :foreground ,cyan)))
    `(rg-warning-face ((,c :inherit bold :foreground ,yellow)))
;;;;; ripgrep
    `(ripgrep-context-face ((,c :inherit shadow)))
    `(ripgrep-error-face ((,c :inherit bold :foreground ,red)))
    `(ripgrep-hit-face ((,c :foreground ,cyan)))
    `(ripgrep-match-face ((,c :inherit modus-themes-special-calm)))
;;;;; rmail
    `(rmail-header-name ((,c :foreground ,cyan-cooler)))
    `(rmail-highlight ((,c :inherit bold :foreground ,magenta-warmer)))
;;;;; ruler-mode
    `(ruler-mode-column-number ((,c :inherit ruler-mode-default :foreground ,fg-main)))
    `(ruler-mode-comment-column ((,c :inherit ruler-mode-default :foreground ,red)))
    `(ruler-mode-current-column ((,c :inherit ruler-mode-default :background ,blue-subtle-bg :foreground ,fg-main)))
    `(ruler-mode-default ((,c :inherit default :background ,bg-alt :foreground "gray50")))
    `(ruler-mode-fill-column ((,c :inherit ruler-mode-default :foreground ,green)))
    `(ruler-mode-fringes ((,c :inherit ruler-mode-default :foreground ,cyan)))
    `(ruler-mode-goal-column ((,c :inherit ruler-mode-default :foreground ,blue)))
    `(ruler-mode-margins ((,c :inherit ruler-mode-default :foreground ,bg-main)))
    `(ruler-mode-pad ((,c :inherit ruler-mode-default :background ,bg-active :foreground ,fg-inactive)))
    `(ruler-mode-tab-stop ((,c :inherit ruler-mode-default :foreground ,fg-special-warm)))
;;;;; semantic
    `(semantic-complete-inline-face ((,c :foreground ,fg-special-warm :underline t)))
    `(semantic-decoration-on-fileless-includes ((,c :inherit modus-themes-refine-green)))
    `(semantic-decoration-on-private-members-face ((,c :inherit modus-themes-refine-cyan)))
    `(semantic-decoration-on-protected-members-face ((,c :background ,bg-dim)))
    `(semantic-decoration-on-unknown-includes ((,c :inherit modus-themes-refine-red)))
    `(semantic-decoration-on-unparsed-includes ((,c :inherit modus-themes-refine-yellow)))
    `(semantic-highlight-edits-face ((,c :background ,bg-alt)))
    `(semantic-highlight-func-current-tag-face ((,c :background ,bg-alt)))
    `(semantic-idle-symbol-highlight ((,c :inherit modus-themes-special-mild)))
    `(semantic-tag-boundary-face ((,c :overline ,blue-intense)))
    `(semantic-unmatched-syntax-face ((,c :underline ,err)))
;;;;; sesman
    `(sesman-browser-button-face ((,c :inherit button)))
    `(sesman-browser-highligh-face ((,c :inherit highlight)))
    `(sesman-buffer-face ((,c :foreground ,magenta)))
    `(sesman-directory-face ((,c :inherit bold :foreground ,blue)))
    `(sesman-project-face ((,c :inherit bold :foreground ,magenta-cooler)))
;;;;; shell-script-mode
    `(sh-heredoc ((,c :foreground ,blue-warmer)))
    `(sh-quoted-exec ((,c :inherit modus-themes-bold :foreground ,magenta-warmer)))
;;;;; shortdoc
    `(shortdoc-heading ((,c :inherit modus-themes-pseudo-header)))
    `(shortdoc-section (())) ; remove the default's variable-pitch style
;;;;; show-paren-mode
    `(show-paren-match ((,c ,@(modus-themes--paren bg-paren-match
                                                       bg-paren-match-intense)
                                :foreground ,fg-main)))
    `(show-paren-match-expression ((,c :background ,bg-paren-expression)))
    `(show-paren-mismatch ((,c :inherit modus-themes-intense-red)))
;;;;; shr
    `(shr-abbreviation ((,c :inherit modus-themes-lang-note)))
    `(shr-code ((,c :inherit modus-themes-markup-verbatim)))
    `(shr-h1 ((,c :inherit modus-themes-heading-1)))
    `(shr-h2 ((,c :inherit modus-themes-heading-2)))
    `(shr-h3 ((,c :inherit modus-themes-heading-3)))
    `(shr-h4 ((,c :inherit modus-themes-heading-4)))
    `(shr-h5 ((,c :inherit modus-themes-heading-5)))
    `(shr-h6 ((,c :inherit modus-themes-heading-6)))
    `(shr-selected-link ((,c :inherit modus-themes-subtle-red)))
;;;;; side-notes
    `(side-notes ((,c :background ,bg-dim :foreground ,fg-dim)))
;;;;; sieve-mode
    `(sieve-action-commands ((,c :inherit font-lock-builtin-face)))
    `(sieve-control-commands ((,c :inherit font-lock-keyword-face)))
    `(sieve-tagged-arguments ((,c :inherit font-lock-type-face)))
    `(sieve-test-commands ((,c :inherit font-lock-function-name-face)))
;;;;; skewer-mode
    `(skewer-error-face ((,c :foreground ,red :underline t)))
;;;;; slime (sldb)
    `(sldb-condition-face ((,c :inherit font-lock-preprocessor-face)))
    `(sldb-restart-number-face ((,c :inherit bold)))
    `(sldb-restart-type-face ((,c :inherit font-lock-type-face)))
    `(sldb-restartable-frame-line-face ((,c :inherit success)))
    `(sldb-section-face ((,c :inherit modus-themes-pseudo-header)))
    `(slime-error-face ((,c :inherit modus-themes-lang-error)))
    `(slime-note-face ((,c :underline t)))
    `(slime-repl-input-face ((,c :inherit bold)))
    `(slime-repl-inputed-output-face ((,c :inherit font-lock-string-face)))
    `(slime-repl-output-mouseover-face ((,c :inherit highlight)))
    `(slime-repl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(slime-style-warning-face ((,c :inherit modus-themes-lang-note)))
    `(slime-warning-face ((,c :inherit modus-themes-lang-warning)))
;;;;; sly
    `(sly-action-face ((,c :inherit font-lock-type-face)))
    `(sly-db-condition-face ((,c :inherit font-lock-preprocessor-face)))
    `(sly-db-restartable-frame-line-face ((,c :inherit success)))
    `(sly-error-face ((,c :inherit modus-themes-lang-error)))
    `(sly-mode-line ((,c :inherit mode-line-emphasis)))
    `(sly-mrepl-output-face ((,c :inherit font-lock-string-face)))
    `(sly-mrepl-output-face ((,c :inherit font-lock-string-face)))
    `(sly-mrepl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(sly-note-face ((,c :inherit modus-themes-lang-note)))
    `(sly-stickers-placed-face ((,c :inherit modus-themes-subtle-neutral)))
    `(sly-style-warning-face ((,c :inherit modus-themes-lang-note)))
    `(sly-warning-face ((,c :inherit modus-themes-lang-warning)))
;;;;; smart-mode-line
    `(sml/charging ((,c :foreground ,green)))
    `(sml/discharging ((,c :foreground ,red)))
    `(sml/filename ((,c :inherit bold :foreground ,blue)))
    `(sml/folder ((,c :foreground ,fg-active)))
    `(sml/git ((,c :inherit bold :foreground ,green)))
    `(sml/global ((,c :foreground ,fg-active)))
    `(sml/line-number ((,c :inherit sml/global)))
    `(sml/minor-modes ((,c :inherit sml/global)))
    `(sml/modes ((,c :inherit bold :foreground ,fg-active)))
    `(sml/modified ((,c :inherit bold :foreground ,magenta)))
    `(sml/mule-info ((,c :inherit sml/global)))
    `(sml/name-filling ((,c :foreground ,yellow)))
    `(sml/not-modified ((,c :inherit sml/global)))
    `(sml/numbers-separator ((,c :inherit sml/global)))
    `(sml/outside-modified ((,c :inherit modus-themes-intense-red)))
    `(sml/position-percentage ((,c :inherit sml/global)))
    `(sml/prefix ((,c :foreground ,green)))
    `(sml/process ((,c :inherit sml/prefix)))
    `(sml/projectile ((,c :inherit sml/git)))
    `(sml/read-only ((,c :inherit bold :foreground ,cyan)))
    `(sml/remote ((,c :inherit sml/global)))
    `(sml/sudo ((,c :inherit modus-themes-subtle-red)))
    `(sml/time ((,c :inherit sml/global)))
    `(sml/vc ((,c :inherit sml/git)))
    `(sml/vc-edited ((,c :inherit bold :foreground ,yellow)))
;;;;; smartparens
    `(sp-pair-overlay-face ((,c :inherit modus-themes-special-warm)))
    `(sp-show-pair-enclosing ((,c :inherit modus-themes-special-mild)))
    `(sp-show-pair-match-face ((,c ,@(modus-themes--paren bg-paren-match
                                                              bg-paren-match-intense)
                                       :foreground ,fg-main)))
    `(sp-show-pair-mismatch-face ((,c :inherit modus-themes-intense-red)))
    `(sp-wrap-overlay-closing-pair ((,c :inherit sp-pair-overlay-face)))
    `(sp-wrap-overlay-face ((,c :inherit sp-pair-overlay-face)))
    `(sp-wrap-overlay-opening-pair ((,c :inherit sp-pair-overlay-face)))
    `(sp-wrap-tag-overlay-face ((,c :inherit sp-pair-overlay-face)))
;;;;; smerge
    `(smerge-base ((,c :inherit modus-themes-diff-changed)))
    `(smerge-lower ((,c :inherit modus-themes-diff-added)))
    `(smerge-markers ((,c :inherit modus-themes-diff-heading)))
    `(smerge-refined-added ((,c :inherit modus-themes-diff-refine-added)))
    `(smerge-refined-changed (()))
    `(smerge-refined-removed ((,c :inherit modus-themes-diff-refine-removed)))
    `(smerge-upper ((,c :inherit modus-themes-diff-removed)))
;;;;; spaceline
    `(spaceline-evil-emacs ((,c :inherit modus-themes-intense-magenta)))
    `(spaceline-evil-insert ((,c :inherit modus-themes-intense-green)))
    `(spaceline-evil-motion ((,c :inherit modus-themes-intense-blue)))
    `(spaceline-evil-normal ((,c :background ,fg-alt :foreground ,bg-alt)))
    `(spaceline-evil-replace ((,c :inherit modus-themes-intense-red)))
    `(spaceline-evil-visual ((,c :inherit modus-themes-intense-cyan)))
    `(spaceline-flycheck-error ((,c :foreground ,red)))
    `(spaceline-flycheck-info ((,c :foreground ,cyan)))
    `(spaceline-flycheck-warning ((,c :foreground ,yellow)))
    `(spaceline-highlight-face ((,c :inherit modus-themes-fringe-blue)))
    `(spaceline-modified ((,c :inherit modus-themes-fringe-magenta)))
    `(spaceline-python-venv ((,c :foreground ,magenta)))
    `(spaceline-read-only ((,c :inherit modus-themes-fringe-red)))
    `(spaceline-unmodified ((,c :inherit modus-themes-fringe-cyan)))
;;;;; speedbar
    `(speedbar-button-face ((,c :inherit button)))
    `(speedbar-directory-face ((,c :inherit bold :foreground ,blue)))
    `(speedbar-file-face ((,c :foreground ,fg-main)))
    `(speedbar-highlight-face ((,c :inherit highlight)))
    `(speedbar-selected-face ((,c :inherit bold :foreground ,cyan)))
    `(speedbar-separator-face ((,c :inherit modus-themes-intense-neutral)))
    `(speedbar-tag-face ((,c :foreground ,yellow-cooler)))
;;;;; spell-fu
    `(spell-fu-incorrect-face ((,c :inherit modus-themes-lang-error)))
;;;;; stripes
    `(stripes ((,c :background ,bg-alt)))
;;;;; suggest
    `(suggest-heading ((,c :inherit bold :foreground ,yellow-cooler)))
;;;;; switch-window
    `(switch-window-background ((,c :background ,bg-dim)))
    `(switch-window-label ((,c :height 3.0 :foreground ,blue-intense)))
;;;;; swiper
    `(swiper-background-match-face-1 (( )))
    `(swiper-background-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(swiper-background-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(swiper-background-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
    `(swiper-line-face ((,c :inherit highlight)))
    `(swiper-match-face-1 (( )))
    `(swiper-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(swiper-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(swiper-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
;;;;; sx
    `(sx-inbox-item-type ((,c :foreground ,magenta-cooler)))
    `(sx-inbox-item-type-unread ((,c :inherit (sx-inbox-item-type bold))))
    `(sx-question-list-answers ((,c :foreground ,green)))
    `(sx-question-list-answers-accepted ((,c :box t :foreground ,green)))
    `(sx-question-list-bounty ((,c :inherit bold :background ,bg-alt :foreground ,yellow)))
    `(sx-question-list-date ((,c :foreground ,fg-special-cold)))
    `(sx-question-list-favorite ((,c :inherit bold :foreground ,fg-special-warm)))
    `(sx-question-list-parent ((,c :foreground ,fg-main)))
    `(sx-question-list-read-question ((,c :inherit shadow)))
    `(sx-question-list-score ((,c :foreground ,fg-special-mild)))
    `(sx-question-list-score-upvoted ((,c :inherit (sx-question-list-score bold))))
    `(sx-question-list-unread-question ((,c :inherit bold :foreground ,fg-main)))
    `(sx-question-mode-accepted ((,c :inherit bold :height 1.3 :foreground ,green)))
    `(sx-question-mode-closed ((,c :inherit modus-themes-intense-yellow :box (:line-width 2 :color nil))))
    `(sx-question-mode-closed-reason ((,c :box (:line-width 2 :color nil) :foreground ,fg-main)))
    `(sx-question-mode-content-face ((,c :background ,bg-dim)))
    `(sx-question-mode-date ((,c :foreground ,blue)))
    `(sx-question-mode-header ((,c :inherit bold :foreground ,cyan)))
    `(sx-question-mode-kbd-tag ((,c :inherit bold :height 0.9 :box (:line-width 3 :color ,fg-main :style released-button) :foreground ,fg-main)))
    `(sx-question-mode-score ((,c :foreground ,fg-dim)))
    `(sx-question-mode-score-downvoted ((,c :foreground ,yellow)))
    `(sx-question-mode-score-upvoted ((,c :inherit bold :foreground ,magenta)))
    `(sx-question-mode-title ((,c :inherit bold :foreground ,fg-main)))
    `(sx-question-mode-title-comments ((,c :inherit (shadow bold))))
    `(sx-tag ((,c :foreground ,magenta-warmer)))
    `(sx-user-name ((,c :foreground ,blue-warmer)))
    `(sx-user-reputation ((,c :inherit shadow)))
;;;;; symbol-overlay
    `(symbol-overlay-default-face ((,c :inherit modus-themes-special-warm)))
    `(symbol-overlay-face-1 ((,c :inherit modus-themes-intense-blue)))
    `(symbol-overlay-face-2 ((,c :inherit modus-themes-refine-magenta)))
    `(symbol-overlay-face-3 ((,c :inherit modus-themes-intense-yellow)))
    `(symbol-overlay-face-4 ((,c :inherit modus-themes-intense-magenta)))
    `(symbol-overlay-face-5 ((,c :inherit modus-themes-intense-red)))
    `(symbol-overlay-face-6 ((,c :inherit modus-themes-refine-red)))
    `(symbol-overlay-face-7 ((,c :inherit modus-themes-intense-cyan)))
    `(symbol-overlay-face-8 ((,c :inherit modus-themes-refine-cyan)))
;;;;; syslog-mode
    `(syslog-debug ((,c :inherit bold :foreground ,cyan-cooler)))
    `(syslog-error ((,c :inherit error)))
    `(syslog-file ((,c :inherit bold :foreground ,fg-special-cold)))
    `(syslog-hide ((,c :background ,bg-main :foreground ,fg-main)))
    `(syslog-hour ((,c :inherit bold :foreground ,magenta-cooler)))
    `(syslog-info ((,c :inherit success)))
    `(syslog-ip ((,c :inherit bold :foreground ,fg-special-mild :underline t)))
    `(syslog-su ((,c :inherit bold :foreground ,red-warmer)))
    `(syslog-warn ((,c :inherit warning)))
;;;;; tab-bar-mode
    `(tab-bar ((,c :inherit ef-themes-ui-variable-pitch :background ,bg-inactive)))
    `(tab-bar-tab-group-current ((,c :inherit bold :background ,bg-main :box (:line-width -2 :color ,bg-main) :foreground ,fg-alt)))
    `(tab-bar-tab-group-inactive ((,c :background ,bg-inactive :box (:line-width -2 :color ,bg-inactive) :foreground ,fg-alt)))
    `(tab-bar-tab ((,c :inherit bold :box (:line-width -2 :color ,bg-main) :background ,bg-main)))
    `(tab-bar-tab-inactive ((,c :box (:line-width -2 :color ,bg-active) :background ,bg-active)))
    `(tab-bar-tab-ungrouped ((,c :inherit tab-bar-tab-inactive)))
;;;;; tab-line-mode
    `(tab-line ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-inactive :height 0.95)))
    `(tab-line-close-highlight ((,c :foreground ,err)))
    `(tab-line-highlight ((,c :inherit highlight)))
    `(tab-line-tab (( )))
    `(tab-line-tab-current ((,c :inherit bold :box (:line-width -2 :color ,bg-main) :background ,bg-main)))
    `(tab-line-tab-inactive ((,c :box (:line-width -2 :color ,bg-active) :background ,bg-active)))
    `(tab-line-tab-inactive-alternate ((,c :inherit tab-line-tab-inactive :foreground ,fg-alt)))
    `(tab-line-tab-modified ((,c :foreground ,warning)))
;;;;; table (built-in table.el)
    `(table-cell ((,c :background ,blue-nuanced-bg)))
;;;;; telega
    `(telega-button ((,c :box t :foreground ,blue)))
    `(telega-button-active ((,c :box ,blue-intense-bg :background ,blue-intense-bg :foreground ,fg-main)))
    `(telega-button-highlight ((,c :inherit modus-themes-subtle-magenta)))
    `(telega-chat-prompt ((,c :inherit bold)))
    `(telega-entity-type-code ((,c :inherit modus-themes-markup-verbatim)))
    `(telega-entity-type-mention ((,c :foreground ,cyan)))
    `(telega-entity-type-pre ((,c :inherit modus-themes-markup-code)))
    `(telega-entity-type-spoiler ((,c :background ,fg-main :foreground ,fg-main)))
    `(telega-msg-heading ((,c :background ,bg-alt)))
    `(telega-msg-self-title ((,c :inherit bold)))
    `(telega-root-heading ((,c :inherit modus-themes-subtle-neutral)))
    `(telega-secret-title ((,c :foreground ,magenta-warmer)))
    `(telega-unmuted-count ((,c :foreground ,blue-cooler)))
    `(telega-user-online-status ((,c :foreground ,cyan)))
    `(telega-username ((,c :foreground ,cyan-cooler)))
    `(telega-webpage-chat-link ((,c :background ,bg-alt)))
    `(telega-webpage-fixed ((,c :inherit modus-themes-fixed-pitch :height 0.85)))
    `(telega-webpage-header ((,c :inherit modus-themes-variable-pitch :height 1.3)))
    `(telega-webpage-preformatted ((,c :inherit modus-themes-fixed-pitch :background ,bg-alt)))
    `(telega-webpage-subheader ((,c :inherit modus-themes-variable-pitch :height 1.15)))
;;;;; telephone-line
    `(telephone-line-accent-active ((,c :background ,fg-inactive :foreground ,bg-inactive)))
    `(telephone-line-accent-inactive ((,c :background ,bg-active :foreground ,fg-active)))
    `(telephone-line-error ((,c :inherit bold :foreground ,red)))
    `(telephone-line-evil ((,c :foreground ,fg-main)))
    `(telephone-line-evil-emacs ((,c :inherit telephone-line-evil :background ,magenta-intense-bg)))
    `(telephone-line-evil-insert ((,c :inherit telephone-line-evil :background ,green-intense-bg)))
    `(telephone-line-evil-motion ((,c :inherit telephone-line-evil :background ,yellow-intense-bg)))
    `(telephone-line-evil-normal ((,c :inherit telephone-line-evil :background ,bg-alt)))
    `(telephone-line-evil-operator ((,c :inherit telephone-line-evil :background ,yellow-subtle-bg)))
    `(telephone-line-evil-replace ((,c :inherit telephone-line-evil :background ,red-intense-bg)))
    `(telephone-line-evil-visual ((,c :inherit telephone-line-evil :background ,cyan-intense-bg)))
    `(telephone-line-projectile ((,c :foreground ,cyan)))
    `(telephone-line-unimportant ((,c :foreground ,fg-inactive)))
    `(telephone-line-warning ((,c :inherit bold :foreground ,yellow)))
;;;;; terraform-mode
    `(terraform--resource-name-face ((,c :foreground ,keyword)))
    `(terraform--resource-type-face ((,c :foreground ,type)))
;;;;; term
    `(term ((,c :background ,bg-main :foreground ,fg-main)))
    `(term-bold ((,c :inherit bold)))
    `(term-color-black ((,c :background "gray35" :foreground "gray35")))
    `(term-color-blue ((,c :background ,blue :foreground ,blue)))
    `(term-color-cyan ((,c :background ,cyan :foreground ,cyan)))
    `(term-color-green ((,c :background ,green :foreground ,green)))
    `(term-color-magenta ((,c :background ,magenta :foreground ,magenta)))
    `(term-color-red ((,c :background ,red :foreground ,red)))
    `(term-color-white ((,c :background "gray65" :foreground "gray65")))
    `(term-color-yellow ((,c :background ,yellow :foreground ,yellow)))
    `(term-underline ((,c :underline t)))
;;;;; textsec
    `(textsec-suspicious (()))
;;;;; tomatinho
    `(tomatinho-ok-face ((,c :foreground ,blue-intense)))
    `(tomatinho-pause-face ((,c :foreground ,yellow-intense)))
    `(tomatinho-reset-face ((,c :inherit shadow)))
;;;;; transient
    `(transient-active-infix ((,c :inherit modus-themes-special-mild)))
    `(transient-amaranth ((,c :inherit bold :foreground ,yellow-warmer)))
    ;; Placate the compiler for what is a spurious warning.  We also
    ;; have to do this with `eldoc-highlight-function-argument'.
    (list 'transient-argument `((,c :inherit bold :background ,cyan-nuanced-bg :foreground ,cyan)))
    `(transient-blue ((,c :inherit bold :foreground ,blue)))
    `(transient-disabled-suffix ((,c :inherit modus-themes-intense-red)))
    `(transient-enabled-suffix ((,c :inherit modus-themes-grue-background-subtle)))
    `(transient-heading ((,c :inherit bold :foreground ,fg-main)))
    `(transient-inactive-argument ((,c :inherit shadow)))
    `(transient-inactive-value ((,c :inherit shadow)))
    `(transient-key ((,c :inherit modus-themes-key-binding)))
    `(transient-mismatched-key ((,c :underline t)))
    `(transient-nonstandard-key ((,c :underline t)))
    `(transient-pink ((,c :inherit bold :foreground ,magenta)))
    `(transient-purple ((,c :inherit bold :foreground ,magenta-cooler)))
    `(transient-red ((,c :inherit bold :foreground ,red-faint)))
    `(transient-teal ((,c :inherit bold :foreground ,cyan-cooler)))
    `(transient-unreachable ((,c :inherit shadow)))
    `(transient-unreachable-key ((,c :inherit shadow)))
    `(transient-value ((,c :inherit bold :background ,yellow-nuanced-bg :foreground ,yellow-cooler)))
;;;;; trashed
    `(trashed-deleted ((,c :inherit modus-themes-mark-del)))
    `(trashed-directory ((,c :foreground ,blue)))
    `(trashed-mark ((,c :inherit modus-themes-mark-symbol)))
    `(trashed-marked ((,c :inherit modus-themes-mark-alt)))
    `(trashed-restored ((,c :inherit modus-themes-mark-sel)))
    `(trashed-symlink ((,c :inherit modus-themes-link-symlink)))
;;;;; tree-sitter
    `(tree-sitter-hl-face:attribute ((,c :inherit font-lock-variable-name-face)))
    `(tree-sitter-hl-face:constant.builtin ((,c :inherit tree-sitter-hl-face:constant)))
    `(tree-sitter-hl-face:escape ((,c :inherit font-lock-regexp-grouping-backslash)))
    `(tree-sitter-hl-face:function ((,c :inherit font-lock-function-name-face)))
    `(tree-sitter-hl-face:function.call ((,c :inherit tree-sitter-hl-face:function)))
    `(tree-sitter-hl-face:label (( )))
    `(tree-sitter-hl-face:method.call (( )))
    `(tree-sitter-hl-face:operator ((,c :inherit modus-themes-bold)))
    `(tree-sitter-hl-face:property (( )))
    `(tree-sitter-hl-face:property.definition ((,c :inherit font-lock-variable-name-face)))
    `(tree-sitter-hl-face:punctuation (( )))
    `(tree-sitter-hl-face:punctuation.bracket (( )))
    `(tree-sitter-hl-face:punctuation.delimiter (( )))
    `(tree-sitter-hl-face:punctuation.special ((,c :inherit font-lock-regexp-grouping-construct)))
    `(tree-sitter-hl-face:string.special ((,c :inherit tree-sitter-hl-face:string)))
    `(tree-sitter-hl-face:tag ((,c :inherit font-lock-function-name-face)))
    `(tree-sitter-hl-face:type.argument (( )))
;;;;; treemacs
    `(treemacs-directory-collapsed-face ((,c :foreground ,magenta-warmer)))
    `(treemacs-directory-face ((,c :inherit dired-directory)))
    `(treemacs-file-face ((,c :foreground ,fg-main)))
    `(treemacs-fringe-indicator-face ((,c :foreground ,fg-main)))
    `(treemacs-git-added-face ((,c :inherit success)))
    `(treemacs-git-conflict-face ((,c :inherit error)))
    `(treemacs-git-ignored-face ((,c :inherit shadow)))
    `(treemacs-git-modified-face ((,c :inherit warning)))
    `(treemacs-git-renamed-face ((,c :inherit italic)))
    `(treemacs-git-unmodified-face ((,c :foreground ,fg-main)))
    `(treemacs-git-untracked-face ((,c :inherit shadow)))
    `(treemacs-help-column-face ((,c :inherit modus-themes-bold :foreground ,magenta-cooler :underline t)))
    `(treemacs-help-title-face ((,c :foreground ,blue-cooler)))
    `(treemacs-on-failure-pulse-face ((,c :inherit modus-themes-intense-red)))
    `(treemacs-on-success-pulse-face ((,c :inherit modus-themes-grue-background-intense)))
    `(treemacs-root-face ((,c :inherit bold :foreground ,blue-cooler :height 1.2 :underline t)))
    `(treemacs-root-remote-disconnected-face ((,c :inherit treemacs-root-remote-face :foreground ,yellow)))
    `(treemacs-root-remote-face ((,c :inherit treemacs-root-face :foreground ,magenta)))
    `(treemacs-root-remote-unreadable-face ((,c :inherit treemacs-root-unreadable-face)))
    `(treemacs-root-unreadable-face ((,c :inherit treemacs-root-face :strike-through t)))
    `(treemacs-tags-face ((,c :foreground ,blue-warmer)))
;;;;; tty-menu
    `(tty-menu-disabled-face ((,c :background ,bg-alt :foreground ,fg-alt)))
    `(tty-menu-enabled-face ((,c :inherit bold :background ,bg-alt :foreground ,fg-main)))
    `(tty-menu-selected-face ((,c :inherit modus-themes-intense-blue)))
;;;;; tuareg
    `(caml-types-def-face ((,c :inherit modus-themes-subtle-red)))
    `(caml-types-expr-face ((,c :inherit modus-themes-subtle-green)))
    `(caml-types-occ-face ((,c :inherit modus-themes-subtle-green)))
    `(caml-types-scope-face ((,c :inherit modus-themes-subtle-blue)))
    `(caml-types-typed-face ((,c :inherit modus-themes-subtle-magenta)))
    `(tuareg-font-double-semicolon-face ((,c :inherit font-lock-preprocessor-face)))
    `(tuareg-font-lock-attribute-face ((,c :inherit font-lock-function-name-face)))
    `(tuareg-font-lock-constructor-face ((,c :foreground ,fg-main)))
    `(tuareg-font-lock-error-face ((,c :inherit (modus-themes-intense-red bold))))
    `(tuareg-font-lock-extension-node-face ((,c :background ,bg-alt :foreground ,magenta)))
    `(tuareg-font-lock-governing-face ((,c :inherit bold :foreground ,fg-main)))
    `(tuareg-font-lock-infix-extension-node-face ((,c :inherit font-lock-function-name-face)))
    `(tuareg-font-lock-interactive-directive-face ((,c :foreground ,fg-special-cold)))
    `(tuareg-font-lock-interactive-error-face ((,c :inherit error)))
    `(tuareg-font-lock-interactive-output-face ((,c :inherit font-lock-constant-face)))
    `(tuareg-font-lock-label-face ((,c :inherit font-lock-type-face)))
    `(tuareg-font-lock-line-number-face ((,c :foreground ,fg-special-warm)))
    `(tuareg-font-lock-module-face ((,c :inherit font-lock-builtin-face)))
    `(tuareg-font-lock-multistage-face ((,c :inherit bold :background ,bg-alt :foreground ,blue)))
    `(tuareg-font-lock-operator-face ((,c :inherit font-lock-preprocessor-face)))
    `(tuareg-opam-error-face ((,c :inherit error)))
    `(tuareg-opam-pkg-variable-name-face ((,c :inherit font-lock-variable-name-face)))
;;;;; typescript
    `(typescript-jsdoc-tag ((,c :inherit (font-lock-builtin-face font-lock-comment-face) :weight normal)))
    `(typescript-jsdoc-type ((,c :inherit (font-lock-type-face font-lock-comment-face) :weight normal)))
    `(typescript-jsdoc-value ((,c :inherit (font-lock-constant-face font-lock-comment-face) :weight normal)))
;;;;; undo-tree
    `(undo-tree-visualizer-active-branch-face ((,c :inherit bold :foreground ,fg-main)))
    `(undo-tree-visualizer-current-face ((,c :foreground ,blue-intense)))
    `(undo-tree-visualizer-default-face ((,c :inherit shadow)))
    `(undo-tree-visualizer-register-face ((,c :foreground ,magenta-intense)))
    `(undo-tree-visualizer-unmodified-face ((,c :foreground ,green-intense)))
;;;;; vc (vc-dir.el, vc-hooks.el)
    `(vc-dir-directory (( )))
    `(vc-dir-file ((,c :foreground ,name)))
    `(vc-dir-header ((,c :inherit bold)))
    `(vc-dir-header-value ((,c :foreground ,string)))
    `(vc-dir-mark-indicator (( )))
    `(vc-dir-status-edited ((,c :inherit italic)))
    `(vc-dir-status-ignored ((,c :inherit shadow)))
    `(vc-dir-status-up-to-date ((,c :foreground ,note)))
    `(vc-dir-status-warning ((,c :inherit error)))
    `(vc-conflict-state ((,c :inherit error)))
    `(vc-edited-state ((,c :inherit italic)))
    `(vc-git-log-edit-summary-max-warning ((,c :inherit error)))
    `(vc-git-log-edit-summary-target-warning ((,c :inherit warning)))
    `(vc-locally-added-state ((,c :inherit italic)))
    `(vc-locked-state ((,c :inherit success)))
    `(vc-missing-state ((,c :inherit error)))
    `(vc-needs-update-state ((,c :inherit error)))
    `(vc-removed-state ((,c :inherit error)))
    `(vc-state-base (( )))
    `(vc-up-to-date-state (( )))
;;;;; vertico
    `(vertico-current ((,c :inherit modus-themes-completion-selected)))
;;;;; vertico-quick
    `(vertico-quick1 ((,c :inherit bold :background ,bg-char-0)))
    `(vertico-quick2 ((,c :inherit bold :background ,bg-char-1)))
;;;;; vimish-fold
    `(vimish-fold-fringe ((,c :foreground ,cyan)))
    `(vimish-fold-mouse-face ((,c :inherit modus-themes-intense-blue)))
    `(vimish-fold-overlay ((,c :background ,bg-alt :foreground ,fg-special-cold)))
;;;;; visible-mark
    `(visible-mark-active ((,c :background ,blue-intense-bg)))
    `(visible-mark-face1 ((,c :background ,cyan-intense-bg)))
    `(visible-mark-face2 ((,c :background ,yellow-intense-bg)))
    `(visible-mark-forward-face1 ((,c :background ,magenta-intense-bg)))
    `(visible-mark-forward-face2 ((,c :background ,green-intense-bg)))
;;;;; visual-regexp
    `(vr/group-0 ((,c :inherit modus-themes-intense-blue)))
    `(vr/group-1 ((,c :inherit modus-themes-intense-magenta)))
    `(vr/group-2 ((,c :inherit modus-themes-intense-green)))
    `(vr/match-0 ((,c :inherit modus-themes-refine-yellow)))
    `(vr/match-1 ((,c :inherit modus-themes-refine-yellow)))
    `(vr/match-separator-face ((,c :inherit (modus-themes-intense-neutral bold))))
;;;;; vterm
    `(vterm-color-black ((,c :background "gray35" :foreground "gray35")))
    `(vterm-color-blue ((,c :background ,blue :foreground ,blue)))
    `(vterm-color-cyan ((,c :background ,cyan :foreground ,cyan)))
    `(vterm-color-default ((,c :background ,bg-main :foreground ,fg-main)))
    `(vterm-color-green ((,c :background ,green :foreground ,green)))
    `(vterm-color-inverse-video ((,c :background ,bg-main :inverse-video t)))
    `(vterm-color-magenta ((,c :background ,magenta :foreground ,magenta)))
    `(vterm-color-red ((,c :background ,red :foreground ,red)))
    `(vterm-color-underline ((,c :foreground ,fg-special-warm :underline t)))
    `(vterm-color-white ((,c :background "gray65" :foreground "gray65")))
    `(vterm-color-yellow ((,c :background ,yellow :foreground ,yellow)))
;;;;; vundo
    `(vundo-highlight ((,c :inherit (bold vundo-node) :foreground ,red-intense)))
;;;;; wcheck-mode
    `(wcheck-default-face ((,c :foreground ,red :underline t)))
;;;;; web-mode
    `(web-mode-annotation-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-annotation-html-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-annotation-tag-face ((,c :inherit web-mode-comment-face :underline t)))
    `(web-mode-block-attr-name-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-block-attr-value-face ((,c :inherit font-lock-type-face)))
    `(web-mode-block-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-block-control-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-block-delimiter-face ((,c :foreground ,fg-main)))
    `(web-mode-block-face ((,c :background ,bg-dim)))
    `(web-mode-block-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-bold-face ((,c :inherit bold)))
    `(web-mode-builtin-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-comment-face ((,c :inherit font-lock-comment-face)))
    `(web-mode-comment-keyword-face ((,c :inherit font-lock-warning-face)))
    `(web-mode-constant-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-css-at-rule-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-css-color-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-css-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-css-function-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-css-priority-face ((,c :inherit font-lock-warning-face)))
    `(web-mode-css-property-name-face ((,c :inherit font-lock-keyword-face)))
    `(web-mode-css-pseudo-class-face ((,c :inherit font-lock-doc-face)))
    `(web-mode-css-selector-face ((,c :inherit font-lock-keyword-face)))
    `(web-mode-css-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-css-variable-face ((,c :foreground ,fg-special-warm)))
    `(web-mode-current-column-highlight-face ((,c :background ,bg-alt)))
    `(web-mode-current-element-highlight-face ((,c :inherit modus-themes-special-mild)))
    `(web-mode-doctype-face ((,c :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(web-mode-error-face ((,c :inherit modus-themes-intense-red)))
    `(web-mode-filter-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-folded-face ((,c :underline t)))
    `(web-mode-function-call-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-function-name-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-html-attr-custom-face ((,c :inherit font-lock-variable-name-face)))
    `(web-mode-html-attr-engine-face ((,c :foreground ,fg-main)))
    `(web-mode-html-attr-equal-face ((,c :foreground ,fg-main)))
    `(web-mode-html-attr-name-face ((,c :inherit font-lock-variable-name-face)))
    `(web-mode-html-attr-value-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-html-entity-face ((,c :inherit font-lock-negation-char-face)))
    `(web-mode-html-tag-bracket-face ((,c :foreground ,fg-dim)))
    `(web-mode-html-tag-custom-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-html-tag-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-html-tag-namespaced-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-html-tag-unclosed-face ((,c :inherit error :underline t)))
    `(web-mode-inlay-face ((,c :background ,bg-alt)))
    `(web-mode-italic-face ((,c :inherit italic)))
    `(web-mode-javascript-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-javascript-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-json-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-json-context-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-json-key-face ((,c :foreground ,blue-nuanced-fg)))
    `(web-mode-json-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-jsx-depth-1-face ((,c :background ,blue-intense-bg :foreground ,fg-main)))
    `(web-mode-jsx-depth-2-face ((,c :background ,blue-subtle-bg :foreground ,fg-main)))
    `(web-mode-jsx-depth-3-face ((,c :background ,bg-special-cold :foreground ,fg-special-cold)))
    `(web-mode-jsx-depth-4-face ((,c :background ,bg-alt :foreground ,blue-refine-fg)))
    `(web-mode-jsx-depth-5-face ((,c :background ,bg-alt :foreground ,blue-nuanced-fg)))
    `(web-mode-keyword-face ((,c :inherit font-lock-keyword-face)))
    `(web-mode-param-name-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-part-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-part-face ((,c :inherit web-mode-block-face)))
    `(web-mode-part-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-preprocessor-face ((,c :inherit font-lock-preprocessor-face)))
    `(web-mode-script-face ((,c :inherit web-mode-part-face)))
    `(web-mode-sql-keyword-face ((,c :inherit font-lock-negation-char-face)))
    `(web-mode-string-face ((,c :inherit font-lock-string-face)))
    `(web-mode-style-face ((,c :inherit web-mode-part-face)))
    `(web-mode-symbol-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-type-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-underline-face ((,c :underline t)))
    `(web-mode-variable-name-face ((,c :inherit font-lock-variable-name-face)))
    `(web-mode-warning-face ((,c :inherit font-lock-warning-face)))
    `(web-mode-whitespace-face ((,c :background ,bg-alt)))
;;;;; wgrep
    `(wgrep-delete-face ((,c :inherit warning)))
    `(wgrep-done-face ((,c :inherit success)))
    `(wgrep-face ((,c :inherit bold)))
    `(wgrep-file-face ((,c :foreground ,fg-special-warm)))
    `(wgrep-reject-face ((,c :inherit error)))
;;;;; which-function-mode
    `(which-func ((,c :foreground ,magenta)))
;;;;; which-key
    `(which-key-command-description-face ((,c :foreground ,fg-main)))
    `(which-key-group-description-face ((,c :foreground ,magenta-warmer)))
    `(which-key-highlighted-command-face ((,c :foreground ,yellow :underline t)))
    `(which-key-key-face ((,c :inherit modus-themes-key-binding)))
    `(which-key-local-map-description-face ((,c :foreground ,fg-main)))
    `(which-key-note-face ((,c :foreground ,fg-special-warm)))
    `(which-key-separator-face ((,c :inherit shadow)))
    `(which-key-special-key-face ((,c :inherit bold :foreground ,red-warmer)))
;;;;; whitespace-mode
    `(whitespace-big-indent ((,c :inherit modus-themes-subtle-red)))
    `(whitespace-empty ((,c :inherit modus-themes-intense-magenta)))
    `(whitespace-hspace ((,c :background ,bg-alt)))
    `(whitespace-indentation ((,c :background ,bg-alt)))
    `(whitespace-line ((,c :inherit modus-themes-subtle-yellow)))
    `(whitespace-newline ((,c :background ,bg-alt)))
    `(whitespace-space ((,c :background ,bg-alt)))
    `(whitespace-space-after-tab ((,c :inherit modus-themes-subtle-magenta)))
    `(whitespace-space-before-tab ((,c :inherit modus-themes-subtle-cyan)))
    `(whitespace-tab ((,c :background ,bg-alt)))
    `(whitespace-trailing ((,c :inherit modus-themes-intense-red)))
;;;;; window-divider-mode
    `(window-divider ((,c :foreground ,border)))
    `(window-divider-first-pixel ((,c :foreground "gray50")))
    `(window-divider-last-pixel ((,c :foreground "gray50")))
;;;;; winum
    `(winum-face ((,c :inherit modus-themes-bold :foreground ,cyan)))
;;;;; writegood-mode
    `(writegood-duplicates-face ((,c :background ,bg-alt :foreground ,red-warmer :underline t)))
    `(writegood-passive-voice-face ((,c :inherit modus-themes-lang-warning)))
    `(writegood-weasels-face ((,c :inherit modus-themes-lang-error)))
;;;;; woman
    `(woman-addition ((,c :foreground ,magenta-cooler)))
    `(woman-bold ((,c :inherit bold :foreground ,magenta-warmer)))
    `(woman-italic ((,c :inherit italic :foreground ,cyan)))
    `(woman-unknown ((,c :foreground ,green-warmer)))
;;;;; xah-elisp-mode
    `(xah-elisp-at-symbol ((,c :inherit font-lock-warning-face)))
    `(xah-elisp-cap-variable ((,c :inherit font-lock-preprocessor-face)))
    `(xah-elisp-command-face ((,c :inherit font-lock-type-face)))
    `(xah-elisp-dollar-symbol ((,c :inherit font-lock-variable-name-face)))
;;;;; xref
    `(xref-file-header ((,c :inherit bold :foreground ,fg-special-cold)))
    `(xref-line-number ((,c :inherit shadow)))
    `(xref-match ((,c :inherit match)))
;;;;; yaml-mode
    `(yaml-tab-face ((,c :inherit modus-themes-intense-red)))
;;;;; yasnippet
    `(yas-field-highlight-face ((,c :inherit highlight)))
;;;;; ztree
    `(ztreep-arrow-face ((,c :foreground ,fg-inactive)))
    `(ztreep-diff-header-face ((,c :inherit bold :height 1.2 :foreground ,fg-special-cold)))
    `(ztreep-diff-header-small-face ((,c :foreground ,fg-main)))
    `(ztreep-diff-model-add-face ((,c :inherit modus-themes-grue)))
    `(ztreep-diff-model-diff-face ((,c :foreground ,red)))
    `(ztreep-diff-model-ignored-face ((,c :inherit shadow :strike-through t)))
    `(ztreep-diff-model-normal-face ((,c :inherit shadow)))
    `(ztreep-expand-sign-face ((,c :inherit ztreep-arrow-face)))
    `(ztreep-header-face ((,c :inherit bold :height 1.2 :foreground ,fg-special-cold)))
    `(ztreep-leaf-face ((,c :foreground ,cyan)))
    `(ztreep-node-count-children-face ((,c :foreground ,fg-special-warm)))
    `(ztreep-node-face ((,c :foreground ,fg-main))))
  "Face specs for use with `modus-themes-theme'.")

(defconst modus-themes-custom-variables
  '(
;;;; ansi-colors
    `(ansi-color-faces-vector [default bold shadow italic underline success warning error])
    `(ansi-color-names-vector ["gray35" ,red ,green ,yellow ,blue ,magenta ,cyan "gray65"])
;;;; awesome-tray
    `(awesome-tray-mode-line-active-color ,blue)
    `(awesome-tray-mode-line-inactive-color ,bg-active)
;;;; chart
    `(chart-face-color-list
      '( ,red-graph-0-bg ,green-graph-0-bg ,yellow-graph-0-bg ,blue-graph-0-bg ,magenta-graph-0-bg ,cyan-graph-0-bg
         ,red-graph-1-bg ,green-graph-1-bg ,yellow-graph-1-bg ,blue-graph-1-bg ,magenta-graph-1-bg ,cyan-graph-1-bg))
;;;; exwm
    `(exwm-floating-border-color ,border)
;;;; flymake fringe indicators
    `(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
    `(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
    `(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
;;;; highlight-changes
    `(highlight-changes-colors nil)
    `(highlight-changes-face-list '(success warning error bold bold-italic))
;;;; ibuffer
    `(ibuffer-deletion-face 'modus-themes-mark-del)
    `(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
    `(ibuffer-marked-face 'modus-themes-mark-sel)
    `(ibuffer-title-face 'default)
;;;; hl-todo
    `(hl-todo-keyword-faces
      '(("HOLD" . ,yellow-warmer)
        ("TODO" . ,magenta)
        ("NEXT" . ,magenta-cooler)
        ("THEM" . ,magenta-warmer)
        ("PROG" . ,cyan)
        ("OKAY" . ,cyan-warmer)
        ("DONT" . ,green-warmer)
        ("FAIL" . ,red)
        ("BUG" . ,red)
        ("DONE" . ,green)
        ("NOTE" . ,yellow-cooler)
        ("KLUDGE" . ,yellow)
        ("HACK" . ,yellow)
        ("TEMP" . ,red-nuanced-fg)
        ("FIXME" . ,red-cooler)
        ("XXX+" . ,red-warmer)
        ("REVIEW" . ,cyan-cooler)
        ("DEPRECATED" . ,blue-nuanced-fg)))
;;;; mini-modeline
    `(mini-modeline-face-attr '(:background unspecified))
;;;; pdf-tools
    `(pdf-view-midnight-colors
      '(,fg-main . ,bg-dim))
;;;; wid-edit
    `(widget-link-prefix ,(if (memq 'all-buttons modus-themes-box-buttons)
                              " "
                            "["))
    `(widget-link-suffix ,(if (memq 'all-buttons modus-themes-box-buttons)
                              " "
                            "]"))
    `(widget-mouse-face '(highlight widget-button))
    `(widget-push-button-prefix ,(if (memq 'all-buttons modus-themes-box-buttons)
                                     " "
                                   "["))
    `(widget-push-button-suffix ,(if (memq 'all-buttons modus-themes-box-buttons)
                                     " "
                                   "]"))
;;;; xterm-color
    `(xterm-color-names ["black" ,red ,green ,yellow ,blue ,magenta ,cyan "gray65"])
    `(xterm-color-names-bright ["gray35" ,red-warmer ,green-warmer ,yellow-warmer ,blue-warmer ,magenta-warmer ,cyan-warmer "white"])
    (if (or (eq modus-themes-org-blocks 'tinted-background)
            (eq modus-themes-org-blocks 'rainbow))
        `(org-src-block-faces
          `(("emacs-lisp" modus-themes-nuanced-magenta)
            ("elisp" modus-themes-nuanced-magenta)
            ("clojure" modus-themes-nuanced-magenta)
            ("clojurescript" modus-themes-nuanced-magenta)
            ("c" modus-themes-nuanced-blue)
            ("c++" modus-themes-nuanced-blue)
            ("sh" modus-themes-nuanced-green)
            ("shell" modus-themes-nuanced-green)
            ("html" modus-themes-nuanced-yellow)
            ("xml" modus-themes-nuanced-yellow)
            ("css" modus-themes-nuanced-red)
            ("scss" modus-themes-nuanced-red)
            ("python" modus-themes-nuanced-green)
            ("ipython" modus-themes-nuanced-magenta)
            ("r" modus-themes-nuanced-cyan)
            ("yaml" modus-themes-nuanced-cyan)
            ("conf" modus-themes-nuanced-cyan)
            ("docker" modus-themes-nuanced-cyan)))
      `(org-src-block-faces '())))
  "Custom variables for `modus-themes-theme'.")

;;; Theme macros

;;;; Instantiate a Modus theme

;;;###autoload
(defmacro modus-themes-theme (name palette)
  "Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.
Those are stored in `modus-themes-faces' and
`modus-themes-custom-variables' respectively."
  (declare (indent 0))
  (let ((sym (gensym))
        (colors (mapcar #'car (symbol-value palette))))
    `(let* ((c '((class color) (min-colors 256)))
            (,sym ,palette)
            ,@(mapcar (lambda (color)
                        (list color
                              `(let* ((value (car (alist-get ',color ,sym))))
                                 (if (stringp value)
                                     value
                                   (car (alist-get value ,sym))))))
                      colors))
       (custom-theme-set-faces ',name ,@modus-themes-faces)
       (custom-theme-set-variables ',name ,@modus-themes-custom-variables))))

;;;; Use theme colors

(defmacro modus-themes-with-colors (&rest body)
  "Evaluate BODY with colors from current palette bound."
  (declare (indent 0))
  (let* ((sym (gensym))
         ;; NOTE 2022-08-23: We just give it a sample palette at this
         ;; stage.  It only needs to collect each car.  Then we
         ;; instantiate the actual theme's palette.  We have to do this
         ;; otherwise the macro does not work properly when called from
         ;; inside a function.
         (colors (mapcar #'car (modus-themes--current-theme-palette))))
    `(let* ((c '((class color) (min-colors 256)))
            (,sym (modus-themes--current-theme-palette))
            ,@(mapcar (lambda (color)
                        (list color
                              `(let* ((value (car (alist-get ',color ,sym))))
                                 (if (stringp value)
                                     value
                                   (car (alist-get value ,sym))))))
                      colors))
       (ignore c ,@colors)            ; Silence unused variable warnings
       ,@body)))

;;;; Add themes from package to path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (equal dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'modus-themes)
;;; modus-themes.el ends here
