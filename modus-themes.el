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
;;     modus-themes-italic-constructs              (boolean)
;;     modus-themes-mixed-fonts                    (boolean)
;;     modus-themes-subtle-line-numbers            (boolean)
;;     modus-themes-variable-pitch-ui              (boolean)
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

(defcustom modus-themes-after-load-theme-hook nil
  "Hook that runs after loading a Modus theme.
This is used by the command `modus-themes-toggle'."
  :type 'hook
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes)

(defalias 'modus-themes-post-load-hook 'modus-themes-after-load-theme-hook)

(make-obsolete 'modus-themes-inhibit-reload nil "4.0.0")
(make-obsolete 'modus-themes-operandi-color-overrides nil "4.0.0")
(make-obsolete 'modus-themes-vivendi-color-overrides nil "4.0.0")

(defvaralias 'modus-themes-slanted-constructs 'modus-themes-italic-constructs)

(defcustom modus-themes-italic-constructs nil
  "Use italic font forms in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Italic constructs"))

(defcustom modus-themes-bold-constructs nil
  "Use bold text in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Bold constructs"))

(defcustom modus-themes-variable-pitch-ui nil
  "Use proportional fonts (variable-pitch) in UI elements.
This includes the mode line, header line, tab bar, and tab line."
  :group 'modus-themes
  :package-version '(modus-themes . "1.1.0")
  :version "28.1"
  :type 'boolean
  :link '(info-link "(modus-themes) UI typeface"))

(defcustom modus-themes-mixed-fonts nil
  "Non-nil to enable inheritance from `fixed-pitch' in some faces.

This is done to allow spacing-sensitive constructs, such as Org
tables and code blocks, to remain monospaced when users opt for
something like the command `variable-pitch-mode'.

Users may need to explicitly configure the font family of
`fixed-pitch' in order to get a consistent experience with their
typography (also check the `fontaine' package on GNU ELPA (by
Protesilaos))."
  :group 'modus-themes
  :package-version '(modus-themes . "1.7.0")
  :version "29.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Mixed fonts"))

(make-obsolete 'modus-themes-intense-mouseovers nil "4.0.0")

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
  :link '(info-link "(modus-themes) Mode line"))

(make-obsolete 'modus-themes-diffs nil "4.0.0")

(defcustom modus-themes-completions
  '((selection . (intense))
    (popup . (intense)))
  "Control the style of completion user interfaces.

This affects Company, Corfu, Flx, Icomplete/Fido, Ido, Ivy,
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
  :link '(info-link "(modus-themes) Completion UIs"))

(defcustom modus-themes-prompts nil
  "Use subtle or intense styles for minibuffer and REPL prompts.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) means to only use a
subtle accented foreground color.

The property `background' applies a background color to the
prompt's text.

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
  :link '(info-link "(modus-themes) Command prompts"))

(make-obsolete 'modus-themes-hl-line nil "4.0.0")

(defcustom modus-themes-subtle-line-numbers nil
  "Use more subtle style for command `display-line-numbers-mode'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.2.0")
  :version "28.1"
  :type 'boolean
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

A `bold' property applies a heavy typographic weight to the text
of the link.

An `italic' property adds a slant to the link's text (italic or
oblique forms, depending on the typeface).

Combinations of any of those properties are expressed as a list,
like in these examples:

    (bold)
    (neutral-underline bold)
    (italic bold no-underline)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-links (quote (neutral-underline bold)))

The placement of the underline, meaning its proximity to the
text, is controlled by `x-use-underline-position-properties',
`x-underline-at-descent-line', `underline-minimum-offset'.
Please refer to their documentation strings."
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type '(set :tag "Properties" :greedy t
              (choice :tag "Underline"
                      (const :tag "Same color as text (default)" nil)
                      (const :tag "Neutral (gray) underline color" neutral-underline)
                      (const :tag "No underline" no-underline))
              (const :tag "Bold font weight" bold)
              (const :tag "Italic font slant" italic))
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
  :link '(info-link "(modus-themes) Deuteranopia style"))

(make-obsolete 'modus-themes-mail-citations nil "4.0.0")
(make-obsolete 'modus-themes-tabs-accented nil "4.0.0")
(make-obsolete 'modus-themes-box-buttons nil "4.0.0")



;;; Helper functions for theme setup

(defconst modus-themes-items '(modus-operandi modus-vivendi)
  "Symbols of the Modus themes.")

(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

(defun modus-themes--list-enabled-themes ()
  "Return list of `custom-enabled-themes' with modus- prefix."
  (cl-remove-if-not
   (lambda (theme)
     (string-prefix-p "modus-" (symbol-name theme)))
   custom-enabled-themes))

(defun modus-themes--enable-themes ()
  "Enable the Modus themes."
  (mapc (lambda (theme)
          (unless (memq theme custom-known-themes)
            (load-theme theme :no-confirm :no-enable)))
        modus-themes-items))

(defun modus-themes--list-known-themes ()
  "Return list of `custom-known-themes' with modus- prefix."
  (modus-themes--enable-themes)
  (cl-remove-if-not
   (lambda (theme)
     (string-prefix-p "modus-" (symbol-name theme)))
   custom-known-themes))

(defun modus-themes--current-theme ()
  "Return first enabled Modus theme."
  (car (modus-themes--list-enabled-themes)))

(defun modus-themes--palette (theme)
  "Return THEME palette as a symbol."
  (when theme
    (intern (format "%s-palette" theme))))

(defun modus-themes--current-theme-palette ()
  "Return palette of active Ef theme, else produce `user-error'."
  (if-let* ((palette (modus-themes--palette (modus-themes--current-theme))))
      (symbol-value palette)
    (user-error "No enabled Modus theme could be found")))

(defun modus-themes-load-theme (theme)
  "Load THEME while disabling other Modus themes.
Run `'modus-themes-after-load-theme-hook'."
  (mapc #'disable-theme (modus-themes--list-known-themes))
  (load-theme theme :no-confirm)
  (run-hooks 'modus-themes-after-load-theme-hook))

;;;; Commands

(make-obsolete 'modus-themes-load-themes nil "4.0.0")
(make-obsolete 'modus-themes-load-operandi nil "4.0.0")
(make-obsolete 'modus-themes-load-vivendi nil "4.0.0")

(defvar modus-themes--select-theme-history nil
  "Minibuffer history of `modus-themes--select-prompt'.")

(defun modus-themes--select-prompt ()
  "Minibuffer prompt to select a Modus theme."
    (intern
     (completing-read
      "Select Modus theme: "
      (modus-themes--list-known-themes)
      nil t nil
      'modus-themes--select-theme-history)))

;;;###autoload
(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes.
Also runs `modus-themes-after-load-theme-hook' at its last stage."
  (interactive)
  (pcase (modus-themes--current-theme)
    ('modus-operandi (modus-themes-load-theme 'modus-vivendi))
    ('modus-vivendi (modus-themes-load-theme 'modus-operandi))
    (_ (modus-themes-load-theme (modus-themes--select-prompt)))))

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
  (let* ((properties (modus-themes--list-or-warn 'modus-themes-prompts))
         (background (memq 'background properties)))
    (list :background
          (if background bg 'unspecified)
          :foreground
          (if background 'unspecified fg)
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

(defun modus-themes--heading (level fg rainbow)
  "Conditional styles for `modus-themes-headings'.

LEVEL is the heading's position in their order.  FG is the
default text color.  RAINBOW is an accented, more saturated value
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
            rainbow)
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

(defun modus-themes--org-block (fg bg)
  "Conditionally set the FG and BG of Org blocks."
  (if (or (eq modus-themes-org-blocks 'gray-background)
          (eq modus-themes-org-blocks 'grayscale)
          (eq modus-themes-org-blocks 'greyscale))
      (list :background bg :foreground fg :extend t)
    (list :foreground fg)))

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

(defun modus-themes--link (fg underline)
  "Conditional application of link styles.
FG is the link's default color for its text and underline
property.  UNDERLINE is a gray color only for the undeline."
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
          :foreground
          (cond
           ((memq 'no-color properties)
            'unspecified)
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



;;;; Face specifications

(defconst modus-themes-faces
  '(
;;;; custom faces
    ;; these bespoke faces are inherited by other constructs below
;;;;; subtle colored backgrounds
    `(modus-themes-subtle-red ((,c :background ,bg-red-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-green ((,c :background ,bg-green-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-yellow ((,c :background ,bg-yellow-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-blue ((,c :background ,bg-blue-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-magenta ((,c :background ,bg-magenta-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-cyan ((,c :background ,bg-cyan-subtle :foreground ,fg-main)))
;;;;; intense colored backgrounds
    `(modus-themes-intense-red ((,c :background ,bg-red :foreground ,fg-main)))
    `(modus-themes-intense-green ((,c :background ,bg-green :foreground ,fg-main)))
    `(modus-themes-intense-yellow ((,c :background ,bg-yellow :foreground ,fg-main)))
    `(modus-themes-intense-blue ((,c :background ,bg-blue :foreground ,fg-main)))
    `(modus-themes-intense-magenta ((,c :background ,bg-magenta :foreground ,fg-main)))
    `(modus-themes-intense-cyan ((,c :background ,bg-cyan :foreground ,fg-main)))
;;;;; mark indicators
    ;; color combinations intended for Dired, Ibuffer, or equivalent
    `(modus-themes-mark-alt ((,c :inherit bold :background ,bg-yellow-subtle :foreground ,yellow)))
    `(modus-themes-mark-del ((,c :inherit bold :background ,bg-red-subtle :foreground ,red)))
    `(modus-themes-mark-sel ((,c :inherit bold :background ,bg-cyan-subtle :foreground ,cyan)))
    `(modus-themes-mark-symbol ((,c :inherit bold)))
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
    `(modus-themes-lang-error ((,c  :underline (:style wave :color ,underline-err))))
    `(modus-themes-lang-note ((,c  :underline (:style wave :color ,underline-note))))
    `(modus-themes-lang-warning ((,c  :underline (:style wave :color ,underline-warning))))
;;;;; links
    `(modus-themes-link-broken ((,c :inherit button ,@(modus-themes--link-color red red-faint))))
    `(modus-themes-link-symlink ((,c :inherit button ,@(modus-themes--link-color cyan cyan-faint))))
;;;;; markup
    `(modus-themes-prose-code ((,c :foreground ,prose-code)))
    `(modus-themes-prose-macro ((,c :foreground ,prose-macro)))
    `(modus-themes-prose-verbatim ((,c :foreground ,prose-verbatim)))
;;;;; search
    `(modus-themes-search-current ((,c :background ,search-current :foreground ,fg-main)))
    `(modus-themes-search-lazy ((,c :background ,search-lazy :foreground ,fg-main)))
;;;;; completion frameworks
    `(modus-themes-completion-match-0 ((,c :inherit bold :foreground ,accent-0)))
    `(modus-themes-completion-match-1 ((,c :inherit bold :foreground ,accent-1)))
    `(modus-themes-completion-match-2 ((,c :inherit bold :foreground ,accent-2)))
    `(modus-themes-completion-match-3 ((,c :inherit bold :foreground ,accent-3)))
    `(modus-themes-completion-selected ((,c :background ,bg-completion)))
;;;;; typography
    `(modus-themes-bold ((,c ,@(modus-themes--bold-weight))))
    `(modus-themes-fixed-pitch ((,c ,@(modus-themes--fixed-pitch))))
    `(modus-themes-slant ((,c ,@(modus-themes--slant))))
    `(modus-themes-ui-variable-pitch ((,c ,@(modus-themes--variable-pitch-ui))))
;;;;; other custom faces
    `(modus-themes-grue ((,c :foreground ,@(modus-themes--deuteran blue success))))
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
    `(fringe ((,c ,@(modus-themes--fringe bg-main bg-inactive bg-active) :foreground ,fg-main)))
    `(vertical-border ((,c :foreground ,border)))
;;;;; basic and/or ungrouped styles
    `(bold ((,c :weight bold)))
    `(bold-italic ((,c :inherit (bold italic))))
    `(underline ((,c :underline ,fg-dim)))
    `(buffer-menu-buffer ((,c :inherit bold)))
    `(child-frame-border ((,c :background ,border)))
    `(comint-highlight-input ((,c :inherit bold)))
    `(comint-highlight-prompt ((,c :inherit modus-themes-prompt)))
    `(confusingly-reordered ((,c :inherit modus-themes-lang-error)))
    `(edmacro-label ((,c :inherit bold :foreground ,accent-0)))
    `(elisp-shorthand-font-lock-face ((,c :inherit font-lock-variable-name-face)))
    `(error ((,c :inherit bold :foreground ,err)))
    `(escape-glyph ((,c :foreground ,err)))
    `(file-name-shadow ((,c :inherit shadow)))
    `(header-line ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-dim)))
    `(header-line-highlight ((,c :inherit highlight)))
    `(help-argument-name ((,c :inherit modus-themes-slant :foreground ,variable)))
    `(help-key-binding ((,c :inherit modus-themes-key-binding)))
    `(homoglyph ((,c :foreground ,warning)))
    `(ibuffer-locked-buffer ((,c :foreground ,warning)))
    `(icon-button ((,c :box ,fg-dim :background ,bg-active))) ; same as `custom-button'
    `(italic ((,c :slant italic)))
    `(nobreak-hyphen ((,c :foreground ,err)))
    `(nobreak-space ((,c :foreground ,err :underline t)))
    `(menu ((,c :inverse-video unspecified :background ,bg-active :foreground ,fg-main)))
    `(minibuffer-prompt ((,c :inherit modus-themes-prompt)))
    `(mm-command-output ((,c :foreground ,mail-4)))
    `(mm-uu-extract ((,c :foreground ,mail-4)))
    `(next-error ((,c :inherit modus-themes-subtle-red :extend t)))
    `(pgtk-im-0 ((,c :inherit modus-themes-intense-cyan)))
    `(read-multiple-choice-face ((,c :inherit (bold modus-themes-mark-alt))))
    `(rectangle-preview ((,c :inherit secondary-selection)))
    `(region ((,c ,@(modus-themes--region bg-region fg-main
                                          bg-active bg-region-accent
                                          bg-region-accent-subtle))))
    `(secondary-selection ((,c :background ,bg-hover-alt)))
    `(separator-line ((,c :underline ,bg-region)))
    `(shadow ((,c :foreground ,fg-dim)))
    `(success ((,c :inherit (bold modus-themes-grue))))
    `(trailing-whitespace ((,c :background ,bg-red)))
    `(warning ((,c :inherit bold :foreground ,warning)))
;;;;; buttons, links, widgets
    `(button ((,c ,@(modus-themes--link link border))))
    `(link ((,c :inherit button)))
    `(link-visited ((,c :inherit button ,@(modus-themes--link link-alt border))))
    `(tooltip ((,c :background ,bg-active)))
;;;;; agda2-mode
    `(agda2-highlight-bound-variable-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-catchall-clause-face ((,c :background ,bg-inactive)))
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
    `(agda2-highlight-number-face ((,c :inherit shadow)))
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
;;;;; all-the-icons
    `(all-the-icons-blue ((,c :foreground ,blue-cooler)))
    `(all-the-icons-blue-warmer ((,c :foreground ,blue-warmer)))
    `(all-the-icons-cyan ((,c :foreground ,cyan-intense)))
    `(all-the-icons-cyan-warmer ((,c :foreground ,cyan-warmer)))
    `(all-the-icons-dblue ((,c :foreground ,blue-faint)))
    `(all-the-icons-dcyan ((,c :foreground ,cyan-faint)))
    `(all-the-icons-dgreen ((,c :foreground ,green-faint)))
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
    `(all-the-icons-lgreen ((,c :foreground ,green)))
    `(all-the-icons-lmaroon ((,c :foreground ,magenta-warmer)))
    `(all-the-icons-lorange ((,c :foreground ,red-warmer)))
    `(all-the-icons-lpink ((,c :foreground ,magenta)))
    `(all-the-icons-lpurple ((,c :foreground ,magenta-faint)))
    `(all-the-icons-lred ((,c :foreground ,red)))
    `(all-the-icons-lyellow ((,c :foreground ,yellow-warmer)))
    `(all-the-icons-maroon ((,c :foreground ,magenta-intense)))
    `(all-the-icons-red ((,c :foreground ,red-intense)))
    `(all-the-icons-red-warmer ((,c :foreground ,red-cooler)))
    `(all-the-icons-yellow ((,c :foreground ,yellow-intense)))
;;;;; all-the-icons-dired
    `(all-the-icons-dired-dir-face ((,c :foreground ,cyan-faint)))
;;;;; all-the-icons-ibuffer
    `(all-the-icons-ibuffer-dir-face ((,c :foreground ,cyan-faint)))
    `(all-the-icons-ibuffer-file-face ((,c :foreground ,blue-faint)))
    `(all-the-icons-ibuffer-mode-face ((,c :foreground ,cyan)))
    `(all-the-icons-ibuffer-size-face ((,c :foreground ,cyan-cooler)))
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
    `(anzu-match-2 ((,c :inherit modus-themes-search-current)))
    `(anzu-match-3 ((,c :inherit modus-themes-subtle-yellow)))
    `(anzu-mode-line ((,c :inherit bold)))
    `(anzu-mode-line-no-match ((,c :inherit error)))
    `(anzu-replace-highlight ((,c :inherit modus-themes-intense-red :underline t)))
    `(anzu-replace-to ((,c :inherit modus-themes-search-current)))
;;;;; auctex and Tex
    `(font-latex-bold-face ((,c :inherit bold)))
    `(font-latex-doctex-documentation-face ((,c :inherit font-lock-doc-face)))
    `(font-latex-doctex-preprocessor-face ((,c :inherit font-lock-preprocessor-face)))
    `(font-latex-italic-face ((,c :inherit italic)))
    `(font-latex-math-face ((,c :inherit font-lock-constant-face)))
    `(font-latex-script-char-face ((,c :inherit font-lock-builtin-face)))
    `(font-latex-sectioning-5-face ((,c :inherit (bold modus-themes-variable-pitch) :foreground ,fg-alt)))
    `(font-latex-sedate-face ((,c :inherit font-lock-keyword-face)))
    `(font-latex-slide-title-face ((,c :inherit modus-themes-heading-1)))
    `(font-latex-string-face ((,c :inherit font-lock-string-face)))
    `(font-latex-subscript-face ((,c :height 0.95)))
    `(font-latex-superscript-face ((,c :height 0.95)))
    `(font-latex-underline-face ((,c :inherit underline)))
    `(font-latex-verbatim-face ((,c :inherit modus-themes-prose-verbatim)))
    `(font-latex-warning-face ((,c :inherit font-lock-warning-face)))
    `(tex-verbatim ((,c :inherit modus-themes-prose-verbatim)))
    ;; `(texinfo-heading ((,c :foreground ,magenta)))
    `(TeX-error-description-error ((,c :inherit error)))
    `(TeX-error-description-help ((,c :inherit success)))
    `(TeX-error-description-tex-said ((,c :inherit success)))
    `(TeX-error-description-warning ((,c :inherit warning)))
;;;;; auto-dim-other-buffers
    `(auto-dim-other-buffers-face ((,c :background ,bg-inactive)))
;;;;; avy
    `(avy-background-face ((,c :background ,bg-dim :foreground ,fg-dim :extend t)))
    `(avy-goto-char-timer-face ((,c :inherit (modus-themes-intense-neutral bold))))
    `(avy-lead-face ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-0)))
    `(avy-lead-face-0 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-1)))
    `(avy-lead-face-1 ((,c :inherit modus-themes-reset-soft :background ,bg-inactive)))
    `(avy-lead-face-2 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-2)))
;;;;; aw (ace-window)
    `(aw-background-face ((,c :foreground "gray50")))
    `(aw-key-face ((,c :inherit modus-themes-key-binding)))
    `(aw-leading-char-face ((,c :inherit (bold modus-themes-reset-soft) :height 1.5 :foreground ,red-intense)))
    `(aw-minibuffer-leading-char-face ((,c :inherit (modus-themes-intense-red bold))))
    `(aw-mode-line-face ((,c :inherit bold)))
;;;;; binder
    `(binder-sidebar-highlight ((,c :inherit modus-themes-subtle-cyan)))
    `(binder-sidebar-marked ((,c :inherit modus-themes-mark-sel)))
    `(binder-sidebar-missing ((,c :inherit modus-themes-subtle-red)))
    `(binder-sidebar-tags ((,c :foreground ,variable)))
;;;;; bongo
    `(bongo-album-title (( )))
    `(bongo-artist ((,c :foreground ,variable)))
    `(bongo-currently-playing-track ((,c :inherit bold)))
    `(bongo-elapsed-track-part ((,c :background ,bg-inactive :underline t)))
    `(bongo-filled-seek-bar ((,c :background ,bg-hover)))
    `(bongo-marked-track ((,c :inherit modus-themes-mark-alt)))
    `(bongo-marked-track-line ((,c :background ,bg-dim)))
    `(bongo-played-track ((,c :inherit shadow :strike-through t)))
    `(bongo-track-length ((,c :inherit shadow)))
    `(bongo-track-title ((,c :foreground ,builtin)))
    `(bongo-unfilled-seek-bar ((,c :background ,bg-dim)))
;;;;; boon
    `(boon-modeline-cmd ((,c :inherit modus-themes-intense-blue)))
    `(boon-modeline-ins ((,c :inherit modus-themes-intense-red)))
    `(boon-modeline-off ((,c :inherit modus-themes-intense-yellow)))
    `(boon-modeline-spc ((,c :inherit modus-themes-intense-green)))
;;;;; bookmark
    `(bookmark-face ((,c :inherit modus-themes-fringe-cyan)))
    `(bookmark-menu-bookmark ((,c :inherit bold)))
;;;;; calendar and diary
    `(calendar-month-header ((,c :inherit bold)))
    `(calendar-today ((,c :inherit bold :underline t)))
    `(calendar-weekday-header ((,c :foreground ,fg-alt)))
    `(calendar-weekend-header ((,c :inherit calendar-weekday-header)))
    `(diary ((,c :background ,bg-dim :foreground ,accent-0)))
    `(diary-anniversary ((,c :foreground ,accent-1)))
    `(diary-time ((,c :foreground ,date)))
    `(holiday ((,c :background ,bg-dim :foreground ,accent-2)))
;;;;; cfrs
    `(cfrs-border-color ((,c :background ,border)))
;;;;; change-log and log-view (`vc-print-log' and `vc-print-root-log')
    `(change-log-acknowledgment ((,c :foreground ,identifier)))
    `(change-log-conditionals ((,c :inherit error)))
    `(change-log-date ((,c :foreground ,date)))
    `(change-log-email ((,c :foreground ,fg-alt)))
    `(change-log-file ((,c :inherit bold)))
    `(change-log-function ((,c :inherit warning)))
    `(change-log-list ((,c :inherit bold)))
    `(change-log-name ((,c :foreground ,name)))
    `(log-edit-header ((,c :inherit bold)))
    `(log-edit-headers-separator ((,c :height 1 :background ,border :extend t)))
    `(log-edit-summary ((,c :inherit bold :foreground ,accent-0)))
    `(log-edit-unknown-header ((,c :inherit shadow)))
    `(log-view-commit-body (( )))
    `(log-view-file ((,c :inherit bold)))
    `(log-view-message ((,c :foreground ,identifier)))
;;;;; cider
    `(cider-deprecated-face ((,c :inherit warning)))
    `(cider-enlightened-face ((,c :box ,warning)))
    `(cider-enlightened-local-face ((,c :inherit warning)))
    `(cider-error-highlight-face ((,c :inherit modus-themes-lang-error)))
    `(cider-fringe-good-face ((,c :inherit modus-themes-grue)))
    `(cider-instrumented-face ((,c :box ,err)))
    `(cider-reader-conditional-face ((,c :inherit font-lock-type-face)))
    `(cider-repl-prompt-face ((,c :inherit minibuffer-prompt)))
    `(cider-repl-stderr-face ((,c :foreground ,err)))
    `(cider-repl-stdout-face (( )))
    `(cider-warning-highlight-face ((,c :inherit modus-themes-lang-warning)))
;;;;; circe (and lui)
    `(circe-fool-face ((,c :inherit shadow)))
    `(circe-highlight-nick-face ((,c :inherit error)))
    `(circe-prompt-face ((,c :inherit modus-themes-prompt)))
    `(circe-server-face ((,c :inherit shadow)))
    `(lui-button-face ((,c :inherit button)))
    `(lui-highlight-face ((,c :inherit error)))
    `(lui-time-stamp-face ((,c :foreground ,date)))
;;;;; citar
    `(citar ((,c :inherit shadow)))
    `(citar-highlight (( )))
;;;;; clojure-mode
    `(clojure-keyword-face ((,c :inherit font-lock-builtin-face)))
;;;;; column-enforce-mode
    `(column-enforce-face ((,c :inherit modus-themes-intense-yellow)))
;;;;; company-mode
    `(company-echo-common ((,c :inherit modus-themes-completion-match-0)))
    `(company-preview ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(company-preview-common ((,c :inherit company-echo-common)))
    `(company-preview-search ((,c :background ,bg-yellow)))
    `(company-scrollbar-bg ((,c :background ,bg-active)))
    `(company-scrollbar-fg ((,c :background ,fg-main)))
    `(company-template-field ((,c :background ,bg-active)))
    `(company-tooltip ((,c :background ,bg-inactive)))
    `(company-tooltip-annotation ((,c :inherit completions-annotations)))
    `(company-tooltip-common ((,c :inherit company-echo-common)))
    `(company-tooltip-deprecated ((,c :inherit company-tooltip :strike-through t)))
    `(company-tooltip-mouse ((,c :inherit highlight)))
    `(company-tooltip-scrollbar-thumb ((,c :background ,fg-alt)))
    `(company-tooltip-scrollbar-track ((,c :background ,bg-inactive)))
    `(company-tooltip-search ((,c :inherit secondary-selection)))
    `(company-tooltip-search-selection ((,c :inherit secondary-selection :underline t)))
    `(company-tooltip-selection ((,c :inherit modus-themes-completion-selected)))
;;;;; compilation
    `(compilation-column-number ((,c :inherit compilation-line-number)))
    `(compilation-error ((,c :inherit modus-themes-bold :foreground ,err)))
    `(compilation-info ((,c :inherit modus-themes-bold :foreground ,note)))
    `(compilation-line-number ((,c :inherit shadow)))
    `(compilation-mode-line-exit ((,c :inherit bold)))
    `(compilation-mode-line-fail ((,c :inherit modus-themes-bold :foreground ,err)))
    `(compilation-mode-line-run ((,c :inherit modus-themes-bold :foreground ,note)))
    `(compilation-warning ((,c :inherit modus-themes-bold :foreground ,warning)))
;;;;; completions
    `(completions-annotations ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(completions-common-part ((,c :inherit modus-themes-completion-match-0)))
    `(completions-first-difference ((,c :inherit modus-themes-completion-match-1)))
;;;;; consult
    `(consult-async-split ((,c :inherit error)))
    `(consult-key ((,c :inherit modus-themes-key-binding)))
    `(consult-imenu-prefix ((,c :inherit shadow)))
    `(consult-line-number ((,c :inherit shadow)))
;;;;; corfu
    `(corfu-current ((,c :inherit modus-themes-completion-selected)))
    `(corfu-bar ((,c :background ,fg-dim)))
    `(corfu-border ((,c :background ,bg-active)))
    `(corfu-default ((,c :background ,bg-dim)))
;;;;; corfu-quick
    `(corfu-quick1 ((,c :inherit bold :background ,bg-char-0)))
    `(corfu-quick2 ((,c :inherit bold :background ,bg-char-1)))
;;;;; counsel
    `(counsel-active-mode ((,c :foreground ,keyword)))
    `(counsel-application-name ((,c :foreground ,name)))
    `(counsel-key-binding ((,c :inherit modus-themes-key-binding)))
    `(counsel-outline-default ((,c :foreground ,fg-main)))
    `(counsel-variable-documentation ((,c :inherit font-lock-doc-face)))
;;;;; cperl-mode
    `(cperl-nonoverridable-face ((,c :foreground unspecified)))
    `(cperl-array-face ((,c :inherit font-lock-keyword-face)))
    `(cperl-hash-face ((,c :inherit font-lock-variable-name-face)))
;;;;; crontab-mode
    ;; TODO 2022-11-29: Review crontab-mode faces
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
    `(ctrlf-highlight-active ((,c :inherit modus-themes-search-current)))
    `(ctrlf-highlight-line ((,c :inherit highlight)))
    `(ctrlf-highlight-passive ((,c :inherit modus-themes-search-lazy)))
;;;;; custom (M-x customize)
    `(custom-button ((,c :box ,fg-dim :background ,bg-active :foreground ,fg-main)))
    `(custom-button-mouse ((,c :inherit (highlight custom-button))))
    `(custom-button-pressed ((,c :inherit (secondary-selection custom-button))))
    `(custom-changed ((,c :background ,bg-changed)))
    `(custom-comment ((,c :inherit shadow)))
    `(custom-comment-tag ((,c :inherit (bold shadow))))
    `(custom-invalid ((,c :inherit error :strike-through t)))
    `(custom-modified ((,c :inherit custom-changed)))
    `(custom-rogue ((,c :inherit custom-invalid)))
    `(custom-set ((,c :inherit success)))
    `(custom-state ((,c :foreground ,fg-alt)))
    `(custom-themed ((,c :inherit custom-changed)))
    `(custom-variable-obsolete ((,c :inherit shadow)))
    `(custom-face-tag ((,c :inherit bold :foreground ,type)))
    `(custom-group-tag ((,c :inherit bold :foreground ,builtin)))
    `(custom-group-tag-1 ((,c :inherit bold :foreground ,constant)))
    `(custom-variable-tag ((,c :inherit bold :foreground ,variable)))
;;;;; deadgrep
    `(deadgrep-filename-face ((,c :inherit bold :foreground ,name)))
    `(deadgrep-match-face ((,c :inherit match)))
    `(deadgrep-meta-face ((,c :inherit shadow)))
    `(deadgrep-regexp-metachar-face ((,c :inherit font-lock-regexp-grouping-construct)))
    `(deadgrep-search-term-face ((,c :inherit success)))
;;;;; deft
    `(deft-filter-string-face ((,c :inherit success)))
    `(deft-header-face ((,c :inherit shadow)))
    `(deft-separator-face ((,c :foreground "gray50")))
    `(deft-summary-face ((,c :inherit (shadow modus-themes-slant))))
    `(deft-time-face ((,c :foreground ,date)))
    `(deft-title-face ((,c :inherit bold)))
;;;;; denote
    `(denote-faces-date ((,c :foreground ,date)))
    `(denote-faces-keywords ((,c :foreground ,name)))
;;;;; devdocs
    `(devdocs-code-block ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))
;;;;; dictionary
    `(dictionary-button-face ((,c :inherit bold)))
    `(dictionary-reference-face ((,c :inherit link)))
    `(dictionary-word-definition-face (( )))
    `(dictionary-word-entry-face ((,c :inherit font-lock-comment-face)))
                                        ;';;; diff-hl
    `(diff-hl-change ((,c :background ,bg-changed-intense)))
    `(diff-hl-delete ((,c :background ,bg-removed-intense)))
    `(diff-hl-insert ((,c :background ,@(modus-themes--deuteran bg-added-intense-deuteran bg-added-intense))))
    `(diff-hl-reverted-hunk-highlight ((,c :background ,fg-main :foreground ,bg-main)))
;;;;; diff-mode
    `(diff-added ((,c :background ,@(modus-themes--deuteran bg-added-deuteran bg-added))))
    `(diff-changed ((,c :background ,bg-changed :extend t)))
    `(diff-changed-unspecified ((,c :inherit diff-changed)))
    `(diff-removed ((,c :background ,bg-removed)))
    `(diff-refine-added ((,c :background ,@(modus-themes--deuteran bg-added-refine-deuteran bg-added-refine))))
    `(diff-refine-changed ((,c :background ,bg-changed-refine)))
    `(diff-refine-removed ((,c :background ,bg-removed-refine)))
    `(diff-indicator-added ((,c :inherit (success diff-added))))
    `(diff-indicator-changed ((,c :inherit (warning diff-changed))))
    `(diff-indicator-removed ((,c :inherit (error diff-removed))))
    `(diff-context (( )))
    `(diff-error ((,c :inherit error)))
    `(diff-file-header ((,c :inherit bold)))
    `(diff-function ((,c :background ,bg-inactive)))
    `(diff-header (( )))
    `(diff-hunk-header ((,c :inherit bold :background ,bg-inactive)))
    `(diff-index ((,c :inherit italic)))
    `(diff-nonexistent ((,c :inherit bold)))
;;;;; dim-autoload
    `(dim-autoload-cookie-line ((,c :inherit font-lock-comment-face)))
;;;;; dired
    `(dired-broken-symlink ((,c :inherit modus-themes-link-broken)))
    `(dired-directory ((,c :foreground ,accent-0)))
    `(dired-flagged ((,c :inherit modus-themes-mark-del)))
    `(dired-header ((,c :inherit bold)))
    `(dired-ignored ((,c :inherit shadow)))
    `(dired-mark ((,c :inherit bold)))
    `(dired-marked ((,c :inherit modus-themes-mark-sel)))
    `(dired-perm-write ((,c :inherit shadow)))
    `(dired-symlink ((,c :inherit modus-themes-link-symlink)))
    `(dired-warning ((,c :inherit warning)))
;;;;; dired-async
    `(dired-async-failures ((,c :inherit error)))
    `(dired-async-message ((,c :inherit bold)))
    `(dired-async-mode-message ((,c :inherit bold)))
;;;;; dired-git
    `(dired-git-branch-else ((,c :inherit bold :foreground ,accent-0)))
    `(dired-git-branch-master ((,c :inherit bold :foreground ,accent-1)))
;;;;; dired-git-info
    `(dgi-commit-message-face ((,c :foreground ,docstring)))
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
    `(diredfl-autofile-name ((,c :background ,bg-inactive)))
    `(diredfl-compressed-file-name ((,c :foreground ,warning)))
    `(diredfl-compressed-file-suffix ((,c :foreground ,err)))
    `(diredfl-date-time ((,c :foreground ,date)))
    `(diredfl-deletion ((,c :inherit dired-flagged)))
    `(diredfl-deletion-file-name ((,c :inherit diredfl-deletion)))
    `(diredfl-dir-heading ((,c :inherit bold)))
    `(diredfl-dir-name ((,c :inherit dired-directory)))
    `(diredfl-dir-priv ((,c :inherit dired-directory)))
    `(diredfl-exec-priv ((,c :foreground ,accent-1)))
    `(diredfl-executable-tag ((,c :inherit diredfl-exec-priv)))
    `(diredfl-file-name ((,c :foreground ,fg-main)))
    `(diredfl-file-suffix ((,c :foreground ,variable)))
    `(diredfl-flag-mark ((,c :inherit dired-marked)))
    `(diredfl-flag-mark-line ((,c :inherit dired-marked)))
    `(diredfl-ignored-file-name ((,c :inherit shadow)))
    `(diredfl-link-priv ((,c :foreground ,link)))
    `(diredfl-no-priv ((,c :inherit shadow)))
    `(diredfl-number ((,c :inherit shadow)))
    `(diredfl-other-priv ((,c :foreground ,accent-2)))
    `(diredfl-rare-priv ((,c :foreground ,accent-3)))
    `(diredfl-read-priv ((,c :foreground ,fg-main)))
    `(diredfl-symlink ((,c :inherit dired-symlink)))
    `(diredfl-tagged-autofile-name ((,c :inherit (diredfl-autofile-name dired-marked))))
    `(diredfl-write-priv ((,c :foreground ,accent-0)))
;;;;; display-fill-column-indicator-mode
    `(fill-column-indicator ((,c :height 1 :background ,bg-region :foreground ,bg-region)))
;;;;; doom-modeline
    `(doom-modeline-bar ((,c :background ,blue)))
    `(doom-modeline-bar-inactive ((,c :background "gray50")))
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
;;;;; ediff
    `(ediff-current-diff-A ((,c :inherit diff-removed)))
    `(ediff-current-diff-Ancestor ((,c :background ,bg-region))) ; TODO 2022-11-29: Needs review
    `(ediff-current-diff-B ((,c :inherit diff-added)))
    `(ediff-current-diff-C ((,c :inherit diff-changed)))
    `(ediff-even-diff-A ((,c :background ,bg-dim)))
    `(ediff-even-diff-Ancestor ((,c :background ,bg-dim)))
    `(ediff-even-diff-B ((,c :background ,bg-dim)))
    `(ediff-even-diff-C ((,c :background ,bg-dim)))
    `(ediff-fine-diff-A ((,c :inherit diff-refine-removed)))
    `(ediff-fine-diff-Ancestor ((,c :inherit diff-refine-cyan)))
    `(ediff-fine-diff-B ((,c :inherit diff-refine-added)))
    `(ediff-fine-diff-C ((,c :inherit diff-refine-changed)))
    `(ediff-odd-diff-A ((,c :inherit ediff-even-diff-A)))
    `(ediff-odd-diff-Ancestor ((,c :inherit ediff-even-diff-Ancestor)))
    `(ediff-odd-diff-B ((,c :inherit ediff-even-diff-B)))
    `(ediff-odd-diff-C ((,c :inherit ediff-even-diff-C)))
;;;;; ein (Emacs IPython Notebook)
    `(ein:basecell-input-area-face ((,c :background ,bg-dim :extend t)))
    `(ein:cell-output-area (( )))
    `(ein:cell-output-area-error ((,c :background ,bg-removed :extend t)))
    `(ein:cell-output-stderr ((,c :background ,bg-removed :extend t)))
    `(ein:markdowncell-input-area-face (( )))
    `(ein:notification-tab-normal ((,c :underline t)))
;;;;; eglot
    `(eglot-mode-line ((,c :inherit modus-themes-bold :foreground ,fnname)))
;;;;; el-search
    `(el-search-highlight-in-prompt-face ((,c :inherit italic)))
    `(el-search-match ((,c :inherit modus-themes-search-current)))
    `(el-search-other-match ((,c :inherit modus-themes-search-lazy)))
    `(el-search-occur-match ((,c :inherit match)))
;;;;; eldoc
    ;; NOTE: see https://github.com/purcell/package-lint/issues/187
    (list 'eldoc-highlight-function-argument `((,c :inherit modus-themes-mark-alt)))
;;;;; eldoc-box
    `(eldoc-box-body ((,c :background ,bg-dim :foreground ,fg-main)))
    `(eldoc-box-border ((,c :background ,border)))
;;;;; elfeed
    `(elfeed-log-date-face ((,c :inherit elfeed-search-date-face)))
    `(elfeed-log-debug-level-face ((,c :inherit elfeed-search-filter-face)))
    `(elfeed-log-error-level-face ((,c :inherit error)))
    `(elfeed-log-info-level-face ((,c :inherit success)))
    `(elfeed-log-warn-level-face ((,c :inherit warning)))
    `(elfeed-search-date-face ((,c :foreground ,date)))
    `(elfeed-search-feed-face ((,c :foreground ,accent-1)))
    `(elfeed-search-filter-face ((,c :inherit bold)))
    `(elfeed-search-last-update-face ((,c :inherit bold :foreground ,date)))
    `(elfeed-search-tag-face ((,c :foreground ,accent-0)))
    `(elfeed-search-title-face ((,c :foreground ,fg-dim)))
    `(elfeed-search-unread-count-face (( )))
    `(elfeed-search-unread-title-face ((,c :inherit bold :foreground ,fg-main)))
;;;;; elfeed-score
    `(elfeed-score-date-face ((,c :foreground ,date)))
    `(elfeed-score-debug-level-face ((,c :inherit bold)))
    `(elfeed-score-error-level-face ((,c :inherit error)))
    `(elfeed-score-info-level-face ((,c  :inherit success)))
    `(elfeed-score-warn-level-face ((,c :inherit warning)))
;;;;; elpher
    `(elpher-gemini-heading1 ((,c :inherit modus-themes-heading-1)))
    `(elpher-gemini-heading2 ((,c :inherit modus-themes-heading-2)))
    `(elpher-gemini-heading3 ((,c :inherit modus-themes-heading-3)))
;;;;; embark
    `(embark-keybinding ((,c :inherit modus-themes-key-binding)))
    `(embark-collect-marked ((,c :inherit modus-themes-mark-sel)))
;;;;; ement (ement.el)
    `(ement-room-fully-read-marker ((,c :inherit success)))
    `(ement-room-membership ((,c :inherit shadow)))
    `(ement-room-mention ((,c :inherit highlight)))
    `(ement-room-name ((,c :inherit bold)))
    `(ement-room-reactions ((,c :inherit shadow)))
    `(ement-room-read-receipt-marker ((,c :inherit match)))
    `(ement-room-self ((,c :inherit bold :foreground ,accent-1)))
    `(ement-room-self-message ((,c :foreground ,fg-alt)))
    `(ement-room-timestamp ((,c :inherit shadow)))
    `(ement-room-timestamp-header ((,c :inherit bold :foreground ,date)))
    `(ement-room-user ((,c :inherit bold :foreground ,accent-0)))
;;;;; emms
    ;; TODO 2022-11-29: Review EMMS faces
    `(emms-browser-album-face ((,c :foreground ,rainbow-1)))
    `(emms-browser-artist-face ((,c :foreground ,rainbow-2)))
    `(emms-browser-composer-face ((,c :foreground ,rainbow-3)))
    `(emms-browser-performer-face ((,c :inherit emms-browser-artist-face)))
    `(emms-browser-track-face ((,c :inherit emms-playlist-track-face)))
    `(emms-browser-year/genre-face ((,c :foreground ,rainbow-4)))
    `(emms-playlist-track-face ((,c :foreground ,accent-0)))
    `(emms-playlist-selected-face ((,c :inherit bold :foreground ,accent-0)))
    `(emms-metaplaylist-mode-current-face ((,c :inherit emms-playlist-selected-face)))
    `(emms-metaplaylist-mode-face ((,c :foreground ,accent-1)))
;;;;; enh-ruby-mode (enhanced-ruby-mode)
    `(enh-ruby-heredoc-delimiter-face ((,c :inherit font-lock-constant-face)))
    `(enh-ruby-op-face ((,c :foreground ,fg-main)))
    `(enh-ruby-regexp-delimiter-face ((,c :inherit font-lock-regexp-grouping-construct)))
    `(enh-ruby-regexp-face ((,c :inherit font-lock-string-face)))
    `(enh-ruby-string-delimiter-face ((,c :inherit font-lock-string-face)))
    `(erm-syn-errline ((,c :inherit modus-themes-lang-error)))
    `(erm-syn-warnline ((,c :inherit modus-themes-lang-warning)))
;;;;; epa
    `(epa-field-body (( )))
    `(epa-field-name ((,c :inherit bold :foreground ,fg-dim)))
    `(epa-mark ((,c :inherit bold)))
    `(epa-string ((,c :foreground ,string)))
    `(epa-validity-disabled ((,c :foreground ,err)))
    `(epa-validity-high ((,c :inherit success)))
    `(epa-validity-low ((,c :inherit shadow)))
    `(epa-validity-medium ((,c :foreground ,note)))
;;;;; erc
    `(erc-action-face ((,c :foreground ,accent-2)))
    `(erc-bold-face ((,c :inherit bold)))
    `(erc-button ((,c :inherit button)))
    `(erc-command-indicator-face ((,c :inherit bold :foreground ,accent-3)))
    `(erc-current-nick-face ((,c :inherit match)))
    `(erc-dangerous-host-face ((,c :inherit modus-themes-intense-red)))
    `(erc-direct-msg-face ((,c :inherit shadow)))
    `(erc-error-face ((,c :inherit error)))
    `(erc-fool-face ((,c :inherit shadow)))
    `(erc-input-face ((,c :foreground ,fnname)))
    `(erc-inverse-face ((,c :inherit erc-default-face :inverse-video t)))
    `(erc-keyword-face ((,c :inherit bold :foreground ,keyword)))
    `(erc-my-nick-face ((,c :inherit bold :foreground ,name)))
    `(erc-my-nick-prefix-face ((,c :inherit erc-my-nick-face)))
    `(erc-nick-default-face ((,c :inherit bold :foreground ,accent-0)))
    `(erc-nick-msg-face ((,c :inherit warning)))
    `(erc-nick-prefix-face ((,c :inherit erc-nick-default-face)))
    `(erc-notice-face ((,c :inherit font-lock-comment-face)))
    `(erc-pal-face ((,c :inherit bold :foreground ,accent-1)))
    `(erc-prompt-face ((,c :inherit modus-themes-prompt)))
    `(erc-timestamp-face ((,c :foreground ,date)))
    `(erc-underline-face ((,c :underline t)))
;;;;; ert
    `(ert-test-result-expected ((,c :inherit modus-themes-intense-cyan)))
    `(ert-test-result-unexpected ((,c :inherit modus-themes-intense-red)))
;;;;; eshell
    `(eshell-ls-archive ((,c :foreground ,accent-2)))
    `(eshell-ls-backup ((,c :inherit shadow)))
    `(eshell-ls-clutter ((,c :inherit shadow)))
    `(eshell-ls-directory ((,c :foreground ,accent-0)))
    `(eshell-ls-executable ((,c :foreground ,accent-1)))
    `(eshell-ls-missing ((,c :inherit error)))
    `(eshell-ls-product ((,c :inherit shadow)))
    `(eshell-ls-readonly ((,c :foreground ,warning)))
    `(eshell-ls-special ((,c :foreground ,accent-3)))
    `(eshell-ls-symlink ((,c :inherit link)))
    `(eshell-ls-unreadable ((,c :inherit shadow)))
    `(eshell-prompt ((,c :inherit modus-themes-prompt)))
;;;;; eshell-fringe-status
    `(eshell-fringe-status-failure ((,c :inherit error)))
    `(eshell-fringe-status-success ((,c :inherit success)))
;;;;; evil-mode
    `(evil-ex-commands ((,c :inherit font-lock-keyword-face)))
    `(evil-ex-info ((,c :inherit font-lock-type-face)))
    `(evil-ex-lazy-highlight ((,c :inherit modus-themes-search-lazy)))
    `(evil-ex-search ((,c :inherit modus-themes-search-current)))
    `(evil-ex-substitute-matches ((,c :inherit modus-themes-intense-yellow :underline t)))
    `(evil-ex-substitute-replacement ((,c :inherit modus-themes-search-current)))
;;;;; eww
    `(eww-invalid-certificate ((,c :foreground ,err)))
    `(eww-valid-certificate ((,c :foreground ,note)))
    `(eww-form-checkbox ((,c :inherit eww-form-text)))
    `(eww-form-file ((,c :inherit eww-form-submit)))
    `(eww-form-select ((,c :inherit eww-form-submit)))
    `(eww-form-submit ((,c :box ,fg-dim :background ,bg-active :foreground ,fg-main)))
    `(eww-form-text ((,c :inherit widget-field)))
    `(eww-form-textarea ((,c :inherit eww-form-text)))
;;;;; eyebrowse
    `(eyebrowse-mode-line-active ((,c :inherit bold :foreground ,note)))
;;;;; flycheck
    `(flycheck-error ((,c :inherit modus-themes-lang-error)))
    `(flycheck-fringe-error ((,c :inherit modus-themes-intense-red)))
    `(flycheck-fringe-info ((,c :inherit modus-themes-intense-cyan)))
    `(flycheck-fringe-warning ((,c :inherit modus-themes-intense-yellow)))
    `(flycheck-info ((,c :inherit modus-themes-lang-note)))
    `(flycheck-warning ((,c :inherit modus-themes-lang-warning)))
;;;;; flycheck-color-mode-line
    `(flycheck-color-mode-line-error-face ((,c :inherit flycheck-fringe-error)))
    `(flycheck-color-mode-line-info-face ((,c :inherit flycheck-fringe-info)))
    `(flycheck-color-mode-line-running-face ((,c :inherit italic)))
    `(flycheck-color-mode-line-info-face ((,c :inherit flycheck-fringe-warning)))
;;;;; flycheck-indicator
    `(flycheck-indicator-disabled ((,c :inherit modus-themes-slant :foreground ,fg-dim)))
    `(flycheck-indicator-error ((,c :inherit error)))
    `(flycheck-indicator-info ((,c :inherit bold)))
    `(flycheck-indicator-running ((,c :inherit modus-themes-slant)))
    `(flycheck-indicator-success ((,c :inherit success)))
    `(flycheck-indicator-warning ((,c :inherit warning)))
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
    `(freeze-it-show ((,c :inherit shadow)))
;;;;; focus
    `(focus-unfocused ((,c :foreground "gray50")))
;;;;; fold-this
    `(fold-this-overlay ((,c :background ,bg-inactive)))
;;;;; font-lock
    `(font-lock-builtin-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(font-lock-comment-delimiter-face ((,c :inherit font-lock-comment-face)))
    `(font-lock-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(font-lock-constant-face ((,c :foreground ,constant)))
    `(font-lock-doc-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(font-lock-function-name-face ((,c :foreground ,fnname)))
    `(font-lock-keyword-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(font-lock-negation-char-face ((,c :inherit error)))
    `(font-lock-preprocessor-face ((,c :foreground ,preprocessor)))
    `(font-lock-regexp-grouping-backslash ((,c :inherit modus-themes-bold :foreground ,rx-backslash)))
    `(font-lock-regexp-grouping-construct ((,c :inherit modus-themes-bold :foreground ,rx-construct)))
    `(font-lock-string-face ((,c :foreground ,string)))
    `(font-lock-type-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(font-lock-variable-name-face ((,c :foreground ,variable)))
    `(font-lock-warning-face ((,c :inherit modus-themes-bold :foreground ,warning)))
;;;;; forge
    `(forge-post-author ((,c :inherit bold :foreground ,name)))
    `(forge-post-date ((,c :foreground ,date)))
    `(forge-topic-closed ((,c :inherit shadow)))
    `(forge-topic-merged ((,c :inherit shadow)))
    `(forge-topic-open ((,c :foreground ,note)))
    `(forge-topic-unmerged ((,c :inherit modus-themes-slant :foreground ,warning)))
    `(forge-topic-unread ((,c :inherit bold :foreground ,fg-main)))
;;;;; geiser
    `(geiser-font-lock-autodoc-current-arg `((,c :inherit modus-themes-mark-alt)))
    `(geiser-font-lock-autodoc-identifier ((,c :foreground ,docstring)))
    `(geiser-font-lock-doc-button ((,c :inherit button)))
    `(geiser-font-lock-doc-link ((,c :inherit button)))
    `(geiser-font-lock-error-link ((,c :inherit button :foreground ,err)))
    `(geiser-font-lock-image-button ((,c :inherit button :foreground ,note)))
    `(geiser-font-lock-repl-input ((,c :inherit bold)))
    `(geiser-font-lock-repl-output ((,c :inherit font-lock-keyword-face)))
    `(geiser-font-lock-repl-prompt ((,c :inherit modus-themes-prompt)))
    `(geiser-font-lock-xref-header ((,c :inherit bold)))
    `(geiser-font-lock-xref-link ((,c :inherit button)))
;;;;; git-commit
    `(git-commit-comment-action ((,c :inherit font-lock-comment-face)))
    `(git-commit-comment-branch-local ((,c :inherit font-lock-comment-face :foreground ,accent-0)))
    `(git-commit-comment-branch-remote ((,c :inherit font-lock-comment-face :foreground ,accent-1)))
    `(git-commit-comment-heading ((,c :inherit (bold font-lock-comment-face))))
    `(git-commit-comment-file ((,c :inherit font-lock-comment-face :foreground ,name)))
    `(git-commit-keyword ((,c :foreground ,keyword)))
    `(git-commit-nonempty-second-line ((,c :inherit error)))
    `(git-commit-overlong-summary ((,c :inherit warning)))
    `(git-commit-summary ((,c :inherit bold :foreground ,note)))
;;;;; git-gutter
    `(git-gutter:added ((,c :background ,@(modus-themes--deuteran bg-added-intense-deuteran bg-added-intense))))
    `(git-gutter:deleted ((,c :inherit modus-themes-intense-red)))
    `(git-gutter:modified ((,c :inherit modus-themes-intense-yellow)))
    `(git-gutter:separator ((,c :inherit modus-themes-intense-cyan)))
    `(git-gutter:unchanged ((,c :inherit modus-themes-intense-magenta)))
;;;;; git-gutter-fr
    `(git-gutter-fr:added ((,c :background ,@(modus-themes--deuteran bg-added-intense-deuteran bg-added-intense))))
    `(git-gutter-fr:deleted ((,c :inherit modus-themes-intense-red)))
    `(git-gutter-fr:modified ((,c :inherit modus-themes-intense-yellow)))
;;;;; git-rebase
    `(git-rebase-comment-hash ((,c :inherit (bold font-lock-comment-face) :foreground ,identifier)))
    `(git-rebase-comment-heading  ((,c :inherit (bold font-lock-comment-face))))
    `(git-rebase-description ((,c :foreground ,fg-main)))
    `(git-rebase-hash ((,c :foreground ,identifier)))
;;;;; git-timemachine
    `(git-timemachine-commit ((,c :inherit warning)))
    `(git-timemachine-minibuffer-author-face ((,c :foreground ,name)))
    `(git-timemachine-minibuffer-detail-face ((,c :foreground ,fg-main)))
;;;;; gnus
    `(gnus-button ((,c :inherit button)))
    `(gnus-cite-1 ((,c :inherit message-cited-text-1)))
    `(gnus-cite-2 ((,c :inherit message-cited-text-2)))
    `(gnus-cite-3 ((,c :inherit message-cited-text-3)))
    `(gnus-cite-4 ((,c :inherit message-cited-text-4)))
    `(gnus-cite-5 ((,c :inherit message-cited-text-1)))
    `(gnus-cite-6 ((,c :inherit message-cited-text-2)))
    `(gnus-cite-7 ((,c :inherit message-cited-text-3)))
    `(gnus-cite-8 ((,c :inherit message-cited-text-4)))
    `(gnus-cite-9 ((,c :inherit message-cited-text-1)))
    `(gnus-cite-10 ((,c :inherit message-cited-text-2)))
    `(gnus-cite-11 ((,c :inherit message-cited-text-3)))
    `(gnus-cite-attribution ((,c :inherit italic)))
    `(gnus-emphasis-bold ((,c :inherit bold)))
    `(gnus-emphasis-bold-italic ((,c :inherit bold-italic)))
    `(gnus-emphasis-highlight-words ((,c :inherit warning)))
    `(gnus-emphasis-italic ((,c :inherit italic)))
    `(gnus-emphasis-underline-bold ((,c :inherit gnus-emphasis-bold :underline t)))
    `(gnus-emphasis-underline-bold-italic ((,c :inherit gnus-emphasis-bold-italic :underline t)))
    `(gnus-emphasis-underline-italic ((,c :inherit gnus-emphasis-italic :underline t)))
    `(gnus-group-mail-1 ((,c :inherit bold :foreground ,rainbow-1)))
    `(gnus-group-mail-1-empty ((,c :foreground ,rainbow-1)))
    `(gnus-group-mail-2 ((,c :inherit bold :foreground ,rainbow-2)))
    `(gnus-group-mail-2-empty ((,c :foreground ,rainbow-2)))
    `(gnus-group-mail-3 ((,c :inherit bold :foreground ,rainbow-3)))
    `(gnus-group-mail-3-empty ((,c :foreground ,rainbow-3)))
    `(gnus-group-mail-low ((,c :inherit bold :foreground ,fg-alt)))
    `(gnus-group-mail-low-empty ((,c :foreground ,fg-alt)))
    `(gnus-group-news-1 ((,c :inherit bold :foreground ,rainbow-1)))
    `(gnus-group-news-1-empty ((,c :foreground ,rainbow-1)))
    `(gnus-group-news-2 ((,c :inherit bold :foreground ,rainbow-2)))
    `(gnus-group-news-2-empty ((,c :foreground ,rainbow-2)))
    `(gnus-group-news-3 ((,c :inherit bold :foreground ,rainbow-3)))
    `(gnus-group-news-3-empty ((,c :foreground ,rainbow-3)))
    `(gnus-group-news-4 ((,c :inherit bold :foreground ,rainbow-4)))
    `(gnus-group-news-4-empty ((,c :foreground ,rainbow-4)))
    `(gnus-group-news-5 ((,c :inherit bold :foreground ,rainbow-5)))
    `(gnus-group-news-5-empty ((,c :foreground ,rainbow-5)))
    `(gnus-group-news-6 ((,c :inherit bold :foreground ,rainbow-6)))
    `(gnus-group-news-6-empty ((,c :foreground ,rainbow-6)))
    `(gnus-group-news-low ((,c :inherit bold :foreground ,fg-alt)))
    `(gnus-group-news-low-empty ((,c :foreground ,fg-alt)))
    `(gnus-header-content ((,c :inherit message-header-other)))
    `(gnus-header-from ((,c :inherit message-header-to :underline nil)))
    `(gnus-header-name ((,c :inherit message-header-name)))
    `(gnus-header-newsgroups ((,c :inherit message-header-newsgroups)))
    `(gnus-header-subject ((,c :inherit message-header-subject)))
    `(gnus-server-agent ((,c :inherit bold)))
    `(gnus-server-closed ((,c :inherit italic)))
    `(gnus-server-cloud ((,c :inherit bold :foreground ,fg-alt)))
    `(gnus-server-cloud-host ((,c :inherit bold :foreground ,fg-alt :underline t)))
    `(gnus-server-denied ((,c :inherit error)))
    `(gnus-server-offline ((,c :inherit shadow)))
    `(gnus-server-opened ((,c :inherit success)))
    `(gnus-summary-cancelled ((,c :inherit italic :foreground ,warning)))
    `(gnus-summary-high-ancient ((,c :inherit bold :foreground ,fg-alt)))
    `(gnus-summary-high-read ((,c :inherit bold :foreground ,fg-dim)))
    `(gnus-summary-high-ticked ((,c :inherit bold :foreground ,err)))
    `(gnus-summary-high-undownloaded ((,c :inherit bold-italic :foreground ,warning)))
    `(gnus-summary-high-unread ((,c :inherit bold)))
    `(gnus-summary-low-ancient ((,c :inherit italic)))
    `(gnus-summary-low-read ((,c :inherit (shadow italic))))
    `(gnus-summary-low-ticked ((,c :inherit italic :foreground ,err)))
    `(gnus-summary-low-undownloaded ((,c :inherit italic :foreground ,warning)))
    `(gnus-summary-low-unread ((,c :inherit italic)))
    `(gnus-summary-normal-ancient (( )))
    `(gnus-summary-normal-read ((,c :inherit shadow)))
    `(gnus-summary-normal-ticked ((,c :foreground ,err)))
    `(gnus-summary-normal-undownloaded ((,c :foreground ,warning)))
    `(gnus-summary-normal-unread (( )))
    `(gnus-summary-selected ((,c :inherit highlight)))
;;;;; gotest
    `(go-test--ok-face ((,c :inherit success)))
    `(go-test--error-face ((,c :inherit error)))
    `(go-test--warning-face ((,c :inherit warning)))
    `(go-test--pointer-face ((,c :foreground ,accent-0)))
    `(go-test--standard-face (( )))
;;;;; golden-ratio-scroll-screen
    `(golden-ratio-scroll-highlight-line-face ((,c :background ,bg-cyan-subtle :foreground ,fg-main)))
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
    `(hi-black-hb ((,c :background ,bg-main :foreground ,fg-dim :inverse-video t)))
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
    `(highlight ((,c :background ,bg-hover)))
    `(highlight-changes ((,c :foreground ,warning :underline nil)))
    `(highlight-changes-delete ((,c :foreground ,err :underline t)))
    `(hl-line ((,c :background ,bg-hl-line :extend t)))
;;;;; highlight-numbers
    `(highlight-numbers-number ((,c :foreground ,constant)))
;;;;; highlight-thing
    `(highlight-thing ((,c :inherit match)))
;;;;; hl-defined
    `(hdefd-functions ((,c :foreground ,note)))
    `(hdefd-undefined ((,c :foreground ,err)))
    `(hdefd-variables ((,c :foreground ,variable)))
;;;;; hl-fill-column
    `(hl-fill-column-face ((,c :background ,bg-active)))
;;;;; hl-todo
    `(hl-todo ((,c :inherit (bold font-lock-comment-face) :foreground ,err)))
;;;;; hydra
    `(hydra-face-amaranth ((,c :inherit bold :foreground ,yellow-warmer)))
    `(hydra-face-blue ((,c :inherit bold :foreground ,blue)))
    `(hydra-face-pink ((,c :inherit bold :foreground ,magenta)))
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
    `(ido-indicator ((,c :inherit bold)))
    `(ido-only-match ((,c :inherit ido-first-match)))
    `(ido-subdir ((,c :foreground ,accent-0)))
    `(ido-virtual ((,c :foreground ,accent-1)))
;;;;; iedit
    `(iedit-occurrence ((,c :inherit modus-themes-intense-blue)))
    `(iedit-read-only-occurrence ((,c :inherit modus-themes-intense-yellow)))
;;;;; iflipb
    `(iflipb-current-buffer-face ((,c :inherit bold :foreground ,name)))
    `(iflipb-other-buffer-face ((,c :inherit shadow)))
;;;;; image-dired
    `(image-dired-thumb-flagged ((,c :inherit modus-themes-intense-red)))
    `(image-dired-thumb-header-file-name ((,c :inherit bold)))
    `(image-dired-thumb-header-file-size ((,c :foreground ,constant)))
    `(image-dired-thumb-mark ((,c :inherit modus-themes-intense-cyan)))
;;;;; imenu-list
    `(imenu-list-entry-face-0 ((,c :foreground ,heading-0)))
    `(imenu-list-entry-face-1 ((,c :foreground ,heading-1)))
    `(imenu-list-entry-face-2 ((,c :foreground ,heading-2)))
    `(imenu-list-entry-face-3 ((,c :foreground ,heading-3)))
    `(imenu-list-entry-subalist-face-0 ((,c :inherit bold :foreground ,heading-4 :underline t)))
    `(imenu-list-entry-subalist-face-1 ((,c :inherit bold :foreground ,heading-5 :underline t)))
    `(imenu-list-entry-subalist-face-2 ((,c :inherit bold :foreground ,heading-6 :underline t)))
    `(imenu-list-entry-subalist-face-3 ((,c :inherit bold :foreground ,heading-7 :underline t)))
;;;;; indium
    `(indium-breakpoint-face ((,c :foreground ,err)))
    `(indium-frame-url-face ((,c :inherit (shadow button))))
    `(indium-keyword-face ((,c :inherit font-lock-keyword-face)))
    `(indium-litable-face ((,c :inherit modus-themes-slant)))
    `(indium-repl-error-face ((,c :inherit error)))
    `(indium-repl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(indium-repl-stdout-face (( )))
;;;;; info
    `(Info-quoted ((,c :inherit modus-themes-prose-verbatim))) ; the capitalization is canonical
    `(info-header-node ((,c :inherit (shadow bold))))
    `(info-header-xref ((,c :foreground ,name)))
    `(info-index-match ((,c :inherit match)))
    `(info-menu-header ((,c :inherit bold)))
    `(info-menu-star ((,c :foreground ,red-intense)))
    `(info-node ((,c :inherit bold)))
    `(info-title-1 ((,c :inherit modus-themes-heading-1)))
    `(info-title-2 ((,c :inherit modus-themes-heading-2)))
    `(info-title-3 ((,c :inherit modus-themes-heading-3)))
    `(info-title-4 ((,c :inherit modus-themes-heading-4)))
;;;;; info+ (info-plus)
    `(info-command-ref-item ((,c :inherit font-lock-function-name-face)))
    `(info-constant-ref-item ((,c :inherit font-lock-constant-face)))
    `(info-custom-delimited ((,c :inherit modus-themes-prose-verbatim)))
    `(info-double-quoted-name ((,c :inherit font-lock-string-face)))
    `(info-file (( )))
    `(info-function-ref-item ((,c :inherit font-lock-function-name-face)))
    `(info-glossary-word ((,c :inherit modus-themes-box-button)))
    `(info-indented-text (( )))
    `(info-isolated-backquote (( )))
    `(info-isolated-quote (( )))
    `(info-macro-ref-item ((,c :inherit font-lock-keyword-face)))
    `(info-menu ((,c :inherit bold)))
    `(info-quoted-name ((,c :inherit modus-themes-prose-verbatim)))
    `(info-reference-item ((,c :inherit bold)))
    `(info-special-form-ref-item ((,c :inherit warning)))
    `(info-string ((,c :inherit font-lock-string-face)))
    `(info-syntax-class-item ((,c :inherit modus-themes-prose-code)))
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
;;;;; ioccur
    `(ioccur-cursor ((,c :foreground ,fg-main)))
    `(ioccur-invalid-regexp ((,c :foreground ,red)))
    `(ioccur-match-face ((,c :inherit match)))
    `(ioccur-match-overlay-face ((,c :background ,bg-inactive :extend t)))
    `(ioccur-num-line-face ((,c :inherit shadow)))
    `(ioccur-overlay-face ((,c :inherit modus-themes-intense-blue :extend t)))
    `(ioccur-regexp-face ((,c :inherit (modus-themes-intense-magenta bold))))
    `(ioccur-title-face ((,c :inherit bold :foreground ,name)))
;;;;; isearch, occur, and the like
    `(isearch ((,c :inherit modus-themes-search-current)))
    `(isearch-fail ((,c :inherit modus-themes-intense-red)))
    `(isearch-group-1 ((,c :inherit modus-themes-intense-blue)))
    `(isearch-group-2 ((,c :inherit modus-themes-intense-magenta)))
    `(lazy-highlight ((,c :inherit modus-themes-search-lazy)))
    `(match ((,c :background ,search-match :foreground ,fg-main)))
    `(query-replace ((,c :inherit modus-themes-intense-red)))
;;;;; ivy
    `(ivy-action ((,c :inherit modus-themes-key-binding)))
    `(ivy-confirm-face ((,c :inherit success)))
    `(ivy-current-match ((,c :inherit modus-themes-completion-selected)))
    `(ivy-match-required-face ((,c :inherit error)))
    `(ivy-minibuffer-match-face-1 (( )))
    `(ivy-minibuffer-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(ivy-minibuffer-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(ivy-minibuffer-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
    `(ivy-remote ((,c :inherit italic)))
    `(ivy-separator ((,c :inherit shadow)))
    `(ivy-subdir ((,c :foreground ,accent-0)))
    `(ivy-virtual ((,c :foreground ,accent-1)))
;;;;; ivy-posframe
    `(ivy-posframe-border ((,c :background ,border)))
    `(ivy-posframe-cursor ((,c :background ,fg-main :foreground ,bg-main)))
;;;;; jira (org-jira)
    `(jiralib-comment-face ((,c :background ,bg-inactive)))
    `(jiralib-comment-header-face ((,c :inherit bold)))
    `(jiralib-issue-info-face ((,c :background ,bg-inactive)))
    `(jiralib-issue-info-header-face ((,c :inherit bold :background ,bg-inactive)))
    `(jiralib-issue-summary-face ((,c :inherit bold)))
    `(jiralib-link-filter-face ((,c :underline t)))
    `(jiralib-link-issue-face ((,c :underline t)))
    `(jiralib-link-project-face ((,c :underline t)))
;;;;; journalctl-mode
    `(journalctl-error-face ((,c :inherit error)))
    `(journalctl-finished-face ((,c :inherit success)))
    `(journalctl-host-face ((,c :foreground ,name)))
    `(journalctl-process-face ((,c :foreground ,warning)))
    `(journalctl-starting-face ((,c :foreground ,note)))
    `(journalctl-timestamp-face ((,c :foreground ,date)))
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
;;;;; kaocha-runner
    `(kaocha-runner-error-face ((,c :inherit error)))
    `(kaocha-runner-success-face ((,c :inherit success)))
    `(kaocha-runner-warning-face ((,c :inherit warning)))
;;;;; keycast
    `(keycast-command ((,c :inherit bold :foreground ,keybind)))
    `(keycast-key ((,c :background ,keybind :foreground ,bg-main)))
;;;;; ledger-mode
    `(ledger-font-auto-xact-face ((,c :inherit font-lock-builtin-face)))
    `(ledger-font-account-name-face ((,c :foreground ,name)))
    `(ledger-font-directive-face ((,c :inherit font-lock-keyword-face)))
    `(ledger-font-posting-date-face ((,c :inherit modus-themes-bold :foreground ,date)))
    `(ledger-font-periodic-xact-face ((,c :inherit font-lock-variable-name-face)))
    `(ledger-font-posting-amount-face ((,c :inherit font-lock-constant-face)))
    `(ledger-font-payee-cleared-face ((,c :inherit success)))
    `(ledger-font-payee-pending-face ((,c :inherit warning)))
    `(ledger-font-payee-uncleared-face ((,c :inherit error)))
    `(ledger-font-xact-highlight-face ((,c :inherit highlight)))
;;;;; leerzeichen
    `(leerzeichen ((,c :background ,bg-inactive)))
;;;;; line numbers (display-line-numbers-mode and global variant)
    ;; Here we cannot inherit `modus-themes-fixed-pitch'.  We need to
    ;; fall back to `default' otherwise line numbers do not scale when
    ;; using `text-scale-adjust'.
    `(line-number
      ((,c :inherit ,(if modus-themes-mixed-fonts '(fixed-pitch default) 'default)
           ,@(modus-themes--line-numbers
              fg-dim bg-dim
              "gray50"))))
    `(line-number-current-line
      ((,c :inherit (bold line-number)
           ,@(modus-themes--line-numbers
              fg-main bg-active
              blue-cooler))))
    `(line-number-major-tick
      ((,c :inherit (bold line-number)
           ,@(modus-themes--line-numbers
              err bg-inactive
              err))))
    `(line-number-minor-tick
      ((,c :inherit (bold line-number)
           ,@(modus-themes--line-numbers
              fg-dim bg-inactive
              fg-dim))))
;;;;; magit
    `(magit-bisect-bad ((,c :inherit error)))
    `(magit-bisect-good ((,c :inherit success)))
    `(magit-bisect-skip ((,c :inherit warning)))
    `(magit-blame-date (( )))
    `(magit-blame-dimmed ((,c :inherit shadow)))
    `(magit-blame-hash (( )))
    `(magit-blame-highlight ((,c :background ,bg-active :foreground ,fg-main)))
    `(magit-blame-name (( )))
    `(magit-blame-summary ((  )))
    `(magit-branch-local ((,c :foreground ,accent-0)))
    `(magit-branch-remote ((,c :foreground ,accent-1)))
    `(magit-branch-upstream ((,c :inherit italic)))
    `(magit-branch-warning ((,c :inherit warning)))
    `(magit-cherry-equivalent ((,c :foreground ,magenta)))
    `(magit-cherry-unmatched ((,c :foreground ,cyan)))
    `(magit-diff-added ((,c :background ,@(modus-themes--deuteran bg-added-faint-deuteran bg-added-faint))))
    `(magit-diff-added-highlight ((,c :background ,@(modus-themes--deuteran bg-added-deuteran bg-added))))
    `(magit-diff-base ((,c :background ,bg-changed-faint)))
    `(magit-diff-base-highlight ((,c :background ,bg-changed)))
    `(magit-diff-context ((,c :inherit shadow)))
    `(magit-diff-context-highlight ((,c :background ,bg-dim)))
    `(magit-diff-file-heading ((,c :inherit bold :foreground ,accent-0)))
    `(magit-diff-file-heading-highlight ((,c :inherit magit-diff-file-heading :background ,bg-inactive)))
    `(magit-diff-file-heading-selection ((,c :inherit bold :background ,bg-hover-alt)))
    `(magit-diff-hunk-heading ((,c :inherit bold :background ,bg-inactive)))
    `(magit-diff-hunk-heading-highlight ((,c :inherit bold :background ,bg-active)))
    `(magit-diff-hunk-heading-selection ((,c :inherit bold :background ,bg-hover-alt)))
    `(magit-diff-hunk-region ((,c :inherit bold)))
    `(magit-diff-lines-boundary ((,c :background ,fg-main)))
    `(magit-diff-lines-heading ((,c :background ,fg-dim :foreground ,bg-inactive)))
    `(magit-diff-removed ((,c :background ,bg-removed-faint)))
    `(magit-diff-removed-highlight ((,c :background ,bg-removed)))
    `(magit-diffstat-added ((,c :inherit success)))
    `(magit-diffstat-removed ((,c :inherit error)))
    `(magit-dimmed ((,c :inherit shadow)))
    `(magit-filename ((,c :foreground ,accent-2)))
    `(magit-hash ((,c :foreground ,identifier)))
    `(magit-head ((,c :inherit magit-branch-local)))
    `(magit-header-line ((,c :inherit bold)))
    `(magit-header-line-key ((,c :inherit modus-themes-key-binding)))
    `(magit-header-line-log-select ((,c :inherit bold)))
    `(magit-keyword ((,c :foreground ,keyword)))
    `(magit-keyword-squash ((,c :inherit bold :foreground ,warning)))
    `(magit-log-author ((,c :foreground ,name)))
    `(magit-log-date ((,c :foreground ,date)))
    `(magit-log-graph ((,c :inherit shadow)))
    `(magit-mode-line-process ((,c :inherit success)))
    `(magit-mode-line-process-error ((,c :inherit error)))
    `(magit-process-ng ((,c :inherit error)))
    `(magit-process-ok ((,c :inherit success)))
    `(magit-reflog-amend ((,c :inherit warning)))
    `(magit-reflog-checkout ((,c :inherit bold :foreground ,blue)))
    `(magit-reflog-cherry-pick ((,c :inherit success)))
    `(magit-reflog-commit ((,c :inherit bold)))
    `(magit-reflog-merge ((,c :inherit success)))
    `(magit-reflog-other ((,c :inherit bold :foreground ,cyan)))
    `(magit-reflog-rebase ((,c :inherit bold :foreground ,magenta)))
    `(magit-reflog-remote ((,c :inherit (bold magit-branch-remote))))
    `(magit-reflog-reset ((,c :inherit error)))
    `(magit-refname ((,c :inherit shadow)))
    `(magit-refname-pullreq ((,c :inherit shadow)))
    `(magit-refname-stash ((,c :inherit shadow)))
    `(magit-refname-wip ((,c :inherit shadow)))
    `(magit-section ((,c :background ,bg-dim :foreground ,fg-main)))
    `(magit-section-heading ((,c :inherit bold)))
    `(magit-section-heading-selection ((,c :inherit bold :background ,bg-hover-alt)))
    `(magit-section-highlight ((,c :background ,bg-dim)))
    `(magit-sequence-done ((,c :inherit success)))
    `(magit-sequence-drop ((,c :inherit error)))
    `(magit-sequence-exec ((,c :inherit bold :foreground ,magenta)))
    `(magit-sequence-head ((,c :inherit bold :foreground ,cyan)))
    `(magit-sequence-onto ((,c :inherit (bold shadow))))
    `(magit-sequence-part ((,c :inherit warning)))
    `(magit-sequence-pick ((,c :inherit bold)))
    `(magit-sequence-stop ((,c :inherit error)))
    `(magit-signature-bad ((,c :inherit error)))
    `(magit-signature-error ((,c :inherit error)))
    `(magit-signature-expired ((,c :inherit warning)))
    `(magit-signature-expired-key ((,c :foreground ,warning)))
    `(magit-signature-good ((,c :inherit success)))
    `(magit-signature-revoked ((,c :inherit bold :foreground ,warning)))
    `(magit-signature-untrusted ((,c :inherit (bold shadow))))
    `(magit-tag ((,c :foreground ,accent-3))) ; compare with branches
;;;;; make-mode (makefiles)
    `(makefile-makepp-perl ((,c :background ,bg-dim)))
    `(makefile-space ((,c :background ,bg-inactive)))
;;;;; man
    `(Man-overstrike ((,c :inherit bold :foreground ,accent-0)))
    `(Man-underline ((,c :foreground ,accent-1 :underline t)))
;;;;; marginalia
    `(marginalia-archive ((,c :foreground ,accent-0)))
    `(marginalia-char ((,c :foreground ,accent-2)))
    `(marginalia-date ((,c :foreground ,date)))
    `(marginalia-documentation ((,c :inherit italic :foreground ,docstring)))
    `(marginalia-file-name (( )))
    `(marginalia-file-owner ((,c :inherit shadow)))
    `(marginalia-file-priv-dir ((,c :foreground ,accent-0)))
    `(marginalia-file-priv-exec ((,c :foreground ,accent-1)))
    `(marginalia-file-priv-link ((,c :foreground ,link)))
    `(marginalia-file-priv-no ((,c :inherit shadow)))
    `(marginalia-file-priv-other ((,c :foreground ,accent-2)))
    `(marginalia-file-priv-rare ((,c :foreground ,accent-3)))
    `(marginalia-file-priv-read ((,c :foreground ,fg-main)))
    `(marginalia-file-priv-write ((,c :foreground ,accent-0)))
    `(marginalia-function ((,c :foreground ,fnname)))
    `(marginalia-key ((,c :inherit modus-themes-key-binding)))
    `(marginalia-lighter ((,c :inherit shadow)))
    `(marginalia-liqst ((,c :inherit shadow)))
    `(marginalia-mode ((,c :foreground ,constant)))
    `(marginalia-modified ((,c :inherit warning)))
    `(marginalia-null ((,c :inherit shadow)))
    `(marginalia-number ((,c :foreground ,constant)))
    `(marginalia-size ((,c :foreground ,variable)))
    `(marginalia-string ((,c :foreground ,string)))
    `(marginalia-symbol ((,c :foreground ,builtin)))
    `(marginalia-true (( )))
    `(marginalia-type ((,c :foreground ,type)))
    `(marginalia-value ((,c :inherit shadow)))
    `(marginalia-version ((,c :foreground ,date)))
;;;;; markdown-mode
    `(markdown-blockquote-face ((,c :inherit font-lock-doc-face)))
    `(markdown-bold-face ((,c :inherit bold)))
    `(markdown-code-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))
    `(markdown-gfm-checkbox-face ((,c :foreground ,warning)))
    `(markdown-header-face (( )))
    `(markdown-header-face-1 ((,c :inherit modus-themes-heading-1)))
    `(markdown-header-face-2 ((,c :inherit modus-themes-heading-2)))
    `(markdown-header-face-3 ((,c :inherit modus-themes-heading-3)))
    `(markdown-header-face-4 ((,c :inherit modus-themes-heading-4)))
    `(markdown-header-face-5 ((,c :inherit modus-themes-heading-5)))
    `(markdown-header-face-6 ((,c :inherit modus-themes-heading-6)))
    `(markdown-highlighting-face ((,c :inherit secondary-selection)))
    `(markdown-inline-code-face ((,c :inherit modus-themes-prose-code)))
    `(markdown-italic-face ((,c :inherit italic)))
    `(markdown-language-keyword-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim)))
    `(markdown-line-break-face ((,c :inherit nobreak-space)))
    `(markdown-link-face ((,c :inherit link)))
    `(markdown-markup-face ((,c :inherit shadow)))
    `(markdown-metadata-key-face ((,c :inherit bold)))
    `(markdown-metadata-value-face ((,c :foreground ,string)))
    `(markdown-missing-link-face ((,c :inherit warning)))
    `(markdown-pre-face ((,c :inherit markdown-code-face)))
    `(markdown-table-face ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-alt))) ; same as `org-table'
    `(markdown-url-face ((,c :foreground ,fg-alt)))
;;;;; markup-faces (`adoc-mode')
    `(markup-attribute-face ((,c :inherit (italic markup-meta-face))))
    `(markup-bold-face ((,c :inherit bold)))
    `(markup-code-face ((,c :foreground ,prose-code)))
    `(markup-comment-face ((,c :inherit font-lock-comment-face)))
    `(markup-complex-replacement-face ((,c :foreground ,prose-macro)))
    `(markup-emphasis-face ((,c :inherit markup-italic-face)))
    `(markup-error-face ((,c :inherit error)))
    `(markup-gen-face ((,c :foreground ,prose-verbatim)))
    `(markup-internal-reference-face ((,c :inherit (shadow modus-themes-slant))))
    `(markup-italic-face ((,c :inherit italic)))
    `(markup-list-face ((,c :background ,bg-inactive)))
    `(markup-meta-face ((,c :inherit (modus-themes-fixed-pitch shadow))))
    `(markup-meta-hide-face ((,c :foreground "gray50")))
    `(markup-reference-face ((,c :inherit modus-themes-slant :foreground ,fg-alt)))
    `(markup-replacement-face ((,c :inherit modus-themes-fixed-pitch :foreground ,err)))
    `(markup-secondary-text-face ((,c :height 0.9 :foreground ,fg-alt)))
    `(markup-small-face ((,c :inherit markup-gen-face :height 0.9)))
    `(markup-strong-face ((,c :inherit markup-bold-face)))
    `(markup-subscript-face ((,c :height 0.9 :foreground ,fg-alt)))
    `(markup-superscript-face ((,c :height 0.9 :foreground ,fg-alt)))
    `(markup-table-cell-face (( )))
    `(markup-table-face ((,c :foreground ,fg-alt)))
    `(markup-table-row-face (( )))
    `(markup-title-0-face ((,c :inherit modus-themes-heading-1)))
    `(markup-title-1-face ((,c :inherit modus-themes-heading-2)))
    `(markup-title-2-face ((,c :inherit modus-themes-heading-3)))
    `(markup-title-3-face ((,c :inherit modus-themes-heading-4)))
    `(markup-title-4-face ((,c :inherit modus-themes-heading-5)))
    `(markup-title-5-face ((,c :inherit modus-themes-heading-6)))
    `(markup-verbatim-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-verbatim)))
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
;;;;; minimap
    `(minimap-active-region-background ((,c :background ,bg-active)))
    `(minimap-current-line-face ((,c :background ,bg-cyan :foreground ,fg-main)))
;;;;; mode-line
    `(mode-line ((,c :inherit modus-themes-ui-variable-pitch
                     ,@(modus-themes--mode-line-attrs
                        fg-main bg-active
                        fg-dim bg-active
                        fg-main bg-active-accent
                        fg-dim bg-active
                        'alt-style bg-main))))
    `(mode-line-active ((,c :inherit mode-line)))
    `(mode-line-buffer-id ((,c :inherit bold)))
    `(mode-line-emphasis ((,c :inherit bold :foreground ,note)))
    `(mode-line-highlight ((,c :inherit highlight)))
    `(mode-line-inactive ((,c :inherit modus-themes-ui-variable-pitch
                              ,@(modus-themes--mode-line-attrs
                                 fg-dim bg-inactive
                                 fg-dim bg-dim
                                 fg-dim bg-inactive
                                 bg-region bg-active))))
;;;;; mood-line
    `(mood-line-modified ((,c :inherit italic)))
    `(mood-line-status-error ((,c :inherit error)))
    `(mood-line-status-info ((,c :inherit success)))
    `(mood-line-status-neutral (( )))
    `(mood-line-status-success ((,c :inherit modus-themes-grue)))
    `(mood-line-status-warning ((,c :inherit warning)))
    `(mood-line-unimportant ((,c :inherit shadow)))
;;;;; mpdel
    `(mpdel-browser-directory-face ((,c :foreground ,accent-0)))
    `(mpdel-playlist-current-song-face ((,c :inherit bold :foreground ,accent-0)))
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
    `(mu4e-compose-separator-face ((,c :inherit message-separator)))
    `(mu4e-contact-face ((,c :inherit message-header-to)))
    `(mu4e-context-face ((,c :inherit bold)))
    `(mu4e-draft-face ((,c :foreground ,warning)))
    `(mu4e-flagged-face ((,c :foreground ,err)))
    `(mu4e-footer-face ((,c :inherit italic :foreground ,fg-alt)))
    `(mu4e-forwarded-face ((,c :inherit italic :foreground ,note)))
    `(mu4e-header-face ((,c :inherit shadow)))
    `(mu4e-header-highlight-face ((,c :inherit highlight)))
    `(mu4e-header-key-face ((,c :inherit message-header-name)))
    `(mu4e-header-marks-face ((,c :inherit mu4e-special-header-value-face)))
    `(mu4e-header-title-face ((,c :foreground ,rainbow-0)))
    `(mu4e-header-value-face ((,c :inherit message-header-other)))
    `(mu4e-highlight-face ((,c :inherit modus-themes-key-binding)))
    `(mu4e-link-face ((,c :inherit link)))
    `(mu4e-modeline-face (( )))
    `(mu4e-moved-face ((,c :inherit italic :foreground ,warning)))
    `(mu4e-ok-face ((,c :inherit success)))
    `(mu4e-region-code ((,c :foreground ,builtin)))
    `(mu4e-related-face ((,c :inherit (italic shadow))))
    `(mu4e-replied-face ((,c :foreground ,note)))
    `(mu4e-special-header-value-face ((,c :inherit message-header-subject)))
    `(mu4e-system-face ((,c :inherit italic)))
    `(mu4e-title-face (( )))
    `(mu4e-trashed-face ((,c :foreground ,err)))
    `(mu4e-unread-face ((,c :inherit bold)))
    `(mu4e-url-number-face ((,c :inherit shadow)))
    `(mu4e-view-body-face (( )))
    `(mu4e-warning-face ((,c :inherit warning)))
;;;;; multiple-cursors
    `(mc/cursor-bar-face ((,c :height 1 :foreground ,fg-main :background ,bg-main)))
    `(mc/cursor-face ((,c :inverse-video t)))
    `(mc/region-face ((,c :inherit region)))
;;;;; neotree
    `(neo-banner-face ((,c :foreground ,accent-0)))
    `(neo-button-face ((,c :inherit button)))
    `(neo-dir-link-face (( )))
    `(neo-expand-btn-face (( )))
    `(neo-file-link-face (( )))
    `(neo-header-face ((,c :inherit bold)))
    `(neo-root-dir-face ((,c :inherit bold :foreground ,accent-0)))
    `(neo-vc-added-face ((,c :inherit success)))
    `(neo-vc-conflict-face ((,c :inherit error)))
    `(neo-vc-default-face (( )))
    `(neo-vc-edited-face ((,c :inherit italic)))
    `(neo-vc-ignored-face ((,c :inherit shadow)))
    `(neo-vc-missing-face ((,c :inherit error)))
    `(neo-vc-needs-merge-face ((,c :inherit italic)))
    `(neo-vc-needs-update-face ((,c :underline t)))
    `(neo-vc-removed-face ((,c :strike-through t)))
    `(neo-vc-unlocked-changes-face ((,c :inherit success)))
    `(neo-vc-up-to-date-face (( )))
    `(neo-vc-user-face ((,c :inherit warning)))
;;;;; notmuch
    `(notmuch-crypto-decryption ((,c :inherit bold)))
    `(notmuch-crypto-part-header ((,c :foreground ,mail-4))) ; like `message-mml'
    `(notmuch-crypto-signature-bad ((,c :inherit error)))
    `(notmuch-crypto-signature-good ((,c :inherit success)))
    `(notmuch-crypto-signature-good-key ((,c :inherit success)))
    `(notmuch-crypto-signature-unknown ((,c :inherit warning)))
    `(notmuch-jump-key ((,c :inherit modus-themes-key-binding)))
    `(notmuch-message-summary-face ((,c :inherit bold :background ,bg-dim)))
    `(notmuch-search-count ((,c :foreground ,fg-dim)))
    `(notmuch-search-date ((,c :foreground ,date)))
    `(notmuch-search-flagged-face ((,c :foreground ,err)))
    `(notmuch-search-matching-authors ((,c :foreground ,mail-recipient)))
    `(notmuch-search-non-matching-authors ((,c :inherit shadow)))
    `(notmuch-search-subject ((,c :foreground ,fg-main)))
    `(notmuch-search-unread-face ((,c :inherit bold)))
    `(notmuch-tag-added ((,c :underline ,note)))
    `(notmuch-tag-deleted ((,c :strike-through ,err)))
    `(notmuch-tag-face ((,c :foreground ,accent-0)))
    `(notmuch-tag-flagged ((,c :foreground ,err)))
    `(notmuch-tag-unread ((,c :foreground ,accent-1)))
    `(notmuch-tree-match-author-face ((,c :inherit notmuch-search-matching-authors)))
    `(notmuch-tree-match-date-face ((,c :inherit notmuch-search-date)))
    `(notmuch-tree-match-face ((,c :foreground ,fg-main)))
    `(notmuch-tree-match-tag-face ((,c :inherit notmuch-tag-face)))
    `(notmuch-tree-no-match-face ((,c :inherit shadow)))
    `(notmuch-tree-no-match-date-face ((,c :inherit shadow)))
    `(notmuch-wash-cited-text ((,c :inherit message-cited-text-1)))
    `(notmuch-wash-toggle-button ((,c :background ,bg-dim)))
;;;;; num3-mode
    `(num3-face-even ((,c :inherit bold :background ,bg-inactive)))
;;;;; nxml-mode
    `(nxml-attribute-colon ((,c :foreground ,fg-main)))
    `(nxml-attribute-local-name ((,c :inherit font-lock-variable-name-face)))
    `(nxml-attribute-prefix ((,c  :inherit font-lock-type-face)))
    `(nxml-attribute-value ((,c :inherit font-lock-constant-face)))
    `(nxml-cdata-section-CDATA ((,c :inherit error)))
    `(nxml-cdata-section-delimiter ((,c :inherit error)))
    `(nxml-char-ref-delimiter ((,c :inherit shadow)))
    `(nxml-char-ref-number ((,c :inherit (shadow modus-themes-bold))))
    `(nxml-delimited-data ((,c :inherit (shadow modus-themes-slant))))
    `(nxml-delimiter ((,c :foreground ,fg-dim)))
    `(nxml-element-colon ((,c :foreground ,fg-main)))
    `(nxml-element-local-name ((,c :inherit font-lock-function-name-face)))
    `(nxml-element-prefix ((,c :inherit font-lock-builtin-face)))
    `(nxml-entity-ref-delimiter ((,c :inherit shadow)))
    `(nxml-entity-ref-name ((,c :inherit (shadow modus-themes-bold))))
    `(nxml-glyph ((,c :background ,bg-active :foreground ,fg-main)))
    `(nxml-hash ((,c :inherit (bold font-lock-string-face))))
    `(nxml-heading ((,c :inherit bold)))
    `(nxml-name ((,c :inherit font-lock-builtin-face)))
    `(nxml-namespace-attribute-colon ((,c :foreground ,fg-main)))
    `(nxml-namespace-attribute-prefix ((,c :inherit font-lock-variable-name-face)))
    `(nxml-processing-instruction-target ((,c :inherit font-lock-keyword-face)))
    `(nxml-prolog-keyword ((,c :inherit font-lock-keyword-face)))
    `(nxml-ref ((,c :inherit (shadow modus-themes-bold))))
    `(rng-error ((,c :inherit error)))
;;;;; olivetti
    `(olivetti-fringe ((,c :background ,bg-main)))
;;;;; orderless
    `(orderless-match-face-0 ((,c :inherit modus-themes-completion-match-0)))
    `(orderless-match-face-1 ((,c :inherit modus-themes-completion-match-1)))
    `(orderless-match-face-2 ((,c :inherit modus-themes-completion-match-2)))
    `(orderless-match-face-3 ((,c :inherit modus-themes-completion-match-3)))
;;;;; org
    `(org-agenda-calendar-event ((,c :foreground ,fg-alt)))
    `(org-agenda-calendar-sexp ((,c :inherit (italic org-agenda-calendar-event))))
    `(org-agenda-column-dateline ((,c :background ,bg-inactive)))
    `(org-agenda-current-time ((,c :foreground ,fg-main)))
    `(org-agenda-date ((,c :inherit modus-themes-heading-1)))
    `(org-agenda-date-today ((,c :inherit org-agenda-date :underline t)))
    `(org-agenda-date-weekend ((,c :inherit org-agenda-date)))
    `(org-agenda-date-weekend-today ((,c :inherit org-agenda-date-today)))
    `(org-agenda-diary ((,c :inherit org-agenda-calendar-sexp)))
    `(org-agenda-dimmed-todo-face ((,c :inherit shadow)))
    `(org-agenda-done ((,c :inherit success)))
    `(org-agenda-filter-category ((,c :inherit bold :foreground ,err)))
    `(org-agenda-filter-effort ((,c :inherit bold :foreground ,err)))
    `(org-agenda-filter-regexp ((,c :inherit bold :foreground ,err)))
    `(org-agenda-filter-tags ((,c :inherit bold :foreground ,err)))
    `(org-agenda-restriction-lock ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(org-agenda-structure ((,c :inherit modus-themes-heading-0)))
    `(org-agenda-structure-filter ((,c :inherit org-agenda-structure :foreground ,rainbow-1)))
    `(org-agenda-structure-secondary ((,c :foreground ,rainbow-1)))
    `(org-archived ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(org-block ((,c :inherit modus-themes-fixed-pitch ,@(modus-themes--org-block fg-main bg-dim))))
    `(org-block-begin-line ((,c :inherit modus-themes-fixed-pitch
                                ,@(modus-themes--org-block
                                   fg-dim bg-inactive))))
    `(org-block-end-line ((,c :inherit org-block-begin-line)))
    `(org-checkbox ((,c :foreground ,warning)))
    `(org-checkbox-statistics-done ((,c :inherit org-done)))
    `(org-checkbox-statistics-todo ((,c :inherit org-todo)))
    `(org-clock-overlay ((,c :background ,bg-hover-alt)))
    `(org-code ((,c :inherit modus-themes-prose-code)))
    `(org-column ((,c :inherit default :background ,bg-dim)))
    `(org-column-title ((,c :inherit (bold default) :underline t :background ,bg-dim)))
    `(org-date ((,c :inherit modus-themes-fixed-pitch :foreground ,date)))
    `(org-date-selected ((,c :foreground ,date :inverse-video t)))
    `(org-document-info ((,c :foreground ,fg-alt)))
    `(org-document-info-keyword ((,c :inherit shadow)))
    `(org-document-title ((,c :inherit modus-themes-heading-0)))
    `(org-done ((,c :inherit modus-themes-grue)))
    `(org-drawer ((,c :inherit (shadow modus-themes-fixed-pitch))))
    `(org-ellipsis (( ))) ; inherits from the heading's color
    `(org-footnote ((,c :inherit link)))
    `(org-formula ((,c :inherit modus-themes-fixed-pitch :foreground ,fnname)))
    `(org-headline-done ((,c :inherit org-done)))
    `(org-headline-todo ((,c :inherit org-todo)))
    `(org-hide ((,c :foreground ,bg-main)))
    `(org-indent ((,c :inherit (fixed-pitch org-hide))))
    `(org-imminent-deadline ((,c :inherit bold :foreground ,err)))
    `(org-latex-and-related ((,c :foreground ,type)))
    `(org-level-1 ((,c :inherit modus-themes-heading-1)))
    `(org-level-2 ((,c :inherit modus-themes-heading-2)))
    `(org-level-3 ((,c :inherit modus-themes-heading-3)))
    `(org-level-4 ((,c :inherit modus-themes-heading-4)))
    `(org-level-5 ((,c :inherit modus-themes-heading-5)))
    `(org-level-6 ((,c :inherit modus-themes-heading-6)))
    `(org-level-7 ((,c :inherit modus-themes-heading-7)))
    `(org-level-8 ((,c :inherit modus-themes-heading-8)))
    `(org-link ((,c :inherit link)))
    `(org-list-dt ((,c :inherit bold)))
    `(org-macro ((,c :inherit modus-themes-prose-macro)))
    `(org-meta-line ((,c :inherit (shadow modus-themes-fixed-pitch))))
    `(org-mode-line-clock (( )))
    `(org-mode-line-clock-overrun ((,c :inherit bold :foreground ,err)))
    `(org-priority ((,c :foreground ,tag)))
    `(org-property-value ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-alt)))
    `(org-quote ((,c :inherit org-block)))
    `(org-scheduled ((,c :foreground ,warning)))
    `(org-scheduled-previously ((,c :inherit org-scheduled)))
    `(org-scheduled-today ((,c :inherit (bold org-scheduled))))
    `(org-sexp-date ((,c :foreground ,date)))
    `(org-special-keyword ((,c :inherit (shadow modus-themes-fixed-pitch))))
    `(org-table ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-alt)))
    `(org-table-header ((,c :inherit (bold org-table))))
    `(org-tag ((,c :foreground ,tag)))
    `(org-tag-group ((,c :inherit (bold org-tag))))
    `(org-target ((,c :underline t)))
    `(org-time-grid ((,c :foreground ,fg-dim)))
    `(org-todo ((,c :foreground ,err)))
    `(org-upcoming-deadline ((,c :foreground ,warning)))
    `(org-upcoming-distant-deadline ((,c :inherit org-upcoming-deadline)))
    `(org-verbatim ((,c :inherit modus-themes-prose-verbatim)))
    `(org-verse ((,c :inherit org-block)))
    `(org-warning ((,c :inherit warning)))
;;;;; org-journal
    `(org-journal-calendar-entry-face ((,c :inherit modus-themes-slant :foreground ,date)))
    `(org-journal-calendar-scheduled-face ((,c :inherit (modus-themes-slant org-scheduled))))
    `(org-journal-highlight ((,c :foreground ,err)))
;;;;; org-noter
    `(org-noter-no-notes-exist-face ((,c :inherit error)))
    `(org-noter-notes-exist-face ((,c :inherit success)))
;;;;; org-pomodoro
    `(org-pomodoro-mode-line ((,c :foreground ,err)))
    `(org-pomodoro-mode-line-break ((,c :foreground ,note)))
    `(org-pomodoro-mode-line-overtime ((,c :inherit error)))
;;;;; org-recur
    `(org-recur ((,c :foreground ,fg-alt)))
;;;;; org-roam
    `(org-roam-dim ((,c :foreground "gray50")))
    `(org-roam-olp ((,c :inherit shadow)))
    `(org-roam-preview-heading ((,c :background ,bg-inactive)))
    `(org-roam-preview-heading-highlight ((,c :background ,bg-active :foreground ,fg-main)))
    `(org-roam-preview-region ((,c :inherit bold)))
    `(org-roam-title ((,c :inherit bold)))
;;;;; org-superstar
    `(org-superstar-item ((,c :foreground ,fg-main)))
;;;;; org-tree-slide
    `(org-tree-slide-header-overlay-face ((,c :inherit org-document-title)))
;;;;; origami
    `(origami-fold-header-face ((,c :background ,bg-dim :foreground ,fg-dim :box t)))
    `(origami-fold-replacement-face ((,c :background ,bg-inactive :foreground ,fg-dim)))
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
    `(package-description ((,c :foreground ,docstring)))
    `(package-help-section-name ((,c :inherit bold)))
    `(package-name ((,c :inherit link)))
    `(package-status-available ((,c :foreground ,date)))
    `(package-status-avail-obso ((,c :inherit error)))
    `(package-status-built-in ((,c :foreground ,builtin)))
    `(package-status-dependency ((,c :foreground ,warning)))
    `(package-status-disabled ((,c :inherit error :strike-through t)))
    `(package-status-held ((,c :foreground ,warning)))
    `(package-status-incompat ((,c :inherit warning)))
    `(package-status-installed ((,c :foreground ,fg-alt)))
    `(package-status-new ((,c :inherit success)))
    `(package-status-unsigned ((,c :inherit error)))
;;;;; page-break-lines
    `(page-break-lines ((,c :inherit default :foreground "gray50")))
;;;;; pandoc-mode
    `(pandoc-citation-key-face ((,c :inherit font-lock-builtin-face)))
    `(pandoc-directive-@@-face ((,c :inherit font-lock-keyword-face)))
    `(pandoc-directive-braces-face ((,c :inherit font-lock-constant-face)))
    `(pandoc-directive-contents-face ((,c :inherit font-lock-string-face)))
    `(pandoc-directive-type-face ((,c :inherit font-lock-type-face)))
;;;;; paren-face
    `(parenthesis ((,c :inherit shadow)))
;;;;; pass
    `(pass-mode-directory-face ((,c :inherit bold :foreground ,accent-0)))
    `(pass-mode-entry-face ((,c :background ,bg-main :foreground ,fg-main)))
    `(pass-mode-header-face ((,c :inherit shadow)))
;;;;; pdf-tools
    `(pdf-links-read-link ((,c :background ,fg-main :foreground ,bg-magenta :inherit bold))) ; Foreground is background and vice versa
    `(pdf-occur-document-face ((,c :inherit shadow)))
    `(pdf-occur-page-face ((,c :inherit shadow)))
;;;;; persp-mode
    `(persp-face-lighter-buffer-not-in-persp ((,c :inherit error)))
    `(persp-face-lighter-default ((,c :inherit bold :foreground ,name)))
    `(persp-face-lighter-nil-persp ((,c :inherit bold)))
;;;;; perspective
    `(persp-selected-face ((,c :inherit bold :foreground ,name)))
;;;;; popup
    `(popup-face ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(popup-isearch-match ((,c :inherit modus-themes-search-current)))
    `(popup-menu-mouse-face ((,c :inherit highlight)))
    `(popup-menu-selection-face ((,c :inherit modus-themes-completion-selected)))
    `(popup-scroll-bar-background-face ((,c :background ,bg-active)))
    `(popup-scroll-bar-foreground-face (( )))
    `(popup-summary-face ((,c :background ,bg-active :foreground ,fg-dim)))
    `(popup-tip-face ((,c :inherit modus-themes-intense-yellow)))
;;;;; prescient
    `(prescient-primary-highlight ((,c :inherit modus-themes-completion-match-0)))
    `(prescient-secondary-highlight ((,c :inherit modus-themes-completion-match-1)))
;;;;; proced
    `(proced-mark ((,c :inherit bold)))
    `(proced-marked ((,c :inherit modus-themes-mark-alt)))
    `(proced-sort-header ((,c :inherit bold :underline t)))
;;;;; prodigy
    `(prodigy-green-face ((,c :inherit success)))
    `(prodigy-red-face ((,c :inherit error)))
    `(prodigy-yellow-face ((,c :inherit warning)))
;;;;; pulse
    `(pulse-highlight-start-face ((,c :background ,bg-active-accent :extend t)))
;;;;; pyim
    `(pyim-page ((,c :background ,bg-active)))
    `(pyim-page-selection ((,c :inherit bold :background ,bg-active :foreground ,note)))
    `(pyim-page-subword ((,c :background ,bg-inactive)))
;;;;; quick-peek
    `(quick-peek-background-face ((,c :background ,bg-inactive)))
    `(quick-peek-border-face ((,c :background ,border :height 1)))
    `(quick-peek-padding-face ((,c :background ,bg-inactive :height 0.15)))
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
    `(rainbow-blocks-unmatched-face ((,c :foreground ,err)))
;;;;; rainbow-delimiters
    `(rainbow-delimiters-base-error-face ((,c :background ,bg-red-subtle :foreground ,fg-main)))
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
    `(rainbow-delimiters-mismatched-face ((,c :inherit (bold modus-themes-intense-yellow))))
    `(rainbow-delimiters-unmatched-face ((,c :inherit (bold modus-themes-intense-red))))
;;;;; rcirc
    `(rcirc-bright-nick ((,c :inherit bold :foreground ,accent-2)))
    `(rcirc-dim-nick ((,c :inherit shadow)))
    `(rcirc-monospace-text ((,c :inherit fixed-pitch)))
    `(rcirc-my-nick ((,c :inherit bold :foreground ,accent-1)))
    `(rcirc-nick-in-message ((,c :inherit rcirc-my-nick)))
    `(rcirc-nick-in-message-full-line ((,c :inherit rcirc-my-nick)))
    `(rcirc-other-nick ((,c :inherit bold :foreground ,accent-0)))
    `(rcirc-prompt ((,c :inherit minibuffer-prompt)))
    `(rcirc-server ((,c :inherit font-lock-comment-face)))
    `(rcirc-timestamp ((,c :foreground ,date)))
    `(rcirc-track-keyword ((,c :inherit warning)))
    `(rcirc-track-nick ((,c :inherit rcirc-my-nick)))
    `(rcirc-url ((,c :inherit link)))
;;;;; recursion-indicator
    `(recursion-indicator-general ((,c :inherit success)))
    `(recursion-indicator-minibuffer ((,c :inherit error)))
;;;;; regexp-builder (re-builder)
    `(reb-match-0 ((,c :inherit modus-themes-intense-cyan)))
    `(reb-match-1 ((,c :inherit modus-themes-subtle-magenta)))
    `(reb-match-2 ((,c :inherit modus-themes-subtle-green)))
    `(reb-match-3 ((,c :inherit modus-themes-intense-yellow)))
    `(reb-regexp-grouping-backslash ((,c :inherit font-lock-regexp-grouping-backslash)))
    `(reb-regexp-grouping-construct ((,c :inherit font-lock-regexp-grouping-construct)))
;;;;; rg (rg.el)
    `(rg-column-number-face ((,c :inherit shadow)))
    `(rg-context-face ((,c :inherit shadow)))
    `(rg-error-face ((,c :inherit error)))
    `(rg-file-tag-face ((,c :inherit font-lock-builtin-face)))
    `(rg-filename-face ((,c :inherit bold :foreground ,name)))
    `(rg-line-number-face ((,c :inherit shadow)))
    `(rg-literal-face ((,c :inherit font-lock-constant-face)))
    `(rg-match-face ((,c :inherit match)))
    `(rg-regexp-face ((,c :foreground ,name)))
    `(rg-toggle-off-face ((,c :inherit (shadow bold))))
    `(rg-toggle-on-face ((,c :inherit success)))
    `(rg-warning-face ((,c :inherit warning)))
;;;;; ripgrep
    `(ripgrep-context-face ((,c :inherit shadow)))
    `(ripgrep-error-face ((,c :inherit error)))
    `(ripgrep-hit-face ((,c :inherit success)))
    `(ripgrep-match-face ((,c :inherit match)))
;;;;; rmail
    `(rmail-header-name ((,c :inherit bold)))
    `(rmail-highlight ((,c :inherit bold :foreground ,mail-other)))
;;;;; ruler-mode
    `(ruler-mode-column-number ((,c :inherit ruler-mode-default)))
    `(ruler-mode-comment-column ((,c :inherit ruler-mode-default :foreground ,red)))
    `(ruler-mode-current-column ((,c :inherit ruler-mode-default :background ,bg-active :foreground ,fg-main)))
    `(ruler-mode-default ((,c :inherit default :background ,bg-dim :foreground ,fg-dim)))
    `(ruler-mode-fill-column ((,c :inherit ruler-mode-default :foreground ,green)))
    `(ruler-mode-fringes ((,c :inherit ruler-mode-default :foreground ,cyan)))
    `(ruler-mode-goal-column ((,c :inherit ruler-mode-default :foreground ,blue)))
    `(ruler-mode-margins ((,c :inherit ruler-mode-default :foreground ,bg-main)))
    `(ruler-mode-pad ((,c :inherit ruler-mode-default :background ,bg-inactive :foreground ,fg-dim)))
    `(ruler-mode-tab-stop ((,c :inherit ruler-mode-default :foreground ,yellow)))
;;;;; sesman
    `(sesman-browser-button-face ((,c :inherit button)))
    `(sesman-browser-highligh-face ((,c :inherit highlight)))
    `(sesman-buffer-face ((,c :foreground ,accent-1)))
    `(sesman-directory-face ((,c :inherit bold :foreground ,accent-0)))
    `(sesman-project-face ((,c :inherit bold :foreground ,accent-2)))
;;;;; shell-script-mode
    `(sh-heredoc ((,c :inherit font-lock-string-face)))
    `(sh-quoted-exec ((,c :inherit font-lock-builtin-face)))
;;;;; shortdoc
    `(shortdoc-heading ((,c :inherit bold)))
    `(shortdoc-section (())) ; remove the default's variable-pitch style
;;;;; show-paren-mode
    `(show-paren-match ((,c ,@(modus-themes--paren bg-paren-match
                                                   bg-paren-match-intense)
                            :foreground ,fg-main)))
    `(show-paren-match-expression ((,c :background ,bg-paren-expression)))
    `(show-paren-mismatch ((,c :inherit modus-themes-intense-red)))
;;;;; shr
    `(shr-abbreviation ((,c :inherit modus-themes-lang-note)))
    `(shr-code ((,c :inherit modus-themes-prose-verbatim)))
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
    `(skewer-error-face ((,c :inherit modus-themes-lang-error)))
;;;;; slime (sldb)
    `(sldb-condition-face ((,c :inherit font-lock-preprocessor-face)))
    `(sldb-restart-number-face ((,c :inherit bold)))
    `(sldb-restart-type-face ((,c :inherit font-lock-type-face)))
    `(sldb-restartable-frame-line-face ((,c :inherit success)))
    `(sldb-section-face ((,c :inherit bold)))
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
    `(sly-stickers-placed-face ((,c :background ,bg-inactive)))
    `(sly-style-warning-face ((,c :inherit modus-themes-lang-note)))
    `(sly-warning-face ((,c :inherit modus-themes-lang-warning)))
;;;;; smart-mode-line
    `(sml/charging ((,c :foreground ,success)))
    `(sml/discharging ((,c :foreground ,err)))
    `(sml/filename ((,c :inherit bold :foreground ,name)))
    `(sml/folder (( )))
    `(sml/git ((,c :inherit success)))
    `(sml/global (( )))
    `(sml/line-number ((,c :inherit sml/global)))
    `(sml/minor-modes ((,c :inherit sml/global)))
    `(sml/modes ((,c :inherit bold)))
    `(sml/modified ((,c :inherit italic)))
    `(sml/mule-info ((,c :inherit sml/global)))
    `(sml/name-filling ((,c :inherit warning)))
    `(sml/not-modified ((,c :inherit sml/global)))
    `(sml/numbers-separator ((,c :inherit sml/global)))
    `(sml/outside-modified ((,c :inherit modus-themes-intense-red)))
    `(sml/position-percentage ((,c :inherit sml/global)))
    `(sml/prefix ((,c :foreground ,fg-alt)))
    `(sml/process ((,c :inherit sml/prefix)))
    `(sml/projectile ((,c :inherit sml/git)))
    `(sml/read-only (( )))
    `(sml/remote ((,c :inherit sml/global)))
    `(sml/sudo ((,c :inherit warning)))
    `(sml/time ((,c :inherit sml/global)))
    `(sml/vc ((,c :inherit sml/git)))
    `(sml/vc-edited ((,c :inherit italic)))
;;;;; smerge
    `(smerge-base ((,c :inherit diff-changed)))
    `(smerge-lower ((,c :inherit diff-added)))
    `(smerge-markers ((,c :inherit diff-heading)))
    `(smerge-refined-added ((,c :inherit diff-refine-added)))
    `(smerge-refined-changed (()))
    `(smerge-refined-removed ((,c :inherit diff-refine-removed)))
    `(smerge-upper ((,c :inherit diff-removed)))
;;;;; speedbar
    `(speedbar-button-face ((,c :inherit button)))
    `(speedbar-directory-face ((,c :inherit bold :foreground ,accent-0)))
    `(speedbar-file-face ((,c :foreground ,fg-main)))
    `(speedbar-highlight-face ((,c :inherit highlight)))
    `(speedbar-selected-face ((,c :inherit modus-themes-mark-sel)))
    `(speedbar-separator-face ((,c :background ,bg-active :foreground ,fg-main)))
    `(speedbar-tag-face ((,c :foreground ,accent-1)))
;;;;; spell-fu
    `(spell-fu-incorrect-face ((,c :inherit modus-themes-lang-error)))
;;;;; stripes
    `(stripes ((,c :background ,bg-inactive)))
;;;;; suggest
    `(suggest-heading ((,c :inherit warning)))
;;;;; switch-window
    `(switch-window-background ((,c :background ,bg-inactive)))
    `(switch-window-label ((,c :height 3.0 :foreground ,red-intense)))
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
;;;;; symbol-overlay
    `(symbol-overlay-default-face ((,c :background ,bg-inactive)))
    `(symbol-overlay-face-1 ((,c :inherit modus-themes-intense-blue)))
    `(symbol-overlay-face-2 ((,c :inherit modus-themes-intense-magenta)))
    `(symbol-overlay-face-3 ((,c :inherit modus-themes-intense-yellow)))
    `(symbol-overlay-face-4 ((,c :inherit modus-themes-intense-magenta)))
    `(symbol-overlay-face-5 ((,c :inherit modus-themes-intense-red)))
    `(symbol-overlay-face-6 ((,c :inherit modus-themes-intense-red)))
    `(symbol-overlay-face-7 ((,c :inherit modus-themes-intense-cyan)))
    `(symbol-overlay-face-8 ((,c :inherit modus-themes-intense-cyan)))
;;;;; syslog-mode
    `(syslog-debug ((,c :inherit italic)))
    `(syslog-error ((,c :inherit error)))
    `(syslog-file ((,c :inherit bold :foreground ,name)))
    `(syslog-hide ((,c :background ,bg-main :foreground ,fg-main)))
    `(syslog-hour ((,c :inherit bold :foreground ,date)))
    `(syslog-info ((,c :inherit success)))
    `(syslog-ip ((,c :inherit bold :foreground ,name :underline t)))
    `(syslog-su ((,c :inherit error :underline t)))
    `(syslog-warn ((,c :inherit warning)))
;;;;; tab-bar-mode
    `(tab-bar ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-inactive)))
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
    `(table-cell ((,c :background ,bg-dim)))
;;;;; telega
    `(telega-button ((,c :box t :foreground ,link)))
    `(telega-button-active ((,c :box ,link :background ,link :foreground ,bg-main)))
    `(telega-button-highlight ((,c :inherit secondary-selection)))
    `(telega-chat-prompt ((,c :inherit modus-themes-prompt)))
    `(telega-entity-type-code ((,c :inherit modus-themes-prose-verbatim)))
    `(telega-entity-type-mention ((,c :foreground ,cyan)))
    `(telega-entity-type-pre ((,c :inherit modus-themes-prose-code)))
    `(telega-entity-type-spoiler ((,c :background ,fg-main :foreground ,fg-main)))
    `(telega-msg-heading ((,c :background ,bg-inactive)))
    `(telega-msg-self-title ((,c :inherit bold)))
    `(telega-root-heading ((,c :background ,bg-inactive)))
    `(telega-secret-title ((,c :foreground ,magenta-warmer)))
    `(telega-unmuted-count ((,c :foreground ,blue-cooler)))
    `(telega-user-online-status ((,c :foreground ,cyan)))
    `(telega-username ((,c :foreground ,cyan-cooler)))
    `(telega-webpage-chat-link ((,c :background ,bg-inactive)))
    `(telega-webpage-fixed ((,c :inherit modus-themes-fixed-pitch :height 0.85)))
    `(telega-webpage-header ((,c :inherit modus-themes-variable-pitch :height 1.3)))
    `(telega-webpage-preformatted ((,c :inherit modus-themes-fixed-pitch :background ,bg-inactive)))
    `(telega-webpage-subheader ((,c :inherit modus-themes-variable-pitch :height 1.15)))
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
    `(textsec-suspicious (( )))
;;;;; transient
    `(transient-active-infix ((,c :inherit highlight)))
    `(transient-amaranth ((,c :inherit bold :foreground ,yellow-warmer)))
    ;; Placate the compiler for what is a spurious warning.  We also
    ;; have to do this with `eldoc-highlight-function-argument'.
    (list 'transient-argument `((,c :inherit (bold modus-themes-subtle-cyan))))
    `(transient-blue ((,c :inherit bold :foreground ,blue)))
    `(transient-disabled-suffix ((,c :inherit modus-themes-intense-red)))
    `(transient-enabled-suffix ((,c :inherit modus-themes-subtle-cyan)))
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
    `(transient-value ((,c :inherit (bold modus-themes-subtle-yellow))))
;;;;; trashed
    `(trashed-deleted ((,c :inherit modus-themes-mark-del)))
    `(trashed-directory ((,c :foreground ,accent-0)))
    `(trashed-mark ((,c :inherit bold)))
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
;;;;; tty-menu
    `(tty-menu-disabled-face ((,c :background ,bg-inactive :foreground ,fg-dim)))
    `(tty-menu-enabled-face ((,c :inherit bold :background ,bg-inactive :foreground ,fg-main)))
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
    ;; `(tuareg-font-lock-extension-node-face ((,c :background ,bg-inactive :foreground ,magenta)))
    `(tuareg-font-lock-governing-face ((,c :inherit bold :foreground ,fg-main)))
    `(tuareg-font-lock-infix-extension-node-face ((,c :inherit font-lock-function-name-face)))
    `(tuareg-font-lock-interactive-directive-face ((,c :inherit font-lock-preprocessor-face)))
    `(tuareg-font-lock-interactive-error-face ((,c :inherit error)))
    `(tuareg-font-lock-interactive-output-face ((,c :inherit font-lock-constant-face)))
    `(tuareg-font-lock-label-face ((,c :inherit font-lock-type-face)))
    `(tuareg-font-lock-line-number-face ((,c :inherit shadow)))
    `(tuareg-font-lock-module-face ((,c :inherit font-lock-builtin-face)))
    ;; `(tuareg-font-lock-multistage-face ((,c :inherit bold :background ,bg-inactive :foreground ,blue)))
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
    `(vimish-fold-overlay ((,c :background ,bg-inactive)))
;;;;; visible-mark
    `(visible-mark-active ((,c :background ,bg-blue)))
    `(visible-mark-face1 ((,c :background ,bg-cyan)))
    `(visible-mark-face2 ((,c :background ,bg-yellow)))
    `(visible-mark-forward-face1 ((,c :background ,bg-magenta)))
    `(visible-mark-forward-face2 ((,c :background ,bg-green)))
;;;;; visual-regexp
    `(vr/group-0 ((,c :inherit modus-themes-intense-blue)))
    `(vr/group-1 ((,c :inherit modus-themes-intense-magenta)))
    `(vr/group-2 ((,c :inherit modus-themes-intense-green)))
    `(vr/match-0 ((,c :inherit modus-themes-intense-yellow)))
    `(vr/match-1 ((,c :inherit modus-themes-intense-yellow)))
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
    `(vterm-color-underline ((,c :underline t)))
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
    `(web-mode-css-variable-face ((,c :inherit font-lock-variable-name-face)))
    `(web-mode-current-column-highlight-face ((,c :background ,bg-inactive)))
    `(web-mode-current-element-highlight-face ((,c :inherit modus-themes-special-mild)))
    `(web-mode-doctype-face ((,c :inherit font-lock-doc-face)))
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
    `(web-mode-inlay-face ((,c :background ,bg-inactive)))
    `(web-mode-italic-face ((,c :inherit italic)))
    `(web-mode-javascript-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-javascript-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-json-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-json-context-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-json-key-face ((,c :foreground ,blue-faint)))
    `(web-mode-json-string-face ((,c :inherit web-mode-string-face)))
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
    `(web-mode-whitespace-face ((,c :background ,bg-inactive)))
;;;;; wgrep
    `(wgrep-delete-face ((,c :inherit warning)))
    `(wgrep-done-face ((,c :inherit success)))
    `(wgrep-face ((,c :inherit bold)))
    `(wgrep-file-face ((,c :foreground ,fg-alt)))
    `(wgrep-reject-face ((,c :inherit error)))
;;;;; which-function-mode
    `(which-func ((,c :inherit modus-themes-bold :foreground ,fnname)))
;;;;; which-key
    ;; TODO 2022-11-29: Review which-key
    `(which-key-command-description-face ((,c :foreground ,fg-main)))
    `(which-key-group-description-face ((,c :foreground ,magenta-warmer)))
    `(which-key-highlighted-command-face ((,c :foreground ,yellow :underline t)))
    `(which-key-key-face ((,c :inherit modus-themes-key-binding)))
    `(which-key-local-map-description-face ((,c :foreground ,fg-main)))
    `(which-key-note-face ((,c :inherit shadow)))
    `(which-key-separator-face ((,c :inherit shadow)))
    `(which-key-special-key-face ((,c :inherit bold :foreground ,red-warmer)))
;;;;; whitespace-mode
    `(whitespace-big-indent ((,c :inherit modus-themes-subtle-red)))
    `(whitespace-empty ((,c :inherit modus-themes-intense-magenta)))
    `(whitespace-hspace ((,c :background ,bg-inactive)))
    `(whitespace-indentation ((,c :background ,bg-inactive)))
    `(whitespace-line ((,c :inherit modus-themes-subtle-yellow)))
    `(whitespace-newline ((,c :background ,bg-inactive)))
    `(whitespace-space ((,c :background ,bg-inactive)))
    `(whitespace-space-after-tab ((,c :inherit modus-themes-subtle-magenta)))
    `(whitespace-space-before-tab ((,c :inherit modus-themes-subtle-cyan)))
    `(whitespace-tab ((,c :background ,bg-inactive)))
    `(whitespace-trailing ((,c :inherit modus-themes-intense-red)))
;;;;; window-divider-mode
    `(window-divider ((,c :foreground ,border)))
    `(window-divider-first-pixel ((,c :foreground "gray50")))
    `(window-divider-last-pixel ((,c :foreground "gray50")))
;;;;; widget
    `(widget-button ((,c :inherit bold :foreground ,link)))
    `(widget-button-pressed ((,c :inherit widget-buton :foreground ,link-alt)))
    `(widget-documentation ((,c :inherit font-lock-doc-face)))
    `(widget-field ((,c :background ,bg-inactive :foreground ,fg-main :extend nil)))
    `(widget-inactive ((,c :inherit shadow :background ,bg-dim)))
    `(widget-single-line-field ((,c :inherit widget-field)))
;;;;; writegood-mode
    `(writegood-duplicates-face ((,c :inherit modus-themes-lang-error)))
    `(writegood-passive-voice-face ((,c :inherit modus-themes-lang-warning)))
    `(writegood-weasels-face ((,c :inherit modus-themes-lang-warning)))
;;;;; woman
    `(woman-addition ((,c :foreground ,accent-2)))
    `(woman-bold ((,c :inherit bold :foreground ,accent-0)))
    `(woman-italic ((,c :inherit italic :foreground ,accent-1)))
    `(woman-unknown ((,c :foreground ,accent-3)))
;;;;; xah-elisp-mode
    `(xah-elisp-at-symbol ((,c :inherit font-lock-warning-face)))
    `(xah-elisp-cap-variable ((,c :inherit font-lock-preprocessor-face)))
    `(xah-elisp-command-face ((,c :inherit font-lock-type-face)))
    `(xah-elisp-dollar-symbol ((,c :inherit font-lock-variable-name-face)))
;;;;; yaml-mode
    `(yaml-tab-face ((,c :inherit modus-themes-intense-red)))
;;;;; yasnippet
    `(yas-field-highlight-face ((,c :inherit highlight))))
  "Face specs for use with `modus-themes-theme'.")

(defconst modus-themes-custom-variables
  '(
;;;; ansi-colors
    `(ansi-color-faces-vector [default bold shadow italic underline success warning error])
    `(ansi-color-names-vector ["gray35" ,red ,green ,yellow ,blue ,magenta ,cyan "gray65"])
;;;; chart
    `(chart-face-color-list
      '( ,red-graph-0-bg ,green-graph-0-bg ,yellow-graph-0-bg ,blue-graph-0-bg ,magenta-graph-0-bg ,cyan-graph-0-bg
         ,red-graph-1-bg ,green-graph-1-bg ,yellow-graph-1-bg ,blue-graph-1-bg ,magenta-graph-1-bg ,cyan-graph-1-bg))
;;;; exwm
    `(exwm-floating-border-color ,border)
;;;; flymake fringe indicators
    `(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-intense-red))
    `(flymake-warning-bitmap '(exclamation-mark modus-themes-intense-yellow))
    `(flymake-note-bitmap '(exclamation-mark modus-themes-intense-cyan))
;;;; highlight-changes
    `(highlight-changes-colors nil)
    `(highlight-changes-face-list '(success warning error bold bold-italic))
;;;; ibuffer
    `(ibuffer-deletion-face 'modus-themes-mark-del)
    `(ibuffer-filter-group-name-face 'bold)
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
        ("TEMP" . ,red-faint)
        ("FIXME" . ,red-cooler)
        ("XXX+" . ,red-warmer)
        ("REVIEW" . ,cyan-cooler)
        ("DEPRECATED" . ,blue-faint)))
;;;; pdf-tools
    `(pdf-view-midnight-colors '(,fg-main . ,bg-dim))
;;;; org-src-block-faces
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
