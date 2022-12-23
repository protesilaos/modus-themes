;;; modus-vivendi-deuteranopia-theme.el --- Elegant, highly legible and customizable dark theme -*- lexical-binding:t -*-

;; Copyright (C) 2019-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Modus-Themes Development <~protesilaos/modus-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/modus-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/modus-themes

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
;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  Please refer to the official Info manual for
;; further documentation (distributed with the themes, or available
;; at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:



(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'modus-themes t))
    (require 'modus-themes))

  (deftheme modus-vivendi-deuteranopia
    "Elegant, highly legible and customizable dark theme.
This variant is optimized for users with red-green color
deficiency (deuteranopia).  It conforms with the highest
legibility standard for color contrast between background and
foreground in any given piece of text, which corresponds to a
minimum contrast in relative luminance of 7:1 (WCAG AAA
standard).")

  (defconst modus-vivendi-deuteranopia-palette
    '(
;;; Basic values

      (bg-main          "#000000")
      (bg-dim           "#1e1e1e")
      (fg-main          "#ffffff")
      (fg-dim           "#989898")
      (fg-alt           "#c6daff")
      (bg-active        "#535353")
      (bg-inactive      "#303030")
      (border           "#646464")

;;; Common accent foregrounds

      (red             "#ff5f59")
      (red-warmer      "#ff6b55")
      (red-cooler      "#ff7f9f")
      (red-faint       "#ff9580")
      (red-intense     "#ff5f5f")
      (green           "#44bc44")
      (green-warmer    "#70b900")
      (green-cooler    "#00c06f")
      (green-faint     "#88ca9f")
      (green-intense   "#44df44")
      (yellow          "#d0bc00")
      (yellow-warmer   "#fec43f")
      (yellow-cooler   "#dfaf7a")
      (yellow-faint    "#d2b580")
      (yellow-intense  "#efef00")
      (blue            "#2fafff")
      (blue-warmer     "#79a8ff")
      (blue-cooler     "#00bcff")
      (blue-faint      "#82b0ec")
      (blue-intense    "#338fff")
      (magenta         "#feacd0")
      (magenta-warmer  "#f78fe7")
      (magenta-cooler  "#b6a0ff")
      (magenta-faint   "#caa6df")
      (magenta-intense "#ff66ff")
      (cyan            "#00d3d0")
      (cyan-warmer     "#4ae2f0")
      (cyan-cooler     "#6ae4b9")
      (cyan-faint      "#9ac8e0")
      (cyan-intense    "#00eff0")

;;; Uncommon accent foregrounds

      (amaranth   "#f0636f")
      (amber      "#df7f3f")
      (turquoise  "#43c39f")
      (azure      "#5faaff")
      (purple     "#af89ff")
      (amethyst   "#e085ef")
      (rose       "#ef69af")
      (rust       "#cf6f5c")
      (gold       "#bf8f5b")
      (olive      "#8ab960")
      (slate      "#669fbf")
      (indigo     "#9080d9")
      (maroon     "#cf7aa7")
      (pink       "#e0a5b4")

;;; Common accent backgrounds

      (bg-red      "#9d1f1f")
      (bg-green    "#2f822f")
      (bg-yellow   "#7a6100")
      (bg-blue     "#1640b0")
      (bg-magenta  "#7030af")
      (bg-cyan     "#2266ae")

      (bg-red-subtle      "#620f2a")
      (bg-green-subtle    "#00422a")
      (bg-yellow-subtle   "#4a4000")
      (bg-blue-subtle     "#242679")
      (bg-magenta-subtle  "#552f5f")
      (bg-cyan-subtle     "#004065")

;;; Graphs

      (bg-graph-red-0     "#705c3c")
      (bg-graph-red-1     "#504420")
      (bg-graph-green-0   "#4f666f")
      (bg-graph-green-1   "#204840")
      (bg-graph-yellow-0  "#c1c00a")
      (bg-graph-yellow-1  "#6f6f00")
      (bg-graph-blue-0    "#2fafef")
      (bg-graph-blue-1    "#1f2f8f")
      (bg-graph-magenta-0 "#7f7f8e")
      (bg-graph-magenta-1 "#4f4f5f")
      (bg-graph-cyan-0    "#376f9a")
      (bg-graph-cyan-1    "#00404f")

;;; Special purpose

      (bg-completion       "#2f447f")
      (bg-hover            "#004f70")
      (bg-hover-secondary  "#654a39")
      (bg-hl-line          "#2f3849")
      (bg-paren-match      "#2f7f9f")
      (bg-paren-expression "#453040")
      (bg-region           "#5c5c5c")
      (bg-region-subtle    "#4f1c2f")
      (bg-prompt           "#5f3f20")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#625a00")

      (bg-mode-line-active        "#525252")
      (fg-mode-line-active        "#ffffff")
      (border-mode-line-active    "#979797")
      (bg-mode-line-inactive      "#2f2f2f")
      (fg-mode-line-inactive      "#969696")
      (border-mode-line-inactive  "#636363")

      (bg-tab-bar      "#313131")
      (bg-tab-current  "#000000")
      (bg-tab-other    "#545454")

;;; Diffs

      (bg-added           "#003366")
      (bg-added-faint     "#001f4f")
      (bg-added-refine    "#0f4f7f")
      (bg-added-intense   "#0f4fbf")
      (fg-added           "#c4d5ff")

      (bg-changed         "#2f123f")
      (bg-changed-faint   "#1f022f")
      (bg-changed-refine  "#3f325f")
      (bg-changed-intense "#7f42af")
      (fg-changed         "#e3cfff")

      (bg-removed         "#3f3f00")
      (bg-removed-faint   "#281f00")
      (bg-removed-refine  "#535300")
      (bg-removed-intense "#dfd23a")
      (fg-removed         "#d4d48f")

;;;; Mappings

      (fringe bg-inactive)
      (cursor fg-main)
      (builtin magenta-warmer)
      (comment yellow-cooler)
      (constant blue-cooler)
      (docstring cyan-faint)
      (docmarkup magenta-faint)
      (fnname magenta)
      (keyword magenta-cooler)
      (preprocessor red-cooler)
      (string blue-warmer)
      (type cyan-cooler)
      (variable cyan)
      (rx-construct yellow-cooler)
      (rx-backslash blue-cooler)

      (keybind blue-cooler)
      (name blue-cooler)
      (identifier yellow-faint)
      (prompt cyan-cooler)

      (err amber)
      (warning yellow-cooler)
      (info blue)

      (underline-err yellow-intense)
      (underline-warning amethyst)
      (underline-note cyan-intense)

      (accent-0 blue-cooler)
      (accent-1 yellow)
      (accent-2 cyan-cooler)
      (accent-3 magenta-warmer)
      (bg-accent-0 bg-blue-subtle)
      (bg-accent-1 bg-yellow-subtle)
      (bg-accent-2 bg-cyan-subtle)
      (bg-accent-3 bg-magenta-subtle)

      (bg-button-active bg-active)
      (fg-button-active fg-main)
      (bg-button-inactive bg-dim)
      (fg-button-inactive fg-dim)

      (date-common cyan)
      (date-deadline yellow-warmer)
      (date-event fg-alt)
      (date-holiday magenta)
      (date-scheduled yellow-cooler)
      (date-weekend blue)

      (bg-line-number-inactive bg-dim)
      (fg-line-number-inactive fg-dim)
      (bg-line-number-active bg-active)
      (fg-line-number-active fg-main)

      (link blue-warmer)
      (link-underline blue-warmer)
      (link-symbolic cyan)
      (link-symbolic-underline cyan)
      (link-visited yellow-faint)
      (link-visited-underline yellow-faint)

      (mail-cite-0 blue-warmer)
      (mail-cite-1 yellow-cooler)
      (mail-cite-2 blue-cooler)
      (mail-cite-3 yellow)
      (mail-part blue)
      (mail-recipient blue)
      (mail-subject yellow-warmer)
      (mail-other cyan-faint)

      (prose-block fg-dim)
      (prose-code yellow)
      (prose-done blue)
      (prose-macro magenta-cooler)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-tag magenta-faint)
      (prose-todo yellow-warmer)
      (prose-verbatim magenta-warmer)

      (heading-0 cyan-cooler)
      (heading-1 fg-main)
      (heading-2 yellow-faint)
      (heading-3 blue-faint)
      (heading-4 magenta)
      (heading-5 green-faint)
      (heading-6 red-faint)
      (heading-7 cyan-faint)
      (heading-8 fg-dim))
    "The entire palette of the `modus-vivendi-deuteranopia' theme.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defcustom modus-vivendi-deuteranopia-palette-overrides nil
    "Overrides for `modus-vivendi-deuteranopia-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Modus themes,
refer to `modus-themes-common-palette-overrides'.

Theme-specific overrides take precedence over shared overrides.
The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Wherea theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
    :group 'modus-themes
    :package-version '(modus-themes . "4.0.0")
    :version "30.1"
    :type '(repeat (list symbol (choice symbol string))) ; TODO 2022-12-18: Refine overrides' :type
    :set #'modus-themes--set-option
    :initialize #'custom-initialize-default
    :link '(info-link "(modus-themes) Palette overrides"))

  (modus-themes-theme modus-vivendi-deuteranopia
                      modus-vivendi-deuteranopia-palette
                      modus-vivendi-deuteranopia-palette-overrides)

  (provide-theme 'modus-vivendi-deuteranopia))

;;;###theme-autoload
(put 'modus-vivendi-deuteranopia 'theme-properties '(:background-mode dark :kind color-scheme :family modus))

;;; modus-vivendi-deuteranopia-theme.el ends here
