;;; modus-vivendi-theme.el --- Elegant, highly legible and customizable dark theme -*- lexical-binding:t -*-

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

  (deftheme modus-vivendi
    "Elegant, highly legible and customizable dark theme.
Conforms with the highest legibility standard for color contrast
between background and foreground in any given piece of text,
which corresponds to a minimum contrast in relative luminance of
7:1 (WCAG AAA standard).")

  (defconst modus-vivendi-palette
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
      (magenta-faint   "#cab2df")
      (magenta-intense "#ff66ff")
      (cyan            "#00d3d0")
      (cyan-warmer     "#4ae2f0")
      (cyan-cooler     "#6ae4b9")
      (cyan-faint      "#9ac8e0")
      (cyan-intense    "#00eff0")

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

      (bg-graph-red-0     "#b52c2c")
      (bg-graph-red-1     "#702020")
      (bg-graph-green-0   "#4fd100")
      (bg-graph-green-1   "#007800")
      (bg-graph-yellow-0  "#f1e00a")
      (bg-graph-yellow-1  "#b08600")
      (bg-graph-blue-0    "#2fafef")
      (bg-graph-blue-1    "#1f2f8f")
      (bg-graph-magenta-0 "#bf94fe")
      (bg-graph-magenta-1 "#5f509f")
      (bg-graph-cyan-0    "#47dfea")
      (bg-graph-cyan-1    "#00808f")

;;; Special purpose

      (bg-completion       "#2f447f")
      (bg-hover            "#004f70")
      (bg-hover-secondary  "#654a39")
      (bg-hl-line          "#2f3849")
      (bg-paren-match      "#2f7f9f")
      (bg-paren-expression "#453040")
      (bg-region           "#5c5c5c")
      (bg-region-subtle    "#4f1c2f")
      (bg-prompt           "#5f3a60")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#625a00")

;;; Diffs

      (bg-added           "#00381f")
      (bg-added-faint     "#002910")
      (bg-added-refine    "#034f2f")
      (bg-added-intense   "#237f3f")
      (fg-added           "#a0e0a0")

      (bg-changed         "#363300")
      (bg-changed-faint   "#2a1f00")
      (bg-changed-refine  "#4a4a00")
      (bg-changed-intense "#8a7a00")
      (fg-changed         "#efef80")

      (bg-removed         "#4f1119")
      (bg-removed-faint   "#380a0f")
      (bg-removed-refine  "#781a1f")
      (bg-removed-intense "#b81a1f")
      (fg-removed         "#ffbfbf")

;;;; Mappings

      (bg-mode-line-active        bg-active)
      (fg-mode-line-active        fg-main)
      (border-mode-line-active    fg-dim)
      (bg-mode-line-inactive      bg-inactive)
      (fg-mode-line-inactive      fg-dim)
      (border-mode-line-inactive  border)

      (fringe bg-inactive)
      (cursor fg-main)
      (builtin magenta-warmer)
      (comment fg-dim)
      (constant blue-cooler)
      (docstring cyan-faint)
      (fnname magenta)
      (keyword magenta-cooler)
      (preprocessor red-cooler)
      (string blue-warmer)
      (type cyan-cooler)
      (variable cyan)
      (rx-construct green-cooler)
      (rx-backslash magenta)

      (date cyan)
      (weekend red-faint) ; for M-x calendar and Org agenda
      (keybind blue-cooler)
      (link blue-warmer)
      (link-symbolic cyan)
      (link-visited magenta)
      (name magenta)
      (identifier yellow-faint)
      (tag magenta-faint)
      (prompt cyan-cooler)

      (err red)
      (warning yellow-warmer)
      (info green)

      (underline-err red-intense)
      (underline-warning yellow-intense)
      (underline-note cyan-intense)

      (accent-0 blue-cooler)
      (accent-1 magenta-warmer)
      (accent-2 cyan-cooler)
      (accent-3 yellow)
      (bg-accent-0 bg-blue-subtle)
      (bg-accent-1 bg-magenta-subtle)
      (bg-accent-2 bg-cyan-subtle)
      (bg-accent-3 bg-yellow-subtle)

      (mail-cite-0 blue-warmer)
      (mail-cite-1 yellow-cooler)
      (mail-cite-2 cyan-cooler)
      (mail-cite-3 red-cooler)
      (mail-part blue)
      (mail-recipient magenta-cooler)
      (mail-subject magenta-warmer)
      (mail-other magenta-faint)

      (heading-0 cyan-cooler)
      (heading-1 fg-main)
      (heading-2 yellow-faint)
      (heading-3 blue-faint)
      (heading-4 magenta)
      (heading-5 green-faint)
      (heading-6 red-faint)
      (heading-7 cyan-faint)
      (heading-8 fg-dim)

      (prose-code cyan-cooler)
      (prose-macro magenta-cooler)
      (prose-verbatim magenta-warmer))
    "The entire palette of the `modus-vivendi' theme.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defcustom modus-vivendi-palette-overrides nil
    "Overrides for `modus-vivendi-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Modus themes,
refer to `modus-themes-common-palette-overrides'."
    :group 'modus-themes
    :package-version '(modus-themes . "4.0.0")
    :version "30.1"
    :type '(repeat (list symbol (choice symbol string))) ; TODO 2022-12-18: Refine overrides' :type
    :set #'modus-themes--set-option
    :initialize #'custom-initialize-default
    :link '(info-link "(modus-themes) Palette overrides"))

  (modus-themes-theme modus-vivendi
                      modus-vivendi-palette
                      modus-vivendi-palette-overrides)

  (provide-theme 'modus-vivendi))

;;;###theme-autoload
(put 'modus-vivendi 'theme-properties '(:background-mode dark :kind color-scheme :family modus))

;;; modus-vivendi-theme.el ends here
