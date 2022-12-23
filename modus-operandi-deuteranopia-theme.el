;;; modus-operandi-deuteranopia-theme.el --- Elegant, highly legible and customizable light theme -*- lexical-binding:t -*-

;; Copyright (C) 2019-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Modus-Themes Development <~protesilaos/modus-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/modus-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/modus-themes
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

  (deftheme modus-operandi-deuteranopia
    "Elegant, highly legible and customizable light theme.
This variant is optimized for users with red-green color
deficiency (deuteranopia).  It conforms with the highest
legibility standard for color contrast between background and
foreground in any given piece of text, which corresponds to a
minimum contrast in relative luminance of 7:1 (WCAG AAA
standard).")

  (defconst modus-operandi-deuteranopia-palette
    '(
;;; Basic values

      (bg-main          "#ffffff")
      (bg-dim           "#f0f0f0")
      (fg-main          "#000000")
      (fg-dim           "#595959")
      (fg-alt           "#193668")
      (bg-active        "#c4c4c4")
      (bg-inactive      "#e0e0e0")
      (border           "#9f9f9f")

;;; Common accent foregrounds

      (red             "#a60000")
      (red-warmer      "#972500")
      (red-cooler      "#a0132f")
      (red-faint       "#7f0000")
      (red-intense     "#d00000")
      (green           "#006800")
      (green-warmer    "#316500")
      (green-cooler    "#00663f")
      (green-faint     "#2a5045")
      (green-intense   "#008900")
      (yellow          "#7b5000")
      (yellow-warmer   "#884900")
      (yellow-cooler   "#7a4f2f")
      (yellow-faint    "#624416")
      (yellow-intense  "#808000")
      (blue            "#0031a9")
      (blue-warmer     "#354fcf")
      (blue-cooler     "#0000b0")
      (blue-faint      "#003497")
      (blue-intense    "#0000ff")
      (magenta         "#721045")
      (magenta-warmer  "#8f0075")
      (magenta-cooler  "#531ab6")
      (magenta-faint   "#7c318f")
      (magenta-intense "#dd22dd")
      (cyan            "#005e8b")
      (cyan-warmer     "#3f578f")
      (cyan-cooler     "#005f5f")
      (cyan-faint      "#005077")
      (cyan-intense    "#008899")

;;; Uncommon accent foregrounds

      (amaranth   "#d3303a")
      (amber      "#a45f22")
      (turquoise  "#008858")
      (azure      "#375cd8")
      (purple     "#6052cf")
      (amethyst   "#ba35af")
      (rose       "#d50f7f")
      (rust       "#8a290f")
      (gold       "#80601f")
      (olive      "#56692d")
      (slate      "#2f3f83")
      (indigo     "#4a3a8a")
      (maroon     "#731c52")
      (pink       "#7b435c")

;;; Common accent backgrounds

      (bg-red      "#ff8f88")
      (bg-green    "#8adf80")
      (bg-yellow   "#f3d000")
      (bg-blue     "#bfc9ff")
      (bg-magenta  "#dfa0f0")
      (bg-cyan     "#a4d5f9")

      (bg-red-subtle      "#ffcfbf")
      (bg-green-subtle    "#b3fabf")
      (bg-yellow-subtle   "#fff576")
      (bg-blue-subtle     "#ccdfff")
      (bg-magenta-subtle  "#ffddff")
      (bg-cyan-subtle     "#bfefff")

      (bg-red-nuanced     "#fff1f0")
      (bg-green-nuanced   "#ecf7ed")
      (bg-yellow-nuanced  "#fff3da")
      (bg-blue-nuanced    "#f3f3ff")
      (bg-magenta-nuanced "#fdf0ff")
      (bg-cyan-nuanced    "#ebf6fa")

;;; Graphs

      (bg-graph-red-0     "#b0b029")
      (bg-graph-red-1     "#e0cab4")
      (bg-graph-green-0   "#90b7c0")
      (bg-graph-green-1   "#a3dfe5")
      (bg-graph-yellow-0  "#ffcf00")
      (bg-graph-yellow-1  "#f9ff00")
      (bg-graph-blue-0    "#7f9fff")
      (bg-graph-blue-1    "#9fc6ff")
      (bg-graph-magenta-0 "#b0b0d0")
      (bg-graph-magenta-1 "#d0dfdf")
      (bg-graph-cyan-0    "#6faad9")
      (bg-graph-cyan-1    "#bfe0ff")

;;; Special purpose

      (bg-completion       "#c0deff")
      (bg-hover            "#94d4ff")
      (bg-hover-secondary  "#f5d0a0")
      (bg-hl-line          "#d0d6ec")
      (bg-paren-match      "#5fcfff")
      (bg-paren-expression "#efd3f5")
      (bg-region           "#bcbcbc")
      (bg-region-subtle    "#f0e0cc")
      (bg-prompt           "#ebc580")

      (bg-char-0 "#7feaff")
      (bg-char-1 "#ffaaff")
      (bg-char-2 "#dff000")

      (bg-mode-line-active        "#c3c3c3")
      (fg-mode-line-active        "#000000")
      (border-mode-line-active    "#545454")
      (bg-mode-line-inactive      "#e1e1e1")
      (fg-mode-line-inactive      "#585858")
      (border-mode-line-inactive  "#a0a0a0")

      (bg-tab-bar      "#dfdfdf")
      (bg-tab-current  "#ffffff")
      (bg-tab-other    "#c2c2c2")

;;; Diffs

      (bg-added           "#d5d5ff")
      (bg-added-faint     "#e4e4ff")
      (bg-added-refine    "#b5b5ef")
      (bg-added-intense   "#579acc")
      (fg-added           "#333399")

      (bg-changed         "#eecfdf")
      (bg-changed-faint   "#f0dde5")
      (bg-changed-refine  "#e0b0d0")
      (bg-changed-intense "#9f7abf")
      (fg-changed         "#6f1343")

      (bg-removed         "#fff588")
      (bg-removed-faint   "#f2f2bb")
      (bg-removed-refine  "#f0e068")
      (bg-removed-intense "#d7c20a")
      (fg-removed         "#553d00")

;;;; Mappings

      (fringe bg-inactive)
      (cursor fg-main)
      (builtin magenta-warmer)
      (comment yellow)
      (constant blue-cooler)
      (docstring green-faint)
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

      (accent-0 blue)
      (accent-1 yellow-warmer)
      (accent-2 cyan)
      (accent-3 magenta-cooler)
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

      (bg-link unspecified)
      (fg-link blue-warmer)
      (underline-link t)
      (bg-link-symbolic unspecified)
      (fg-link-symbolic cyan)
      (underline-link-symbolic t)
      (bg-link-visited unspecified)
      (fg-link-visited yellow-faint)
      (underline-link-visited t)

      (mail-cite-0 blue-warmer)
      (mail-cite-1 yellow)
      (mail-cite-2 blue-cooler)
      (mail-cite-3 yellow-faint)
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
      (heading-3 fg-alt)
      (heading-4 magenta)
      (heading-5 green-faint)
      (heading-6 red-faint)
      (heading-7 cyan-warmer)
      (heading-8 fg-dim))
    "The entire palette of the `modus-operandi-deuteranopia' theme.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defcustom modus-operandi-deuteranopia-palette-overrides nil
    "Overrides for `modus-operandi-deuteranopia-palette'.

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

  (modus-themes-theme modus-operandi-deuteranopia
                      modus-operandi-deuteranopia-palette
                      modus-operandi-deuteranopia-palette-overrides)

  (provide-theme 'modus-operandi-deuteranopia))

;;;###theme-autoload
(put 'modus-operandi-deuteranopia 'theme-properties '(:background-mode light :kind color-scheme :family modus))

;;; modus-operandi-deuteranopia-theme.el ends here
