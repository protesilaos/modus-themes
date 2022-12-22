;;; modus-operandi-tinted-theme.el --- Elegant, highly legible and customizable light theme -*- lexical-binding:t -*-

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

  (deftheme modus-operandi-tinted
    "Elegant, highly legible and customizable light theme.
Conforms with the highest legibility standard for color contrast
between background and foreground in any given piece of text,
which corresponds to a minimum contrast in relative luminance of
7:1 (WCAG AAA standard).")

  (defconst modus-operandi-tinted-palette
    '(
;;; Basic values

      (bg-main          "#fbf7f0")
      (bg-dim           "#ede7db")
      (fg-main          "#000000")
      (fg-dim           "#595959")
      (fg-alt           "#193668")
      (bg-active        "#c9b9b0")
      (bg-inactive      "#dfd5cf")
      (border           "#9f9690")

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
      (yellow          "#6f5500")
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

;;; Graphs

      (bg-graph-red-0     "#ef7969")
      (bg-graph-red-1     "#ffaab4")
      (bg-graph-green-0   "#4faa09")
      (bg-graph-green-1   "#8fef00")
      (bg-graph-yellow-0  "#ffcf00")
      (bg-graph-yellow-1  "#f9ff00")
      (bg-graph-blue-0    "#7090ff")
      (bg-graph-blue-1    "#9fc6ff")
      (bg-graph-magenta-0 "#e07fff")
      (bg-graph-magenta-1 "#fad0ff")
      (bg-graph-cyan-0    "#70d3f0")
      (bg-graph-cyan-1    "#afefff")

;;; Special purpose

      (bg-completion       "#f0c1cf")
      (bg-hover            "#94d4ff")
      (bg-hover-secondary  "#f5d0a0")
      (bg-hl-line          "#f1d5d0")
      (bg-paren-match      "#7fdfcf")
      (bg-paren-expression "#efd3f5")
      (bg-region           "#c2bcb5")
      (bg-region-subtle    "#d9f2c0")
      (bg-prompt           "#dbc5f0")

      (bg-char-0 "#7feaff")
      (bg-char-1 "#ffaaff")
      (bg-char-2 "#dff000")

      (bg-mode-line-active        "#c9b8b1")
      (fg-mode-line-active        "#000000")
      (border-mode-line-active    "#545454")
      (bg-mode-line-inactive      "#dfd6cd")
      (fg-mode-line-inactive      "#585858")
      (border-mode-line-inactive  "#9f9790")

      (bg-tab-bar      "#e0d4ce")
      (bg-tab-current  "#fbf7f0")
      (bg-tab-other    "#c8b8b2")

;;; Diffs

      (bg-added           "#c3ebc1")
      (bg-added-faint     "#dcf8d1")
      (bg-added-refine    "#acd6a5")
      (bg-added-intense   "#8cca8c")
      (fg-added           "#005000")

      (bg-changed         "#ffdfa9")
      (bg-changed-faint   "#ffefbf")
      (bg-changed-refine  "#fac090")
      (bg-changed-intense "#d7c20a")
      (fg-changed         "#553d00")

      (bg-removed         "#f4d0cf")
      (bg-removed-faint   "#ffe9e5")
      (bg-removed-refine  "#f3b5a7")
      (bg-removed-intense "#d84a4f")
      (fg-removed         "#8f1313")

;;;; Mappings

      (fringe bg-inactive)
      (cursor red)
      (builtin magenta-warmer)
      (comment red-faint)
      (constant blue-cooler)
      (docstring green-faint)
      (docmarkup magenta-faint)
      (fnname magenta)
      (keyword magenta-cooler)
      (preprocessor red-cooler)
      (string blue-warmer)
      (type cyan-cooler)
      (variable cyan)
      (rx-construct green-cooler)
      (rx-backslash magenta)

      (keybind blue-cooler)
      (link blue-warmer)
      (link-symbolic cyan)
      (link-visited magenta)
      (name magenta)
      (identifier yellow-cooler)
      (prompt cyan-cooler)

      (err red)
      (warning yellow-warmer)
      (info green)

      (underline-err red-intense)
      (underline-warning yellow-intense)
      (underline-note cyan-intense)

      (accent-0 blue)
      (accent-1 magenta-warmer)
      (accent-2 cyan)
      (accent-3 red)
      (bg-accent-0 bg-blue-subtle)
      (bg-accent-1 bg-magenta-subtle)
      (bg-accent-2 bg-cyan-subtle)
      (bg-accent-3 bg-red-subtle)

      (bg-button-active bg-active)
      (fg-button-active fg-main)
      (bg-button-inactive bg-dim)
      (fg-button-inactive fg-dim)

      (date-common cyan)
      (date-deadline red)
      (date-event fg-alt)
      (date-holiday magenta)
      (date-scheduled yellow-warmer)
      (date-weekend red-faint)

      (bg-line-number-inactive bg-dim)
      (fg-line-number-inactive fg-dim)
      (bg-line-number-active bg-active)
      (fg-line-number-active fg-main)

      (mail-cite-0 blue-faint)
      (mail-cite-1 yellow-warmer)
      (mail-cite-2 cyan-cooler)
      (mail-cite-3 red-cooler)
      (mail-part cyan)
      (mail-recipient magenta-cooler)
      (mail-subject magenta-warmer)
      (mail-other magenta-faint)

      (prose-block fg-dim)
      (prose-code green-cooler)
      (prose-done green)
      (prose-macro magenta-cooler)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-tag magenta-faint)
      (prose-todo red)
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
    "The entire palette of the `modus-operandi-tinted' theme.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defcustom modus-operandi-tinted-palette-overrides nil
    "Overrides for `modus-operandi-tinted-palette'.

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

  (modus-themes-theme modus-operandi-tinted
                      modus-operandi-tinted-palette
                      modus-operandi-tinted-palette-overrides)

  (provide-theme 'modus-operandi-tinted))

;;;###theme-autoload
(put 'modus-operandi-tinted 'theme-properties '(:background-mode light :kind color-scheme :family modus))

;;; modus-operandi-tinted-theme.el ends here
