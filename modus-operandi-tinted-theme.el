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
      (magenta-faint   "#6f3f7f")
      (magenta-intense "#dd22dd")
      (cyan            "#005e8b")
      (cyan-warmer     "#3f578f")
      (cyan-cooler     "#005f5f")
      (cyan-faint      "#005077")
      (cyan-intense    "#008899")

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

;;; Diffs

      (bg-added           "#c1f2d1")
      (bg-added-faint     "#d8f8e1")
      (bg-added-refine    "#aee5be")
      (bg-added-intense   "#8cca8c")
      (fg-added           "#005000")

      (bg-changed         "#ffdfa9")
      (bg-changed-faint   "#ffefbf")
      (bg-changed-refine  "#fac090")
      (bg-changed-intense "#d7c20a")
      (fg-changed         "#553d00")

      (bg-removed         "#ffd8d5")
      (bg-removed-faint   "#ffe9e9")
      (bg-removed-refine  "#f3b5af")
      (bg-removed-intense "#d84a4f")
      (fg-removed         "#8f1313")

;;;; Mappings

      (bg-mode-line-active        bg-active)
      (fg-mode-line-active        fg-main)
      (border-mode-line-active    fg-dim)
      (bg-mode-line-inactive      bg-inactive)
      (fg-mode-line-inactive      fg-dim)
      (border-mode-line-inactive  border)

      (fringe bg-inactive)
      (cursor red)
      (builtin magenta-warmer)
      (comment red-faint)
      (constant blue-cooler)
      (docstring green-faint)
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
      (identifier yellow-cooler)
      (tag magenta-faint)
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

      (mail-cite-0 blue-faint)
      (mail-cite-1 yellow-warmer)
      (mail-cite-2 cyan-cooler)
      (mail-cite-3 red-cooler)
      (mail-part cyan)
      (mail-recipient magenta-cooler)
      (mail-subject magenta-warmer)
      (mail-other magenta-faint)

      (heading-0 cyan-cooler)
      (heading-1 fg-main)
      (heading-2 yellow-faint)
      (heading-3 fg-alt)
      (heading-4 magenta)
      (heading-5 green-faint)
      (heading-6 red-faint)
      (heading-7 cyan-warmer)
      (heading-8 fg-dim)

      (prose-code green-cooler)
      (prose-macro magenta-cooler)
      (prose-verbatim magenta-warmer))
    "The entire palette of the `modus-operandi-tinted' theme.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defvar modus-operandi-tinted-palette-overrides nil
    "Overrides for `modus-operandi-tinted-palette'.")

  (modus-themes-theme modus-operandi-tinted modus-operandi-tinted-palette modus-operandi-tinted-palette-overrides)

  (provide-theme 'modus-operandi-tinted))

;;;###theme-autoload
(put 'modus-operandi-tinted 'theme-properties '(:background-mode light :kind color-scheme :family modus))

;;; modus-operandi-tinted-theme.el ends here
