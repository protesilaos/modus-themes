;;; modus-operandi-theme.el --- Elegant, highly legible and customizable light theme -*- lexical-binding:t -*-

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

  (deftheme modus-operandi
    "Elegant, highly legible and customizable light theme.
Conforms with the highest legibility standard for color contrast
between background and foreground in any given piece of text,
which corresponds to a minimum contrast in relative luminance of
7:1 (WCAG AAA standard).")

  (defconst modus-operandi-palette
    '(
;;; Basic values

      (bg-main          "#ffffff")
      (bg-dim           "#f0f0f0")
      (fg-main          "#000000")
      (fg-dim           "#585858")
      (fg-alt           "#193668")
      (bg-active        "#c4c4c4")
      (bg-inactive      "#e0e0e0")
      (bg-active-accent "#bbc3ff")
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
      (bg-yellow   "#fac200")
      (bg-blue     "#cbcfff")
      (bg-magenta  "#df8fff")
      (bg-cyan     "#88c8ff")

      (bg-red-subtle      "#ffcfbf")
      (bg-green-subtle    "#b3fabf")
      (bg-yellow-subtle   "#fff576")
      (bg-blue-subtle     "#ccdfff")
      (bg-magenta-subtle  "#ffddff")
      (bg-cyan-subtle     "#bfefff")

;;; Graphs

      (red-graph-0-bg     "#ef7969")
      (red-graph-1-bg     "#ffaab4")
      (green-graph-0-bg   "#4faa09")
      (green-graph-1-bg   "#8fef00")
      (yellow-graph-0-bg  "#ffcf00")
      (yellow-graph-1-bg  "#f9ff00")
      (blue-graph-0-bg    "#7090ff")
      (blue-graph-1-bg    "#9fc6ff")
      (magenta-graph-0-bg "#e07fff")
      (magenta-graph-1-bg "#fad0ff")
      (cyan-graph-0-bg    "#70d3f0")
      (cyan-graph-1-bg    "#afefff")

;;; Special purpose

      (bg-completion       "#baeaff")
      (bg-hover            "#94d4ff")
      (bg-hover-secondary  "#f5d0a0")
      (bg-hl-line          "#d0d6ec")
      (bg-paren-match      "#5fcfff")
      (bg-paren-expression "#efd3f5")
      (bg-region           "#bcbcbc")
      (bg-region-subtle    "#f0e0cc")
      (bg-prompt           "#bbe5d0")

      (bg-char-0 "#7feaff")
      (bg-char-1 "#ffaaff")
      (bg-char-2 "#dff000")

;;; Diffs

      (bg-added          "#c1f2d1")
      (bg-added-faint    "#d8f8e1")
      (bg-added-refine   "#b1e4c1")
      (bg-added-intense  "#8cca8c")
      (fg-added          "#005000")

      (bg-added-deuteran         "#dbdbff")
      (bg-added-faint-deuteran   "#e4e4ff")
      (bg-added-refine-deuteran  "#c0c0ef")
      (bg-added-intense-deuteran "#8fcfff")
      (fg-added-deuteran         "#303399")

      (bg-changed         "#ffdfa9")
      (bg-changed-faint   "#ffefbf")
      (bg-changed-refine  "#fac090")
      (bg-changed-intense "#dfd23a")
      (fg-changed         "#553d00")

      (bg-removed         "#ffd8d5")
      (bg-removed-faint   "#ffe9e9")
      (bg-removed-refine  "#f3b5af")
      (bg-removed-intense "#d84a4f")
      (fg-removed         "#8f1313")

;;;; Mappings

      (cursor fg-main)
      (builtin magenta-warmer)
      (comment fg-dim)
      (constant blue-cooler)
      (docstring green-faint)
      (fnname magenta)
      (keyword magenta-cooler)
      (preprocessor red-cooler)
      (string blue-warmer)
      (type cyan-cooler)
      (variable cyan)

      (date cyan)
      (weekend red-faint) ; for M-x calendar and Org agenda
      (keybind blue-cooler)
      (link blue-warmer)
      (link-alt magenta)
      (name magenta)
      (identifier yellow)
      (tag magenta-faint)
      (prompt cyan-cooler)

      (rx-construct green-cooler)
      (rx-backslash magenta)

      (search-current bg-yellow)
      (search-lazy bg-cyan)
      (search-match bg-magenta-subtle)

      (err red)
      (warning yellow-warmer)
      (success green)
      (note cyan)

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

      (mail-cite-0 blue)
      (mail-cite-1 yellow-warmer)
      (mail-cite-2 magenta)
      (mail-cite-3 green-cooler)
      (mail-part cyan-cooler)
      (mail-recipient magenta-cooler) ; compare with name and keep them similar
      (mail-subject magenta-warmer)
      (mail-other cyan)

      (heading-0 cyan-cooler)
      (heading-1 fg-main)
      (heading-2 yellow-faint)
      (heading-3 fg-alt)
      (heading-4 magenta)
      (heading-5 green-faint)
      (heading-6 red-faint)
      (heading-7 cyan-warmer)
      (heading-8 fg-dim)

      (rainbow-0 fg-main)
      (rainbow-1 magenta-intense)
      (rainbow-2 cyan-intense)
      (rainbow-3 red-cooler)
      (rainbow-4 yellow-intense)
      (rainbow-5 magenta-cooler)
      (rainbow-6 green-intense)
      (rainbow-7 blue-warmer)
      (rainbow-8 magenta-warmer)

      (prose-code green-cooler)
      (prose-macro magenta-cooler)
      (prose-verbatim magenta-warmer))
    "The entire palette of the `modus-operandi' theme.
Each element has the form (NAME HEX) with the former as a
symbol and the latter as a string.")

  (defvar modus-operandi-palette-overrides nil
    "Overrides for `modus-operandi-palette'.")

  (modus-themes-theme modus-operandi modus-operandi-palette modus-operandi-palette-overrides)

  (provide-theme 'modus-operandi))

;;;###theme-autoload
(put 'modus-operandi 'theme-properties '(:background-mode light :kind color-scheme :family modus))

;;; modus-operandi-theme.el ends here
