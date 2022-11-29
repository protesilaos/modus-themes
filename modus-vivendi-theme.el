;;; modus-vivendi-theme.el --- Elegant, highly legible and customizable dark theme -*- lexical-binding:t -*-

;; Copyright (C) 2019-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Modus-Themes Development <~protesilaos/modus-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/modus-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/modus-themes
;; Version: 3.0.0
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
;; Modus Vivendi is the dark variant of the Modus themes (Modus Operandi
;; is the light one).  The themes are designed for color-contrast
;; accessibility.  More specifically:
;;
;;     1. Provide a consistent minimum contrast ratio between background
;;     and foreground values of 7:1 or higher.  This meets the highest
;;     such accessibility criterion per the guidelines of the Worldwide
;;     Web Consortium's Working Group on Accessibility (WCAG AAA
;;     standard).
;;
;;     2. Offer as close to full face coverage as possible.  The list is
;;     already quite long, with more additions to follow as part of the
;;     ongoing development process.
;;
;; For a complete view of the project, also refer to the following files
;; (should be distributed in the same repository/directory as the
;; current item):
;;
;; - modus-themes.el            (Main code shared between the themes)
;; - modus-operandi-theme.el    (Light theme)

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

  (define-obsolete-variable-alias
    'modus-themes-vivendi-colors
    'modus-vivendi-palette
    "3.0.0")

  (defconst modus-vivendi-palette
    '(
;;; Basic values
      (bg-main          "#000000")
      (bg-dim           "#1e1e1e")
      (fg-main          "#ffffff")
      (fg-dim           "#989898")
      (fg-alt           "#a0bfef")
      (bg-active        "#535353")
      (bg-inactive      "#303030")
      (bg-active-accent "#2a2a66")
      (border           "#646464")

;;; Common accent foregrounds

      (red             "#ff5f59")
      (red-warmer      "#ff6b55")
      (red-cooler      "#ff658f")
      (red-faint       "#efa080")
      (red-intense     "#ff5f5f")
      (green           "#44bc44")
      (green-warmer    "#70b900")
      (green-cooler    "#00c06f")
      (green-faint     "#88ca9f")
      (green-intense   "#44df44")
      (yellow          "#d0bc00")
      (yellow-warmer   "#c0c530")
      (yellow-cooler   "#d3b55f")
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
      (magenta-faint   "#e0b2d6")
      (magenta-intense "#ff66ff")
      (cyan            "#00d3d0")
      (cyan-warmer     "#4ae2f0")
      (cyan-cooler     "#6ae4b9")
      (cyan-faint      "#9ac8e0")
      (cyan-intense    "#00eff0")

;;; Common accent backgrounds

      (bg-red      "#cd2f30")
      (bg-green    "#20a020")
      (bg-yellow   "#8f5040")
      (bg-blue     "#4648d0")
      (bg-magenta  "#a050cf")
      (bg-cyan     "#2270be")

      (bg-red-subtle      "#72002a")
      (bg-green-subtle    "#00422a")
      (bg-yellow-subtle   "#603000")
      (bg-blue-subtle     "#242679")
      (bg-magenta-subtle  "#552f5f")
      (bg-cyan-subtle     "#004065")

;;; Graphs

      (red-graph-0-bg     "#b52c2c")
      (red-graph-1-bg     "#702020")
      (green-graph-0-bg   "#4fd100")
      (green-graph-1-bg   "#007800")
      (yellow-graph-0-bg  "#f1e00a")
      (yellow-graph-1-bg  "#b08600")
      (blue-graph-0-bg    "#2fafef")
      (blue-graph-1-bg    "#1f2f8f")
      (magenta-graph-0-bg "#bf94fe")
      (magenta-graph-1-bg "#5f509f")
      (cyan-graph-0-bg    "#47dfea")
      (cyan-graph-1-bg    "#00808f")

;;; Special purpose

      (bg-completion           "#2f3f5f")
      (bg-hover                "#004f70")
      (bg-hover-alt            "#283355")
      (bg-hl-line              "#2f3849")
      (bg-paren-match          "#6f3355")
      (bg-paren-match-intense  "#7416b5")
      (bg-paren-expression     "#221044")
      (bg-region               "#3c3c3c")
      (bg-region-accent        "#4f3d88")
      (bg-region-accent-subtle "#240f55")
      (bg-prompt               "#2c5a55")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#625a00")

;;; Diffs

      (bg-added          "#00381f")
      (bg-added-faint    "#002910")
      (bg-added-refine   "#034f2f")
      (bg-added-intense  "#237f3f")

      (bg-added-deuteran         "#00234f")
      (bg-added-faint-deuteran   "#00143f")
      (bg-added-refine-deuteran  "#03395f")
      (bg-added-intense-deuteran "#03699f")

      (bg-changed         "#363300")
      (bg-changed-faint   "#2a1f00")
      (bg-changed-refine  "#4a4a00")
      (bg-changed-intense "#8a7a00")

      (bg-removed         "#4f1119")
      (bg-removed-faint   "#380a0f")
      (bg-removed-refine  "#781a1f")
      (bg-removed-intense "#b81a1f")

;;;; Mappings

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

      (date cyan)
      (keybind blue-cooler)
      (link blue-warmer)
      (link-alt magenta)
      (name magenta-warmer)
      (prompt cyan-cooler)

      (rx-construct green-cooler)
      (rx-backslash magenta)

      (search-current bg-yellow)
      (search-lazy bg-cyan)
      (search-match bg-magenta-subtle)

      (err red)
      (warning yellow)
      (success green)
      (note cyan)

      (underline-err red-intense)
      (underline-warning yellow-intense)
      (underline-note cyan-intense)

      (accent-0 blue-warmer)
      (accent-1 red-cooler)
      (accent-2 green-cooler)
      (accent-3 magenta-warmer)

      (mail-0 blue-warmer)
      (mail-1 magenta)
      (mail-2 green-warmer)
      (mail-3 yellow-cooler)
      (mail-4 cyan)
      (mail-recipient magenta-cooler) ; compare with name and keep them similar
      (mail-subject blue-cooler)
      (mail-other magenta-warmer)

      (heading-0 cyan-cooler)
      (heading-1 fg-main)
      (heading-2 fg-special-warm)
      (heading-3 fg-special-cold)
      (heading-4 fg-special-calm)
      (heading-5 fg-special-mild)
      (heading-6 yellow-faint)
      (heading-7 red-faint)
      (heading-8 magenta-faint)

      (heading-rainbow-0 blue-warmer)
      (heading-rainbow-1 magenta-cooler)
      (heading-rainbow-2 magenta)
      (heading-rainbow-3 magenta-warmer)
      (heading-rainbow-4 red-cooler)
      (heading-rainbow-5 red)
      (heading-rainbow-6 red-warmer)
      (heading-rainbow-7 yellow-warmer)
      (heading-rainbow-8 yellow)

      (rainbow-0 fg-main)
      (rainbow-1 magenta-cooler)
      (rainbow-2 cyan-intense)
      (rainbow-3 red-cooler)
      (rainbow-4 green-intense)
      (rainbow-5 magenta-intense)
      (rainbow-6 yellow-intense)
      (rainbow-7 blue-intense)
      (rainbow-8 magenta)

      (prose-code cyan)
      (prose-macro magenta-cooler)
      (prose-verbatim magenta-warmer))
    "The entire palette of the `modus-vivendi' theme.
Each element has the form (NAME HEX) with the former as a
symbol and the latter as a string.")

  (modus-themes-theme modus-vivendi modus-vivendi-palette)

  (provide-theme 'modus-vivendi))

;;;###theme-autoload
(put 'modus-vivendi 'theme-properties '(:background-mode dark :kind color-scheme :family modus))

;;; modus-vivendi-theme.el ends here
