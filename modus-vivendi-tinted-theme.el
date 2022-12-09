;;; modus-vivendi-tinted-theme.el --- Elegant, highly legible and customizable dark theme -*- lexical-binding:t -*-

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

  (deftheme modus-vivendi-tinted
    "Elegant, highly legible and customizable dark theme.
Conforms with the highest legibility standard for color contrast
between background and foreground in any given piece of text,
which corresponds to a minimum contrast in relative luminance of
7:1 (WCAG AAA standard).")

  (defconst modus-vivendi-tinted-palette
    '(
;;; Basic values

      (bg-main          "#0d0e1c")
      (bg-dim           "#1d2235")
      (fg-main          "#ffffff")
      (fg-dim           "#989898")
      (fg-alt           "#c6daff")
      (bg-active        "#4a4f69")
      (bg-inactive      "#2b3045")
      (bg-active-accent "#2f337f")
      (border           "#61647a")

;;; Common accent foregrounds

      (red             "#ff5f59")
      (red-warmer      "#ff6b55")
      (red-cooler      "#ff7f9f")
      (red-faint       "#ff9f80")
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
      (magenta-faint   "#cab2df")
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

      (bg-completion       "#2f446b")
      (bg-hover            "#004f70")
      (bg-hover-secondary  "#654a39")
      (bg-hl-line          "#303a6f")
      (bg-paren-match      "#2f7f9f")
      (bg-paren-expression "#453040")
      (bg-region           "#555a66")
      (bg-region-subtle    "#0f4f30")
      (bg-prompt           "#2c5a55")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#625a00")

;;; Diffs

      (bg-added          "#003826")
      (bg-added-faint    "#002915")
      (bg-added-refine   "#034f36")
      (bg-added-intense  "#237f3f")

      (bg-added-deuteran         "#00234f")
      (bg-added-faint-deuteran   "#00143f")
      (bg-added-refine-deuteran  "#03395f")
      (bg-added-intense-deuteran "#03699f")

      (bg-changed         "#363300")
      (bg-changed-faint   "#2a1f00")
      (bg-changed-refine  "#4a4a00")
      (bg-changed-intense "#8a7a00")

      (bg-removed         "#4f111f")
      (bg-removed-faint   "#380a13")
      (bg-removed-refine  "#781a33")
      (bg-removed-intense "#b81a1f")

;;;; Mappings

      (cursor blue)
      (builtin magenta-warmer)
      (comment red-faint)
      (constant blue-cooler)
      (docstring cyan-faint)
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
      (identifier yellow-faint)
      (tag magenta-faint)
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
      (mail-cite-2 magenta)
      (mail-cite-3 cyan-cooler)
      (mail-part green-cooler)
      (mail-recipient magenta-cooler) ; compare with name and keep them similar
      (mail-subject magenta-warmer)
      (mail-other cyan)

      (heading-0 cyan-cooler)
      (heading-1 fg-main)
      (heading-2 yellow-faint)
      (heading-3 blue-faint)
      (heading-4 magenta)
      (heading-5 green-faint)
      (heading-6 red-faint)
      (heading-7 cyan-faint)
      (heading-8 fg-dim)

      (rainbow-0 fg-main)
      (rainbow-1 magenta-warmer)
      (rainbow-2 cyan-intense)
      (rainbow-3 red-warmer)
      (rainbow-4 yellow-intense)
      (rainbow-5 magenta-cooler)
      (rainbow-6 green-intense)
      (rainbow-7 blue-intense)
      (rainbow-8 magenta-intense)

      (prose-code cyan-cooler)
      (prose-macro magenta-cooler)
      (prose-verbatim magenta-warmer))
    "The entire palette of the `modus-vivendi-tinted' theme.
Each element has the form (NAME HEX) with the former as a
symbol and the latter as a string.")

  (defvar modus-vivendi-tinted-palette-overrides nil)

  (modus-themes-theme modus-vivendi-tinted modus-vivendi-tinted-palette modus-vivendi-tinted-palette-overrides)

  (provide-theme 'modus-vivendi-tinted))

;;;###theme-autoload
(put 'modus-vivendi-tinted 'theme-properties '(:background-mode dark :kind color-scheme :family modus))

;;; modus-vivendi-tinted-theme.el ends here
