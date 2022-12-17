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

      (bg-mode-line-active        bg-active)
      (fg-mode-line-active        fg-main)
      (border-mode-line-active    fg-dim)
      (bg-mode-line-inactive      bg-inactive)
      (fg-mode-line-inactive      fg-dim)
      (border-mode-line-inactive  border)

      (cursor fg-main)
      (builtin magenta-warmer)
      (comment yellow)
      (constant blue-cooler)
      (docstring green-faint)
      (fnname magenta)
      (keyword magenta-cooler)
      (preprocessor red-cooler)
      (string blue-warmer)
      (type cyan-cooler)
      (variable cyan)

      (date cyan)
      (weekend blue) ; for M-x calendar and Org agenda
      (keybind blue-cooler)
      (link blue-warmer)
      (link-symbolic cyan)
      (link-visited yellow-faint)
      (name blue-cooler)
      (identifier yellow-faint)
      (tag magenta-faint)
      (prompt cyan-cooler)

      (rx-construct yellow-cooler)
      (rx-backslash blue-cooler)

      (err yellow-warmer)
      (warning yellow-cooler)
      (info blue)

      (underline-err red-intense)
      (underline-warning yellow-intense)
      (underline-note cyan-intense)

      (accent-0 blue)
      (accent-1 yellow-warmer)
      (accent-2 cyan)
      (accent-3 magenta-cooler)
      (bg-accent-0 bg-blue-subtle)
      (bg-accent-1 bg-yellow-subtle)
      (bg-accent-2 bg-cyan-subtle)
      (bg-accent-3 bg-magenta-subtle)

      (mail-cite-0 blue-warmer)
      (mail-cite-1 yellow)
      (mail-cite-2 blue-cooler)
      (mail-cite-3 yellow-faint)
      (mail-part blue)
      (mail-recipient blue)
      (mail-subject yellow-warmer)
      (mail-other cyan-faint)

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
    "The entire palette of the `modus-operandi-deuteranopia' theme.
Each element has the form (NAME HEX) with the former as a
symbol and the latter as a string.")

  (defvar modus-operandi-deuteranopia-palette-overrides nil
    "Overrides for `modus-operandi-deuteranopia-palette'.")

  (modus-themes-theme modus-operandi-deuteranopia modus-operandi-deuteranopia-palette modus-operandi-deuteranopia-palette-overrides)

  (provide-theme 'modus-operandi-deuteranopia))

;;;###theme-autoload
(put 'modus-operandi-deuteranopia 'theme-properties '(:background-mode light :kind color-scheme :family modus))

;;; modus-operandi-deuteranopia-theme.el ends here
