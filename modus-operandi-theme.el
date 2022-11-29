;;; modus-operandi-theme.el --- Elegant, highly legible and customizable light theme -*- lexical-binding:t -*-

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
;; Modus Operandi is the light variant of the Modus themes (Modus
;; Vivendi is the dark one).  The themes are designed for color-contrast
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
;; - modus-vivendi-theme.el     (Dark theme)

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

  (define-obsolete-variable-alias
    'modus-themes-operandi-colors
    'modus-operandi-palette
    "3.0.0")

  (defconst modus-operandi-palette
    '(;; base values
      (bg-main "#ffffff")
      (bg-dim  "#f8f8f8")
      (bg-alt  "#f0f0f0")
      (fg-main "#000000")
      (fg-dim  "#282828")
      (fg-alt  "#505050")
      ;; specifically for on/off states and must be combined with
      ;; themselves, though the backgrounds are also meant to be used with
      ;; other "active" values, defined further below; bg-active-accent
      ;; can work as a substitute for bg-active
      (bg-active        "#d7d7d7")
      (bg-inactive      "#efefef")
      (bg-active-accent "#d0d6ff")
      (fg-active        "#0a0a0a")
      (fg-inactive      "#404148")
      ;; these special values are intended as alternatives to the base
      ;; values for cases where we need to avoid confusion between the
      ;; highlighted constructs; they must either be used as pairs based
      ;; on their name or each can be combined with {fg,bg}-{main,alt,dim}
      ;; always in accordance with their role as background or foreground
      (bg-special-cold "#dde3f4") (bg-special-faint-cold "#f0f1ff") (fg-special-cold "#093060")
      (bg-special-mild "#c4ede0") (bg-special-faint-mild "#ebf5eb") (fg-special-mild "#184034")
      (bg-special-warm "#f0e0d4") (bg-special-faint-warm "#fef2ea") (fg-special-warm "#5d3026")
      (bg-special-calm "#f8ddea") (bg-special-faint-calm "#faeff9") (fg-special-calm "#61284f")
      ;; foregrounds that can be combined with bg-main, bg-dim, bg-alt
      (red                  "#a60000")
      (red-warmer           "#972500")
      (red-cooler           "#a0132f")
      (red-faint            "#7f1010")
      (red-warmer-faint     "#702f00")
      (red-cooler-faint     "#7f002f")
      (green                "#005e00")
      (green-warmer         "#315b00")
      (green-cooler         "#145c33")
      (green-faint          "#104410")
      (green-warmer-faint   "#30440f")
      (green-cooler-faint   "#0f443f")
      (yellow               "#813e00")
      (yellow-warmer        "#70480f")
      (yellow-cooler        "#863927")
      (yellow-faint         "#5f4400")
      (yellow-warmer-faint  "#5d5000")
      (yellow-cooler-faint  "#5e3a20")
      (blue                 "#0031a9")
      (blue-warmer          "#2544bb")
      (blue-cooler          "#0000c0")
      (blue-faint           "#003497")
      (blue-warmer-faint    "#0f3d8c")
      (blue-cooler-faint    "#001087")
      (magenta              "#721045")
      (magenta-warmer       "#8f0075")
      (magenta-cooler       "#5317ac")
      (magenta-faint        "#752f50")
      (magenta-warmer-faint "#7b206f")
      (magenta-cooler-faint "#55348e")
      (cyan                 "#00538b")
      (cyan-warmer          "#30517f")
      (cyan-cooler          "#005a5f")
      (cyan-faint           "#005077")
      (cyan-warmer-faint    "#354f6f")
      (cyan-cooler-faint    "#125458")
      ;; those foregrounds are meant exclusively for bg-active, bg-inactive
      (red-active     "#8a0000")
      (green-active   "#004c2e")
      (yellow-active  "#702f00")
      (blue-active    "#0030b4")
      (magenta-active "#5c2092")
      (cyan-active    "#003f8a")
      ;; the "subtle" values below be combined with fg-dim, while the
      ;; "intense" should be paired with fg-main
      (red-subtle-bg      "#f2b0a2")
      (red-intense-bg     "#ff9f9f")
      (green-subtle-bg    "#aecf90")
      (green-intense-bg   "#5ada88")
      (yellow-subtle-bg   "#e4c340")
      (yellow-intense-bg  "#f5df23")
      (blue-subtle-bg     "#b5d0ff")
      (blue-intense-bg    "#77baff")
      (magenta-subtle-bg  "#f0d3ff")
      (magenta-intense-bg "#d5baff")
      (cyan-subtle-bg     "#c0efff")
      (cyan-intense-bg    "#42cbd4")
      ;; those background values must be combined with fg-main and should
      ;; only be used for indicators that are placed on the fringes
      (red-fringe-bg     "#f08290")
      (green-fringe-bg   "#62c86a")
      (yellow-fringe-bg  "#dbba3f")
      (blue-fringe-bg    "#82afff")
      (magenta-fringe-bg "#e0a3ff")
      (cyan-fringe-bg    "#2fcddf")
      ;; those background values should only be used for graphs or similar
      ;; applications where colored blocks are expected to be positioned
      ;; next to each other
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
      ;; the following are for cases where both the foreground and the
      ;; background need to have a similar hue and so must be combined
      ;; with themselves, even though the foregrounds can be paired with
      ;; any of the base backgrounds
      (red-refine-bg      "#ffcccc") (red-refine-fg      "#780000")
      (green-refine-bg    "#aceaac") (green-refine-fg    "#004c00")
      (yellow-refine-bg   "#fff29a") (yellow-refine-fg   "#604000")
      (blue-refine-bg     "#8fcfff") (blue-refine-fg     "#002f88")
      (magenta-refine-bg  "#ffccff") (magenta-refine-fg  "#770077")
      (cyan-refine-bg     "#8eecf4") (cyan-refine-fg     "#004850")
      ;; the "nuanced" backgrounds can be combined with all of the above
      ;; foregrounds, as well as those included here, while the "nuanced"
      ;; foregrounds can in turn also be combined with bg-main, bg-dim,
      ;; bg-alt
      (red-nuanced-bg      "#fff1f0") (red-nuanced-fg      "#5f0000")
      (green-nuanced-bg    "#ecf7ed") (green-nuanced-fg    "#004000")
      (yellow-nuanced-bg   "#fff3da") (yellow-nuanced-fg   "#3f3000")
      (blue-nuanced-bg     "#f3f3ff") (blue-nuanced-fg     "#201f55")
      (magenta-nuanced-bg  "#fdf0ff") (magenta-nuanced-fg  "#541f4f")
      (cyan-nuanced-bg     "#ebf6fa") (cyan-nuanced-fg     "#0f3360")
      ;; the following are reserved for specific cases
      ;;
      ;; bg-hl-line is between bg-dim and bg-alt, so it should
      ;; work with all accents that cover those two, plus bg-main
      ;;
      ;; bg-hl-alt and bg-hl-alt-intense should only be used when no
      ;; other grayscale or fairly neutral background is available to
      ;; properly draw attention to a given construct
      ;;
      ;; bg-header is between bg-active and bg-inactive, so it
      ;; can be combined with any of the "active" values, plus the
      ;; "special" and base foreground colors
      ;;
      ;; bg-paren-match, bg-paren-match-intense, bg-region,
      ;; bg-region-accent and bg-tab-active must be combined with fg-main,
      ;; while bg-tab-inactive should be combined with fg-dim, whereas
      ;; bg-tab-inactive-alt goes together with fg-main
      ;;
      ;; bg-completion-* and bg-char-* variants are meant to be combined
      ;; with fg-main
      ;;
      ;; fg-escape-char-construct and fg-escape-char-backslash can
      ;; be combined bg-main, bg-dim, bg-alt
      ;;
      ;; fg-lang-error, fg-lang-warning, fg-lang-note can be
      ;; combined with bg-main, bg-dim, bg-alt
      ;;
      ;; fg-mark-sel, fg-mark-del, fg-mark-alt can be combined
      ;; with bg-main, bg-dim, bg-alt, bg-hl-line
      ;;
      ;; fg-unfocused must be combined with bg-main
      ;;
      ;; fg-docstring, fg-comment-yellow can be combined with
      ;; bg-main, bg-dim, bg-alt
      ;;
      ;; the window divider colors apply to faces with just an fg value
      ;;
      ;; all pairs are combinable with themselves
      (bg-hl-line                "#f2eff3")
      (bg-hl-line-intense        "#e0e0e0")
      (bg-hl-line-intense-accent "#cfe2ff")
      (bg-hl-alt                 "#fbeee0")
      (bg-hl-alt-intense         "#e8dfd1")
      (bg-paren-match            "#e0af82")
      (bg-paren-match-intense    "#c488ff")
      (bg-paren-expression       "#dff0ff")
      (bg-region                 "#bcbcbc")
      (bg-region-accent          "#afafef")
      (bg-region-accent-subtle   "#efdfff")

      (bg-completion        "#b7dbff")
      (bg-completion-subtle "#def3ff")

      (bg-char-0 "#7feaff")
      (bg-char-1 "#ffaaff")
      (bg-char-2 "#dff000")

      (bg-tab-active              "#f6f6f6")
      (bg-tab-inactive            "#b7b7b7")
      (bg-tab-inactive-accent     "#a9b4f6")
      (bg-tab-inactive-alt        "#9f9f9f")
      (bg-tab-inactive-alt-accent "#9fa6d0")

      (red-tab     "#680000")
      (green-tab   "#003900")
      (yellow-tab  "#393000")
      (orange-tab  "#502300")
      (blue-tab    "#000080")
      (cyan-tab    "#052f60")
      (magenta-tab "#5f004d")
      (purple-tab  "#400487")

      (fg-escape-char-construct "#8b1030")
      (fg-escape-char-backslash "#654d0f")

      (fg-lang-error             "#9f004f")
      (fg-lang-warning           "#604f0f")
      (fg-lang-note              "#4040ae")
      (fg-lang-underline-error   "#ef4f54")
      (fg-lang-underline-warning "#cf9f00")
      (fg-lang-underline-note    "#3f6fef")

      (fg-window-divider-inner "#888888")
      (fg-window-divider-outer "#585858")

      (fg-unfocused "#56576d")

      (fg-docstring      "#2a486a")
      (fg-comment-yellow "#794319")

      (bg-header "#e5e5e5") (fg-header "#2a2a2a")

      (bg-whitespace "#f5efef") (fg-whitespace "#624956")

      (bg-diff-heading                "#b7cfe0") (fg-diff-heading                "#041645")
      (bg-diff-added                  "#d4fad4") (fg-diff-added                  "#004500")
      (bg-diff-added-deuteran         "#daefff") (fg-diff-added-deuteran         "#002044")
      (bg-diff-changed                "#fcefcf") (fg-diff-changed                "#524200")
      (bg-diff-removed                "#ffe8ef") (fg-diff-removed                "#691616")

      (bg-diff-refine-added           "#94cf94") (fg-diff-refine-added           "#002a00")
      (bg-diff-refine-added-deuteran  "#77c0ef") (fg-diff-refine-added-deuteran  "#000035")
      (bg-diff-refine-changed         "#cccf8f") (fg-diff-refine-changed         "#302010")
      (bg-diff-refine-removed         "#daa2b0") (fg-diff-refine-removed         "#400000")

      (bg-diff-focus-added            "#bbeabb") (fg-diff-focus-added            "#002c00")
      (bg-diff-focus-added-deuteran   "#bacfff") (fg-diff-focus-added-deuteran   "#001755")
      (bg-diff-focus-changed          "#ecdfbf") (fg-diff-focus-changed          "#392900")
      (bg-diff-focus-removed          "#efcbcf") (fg-diff-focus-removed          "#4a0000")

      (bg-mark-sel "#a0f0cf") (fg-mark-sel "#005040")
      (bg-mark-del "#ffccbb") (fg-mark-del "#840040")
      (bg-mark-alt "#f5d88f") (fg-mark-alt "#782900"))
    "The entire palette of the `modus-operandi' theme.
Each element has the form (NAME HEX) with the former as a
symbol and the latter as a string.")

  (modus-themes-theme modus-operandi modus-operandi-palette)

  (provide-theme 'modus-operandi))

;;;###theme-autoload
(put 'modus-operandi 'theme-properties '(:background-mode light :kind color-scheme :family modus))

;;; modus-operandi-theme.el ends here
