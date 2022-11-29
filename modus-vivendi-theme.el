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
    '(;; base values
      (bg-main "#000000")
      (bg-dim  "#100f10")
      (bg-alt  "#191a1b")
      (fg-main "#ffffff")
      (fg-dim  "#e0e6f0")
      (fg-alt  "#a8a8a8")
      ;; specifically for on/off states and must be combined with
      ;; themselves, though the backgrounds are also meant to be used with
      ;; other "active" values, defined further below; bg-active-accent
      ;; can work as a substitute for bg-active
      (bg-active        "#323232")
      (bg-inactive      "#1e1e1e")
      (bg-active-accent "#2a2a66")
      (fg-active        "#f4f4f4")
      (fg-inactive      "#bfc0c4")
      ;; these special values are intended as alternatives to the base
      ;; values for cases where we need to avoid confusion between the
      ;; highlighted constructs; they must either be used as pairs based
      ;; on their name or each can be combined with {fg,bg}-{main,alt,dim}
      ;; always in accordance with their role as background or foreground
      (bg-special-cold "#203448") (bg-special-faint-cold "#0e183a") (fg-special-cold "#c6eaff")
      (bg-special-mild "#00322e") (bg-special-faint-mild "#001f1a") (fg-special-mild "#bfebe0")
      (bg-special-warm "#382f27") (bg-special-faint-warm "#241613") (fg-special-warm "#f8dec0")
      (bg-special-calm "#392a48") (bg-special-faint-calm "#251232") (fg-special-calm "#fbd6f4")
      ;; foregrounds that can be combined with bg-main, bg-dim, bg-alt
      (red                  "#ff8059")
      (red-warmer           "#ef8b50")
      (red-cooler           "#ff9077")
      (red-faint            "#ffa0a0")
      (red-warmer-faint     "#f5aa80")
      (red-cooler-faint     "#ff9fbf")
      (red-intense          "#ff5f5f")
      (green                "#44bc44")
      (green-warmer         "#70b900")
      (green-cooler         "#00c06f")
      (green-faint          "#78bf78")
      (green-warmer-faint   "#99b56f")
      (green-cooler-faint   "#88bf99")
      (green-intense        "#44df44")
      (yellow               "#d0bc00")
      (yellow-warmer        "#c0c530")
      (yellow-cooler        "#d3b55f")
      (yellow-faint         "#d2b580")
      (yellow-warmer-faint  "#cabf77")
      (yellow-cooler-faint  "#d0ba95")
      (yellow-intense       "#efef00")
      (blue                 "#2fafff")
      (blue-warmer          "#79a8ff")
      (blue-cooler          "#00bcff")
      (blue-faint           "#82b0ec")
      (blue-warmer-faint    "#a0acef")
      (blue-cooler-faint    "#80b2f0")
      (blue-intense         "#338fff")
      (magenta              "#feacd0")
      (magenta-warmer       "#f78fe7")
      (magenta-cooler       "#b6a0ff")
      (magenta-faint        "#e0b2d6")
      (magenta-warmer-faint "#ef9fe4")
      (magenta-cooler-faint "#cfa6ff")
      (magenta-intense      "#ff66ff")
      (cyan                 "#00d3d0")
      (cyan-warmer          "#4ae2f0")
      (cyan-cooler          "#6ae4b9")
      (cyan-faint           "#90c4ed")
      (cyan-warmer-faint    "#a0bfdf")
      (cyan-cooler-faint    "#a4d0bb")
      (cyan-intense         "#00eff0")
      ;; those foregrounds are meant exclusively for bg-active, bg-inactive
      (red-active     "#ffa7ba")
      (green-active   "#70d73f")
      (yellow-active  "#dbbe5f")
      (blue-active    "#34cfff")
      (magenta-active "#d5b1ff")
      (cyan-active    "#00d8b4")
      ;; the "subtle" values below be combined with fg-dim, while the
      ;; "intense" should be paired with fg-main
      (red-subtle-bg      "#762422")
      (red-intense-bg     "#a4202a")
      (green-subtle-bg    "#2f4a00")
      (green-intense-bg   "#006800")
      (yellow-subtle-bg   "#604200")
      (yellow-intense-bg  "#874900")
      (blue-subtle-bg     "#10387c")
      (blue-intense-bg    "#2a40b8")
      (magenta-subtle-bg  "#49366e")
      (magenta-intense-bg "#7042a2")
      (cyan-subtle-bg     "#00415e")
      (cyan-intense-bg    "#005f88")
      ;; those background values must be combined with fg-main and should
      ;; only be used for indicators that are placed on the fringes
      (red-fringe-bg     "#8f1f4b")
      (green-fringe-bg   "#006700")
      (yellow-fringe-bg  "#6f4f00")
      (blue-fringe-bg    "#3f33af")
      (magenta-fringe-bg "#6f2f89")
      (cyan-fringe-bg    "#004f8f")
      ;; those background values should only be used for graphs or similar
      ;; applications where colored blocks are expected to be positioned
      ;; next to each other
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
      ;; the following are for cases where both the foreground and the
      ;; background need to have a similar hue and so must be combined
      ;; with themselves, even though the foregrounds can be paired with
      ;; any of the base backgrounds
      (red-refine-bg      "#77002a") (red-refine-fg      "#ffb9ab")
      (green-refine-bg    "#00422a") (green-refine-fg    "#9ff0cf")
      (yellow-refine-bg   "#693200") (yellow-refine-fg   "#e2d980")
      (blue-refine-bg     "#242679") (blue-refine-fg     "#8ecfff")
      (magenta-refine-bg  "#71206a") (magenta-refine-fg  "#ffcaf0")
      (cyan-refine-bg     "#004065") (cyan-refine-fg     "#8ae4f2")
      ;; the "nuanced" backgrounds can be combined with all of the above
      ;; foregrounds, as well as those included here, while the "nuanced"
      ;; foregrounds can in turn also be combined with bg-main, bg-dim,
      ;; bg-alt
      (red-nuanced-bg      "#2c0614") (red-nuanced-fg      "#ffcccc")
      (green-nuanced-bg    "#001904") (green-nuanced-fg    "#b8e2b8")
      (yellow-nuanced-bg   "#221000") (yellow-nuanced-fg   "#dfdfb0")
      (blue-nuanced-bg     "#0f0e39") (blue-nuanced-fg     "#bfd9ff")
      (magenta-nuanced-bg  "#230631") (magenta-nuanced-fg  "#e5cfef")
      (cyan-nuanced-bg     "#041529") (cyan-nuanced-fg     "#a8e5e5")
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
      (bg-hl-line                "#151823")
      (bg-hl-line-intense        "#292929")
      (bg-hl-line-intense-accent "#002a4f")
      (bg-hl-alt                 "#181732")
      (bg-hl-alt-intense         "#282e46")
      (bg-paren-match            "#6f3355")
      (bg-paren-match-intense    "#7416b5")
      (bg-paren-expression       "#221044")
      (bg-region                 "#3c3c3c")
      (bg-region-accent          "#4f3d88")
      (bg-region-accent-subtle   "#240f55")

      (bg-completion        "#142f69")
      (bg-completion-subtle "#0e194b")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#625a00")

      (bg-tab-active              "#0e0e0e")
      (bg-tab-inactive            "#424242")
      (bg-tab-inactive-accent     "#35398f")
      (bg-tab-inactive-alt        "#595959")
      (bg-tab-inactive-alt-accent "#505588")

      (fg-escape-char-construct "#e7a59a")
      (fg-escape-char-backslash "#abab00")

      (fg-lang-error             "#ef8690")
      (fg-lang-warning           "#b0aa00")
      (fg-lang-note              "#9d9def")
      (fg-lang-underline-error   "#ff4a6f")
      (fg-lang-underline-warning "#d0de00")
      (fg-lang-underline-note    "#5f6fff")

      (fg-window-divider-inner "#646464")
      (fg-window-divider-outer "#969696")

      (fg-unfocused "#93959b")

      (fg-docstring      "#b0d6f5")
      (fg-comment-yellow "#d0a070")
      (heading-0 cyan-cooler)
      (heading-1 fg-main)
      (heading-2 fg-special-warm)
      (heading-3 fg-special-cold)
      (heading-4 fg-special-calm)
      (heading-5 fg-special-mild)
      (heading-6 yellow-faint)
      (heading-7 red-faint)
      (heading-8 magenta-faint)

      (bg-header "#212121") (fg-header "#dddddd")
      (heading-rainbow-0 blue-warmer)
      (heading-rainbow-1 magenta-cooler)
      (heading-rainbow-2 magenta)
      (heading-rainbow-3 magenta-warmer)
      (heading-rainbow-4 red-cooler)
      (heading-rainbow-5 red)
      (heading-rainbow-6 red-warmer)
      (heading-rainbow-7 yellow-warmer)
      (heading-rainbow-8 yellow)

      (bg-whitespace "#101424") (fg-whitespace "#aa9e9f")
      (rainbow-0 fg-main)
      (rainbow-1 magenta-cooler)
      (rainbow-2 cyan-intense)
      (rainbow-3 yellow-intense)
      (rainbow-4 magenta-intense)
      (rainbow-5 green-intense)
      (rainbow-6 red-cooler)
      (rainbow-7 blue-intense)
      (rainbow-8 yellow)

      (bg-diff-heading                "#304466") (fg-diff-heading                "#dae7ff")
      (bg-diff-added                  "#0a280a") (fg-diff-added                  "#94ba94")
      (bg-diff-added-deuteran         "#001a3f") (fg-diff-added-deuteran         "#c4cdf2")
      (bg-diff-changed                "#2a2000") (fg-diff-changed                "#b0ba9f")
      (bg-diff-removed                "#40160f") (fg-diff-removed                "#c6adaa")

      (bg-diff-refine-added           "#005a36") (fg-diff-refine-added           "#e0f6e0")
      (bg-diff-refine-added-deuteran  "#234f8f") (fg-diff-refine-added-deuteran  "#dde4ff")
      (bg-diff-refine-changed         "#585800") (fg-diff-refine-changed         "#ffffcc")
      (bg-diff-refine-removed         "#852828") (fg-diff-refine-removed         "#ffd9eb")

      (bg-diff-focus-added            "#1d3c25") (fg-diff-focus-added            "#b4ddb4")
      (bg-diff-focus-added-deuteran   "#003959") (fg-diff-focus-added-deuteran   "#bfe4ff")
      (bg-diff-focus-changed          "#424200") (fg-diff-focus-changed          "#d0daaf")
      (bg-diff-focus-removed          "#601f29") (fg-diff-focus-removed          "#eebdba")

      (bg-mark-sel "#002f2f") (fg-mark-sel "#60cfa2")
      (bg-mark-del "#5a0000") (fg-mark-del "#ff99aa")
      (bg-mark-alt "#3f2210") (fg-mark-alt "#f0aa20"))
    "The entire palette of the `modus-vivendi' theme.
Each element has the form (NAME HEX) with the former as a
symbol and the latter as a string.")

  (modus-themes-theme modus-vivendi modus-vivendi-palette)

  (provide-theme 'modus-vivendi))

;;;###theme-autoload
(put 'modus-vivendi 'theme-properties '(:background-mode dark :kind color-scheme :family modus))

;;; modus-vivendi-theme.el ends here
