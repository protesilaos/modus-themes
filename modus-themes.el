;;; modus-themes.el --- Testing -*- lexical-binding:t -*-
;; URL: https://www.protesilaos.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:
;; -
;;; Code:

;;; Custom faces

(defgroup modus-theme ()
  "Custom faces for the Modus themes."
  :group 'faces
  :prefix "modus-theme-"
  :link '(url-link :tag "GitLab" "https://gitlab.com/protesilaos/modus-themes")
  :tag "Modus Operandi")

(defface modus-theme-subtle-red nil nil)
(defface modus-theme-subtle-green nil nil)
(defface modus-theme-subtle-yellow nil nil)
(defface modus-theme-subtle-blue nil nil)
(defface modus-theme-subtle-magenta nil nil)
(defface modus-theme-subtle-cyan nil nil)
(defface modus-theme-subtle-neutral nil nil)
(defface modus-theme-intense-red nil nil)
(defface modus-theme-intense-green nil nil)
(defface modus-theme-intense-yellow nil nil)
(defface modus-theme-intense-blue nil nil)
(defface modus-theme-intense-magenta nil nil)
(defface modus-theme-intense-cyan nil nil)
(defface modus-theme-intense-neutral nil nil)
(defface modus-theme-refine-red nil nil)
(defface modus-theme-refine-green nil nil)
(defface modus-theme-refine-yellow nil nil)
(defface modus-theme-refine-blue nil nil)
(defface modus-theme-refine-magenta nil nil)
(defface modus-theme-refine-cyan nil nil)
(defface modus-theme-active-red nil nil)
(defface modus-theme-active-green nil nil)
(defface modus-theme-active-yellow nil nil)
(defface modus-theme-active-blue nil nil)
(defface modus-theme-active-magenta nil nil)
(defface modus-theme-active-cyan nil nil)
(defface modus-theme-fringe-red nil nil)
(defface modus-theme-fringe-green nil nil)
(defface modus-theme-fringe-yellow nil nil)
(defface modus-theme-fringe-blue nil nil)
(defface modus-theme-fringe-magenta nil nil)
(defface modus-theme-fringe-cyan nil nil)
(defface modus-theme-nuanced-red nil nil)
(defface modus-theme-nuanced-green nil nil)
(defface modus-theme-nuanced-yellow nil nil)
(defface modus-theme-nuanced-blue nil nil)
(defface modus-theme-nuanced-magenta nil nil)
(defface modus-theme-nuanced-cyan nil nil)
(defface modus-theme-special-cold nil nil)
(defface modus-theme-special-mild nil nil)
(defface modus-theme-special-warm nil nil)
(defface modus-theme-special-calm nil nil)
(defface modus-theme-diff-added nil nil)
(defface modus-theme-diff-changed nil nil)
(defface modus-theme-diff-removed nil nil)
(defface modus-theme-diff-refine-added nil nil)
(defface modus-theme-diff-refine-changed nil nil)
(defface modus-theme-diff-refine-removed nil nil)
(defface modus-theme-diff-focus-added nil nil)
(defface modus-theme-diff-focus-changed nil nil)
(defface modus-theme-diff-focus-removed nil nil)
(defface modus-theme-diff-heading nil nil)
(defface modus-theme-pseudo-header nil nil)
(defface modus-theme-mark-alt nil nil)
(defface modus-theme-mark-del nil nil)
(defface modus-theme-mark-sel nil nil)
(defface modus-theme-mark-symbol nil nil)
(defface modus-theme-heading-1 nil nil)
(defface modus-theme-heading-2 nil nil)
(defface modus-theme-heading-3 nil nil)
(defface modus-theme-heading-4 nil nil)
(defface modus-theme-heading-5 nil nil)
(defface modus-theme-heading-6 nil nil)
(defface modus-theme-heading-7 nil nil)
(defface modus-theme-heading-8 nil nil)
(defface modus-theme-hl-line nil nil)
(defface modus-theme-bold nil nil)
(defface modus-theme-slant nil nil)
(defface modus-theme-variable-pitch nil nil)

;;;; Theme variables

;;;; Modus Operandi
(deftheme modus-operandi
  "Light theme that conforms with the highest accessibility
  standard for color contrast between background and foreground
  elements (WCAG AAA).")

(define-obsolete-variable-alias
  'modus-operandi-theme-default-colors-alist
  'modus-themes-operandi-colors
  "1.0.0")

(defconst modus-themes-operandi-colors
  '(;; base values
    (bg-main . "#ffffff") (fg-main . "#000000")
    (bg-alt . "#f0f0f0") (fg-alt . "#505050")
    (bg-dim . "#f8f8f8") (fg-dim . "#282828")
    ;; specifically for on/off states (e.g. `mode-line')
    ;;
    ;; must be combined with themselves
    (bg-active . "#d7d7d7") (fg-active . "#0a0a0a")
    (bg-inactive . "#efefef") (fg-inactive . "#404148")
    ;; special base values, used only for cases where the above
    ;; fg-* or bg-* cannot or should not be used (to avoid confusion)
    ;; must be combined with: {fg,bg}-{main,alt,dim}
    (bg-special-cold . "#dde3f4") (fg-special-cold . "#093060")
    (bg-special-mild . "#c4ede0") (fg-special-mild . "#184034")
    (bg-special-warm . "#f0e0d4") (fg-special-warm . "#5d3026")
    (bg-special-calm . "#f8ddea") (fg-special-calm . "#61284f")
    ;; styles for the main constructs
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
    (red . "#a60000") (green . "#005e00")
    (yellow . "#813e00") (blue . "#0031a9")
    (magenta . "#721045") (cyan . "#00538b")
    ;; styles for common, but still specialized constructs
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
    (red-alt . "#972500") (green-alt . "#315b00")
    (yellow-alt . "#70480f") (blue-alt . "#2544bb")
    (magenta-alt . "#8f0075") (cyan-alt . "#30517f")
    ;; same purpose as above, just slight differences
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
    (red-alt-other . "#a0132f") (green-alt-other . "#145c33")
    (yellow-alt-other . "#863927") (blue-alt-other . "#0000c0")
    (magenta-alt-other . "#5317ac") (cyan-alt-other . "#005a5f")
    ;; styles for desaturated foreground text
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
    (red-faint . "#7f1010") (green-faint . "#104410")
    (yellow-faint . "#5f4400") (blue-faint . "#002f88")
    (magenta-faint . "#752f50") (cyan-faint . "#12506f")

    (red-alt-faint . "#702f00") (green-alt-faint . "#30440f")
    (yellow-alt-faint . "#5d5000") (blue-alt-faint . "#003f78")
    (magenta-alt-faint . "#702565") (cyan-alt-faint . "#354f6f")

    (red-alt-other-faint . "#7f002f") (green-alt-other-faint . "#0f443f")
    (yellow-alt-other-faint . "#5e3a20") (blue-alt-other-faint . "#1f0f6f")
    (magenta-alt-other-faint . "#5f3f7f") (cyan-alt-other-faint . "#2e584f")
    ;; styles for elements that should be very subtle, yet accented
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim' or any of
    ;; the "nuanced" backgrounds
    (red-nuanced . "#5f0000") (green-nuanced . "#004000")
    (yellow-nuanced . "#3f3000") (blue-nuanced . "#201f55")
    (magenta-nuanced . "#541f4f") (cyan-nuanced . "#0f3360")
    ;; styles for slightly accented background
    ;;
    ;; must be combined with any of the above foreground values
    (red-nuanced-bg . "#fff1f0") (green-nuanced-bg . "#ecf7ed")
    (yellow-nuanced-bg . "#fff3da") (blue-nuanced-bg . "#f3f3ff")
    (magenta-nuanced-bg . "#fdf0ff") (cyan-nuanced-bg . "#ebf6fa")
    ;; styles for elements that should draw attention to themselves
    ;;
    ;; must be combined with: `bg-main'
    (red-intense . "#b60000") (green-intense . "#006800")
    (yellow-intense . "#904200") (blue-intense . "#1111ee")
    (magenta-intense . "#7000e0") (cyan-intense . "#205b93")
    ;; styles for background elements that should be visible yet
    ;; subtle
    ;;
    ;; must be combined with: `fg-dim'
    (red-subtle-bg . "#f2b0a2") (green-subtle-bg . "#aecf90")
    (yellow-subtle-bg . "#e4c340") (blue-subtle-bg . "#b5d0ff")
    (magenta-subtle-bg . "#f0d3ff") (cyan-subtle-bg . "#c0efff")
    ;; styles for background elements that should be visible and
    ;; distinguishable
    ;;
    ;; must be combined with: `fg-main'
    (red-intense-bg . "#ff8892") (green-intense-bg . "#5ada88")
    (yellow-intense-bg . "#f5df23") (blue-intense-bg . "#6aaeff")
    (magenta-intense-bg . "#d5baff") (cyan-intense-bg . "#42cbd4")
    ;; styles for refined contexts where both the foreground and the
    ;; background need to have the same/similar hue
    ;;
    ;; must be combined with themselves OR the foregrounds can be
    ;; combined with any of the base backgrounds
    (red-refine-bg . "#ffcccc") (red-refine-fg . "#780000")
    (green-refine-bg . "#aceaac") (green-refine-fg . "#004c00")
    (yellow-refine-bg . "#fff29a") (yellow-refine-fg . "#604000")
    (blue-refine-bg . "#8ac7ff") (blue-refine-fg . "#002288")
    (magenta-refine-bg . "#ffccff") (magenta-refine-fg . "#770077")
    (cyan-refine-bg . "#8eecf4") (cyan-refine-fg . "#004850")
    ;; styles that are meant exclusively for the mode line
    ;;
    ;; must be combined with: `bg-active', `bg-inactive'
    (red-active . "#8a0000") (green-active . "#004c2e")
    (yellow-active . "#702d1f") (blue-active . "#0030b4")
    (magenta-active . "#5c2092") (cyan-active . "#003f8a")
    ;; styles that are meant exclusively for the fringes
    ;;
    ;; must be combined with `fg-main'
    (red-fringe-bg . "#f08290") (green-fringe-bg . "#62c86a")
    (yellow-fringe-bg . "#dbba3f") (blue-fringe-bg . "#82afff")
    (magenta-fringe-bg . "#e0a3ff") (cyan-fringe-bg . "#2fcddf")
    ;; styles reserved for specific faces
    ;;
    ;; `bg-hl-line' is between `bg-dim' and `bg-alt', so it should
    ;; work with all accents that cover those two, plus `bg-main'
    ;;
    ;; `bg-hl-alt' and `bg-hl-alt-intense' should only be used when no
    ;; other greyscale or fairly neutral background is available to
    ;; properly draw attention to a given construct
    ;;
    ;; `bg-header' is between `bg-active' and `bg-inactive', so it
    ;; can be combined with any of the "active" values, plus the
    ;; "special" and base foreground colors
    ;;
    ;; `bg-paren-match', `bg-paren-match-intense', `bg-region' and
    ;; `bg-tab-active' must be combined with `fg-main', while
    ;; `bg-tab-inactive' should be combined with `fg-dim'
    ;;
    ;; `bg-tab-bar' is only intended for the bar that holds the tabs and
    ;; can only be combined with `fg-main'
    ;;
    ;; `fg-tab-active' is meant to be combined with `bg-tab-active',
    ;; though only for styling special elements, such as underlining
    ;; the current tab
    ;;
    ;; `fg-escape-char-construct' and `fg-escape-char-backslash' can
    ;; be combined `bg-main', `bg-dim', `bg-alt'
    ;;
    ;; `fg-lang-error', `fg-lang-warning', `fg-lang-note' can be
    ;; combined with `bg-main', `bg-dim', `bg-alt'
    ;;
    ;; `fg-mark-sel', `fg-mark-del', `fg-mark-alt' can be combined
    ;; with `bg-main', `bg-dim', `bg-alt', `bg-hl-line'
    ;;
    ;; `fg-unfocused' must be combined with `fg-main'
    ;;
    ;; `fg-docstring', `fg-comment-yellow' can be combined with
    ;; `bg-main', `bg-dim', `bg-alt'
    ;;
    ;; the window divider colors apply to faces with just an fg value
    ;;
    ;; all pairs are combinable with themselves
    (bg-hl-line . "#f2eff3")
    (bg-hl-line-intense . "#e0e0e0")
    (bg-hl-alt . "#fbeee0")
    (bg-hl-alt-intense . "#e8dfd1")
    (bg-paren-match . "#e0af82")
    (bg-paren-match-intense . "#c488ff")
    (bg-region . "#bcbcbc")

    (bg-tab-bar . "#d5d5d5")
    (bg-tab-active . "#f6f6f6")
    (bg-tab-inactive . "#bdbdbd")
    (fg-tab-active . "#30169e")

    (fg-escape-char-construct . "#8b1030")
    (fg-escape-char-backslash . "#654d0f")

    (fg-lang-error . "#9f004f")
    (fg-lang-warning . "#604f0f")
    (fg-lang-note . "#4040ae")

    (fg-window-divider-inner . "#888888")
    (fg-window-divider-outer . "#585858")

    (fg-unfocused . "#56576d")

    (fg-docstring . "#2a486a")
    (fg-comment-yellow . "#5f4400")

    (bg-header . "#e5e5e5") (fg-header . "#2a2a2a")

    (bg-whitespace . "#fff8fc") (fg-whitespace . "#645060")

    (bg-diff-heading . "#b7c2dd") (fg-diff-heading . "#043355")
    (bg-diff-added . "#d4fad4") (fg-diff-added . "#004500")
    (bg-diff-changed . "#fcefcf") (fg-diff-changed . "#524200")
    (bg-diff-removed . "#ffe8ef") (fg-diff-removed . "#691616")

    (bg-diff-refine-added . "#94cf94") (fg-diff-refine-added . "#002a00")
    (bg-diff-refine-changed . "#cccf8f") (fg-diff-refine-changed . "#302010")
    (bg-diff-refine-removed . "#daa2b0") (fg-diff-refine-removed . "#400000")

    (bg-diff-focus-added . "#bbeabb") (fg-diff-focus-added . "#002c00")
    (bg-diff-focus-changed . "#ecdfbf") (fg-diff-focus-changed . "#392900")
    (bg-diff-focus-removed . "#efcbcf") (fg-diff-focus-removed . "#4a0000")

    (bg-diff-neutral-0 . "#979797") (fg-diff-neutral-0 . "#040404")
    (bg-diff-neutral-1 . "#b0b0b0") (fg-diff-neutral-1 . "#252525")
    (bg-diff-neutral-2 . "#cccccc") (fg-diff-neutral-2 . "#3a3a3a")

    (bg-mark-sel . "#a0f0cf") (fg-mark-sel . "#005040")
    (bg-mark-del . "#ffccbb") (fg-mark-del . "#840040")
    (bg-mark-alt . "#f5d88f") (fg-mark-alt . "#782900"))
  "The entire palette of `modus-operandi-theme'.
Each element has the form (NAME . HEX) with the former as a
symbol and the latter as a string.")

;;;; Modus Vivendi

(deftheme modus-vivendi
  "Dark theme that conforms with the highest accessibility
  standard for color contrast between background and foreground
  elements (WCAG AAA).")

(define-obsolete-variable-alias
  'modus-vivendi-theme-default-colors-alist
  'modus-themes-vivendi-colors
  "1.0.0")

(defconst modus-themes-vivendi-colors
'(;; base values
    (bg-main . "#000000") (fg-main . "#ffffff")
    (bg-alt . "#181a20") (fg-alt . "#a8a8a8")
    (bg-dim . "#110b11") (fg-dim . "#e0e6f0")
    ;; specifically for on/off states (e.g. `mode-line')
    ;;
    ;; must be combined with themselves
    (bg-active . "#323232") (fg-active . "#f4f4f4")
    (bg-inactive . "#1e1e1e") (fg-inactive . "#bfc0c4")
    ;; special base values, used only for cases where the above
    ;; fg-* or bg-* cannot or should not be used (to avoid confusion)
    ;; must be combined with: {fg,bg}-{main,alt,dim}
    (bg-special-cold . "#203448") (fg-special-cold . "#c6eaff")
    (bg-special-mild . "#00322e") (fg-special-mild . "#bfebe0")
    (bg-special-warm . "#382f27") (fg-special-warm . "#f8dec0")
    (bg-special-calm . "#392a48") (fg-special-calm . "#fbd6f4")
    ;; styles for the main constructs
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
    (red . "#ff8059") (green . "#44bc44")
    (yellow . "#eecc00") (blue . "#2fafff")
    (magenta . "#feacd0") (cyan . "#00d3d0")
    ;; styles for common, but still specialized constructs
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
    (red-alt . "#f4923b") (green-alt . "#80d200")
    (yellow-alt . "#cfdf30") (blue-alt . "#79a8ff")
    (magenta-alt . "#f78fe7") (cyan-alt . "#4ae8fc")
    ;; same purpose as above, just slight differences
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
    (red-alt-other . "#ff9977") (green-alt-other . "#00cd68")
    (yellow-alt-other . "#f0ce43") (blue-alt-other . "#00bcff")
    (magenta-alt-other . "#b6a0ff") (cyan-alt-other . "#6ae4b9")
    ;; styles for desaturated foreground text
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
    (red-faint . "#ffa0a0") (green-faint . "#88cf88")
    (yellow-faint . "#d2b580") (blue-faint . "#92baff")
    (magenta-faint . "#e0b2d6") (cyan-faint . "#a0bfdf")

    (red-alt-faint . "#f5aa80") (green-alt-faint . "#a8cf88")
    (yellow-alt-faint . "#cabf77") (blue-alt-faint . "#a4b0ff")
    (magenta-alt-faint . "#ef9fe4") (cyan-alt-faint . "#90c4ed")

    (red-alt-other-faint . "#ff9fbf") (green-alt-other-faint . "#88cfaf")
    (yellow-alt-other-faint . "#d0ba95") (blue-alt-other-faint . "#8fc5ff")
    (magenta-alt-other-faint . "#d0b4ff") (cyan-alt-other-faint . "#a4d0bb")
    ;; styles for elements that should be very subtle, yet accented
    ;;
    ;; must be combined with: `bg-main', `bg-alt', `bg-dim' or any of
    ;; the "nuanced" backgrounds
    (red-nuanced . "#ffcccc") (green-nuanced . "#b8e2b8")
    (yellow-nuanced . "#dfdfb0") (blue-nuanced . "#bfd9ff")
    (magenta-nuanced . "#e5cfef") (cyan-nuanced . "#a8e5e5")
    ;; styles for slightly accented background
    ;;
    ;; must be combined with any of the above foreground values
    (red-nuanced-bg . "#2c0614") (green-nuanced-bg . "#001904")
    (yellow-nuanced-bg . "#221000") (blue-nuanced-bg . "#0f0e39")
    (magenta-nuanced-bg . "#230631") (cyan-nuanced-bg . "#041529")
    ;; styles for elements that should draw attention to themselves
    ;;
    ;; must be combined with: `bg-main'
    (red-intense . "#fb6859") (green-intense . "#00fc50")
    (yellow-intense . "#ffdd00") (blue-intense . "#00a2ff")
    (magenta-intense . "#ff8bd4") (cyan-intense . "#30ffc0")
    ;; styles for background elements that should be visible yet
    ;; subtle
    ;;
    ;; must be combined with: `fg-dim'
    (red-subtle-bg . "#762422") (green-subtle-bg . "#2f4a00")
    (yellow-subtle-bg . "#604200") (blue-subtle-bg . "#10387c")
    (magenta-subtle-bg . "#49366e") (cyan-subtle-bg . "#00415e")
    ;; styles for background elements that should be visible and
    ;; distinguishable
    ;;
    ;; must be combined with: `fg-main'
    (red-intense-bg . "#a4202a") (green-intense-bg . "#006800")
    (yellow-intense-bg . "#874900") (blue-intense-bg . "#2a40b8")
    (magenta-intense-bg . "#7042a2") (cyan-intense-bg . "#005f88")
    ;; styles for refined contexts where both the foreground and the
    ;; background need to have the same/similar hue
    ;;
    ;; must be combined with themselves OR the foregrounds can be
    ;; combined with any of the base backgrounds
    (red-refine-bg . "#77002a") (red-refine-fg . "#ffb9ab")
    (green-refine-bg . "#00422a") (green-refine-fg . "#9ff0cf")
    (yellow-refine-bg . "#693200") (yellow-refine-fg . "#e2d980")
    (blue-refine-bg . "#242679") (blue-refine-fg . "#8ec6ff")
    (magenta-refine-bg . "#71206a") (magenta-refine-fg . "#ffcaf0")
    (cyan-refine-bg . "#004065") (cyan-refine-fg . "#8ae4f2")
    ;; styles that are meant exclusively for the mode line
    ;;
    ;; must be combined with: `bg-active', `bg-inactive'
    (red-active . "#ffa7ba") (green-active . "#70d73f")
    (yellow-active . "#dbbe5f") (blue-active . "#34cfff")
    (magenta-active . "#d5b1ff") (cyan-active . "#00d8b4")
    ;; styles that are meant exclusively for the fringes
    ;;
    ;; must be combined with `fg-main'
    (red-fringe-bg . "#8f1f4b") (green-fringe-bg . "#006700")
    (yellow-fringe-bg . "#6f4f00") (blue-fringe-bg . "#3f33af")
    (magenta-fringe-bg . "#6f2f89") (cyan-fringe-bg . "#004f8f")
    ;; styles reserved for specific faces
    ;;
    ;; `bg-hl-line' is between `bg-dim' and `bg-alt', so it should
    ;; work with all accents that cover those two, plus `bg-main'
    ;;
    ;; `bg-hl-alt' and `bg-hl-alt-intense' should only be used when no
    ;; other greyscale or fairly neutral background is available to
    ;; properly draw attention to a given construct
    ;;
    ;; `bg-header' is between `bg-active' and `bg-inactive', so it
    ;; can be combined with any of the "active" values, plus the
    ;; "special" and base foreground colors
    ;;
    ;; `bg-paren-match', `bg-paren-match-intense', `bg-region' and
    ;; `bg-tab-active' must be combined with `fg-main', while
    ;; `bg-tab-inactive' should be combined with `fg-dim'
    ;;
    ;; `bg-tab-bar' is only intended for the bar that holds the tabs and
    ;; can only be combined with `fg-main'
    ;;
    ;; `fg-tab-active' is meant to be combined with `bg-tab-active',
    ;; though only for styling special elements, such as underlining
    ;; the current tab
    ;;
    ;; `fg-escape-char-construct' and `fg-escape-char-backslash' can
    ;; be combined `bg-main', `bg-dim', `bg-alt'
    ;;
    ;; `fg-lang-error', `fg-lang-warning', `fg-lang-note' can be
    ;; combined with `bg-main', `bg-dim', `bg-alt'
    ;;
    ;; `fg-mark-sel', `fg-mark-del', `fg-mark-alt' can be combined
    ;; with `bg-main', `bg-dim', `bg-alt', `bg-hl-line'
    ;;
    ;; `fg-unfocused' must be combined with `fg-main'
    ;;
    ;; `fg-docstring', `fg-comment-yellow' can be `bg-main', `bg-dim',
    ;; `bg-alt'
    ;;
    ;; the window divider colors apply to faces with just an fg value
    ;;
    ;; all pairs are combinable with themselves
    (bg-hl-line . "#151823")
    (bg-hl-line-intense . "#2f2f2f")
    (bg-hl-alt . "#181732")
    (bg-hl-alt-intense . "#282e46")
    (bg-paren-match . "#5f362f")
    (bg-paren-match-intense . "#7416b5")
    (bg-region . "#3c3c3c")

    (bg-tab-bar . "#2c2c2c")
    (bg-tab-active . "#0e0e0e")
    (bg-tab-inactive . "#3d3d3d")
    (fg-tab-active . "#5ac3cf")

    (fg-escape-char-construct . "#e7a59a")
    (fg-escape-char-backslash . "#abab00")

    (fg-lang-error . "#ef8690")
    (fg-lang-warning . "#b0aa00")
    (fg-lang-note . "#9d9def")

    (fg-window-divider-inner . "#646464")
    (fg-window-divider-outer . "#969696")

    (fg-unfocused . "#93959b")

    (fg-docstring . "#b0d6f5")
    (fg-comment-yellow . "#cab98f")

    (bg-header . "#212121") (fg-header . "#dddddd")

    (bg-whitespace . "#170016") (fg-whitespace . "#a4959f")

    (bg-diff-heading . "#304466") (fg-diff-heading . "#dadffe")
    (bg-diff-added . "#0a280a") (fg-diff-added . "#94ba94")
    (bg-diff-changed . "#2a2000") (fg-diff-changed . "#b0ba9f")
    (bg-diff-removed . "#40160f") (fg-diff-removed . "#c6adaa")

    (bg-diff-refine-added . "#005a36") (fg-diff-refine-added . "#e0f6e0")
    (bg-diff-refine-changed . "#585800") (fg-diff-refine-changed . "#ffffcc")
    (bg-diff-refine-removed . "#852828") (fg-diff-refine-removed . "#ffd9eb")

    (bg-diff-focus-added . "#203d20") (fg-diff-focus-added . "#b4ddb4")
    (bg-diff-focus-changed . "#4a3a10") (fg-diff-focus-changed . "#d0daaf")
    (bg-diff-focus-removed . "#5e2526") (fg-diff-focus-removed . "#eebdba")

    (bg-diff-neutral-0 . "#575757") (fg-diff-neutral-0 . "#fcfcfc")
    (bg-diff-neutral-1 . "#454545") (fg-diff-neutral-1 . "#dddddd")
    (bg-diff-neutral-2 . "#313131") (fg-diff-neutral-2 . "#bfbfbf")

    (bg-mark-sel . "#002f2f") (fg-mark-sel . "#60cfa2")
    (bg-mark-del . "#5a0000") (fg-mark-del . "#ff99aa")
    (bg-mark-alt . "#3f2210") (fg-mark-alt . "#f0aa20"))
  "The entire palette of `modus-vivendi-theme'.
Each element has the form (NAME . HEX) with the former as a
symbol and the latter as a string.")

(make-obsolete 'modus-operandi-theme-override-colors-alist nil "1.0.0")

(make-obsolete 'modus-vivendi-theme-override-colors-alist nil "1.0.0")

;;;; Face specifications

(defvar modus-themes
  '(
   `(default ((,class :background ,bg-main :foreground ,fg-main)))
   `(font-lock-comment-face ((,class :foreground "blue"))))
  "Face specs for use with `modus-themes-core-theme'.")

(provide 'modus-themes)
;;; modus-themes.el ends here
