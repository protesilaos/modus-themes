;; modus-operandi.el --- very accessible light theme (WCAG AAA)

;; Copyright (c) 2019 Protesilaos Stavrou <info@protesilaos.com>

;; Version: 0.1.0alpha

;; This program is free software; you can redistribute it and/or 
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Early prototype.  Tested on Emacs 26.1 (Debian 10).  The purpose of
;; this theme is to provide a consistent _minimum contrast ratio_
;; between background and foreground values of 7:1.  This meets the
;; highest such accessibility criterion per the guidelines of the
;; Worldwide Web Consortium's Working Group on Accessibility.

;; The template is provided by `customize-create-theme'.

(deftheme modus-operandi
  "Created 2019-07-08.")

(custom-theme-set-faces
 'modus-operandi
 '(default ((t (:foreground "#000000" :background "#ffffff"))))
 '(cursor ((t (:background "#000000"))))
 '(italic ((t (:foreground "#8b3800"))))
 '(warning ((t (:foreground "#8b3800" :weight bold))))
 '(escape-glyph ((t (:weight bold :foreground "#714900"))))
 '(homoglyph ((t (:foreground "#714900"))))
 '(minibuffer-prompt ((t (:foreground "#0000ff"))))
 '(highlight ((t (:foreground "#000000" :background "#00eeff" :weight bold))))
 '(region ((t (:foreground "#000000" :background "#d3f1ff"))))
 '(shadow ((t (:foreground "#68607d"))))
 '(secondary-selection ((t (:foreground "#222222" :background "#55d0d0"))))
 '(whitespace-tab ((t (:foreground "#888888"))))
 '(trailing-whitespace ((t (:background "#880000"))))
 '(font-lock-builtin-face ((t (:foreground "#800090"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#5f4d4f"))))
 '(font-lock-constant-face ((t (:foreground "#0000ff"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#b30042"))))
 '(font-lock-keyword-face ((t (:foreground "#7000f0"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#0047d0"))))
 '(font-lock-type-face ((t (:weight bold :foreground "#a0009d"))))
 '(font-lock-variable-name-face ((t (:weight bold :foreground "#714900"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#9b2230"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#003399"))))
 '(link-visited ((t (:foreground "#7733bb"))))
 '(fringe ((t (:inherit (default)))))
 '(header-line ((t (:inverse-video t :inherit (default)))))
 '(tooltip ((t (:foreground "#000000" :background "lightyellow"))))
 '(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#464340" :background "#f8f2ff"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold :inherit (highlight)))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "#000000" :style released-button)))))
 '(mode-line-inactive ((t (:foreground "#58506d" :background "#e9e9ee"))))
 '(isearch ((t (:weight bold :foreground "#000000" :background "#90ff90"))))
 '(isearch-fail ((t (:foreground "#000000" :background "#eebbbb"))))
 '(lazy-highlight ((t (:foreground "#111111" :background "#ddeecc"))))
 '(match ((t (:foreground "#ffffff" :background "#005f26"))))
 '(next-error ((t (:foreground "#f3f1f3" :background "#b94000"))))
 '(query-replace ((t (:inherit (isearch)))))
 '(show-paren-match ((t (:background "#ffccbb" :foreground "#000000"))))
 '(show-paren-match-expression ((t (:inherit (show-paren-match)))))
 '(show-paren-mismatch ((t (:background "#ffff00" :foreground "#000000"))))
 '(ido-first-match ((t (:weight bold))))
 '(ido-only-match ((t (:foreground "#0000aa"))))
 '(ido-incomplete-regexp ((t (:inherit (font-lock-warning-face)))))
 '(ido-subdir ((t (:foreground "#8b3800" :weight bold))))
 '(ido-indicator ((t (:background "#ffa200" :foreground "#000000" :width condensed))))
 '(ido-virtual ((t (:inherit (font-lock-builtin-face)))))
 '(completions-annotations ((t (:inherit (italic)))))
 '(completions-common-part ((t nil)))
 '(completions-first-difference ((t (:inherit (bold)))))
 '(line-number ((t (:foreground "#5f4d4f" :background "#f3f1f3"))))
 '(line-number-current-line ((t (:foreground "#000000" :weight bold)))))
 '(markdown-header-face ((t (:weight bold))))
 '(sh-heredoc ((t (:inherit (font-lock-string-face)))))
 '(sh-quoted-exec ((t (:inherit (font-lock-builtin-face)))))
 '(diff-added ((t (:foreground "#004400" :background "#f3fff3"))))
 '(diff-indicator-added ((t (:inherit (diff-added))))) 
 '(diff-changed ((t (:foreground "#555500" :background "#fffef3"))))
 '(diff-indicator-changed ((t (:inherit (diff-changed))))) 
 '(diff-file-header ((t (:foreground "#000000" :background "#f3f1f3" :weight bold))))
 '(diff-function ((t (:inherit (diff-file-header) :weight normal))))
 '(diff-header ((t (:inherit (diff-function)))))
 '(diff-hunk-header ((t (:inherit (diff-function)))))
'(diff-index-header ((t (:inherit (diff-file-header) :weight normal))))

(provide-theme 'modus-operandi)
