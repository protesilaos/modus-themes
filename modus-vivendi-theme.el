;; modus-vivendi.el --- very accessible dark theme (WCAG AAA)

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

(deftheme modus-vivendi
  "Created 2019-07-08.")

(custom-theme-set-faces
 'modus-vivendi
 '(default ((t (:foreground "#ffffff" :background "#000000"))))
 '(cursor ((t (:background "#00ff00"))))
 '(italic ((t (:foreground "#c0c022"))))
 '(warning ((t (:foreground "#ffa900" :weight bold))))
 '(escape-glyph ((t (:weight bold :foreground "#d7ae00"))))
 '(homoglyph ((t (:foreground "#d7ae00"))))
 '(minibuffer-prompt ((t (:foreground "#00ff00"))))
 '(highlight ((t (:foreground "#ffffff" :background "#b00055" :weight bold))))
 '(region ((t (:foreground "#ffffff" :background "#003388"))))
 '(shadow ((t (:foreground "#c4bdaf"))))
 '(secondary-selection ((t (:foreground "#ebebeb" :background "#444488"))))
 '(trailing-whitespace ((t (:background "#ef6f44"))))
 '(font-lock-builtin-face ((t (:foreground "#33dd99"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#c4bdaf"))))
 '(font-lock-constant-face ((t (:foreground "#ffaba3"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#aaef55"))))
 '(font-lock-keyword-face ((t (:foreground "#eecc77"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#ccccff"))))
 '(font-lock-type-face ((t (:weight bold :foreground "#00ddaa"))))
 '(font-lock-variable-name-face ((t (:weight bold :foreground "#9999ff"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#f69d6a"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#1db5c3"))))
 '(link-visited ((t (:foreground "#ee88ff"))))
 '(fringe ((t (:inherit (default)))))
 '(header-line ((t (:inverse-video t :inherit (default)))))
 '(tooltip ((t (:foreground "#000000" :background "lightyellow"))))
 '(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#eeeeee" :background "#121212"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold :inherit (highlight)))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "#ffffff" :style released-button)))))
 '(mode-line-inactive ((t (:foreground "#cccccc" :background "#0a0a0a"))))
 '(isearch ((t (:weight bold :foreground "#ffffff" :background "#a0008d"))))
 '(isearch-fail ((t (:foreground "#ffffff" :background "#880000"))))
 '(lazy-highlight ((t (:foreground "#dedede" :background "#804080"))))
 '(match ((t (:foreground "#000000" :background "#66aaff"))))
 '(next-error ((t (:foreground "#333333" :background "#ffcc00"))))
 '(query-replace ((t (:inherit (isearch)))))
 '(show-paren-match ((t (:background "#8028c1" :foreground "#ffffff"))))
 '(show-paren-match-expression ((t (:inherit (show-paren-match)))))
 '(show-paren-mismatch ((t (:background "#555500" :foreground "#ffffff"))))
 '(ido-first-match ((t (:weight bold))))
 '(ido-only-match ((t (:foreground "#0000aa"))))
 '(ido-incomplete-regexp ((t (:inherit (font-lock-warning-face)))))
 '(ido-subdir ((t (:foreground "#8b3800"))))
 '(ido-indicator ((t (:background "#ffa200" :foreground "#000000" :width condensed))))
 '(ido-virtual ((t (:inherit (font-lock-builtin-face)))))
 '(completions-annotations ((t (:inherit (italic)))))
 '(completions-common-part ((t nil)))
 '(completions-first-difference ((t (:inherit (bold)))))
 '(line-number ((t (:foreground "#c4bdaf" :background "#202020"))))
 '(line-number-current-line ((t (:foreground "#fff4f0" :weight bold)))))
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

(provide-theme 'modus-vivendi)
