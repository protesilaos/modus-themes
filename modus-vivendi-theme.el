;; modus-vivendi-theme.el --- very accessible dark theme (WCAG AAA)
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2019 Protesilaos Stavrou <info@protesilaos.com>
;;
;; This program is free software; you can redistribute it and/or
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The primary purpose of this theme is to provide a consistent
;; minimum contrast ratio between background and foreground values of
;; 7:1 or higher.  This meets the highest such accessibility criterion
;; per the guidelines of the Worldwide Web Consortium's Working Group
;; on Accessibility.
;;
;; The secondary goal is to provide as close to full coverage as
;; possible.  The output of `list-faces-display' offers all we need.
;; Note though, that it is difficult to create theme styles without
;; actually using the packages/interfaces that require them.  As such,
;; the development of this theme will be incremental, gradually
;; incorporating the customisations for packages I use or have been
;; exposed to.
;;
;; The original template is provided by `customize-create-theme'.  The
;; design of the colour variables was taken from the Tango theme that
;; comes packaged with GNU Emacs (at least it does on Debian 10).

(deftheme modus-vivendi
  "Dark theme that conforms with the highest accessibility
  standard for colour contrast between background and
  foreground elements (WCAG AAA).")

;; These faces will be inherited by actual constructs.  They are meant
;; for those cases where a face needs to distinguish its output from
;; the rest of the text, such as `isearch' and `occur'…  We define
;; these separately in order to combine each colour with its
;; appropriate foreground value.  This is to ensure a consistent
;; contrast ratio of >= 7:1.
(defface modus-theme-subtle-bg-red nil t)
(defface modus-theme-subtle-bg-green nil t)
(defface modus-theme-subtle-bg-yellow nil t)
(defface modus-theme-subtle-bg-blue nil t)
(defface modus-theme-subtle-bg-magenta nil t)
(defface modus-theme-subtle-bg-cyan nil t)
(defface modus-theme-subtle-bg-neutral nil t)
(defface modus-theme-intense-bg-red nil t)
(defface modus-theme-intense-bg-green nil t)
(defface modus-theme-intense-bg-yellow nil t)
(defface modus-theme-intense-bg-blue nil t)
(defface modus-theme-intense-bg-magenta nil t)
(defface modus-theme-intense-bg-cyan nil t)
(defface modus-theme-intense-bg-neutral nil t)

;; Define colour palette.  Each colour must have a >= 7:1 contrast
;; ratio relative to the foreground/background colour it is rendered
;; against.
(let ((class '((class color) (min-colors 89)))
	  (fg-main "#ffffff") (bg-main "#000000")
	  (fg-alt "#d8cdcf") (bg-alt "#201626")
	  (fg-dim "#e0e6f0") (bg-dim "#14091c")
	  ;; specifically for on/off states (e.g. mode-line)
	  ;; must be combined with themselves
	  (fg-active "#f5f5f5") (bg-active "#303030")
	  (fg-inactive "#a3a3a3") (bg-inactive "#181818")
	  ;; styles for the main constructs
	  ;; must be combined with: bg-main, bg-alt, bg-dim
	  (red "#ff9566") (green "#44bc44")
	  (yellow "#eecc00") (blue "#33beff")
	  (magenta "#df8cf2") (cyan "#00d3d0")
	  ;; styles for common, but still specialised constructs
	  ;; must be combined with: bg-main, bg-alt, bg-dim
	  (red-alt "#f0a500") (green-alt "#58dd13")
	  (yellow-alt "#f0ce33") (blue-alt "#68b4ff")
	  (magenta-alt "#ba94fa") (cyan-alt "#4ae8fc")
	  ;; same purpose as above, just slight differences
	  ;; must be combined with: bg-main, bg-alt, bg-dim
	  (red-alt-other "#ffaa99") (green-alt-other "#9afd00")
	  (yellow-alt-other "#e0dd00") (blue-alt-other "#00aeff")
	  (magenta-alt-other "#fe85e0") (cyan-alt-other "#69eacf")
	  ;; styles for elements that should draw attention to themselves
	  ;; must be combined with: bg-main
	  (red-intense "#ff6230") (green-intense "#00fc50")
	  (yellow-intense "#ffdd00") (blue-intense "#9dafff")
	  (magenta-intense "#ff70cf") (cyan-intense "#30ffc0")
	  ;; styles for background elements that should be visible yet subtle
	  ;; must be combined with: fg-dim
	  (red-subtle-bg "#990000") (green-subtle-bg "#3f5100")
	  (yellow-subtle-bg "#654230") (blue-subtle-bg "#2c3fa8")
	  (magenta-subtle-bg "#720a4f") (cyan-subtle-bg "#00425f")
	  ;; styles for background elements that should be visible and distinguishable
	  ;; must be combined with: fg-main
	  (red-intense-bg "#b60000") (green-intense-bg "#006800")
	  (yellow-intense-bg "#973d00") (blue-intense-bg "#0a12ca")
	  (magenta-intense-bg "#7f2dba") (cyan-intense-bg "#064a70"))
  (custom-theme-set-faces
   'modus-vivendi
   ;; custom faces that are inherited by other constructs below
   ;;; subtle coloured backgrounds
   `(modus-theme-subtle-bg-red ((,class (:background ,red-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-bg-green ((,class (:background ,green-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-bg-yellow ((,class (:background ,yellow-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-bg-blue ((,class (:background ,blue-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-bg-magenta ((,class (:background ,magenta-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-bg-cyan ((,class (:background ,cyan-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-bg-neutral ((,class (:background ,bg-alt :foreground ,fg-alt))))
   ;;; intense coloured backgrounds
   `(modus-theme-intense-bg-red ((,class (:background ,red-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-bg-green ((,class (:background ,green-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-bg-yellow ((,class (:background ,yellow-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-bg-blue ((,class (:background ,blue-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-bg-magenta ((,class (:background ,magenta-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-bg-cyan ((,class (:background ,cyan-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-bg-neutral ((,class (:background ,bg-dim :foreground ,fg-main))))
   ;; actual styles
   `(default ((,class (:background ,bg-main :foreground ,fg-main))))
   `(fringe ((t (:inherit (default)))))
   `(cursor ((,class (:background ,fg-main))))
   `(italic ((,class (:foreground ,yellow :slant italic))))
   `(warning ((,class (:foreground ,red :weight bold))))
   `(error ((t (:inherit (modus-theme-intense-bg-red) :weight bold))))
   `(success ((t (:inherit (modus-theme-intense-bg-green) :weight bold))))
   `(escape-glyph ((,class (:weight bold :foreground ,yellow-alt))))
   `(homoglyph ((,class (:foreground ,yellow-alt-other))))
   `(minibuffer-prompt ((,class (:foreground ,blue-intense))))
   `(highlight ((t (:inherit (modus-theme-subtle-bg-blue)))))
   `(highlight-changes ((,class (:foreground ,yellow-alt-other))))
   `(highlight-changes-delete ((,class (:foreground ,red-alt-other :underline t))))
   `(hl-line ((,class (:background ,bg-alt))))
   `(hi-black-b ((t ((:background ,fg-main :foreground ,bg-main)))))
   `(hi-green-b ((t (:inherit (modus-theme-intense-bg-green)))))
   `(hi-blue-b ((t (:inherit (modus-theme-intense-bg-blue)))))
   `(hi-red-b ((t (:inherit (modus-theme-intense-bg-red)))))
   `(hi-green ((,class (:background ,bg-alt :underline (:color foreground-color :style line) :foreground ,green))))
   `(hi-blue ((,class (:background ,bg-alt :underline (:color foreground-color :style line) :foreground ,blue))))
   `(hi-yellow ((,class (:background ,bg-alt :underline (:color foreground-color :style line) :foreground ,yellow))))
   `(hi-pink ((,class (:background ,bg-alt :underline (:color foreground-color :style line) :foreground ,magenta))))
   `(region ((,class (:inherit (modus-theme-intense-bg-cyan)))))
   `(shadow ((,class (:foreground ,fg-alt))))
   `(secondary-selection ((t (:inherit (modus-theme-subtle-bg-magenta)))))
   `(whitespace-tab ((,class (:foreground ,fg-alt))))
   `(trailing-whitespace ((,class (:background ,red-alt))))
   `(font-lock-builtin-face ((,class (:foreground ,magenta-alt))))
   `(font-lock-comment-face ((t (:foreground ,fg-alt))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-constant-face ((,class (:foreground ,blue-alt-other))))
   `(font-lock-string-face ((,class (:foreground ,blue-alt))))
   `(font-lock-doc-face ((,class (:foreground ,cyan-alt-other))))
   `(font-lock-function-name-face ((,class (:foreground ,magenta :weight normal))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta-alt-other))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((,class (:foreground ,magenta))))
   `(font-lock-regexp-grouping-backslash ((t (:weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:weight bold))))
   `(font-lock-type-face ((,class (:foreground ,magenta-alt :weight normal))))
   `(font-lock-variable-name-face ((,class (:foreground ,cyan :weight normal))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,yellow-alt-other))))
   `(link ((,class (:underline (:color foreground-color :style line) :foreground ,blue-alt-other))))
   `(link-visited ((,class (:foreground ,magenta-alt-other))))
   `(button ((t (:inherit (link)))))
   `(header-line ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(tooltip ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   `(mode-line ((,class (:box (:line-width 1 :color ,bg-active) :background ,bg-active :foreground ,fg-active))))
   `(mode-line-buffer-id ((t (:weight normal))))
   `(mode-line-emphasis ((t (:weight bold :inherit (highlight)))))
   `(mode-line-highlight ((,class (:box (:line-width 1 :color ,fg-main) :background ,bg-main))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color ,bg-inactive) :background ,bg-inactive :foreground ,fg-inactive))))
   `(which-func ((,class (:foreground ,fg-main))))
   `(isearch ((t (:weight bold :inherit (modus-theme-intense-bg-green)))))
   `(isearch-fail ((t (:inherit (modus-theme-subtle-bg-red)))))
   `(lazy-highlight ((t (:inherit (modus-theme-subtle-bg-green)))))
   `(match ((t (:inherit (modus-theme-intense-bg-blue)))))
   `(next-error ((t (:inherit (modus-theme-intense-bg-red))))) ;; check
   `(query-replace ((t (:inherit (isearch)))))
   `(show-paren-match ((t (:inherit (modus-theme-intense-bg-magenta) :weight bold))))
   `(show-paren-match-expression ((,class :background ,bg-active :foreground ,fg-active)))
   `(show-paren-mismatch ((t (:inherit (modus-theme-intense-bg-red)))))
   `(completions-annotations ((t (:inherit (italic)))))
   `(completions-common-part ((t nil)))
   `(completions-first-difference ((t (:inherit (bold)))))
   ;;;; compilation feedback
   `(compilation-error ((t (:inherit (modus-theme-intense-bg-red)))))
   `(compilation-mode-line-exit ((t (:inherit (modus-theme-intense-bg-cyan) :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit (modus-theme-intense-bg-red) :weight bold))))
   `(compilation-mode-line-run ((t (:inherit (modus-theme-subtle-bg-magenta)))))
   `(compilation-warning ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   ;;;; ido-mode
   `(ido-first-match ((t (:weight bold))))
   `(ido-only-match ((,class (:foreground ,cyan-alt-other))))
   `(ido-incomplete-regexp ((t (:inherit (font-lock-warning-face)))))
   `(ido-subdir ((,class (:foreground ,cyan))))
   `(ido-indicator ((,class (:background ,cyan-intense :foreground ,bg-main)))) ;; check
   `(ido-virtual ((t (:inherit (font-lock-builtin-face)))))
   ;;;; dired
   `(dired-header ((t (:foreground ,fg-main :weight bold))))
   `(dired-directory ((t (:foreground ,blue :weight bold))))
   `(dired-flagged ((t (:inherit (modus-theme-intense-bg-red)))))
   `(dired-marked ((t (:inherit (modus-theme-intense-bg-magenta)))))
   `(dired-subtree-depth-1-face ((,class (:background ,bg-dim))))
   `(dired-subtree-depth-2-face ((,class (:background ,bg-alt))))
   `(dired-subtree-depth-3-face ((,class (:background ,bg-main))))
   `(dired-subtree-depth-4-face ((,class (:background ,bg-dim))))
   `(dired-subtree-depth-5-face ((,class (:background ,bg-alt))))
   `(dired-subtree-depth-6-face ((,class (:background ,bg-main))))
   `(dired-async-failures ((t (:inherit (modus-theme-intense-bg-red)))))
   `(dired-async-message ((t (:inherit (modus-theme-intense-bg-yellow)))))
   `(dired-async-mode-message ((t (:inherit (modus-theme-intense-bg-magenta)))))
   ;;;; info pages (help pages)
   `(info-node ((t ((:weight bold)))))
   `(info-header-node ((t ((:weight bold)))))
   `(info-menu-star ((t (:inherit default))))
   ;;;; display-line-numbers-mode (and global variant)
   `(line-number ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(line-number-current-line ((,class (:background ,bg-dim :foreground ,fg-dim :weight bold))))
   ;;;; flyspell
   `(flyspell-duplicate ((t (:underline (:color foreground-color :style line) :inherit (modus-theme-subtle-bg-yellow)))))
   `(flyspell-incorrect ((t (:underline (:color foreground-color :style line) :inherit (modus-theme-subtle-bg-red)))))
   ;;;; markdown format
   `(markdown-header-face ((t (:weight bold))))
   `(markdown-pre-face ((,class (:background ,bg-dim :foreground ,magenta-alt))))
   `(markdown-inline-code-face ((t (:inherit (markdown-pre-face)))))
   `(markdown-blockquote-face ((,class (:background ,bg-alt))))
   ;;;; shell scripts
   `(sh-heredoc ((t (:inherit (font-lock-string-face)))))
   `(sh-quoted-exec ((t (:inherit (font-lock-builtin-face)))))
   ;;;; diff-mode
   `(diff-added ((t (:inherit (modus-theme-subtle-bg-green)))))
   `(diff-indicator-added ((t (:inherit (diff-added)))))
   `(diff-changed ((t (:inherit (diff-added) :weight bold))))
   `(diff-indicator-changed ((t (:inherit (diff-changed)))))
   `(diff-removed ((t (:inherit (modus-theme-subtle-bg-red)))))
   `(diff-indicator-removed ((t (:inherit (diff-removed)))))
   `(diff-file-header ((,class (:background ,bg-alt :weight bold :foreground ,fg-main))))
   `(diff-function ((t (:inherit (diff-file-header) :weight normal))))
   `(diff-header ((t (:inherit (diff-function)))))
   `(diff-hunk-header ((t (:inherit (diff-function)))))
   `(diff-index-header ((t (:inherit (diff-function)))))
   ;;;; git
   `(git-commit-overlong-summary ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   ;;;; magit
   `(magit-section ((,class (:background ,bg-dim :foreground ,fg-main))))
   `(magit-section-heading ((t (:inherit font-lock-variable-name-face))))
   `(magit-section-highlight ((,class (:background ,bg-main))))
   `(magit-diff-file-heading ((t (:inherit (diff-file-header)))))
   `(magit-diff-file-heading-highlight ((t (:inherit (magit-section-highlight)))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg-active :foreground ,fg-active))))
   `(magit-diff-context ((,class (:background ,bg-dim))))
   `(magit-diff-context-highlight ((t (:background ,bg-main))))
   `(magit-diff-removed ((t (:inherit (modus-theme-subtle-bg-red)))))
   `(magit-diff-removed-highlight ((t (:inherit (modus-theme-intense-bg-red)))))
   `(magit-diff-added ((t (:inherit (modus-theme-subtle-bg-green)))))
   `(magit-diff-added-highlight ((t (:inherit (modus-theme-intense-bg-green)))))
   `(magit-diff-base ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   `(magit-diff-base-highlight ((t (:inherit (modus-theme-intense-bg-yellow)))))
   `(magit-diff-lines-heading ((t (:inherit (modus-theme-subtle-bg-magenta)))))
   `(magit-diff-changed ((t (:weight bold))))
   `(magit-diff-changed-highlight ((t (:weight bold))))
   `(magit-diffstat-added ((t (:inherit (magit-diff-added)))))
   `(magit-diffstat-removed ((t (:inherit (magit-diff-removed)))))
   `(magit-branch-local ((t (:inherit (font-lock-builtin-face)))))
   `(magit-branch-current ((t (:inherit (font-lock-type-face)))))
   `(magit-branch-remote ((t (:inherit (font-lock-function-name-face)))))
   `(magit-hash ((t (:inherit (shadow)))))
   `(magit-process-ok ((t (:inherit (success)))))
   `(magit-process-ng ((t (:inherit (error)))))
   `(magit-reflog-amend ((,class (:foreground ,magenta-intense))))
   `(magit-reflog-checkout ((,class (:foreground ,blue-intense))))
   `(magit-reflog-merge ((,class (:foreground ,green-intense))))
   `(magit-reflog-commit ((,class (:foreground ,green-intense))))
   `(magit-reflog-other ((,class (:foreground ,cyan-intense))))
   `(magit-reflog-rebase ((,class (:foreground ,magenta-intense))))
   `(magit-reflog-remote ((,class (:foreground ,cyan-intense))))
   `(magit-reflog-reset ((,class (:foreground ,red-intense))))
   `(magit-reflog-cherry-pick ((,class (:foreground ,green-intense))))
   `(magit-cherry-equivalent ((,class (:foreground ,magenta-intense))))
   `(magit-cherry-unmatched ((,class (:foreground ,cyan-intense))))
   `(magit-signature-untrusted ((,class (:foreground ,cyan-intense))))
   `(magit-signature-revoked ((,class (:foreground ,magenta-intense))))
   `(magit-signature-good ((,class (:foreground ,green-intense))))
   `(magit-signature-bad ((,class (:foreground ,red-intense :weight bold))))
   `(magit-signature-error ((,class (:foreground ,red-intense))))
   `(magit-signature-expired ((,class (:foreground ,yellow-intense))))
   `(magit-signature-expired-key ((,class (:foreground ,yellow-intense))))
   ;;;; elfeed
   `(elfeed-search-date-face ((t (:inherit (default)))))
   `(elfeed-search-date-face ((t (:inherit (default) :weight bold))))
   `(elfeed-search-feed-face ((t (:inherit (default)))))
   `(elfeed-search-tag-face ((t (:inherit (default)))))
   `(elfeed-log-debug-level-face ((t (:inherit (modus-theme-intense-bg-magenta)))))
   `(elfeed-log-error-level-face ((t (:inherit (modus-theme-intense-bg-red)))))
   `(elfeed-log-warn-level-face ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   `(elfeed-log-info-level-face ((t (:inherit (modus-theme-subtle-bg-cyan)))))
   ;;;; found in elfeed, should apply elsewhere check
   `(message-header-name ((t (:inherit (default) :weight bold))))
   `(message-header-to ((t (:inherit (default) :weight bold))))
   `(message-header-subject ((t (:inherit (default) :weight bold))))
   `(message-header-other ((t (:inherit (default)))))
   ;;;; org-mode
   `(org-level-1 ((,class (:weight bold :foreground ,fg-main))))
   `(org-level-2 ((,class (:weight bold :foreground ,cyan-alt-other))))
   `(org-level-3 ((,class (:weight bold :foreground ,magenta-alt-other))))
   `(org-level-4 ((,class (:weight bold :foreground ,green-alt-other))))
   `(org-level-5 ((,class (:weight bold :foreground ,fg-alt))))
   `(org-level-6 ((,class (:weight bold :foreground ,yellow-alt-other))))
   `(org-level-7 ((,class (:weight bold :foreground ,blue-alt-other))))
   `(org-level-8 ((,class (:weight bold :foreground ,red-alt-other))))
   `(org-todo ((t (:inherit (warning)))))
   `(org-emphasis ((t (:inherit (italic)))))
   `(org-block ((,class (:background ,bg-main :foreground ,fg-main))))
   `(org-block-begin-line ((,class (:background ,bg-dim :foreground ,fg-alt))))
   `(org-block-end-line ((t (:inherit (org-block-begin-line)))))
   `(org-verbatim ((,class (:background ,bg-dim :foreground ,green))))
   ;;;; ivy
   `(ivy-cursor ((t (:inherit (cursor)))))
   `(ivy-grep-info ((t (:inherit (modus-theme-intense-bg-green)))))
   `(ivy-match-required-face ((t (:inherit (error)))))
   `(ivy-confirm-face ((t (:inherit (font-lock-variable-name-face)))))
   `(ivy-grep-line-number ((t (:inherit (line-number)))))
   `(ivy-current-match ((t (:inherit (modus-theme-intense-bg-green) :weight bold))))
   `(ivy-minibuffer-match-face-1 ((t (:inherit (modus-theme-subtle-bg-neutral)))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit (modus-theme-subtle-bg-green)))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit (modus-theme-subtle-bg-magenta)))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   `(ivy-remote ((t (:inherit (font-lock-function-name-face)))))
   `(ivy-org ((t (:inherit (default)))))
   ;;;; swiper
   `(swiper-line-face ((t (:inherit (ivy-current-match)))))
   `(swiper-background-match-face-1 ((t (:inherit (modus-theme-subtle-bg-neutral)))))
   `(swiper-background-match-face-2 ((t (:inherit (modus-theme-subtle-bg-magenta)))))
   `(swiper-background-match-face-3 ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   `(swiper-background-match-face-4 ((t (:inherit (modus-theme-subtle-bg-green)))))
   `(swiper-match-face-1 ((t (:inherit (modus-theme-intense-bg-neutral)))))
   `(swiper-match-face-2 ((t (:inherit (modus-theme-intense-bg-magenta)))))
   `(swiper-match-face-3 ((t (:inherit (modus-theme-intense-bg-yellow)))))
   `(swiper-match-face-4 ((t (:inherit (modus-theme-intense-bg-green)))))
   ;;;; visual-regexp
   `(vr/group-0 ((t (:inherit (modus-theme-intense-bg-cyan) :weight bold))))
   `(vr/group-1 ((t (:inherit (modus-theme-intense-bg-magenta) :weight bold))))
   `(vr/group-2 ((t (:inherit (modus-theme-intense-bg-blue) :weight bold))))
   `(vr/match-0 ((t (:inherit (modus-theme-subtle-bg-green)))))
   `(vr/match-1 ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   `(vr/match-separator-face ((t (:inherit (modus-theme-intense-bg-red)))))
   ;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,fg-main :weight bold))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,blue-intense))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,fg-alt))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,magenta-intense))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,green-intense))))
   ;;;; whitespace-mode
   `(whitespace-space ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(whitespace-empty ((t (:inherit (modus-theme-intense-bg-magenta)))))
   `(whitespace-hspace ((t (whitespace-space))))
   `(whitespace-indentation ((t (:inherit (whitespace-space)))))
   `(whitespace-line ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   `(whitespace-newline ((t (:inherit (whitespace-space)))))
   `(whitespace-space-after-tab ((t (:inherit (modus-theme-subtle-bg-magenta)))))
   `(whitespace-space-before-tab ((t (:inherit (modus-theme-subtle-bg-yellow)))))
   `(whitespace-tab ((t (:inherit (whitespace-space)))))
   `(whitespace-trailing ((t (:inherit (modus-theme-intense-bg-red)))))
   `(whitespace-big-indent ((t (:inherit (modus-theme-intense-bg-red)))))
   ;;;; which-key
   `(which-key-key-face ((,class (:foreground ,blue-alt :weight bold))))
   `(which-key-separator-face ((,class (:foreground ,fg-dim))))
   `(which-key-note-face ((,class (:foreground ,fg-alt))))
   `(which-key-special-key-face ((,class (:foreground ,green-alt))))
   `(which-key-group-description-face ((,class (:foreground ,magenta-alt))))
   `(which-key-command-description-face ((,class (:foreground ,cyan-alt))))
   `(which-key-local-map-description-face ((,class (:foreground ,cyan-alt))))
   ;;;; emms
   `(emms-playlist-track-face ((,class (:foreground ,blue))))
   `(emms-playlist-selected-face ((,class (:foreground ,magenta :weight bold))))
   ;;;; eww
   `(eww-invalid-certificate ((t (:inherit (error)))))
   `(eww-valid-certificate ((t (:inherit (success)))))
   ;;;; company-mode
   `(company-preview ((t (:inherit (modus-theme-intense-bg-neutral)))))
   `(company-tooltip ((,class (:background ,bg-alt))))
   `(company-scrollbar-bg ((,class (:background ,bg-active))))
   `(company-scrollbar-fg ((,class (:background ,fg-active))))
   `(company-echo-common ((,class (:foreground ,magenta-alt-other))))
   `(company-preview-common ((,class (:foreground ,magenta-alt-other))))
   `(company-tooltip-common ((,class (:foreground ,magenta-alt-other))))
   `(company-tooltip-annotation ((,class (:foreground ,magenta-alt-other))))
   `(company-tooltip-selection ((t (:inherit (modus-theme-intense-bg-cyan)))))
   `(company-tooltip-mouse ((t (:inherit (modus-theme-intense-bg-blue)))))
   `(company-tooltip-search ((t (:inherit (modus-theme-subtle-bg-green)))))
   `(company-template-field ((t (:inherit (modus-theme-intense-bg-magenta)))))
   `(company-tooltip-search-selection ((t (:inherit (modus-theme-intense-bg-green)))))
   `(company-tooltip-common-selection ((,class (:foreground ,fg-main :weight bold))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,fg-main :weight bold))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
(file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'modus-vivendi)

;; modus-vivendi-theme.el ends here
