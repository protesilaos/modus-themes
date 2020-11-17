;;; modus-vivendi-theme.el --- Accessible dark theme (WCAG AAA) -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/modus-themes
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
;;    2. Offer as close to full face coverage as possible.  The list is
;;    already quite long (see further below), with more additions to
;;    follow as part of the ongoing development process.
;;
;; The themes share the following customization options, all of which
;; are disabled by default:
;;
;;     modus-themes-slanted-constructs             (boolean)
;;     modus-themes-bold-constructs                (boolean)
;;     modus-themes-variable-pitch-headings        (boolean)
;;     modus-themes-no-mixed-fonts                 (boolean)
;;     modus-themes-headings                       (alist)
;;     modus-themes-scale-headings                 (boolean)
;;     modus-themes-fringes                        (choice)
;;     modus-themes-org-blocks                     (choice)
;;     modus-themes-prompts                        (choice)
;;     modus-themes-mode-line                      (choice)
;;     modus-themes-diffs                          (choice)
;;     modus-themes-syntax                         (choice)
;;     modus-themes-intense-hl-line                (boolean)
;;     modus-themes-intense-paren-match            (boolean)
;;     modus-themes-links                          (choice)
;;     modus-themes-completions                    (choice)
;;
;; The default scale for headings is as follows (it can be customized as
;; well):
;;
;;     modus-themes-scale-1 1.05
;;     modus-themes-scale-2 1.1
;;     modus-themes-scale-3 1.15
;;     modus-themes-scale-4 1.2
;;     modus-themes-scale-5 1.3
;;
;; Below is the list of explicitly supported packages or face groups
;; (there are implicitly supported packages as well, which inherit from
;; font-lock or some basic group).  You are encouraged to report of any
;; missing package or change you would like to see.
;;
;;     ace-window
;;     ag
;;     alert
;;     all-the-icons
;;     annotate
;;     anzu
;;     apropos
;;     apt-sources-list
;;     artbollocks-mode
;;     auctex and TeX
;;     auto-dim-other-buffers
;;     avy
;;     awesome-tray
;;     binder
;;     bm
;;     bongo
;;     boon
;;     breakpoint (provided by built-in gdb-mi.el)
;;     buffer-expose
;;     calendar and diary
;;     calfw
;;     centaur-tabs
;;     change-log and log-view (`vc-print-log' and `vc-print-root-log')
;;     cider
;;     circe
;;     color-rg
;;     column-enforce-mode
;;     company-mode
;;     company-posframe
;;     compilation-mode
;;     completions
;;     counsel
;;     counsel-css
;;     counsel-notmuch
;;     counsel-org-capture-string
;;     cov
;;     cperl-mode
;;     csv-mode
;;     ctrlf
;;     custom (M-x customize)
;;     dap-mode
;;     dashboard (emacs-dashboard)
;;     deadgrep
;;     debbugs
;;     define-word
;;     deft
;;     dictionary
;;     diff-hl
;;     diff-mode
;;     dim-autoload
;;     dir-treeview
;;     dired
;;     dired-async
;;     dired-git
;;     dired-git-info
;;     dired-narrow
;;     dired-subtree
;;     diredfl
;;     disk-usage
;;     doom-modeline
;;     dynamic-ruler
;;     easy-jekyll
;;     easy-kill
;;     ebdb
;;     ediff
;;     eglot
;;     el-search
;;     eldoc
;;     eldoc-box
;;     elfeed
;;     elfeed-score
;;     emms
;;     enhanced-ruby-mode
;;     epa
;;     equake
;;     erc
;;     eros
;;     ert
;;     eshell
;;     eshell-fringe-status
;;     eshell-git-prompt
;;     eshell-prompt-extras (epe)
;;     eshell-syntax-highlighting
;;     evil (evil-mode)
;;     evil-goggles
;;     evil-visual-mark-mode
;;     eww
;;     eyebrowse
;;     fancy-dabbrev
;;     flycheck
;;     flycheck-color-mode-line
;;     flycheck-indicator
;;     flycheck-posframe
;;     flymake
;;     flyspell
;;     flyspell-correct
;;     flx
;;     freeze-it
;;     frog-menu
;;     focus
;;     fold-this
;;     font-lock (generic syntax highlighting)
;;     forge
;;     fountain (fountain-mode)
;;     geiser
;;     git-commit
;;     git-gutter (and variants)
;;     git-lens
;;     git-rebase
;;     git-timemachine
;;     git-walktree
;;     gnus
;;     golden-ratio-scroll-screen
;;     helm
;;     helm-ls-git
;;     helm-switch-shell
;;     helm-xref
;;     helpful
;;     highlight-blocks
;;     highlight-defined
;;     highlight-escape-sequences (`hes-mode')
;;     highlight-indentation
;;     highlight-numbers
;;     highlight-symbol
;;     highlight-tail
;;     highlight-thing
;;     hl-defined
;;     hl-fill-column
;;     hl-line-mode
;;     hl-todo
;;     hydra
;;     hyperlist
;;     ibuffer
;;     icomplete
;;     ido-mode
;;     iedit
;;     iflipb
;;     imenu-list
;;     indium
;;     info
;;     info-colors
;;     interaction-log
;;     ioccur
;;     isearch, occur, etc.
;;     ivy
;;     ivy-posframe
;;     jira (org-jira)
;;     journalctl-mode
;;     js2-mode
;;     julia
;;     jupyter
;;     kaocha-runner
;;     keycast
;;     line numbers (`display-line-numbers-mode' and global variant)
;;     lsp-mode
;;     lsp-ui
;;     magit
;;     magit-imerge
;;     make-mode
;;     man
;;     markdown-mode
;;     markup-faces (`adoc-mode')
;;     mentor
;;     messages
;;     minibuffer-line
;;     minimap
;;     modeline
;;     mood-line
;;     mpdel
;;     mu4e
;;     mu4e-conversation
;;     multiple-cursors
;;     neotree
;;     no-emoji
;;     notmuch
;;     num3-mode
;;     nxml-mode
;;     objed
;;     orderless
;;     org
;;     org-journal
;;     org-noter
;;     org-pomodoro
;;     org-recur
;;     org-roam
;;     org-superstar
;;     org-table-sticky-header
;;     org-treescope
;;     origami
;;     outline-mode
;;     outline-minor-faces
;;     package (M-x list-packages)
;;     page-break-lines
;;     paradox
;;     paren-face
;;     parrot
;;     pass
;;     pdf-tools
;;     persp-mode
;;     perspective
;;     phi-grep
;;     phi-search
;;     pkgbuild-mode
;;     pomidor
;;     popup
;;     powerline
;;     powerline-evil
;;     proced
;;     prodigy
;;     racket-mode
;;     rainbow-blocks
;;     rainbow-identifiers
;;     rainbow-delimiters
;;     rcirc
;;     regexp-builder (also known as `re-builder')
;;     rg
;;     ripgrep
;;     rmail
;;     ruler-mode
;;     sallet
;;     selectrum
;;     semantic
;;     sesman
;;     shell-script-mode
;;     show-paren-mode
;;     shr
;;     side-notes
;;     sieve-mode
;;     skewer-mode
;;     smart-mode-line
;;     smartparens
;;     smerge
;;     spaceline
;;     speedbar
;;     spell-fu
;;     stripes
;;     suggest
;;     switch-window
;;     swiper
;;     swoop
;;     sx
;;     symbol-overlay
;;     tab-bar-mode
;;     tab-line-mode
;;     syslog-mode
;;     table (built-in table.el)
;;     telephone-line
;;     term
;;     tomatinho
;;     transient (pop-up windows like Magit's)
;;     trashed
;;     treemacs
;;     tty-menu
;;     tuareg
;;     typescript
;;     undo-tree
;;     vc (built-in mode line status for version control)
;;     vc-annotate (C-x v g)
;;     vdiff
;;     vimish-fold
;;     visible-mark
;;     visual-regexp
;;     volatile-highlights
;;     vterm
;;     wcheck-mode
;;     web-mode
;;     wgrep
;;     which-function-mode
;;     which-key
;;     whitespace-mode
;;     window-divider-mode
;;     winum
;;     writegood-mode
;;     woman
;;     xah-elisp-mode
;;     xref
;;     xterm-color (and ansi-colors)
;;     yaml-mode
;;     yasnippet
;;     ztree

;;; Code:



(eval-and-compile
  (require 'modus-themes))

(eval-when-compile
  (require 'modus-themes-core))

(modus-themes-core-theme
 'modus-vivendi)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'modus-vivendi)

(provide 'modus-vivendi-theme)

;;; modus-vivendi-theme.el ends here
