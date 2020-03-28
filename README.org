#+TITLE: Modus Themes for GNU Emacs
#+AUTHOR: Protesilaos Stavrou
#+EMAIL: public@protesilaos.com

* Overview
  :PROPERTIES:
  :CUSTOM_ID: h:d42d56a4-9252-4858-ac8e-3306cdd24e19
  :END:

This is a set of accessible themes for GNU Emacs.  The contrast ratio
between foreground and background values should always be >= 7:1, which
conforms with the WCAG AAA accessibility standard.  This is the highest
standard of its kind.

The /Modus themes/ project consists of two standalone items, one where
dark text is cast on a light backdrop (Modus Operandi) and another where
light text is displayed against a dark background (Modus Vivendi).

*Check the [[https://gitlab.com/protesilaos/modus-themes/wikis/Screenshots][Wiki page with the screen shots]].* Also note that I demo these
themes in [[https://protesilaos.com/code-casts][my Emacs-related screen casts]] (though older videos contain
earlier, "alpha" versions).

* Install and auto-load
  :PROPERTIES:
  :CUSTOM_ID: h:25c3ecd3-8025-414c-9b96-e4d6266c6fe8
  :END:

** Install the packages
   :PROPERTIES:
   :CUSTOM_ID: h:c3e293e8-8464-4196-aefd-184027116ded
   :END:

I maintain /Modus Operandi/ (light theme) and /Modus Vivendi/ (dark) as
standalone packages in ELPA, MELPA, and MELPA Stable.

Just run:

=M-x package-install RET modus-operandi-theme RET=

And/or:

=M-x package-install RET modus-vivendi-theme RET=

To be clear, that sequence means:

+ press `Meta-X'
+ type `package-install'
+ hit the Return key
+ type the name of the package
+ hit Return to confirm your choice

*** With `use-package'
    :PROPERTIES:
    :CUSTOM_ID: h:3ab0ac39-38fb-405b-8a15-771cbd843b6d
    :END:

For a declarative approach with =use-package=, you can write something
like this:

#+BEGIN_SRC emacs-lisp
(use-package modus-operandi-theme
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t)
#+END_SRC

** Manual installation method
   :PROPERTIES:
   :CUSTOM_ID: h:0317c29a-3ddb-4a0a-8ffd-16c781733ea2
   :END:

Download the files in this repository ending in =*-theme.el= and place
them in an appropriate directory, such as =~/.emacs.d/themes/=.  To make
sure the filesystem path of your choice is read by Emacs, insert the
following in your initialisation file:

#+BEGIN_SRC emacs-lisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
#+END_SRC

** Load automatically
   :PROPERTIES:
   :CUSTOM_ID: h:ae978e05-526f-4509-a007-44a0925b8bce
   :END:

To load the theme from your Emacs initialisation file use the relevant
snippet:

#+BEGIN_SRC emacs-lisp
(load-theme 'modus-operandi t)          ; Light theme
(load-theme 'modus-vivendi t)           ; Dark theme
#+END_SRC

Make sure to /remove any other theme/ that is being loaded, otherwise
you might run into unexpected issues.

* Customisation options
  :PROPERTIES:
  :CUSTOM_ID: h:d414ca47-6dce-4905-9f2e-de1465bf23bb
  :END:

Both of the Modus themes expose some variables that allow users to tweak
the look of the theme.  By default, all variables are deactivated,
meaning that *you need to explicitly opt in*.

This is what is available right now (use the ones appropriate to the
theme of your choice):

#+BEGIN_SRC emacs-lisp
;; Choose to render more code constructs in slanted text (italics).  The
;; default, shown below, is to not use italics, unless it is absolutely
;; necessary.
(setq modus-operandi-theme-slanted-constructs nil)

(setq modus-vivendi-theme-slanted-constructs nil)

;; Opt to display some additional code constructs in bold.  The default,
;; shown below, is to use bold weight only where necessary.
(setq modus-operandi-theme-bold-constructs nil)

(setq modus-vivendi-theme-bold-constructs nil)

;; Use proportionately-spaced fonts (variable-pitch) for headings.  The
;; default is to use whatever font the user has selected, typically a
;; monospaced typeface.
(setq modus-operandi-theme-proportional-fonts nil)

(setq modus-vivendi-theme-proportional-fonts nil)

;; Whether headings should be scaled or have the same height as body
;; text.  The default is to keep everything the same as the base size.
(setq modus-operandi-theme-scale-headings nil)

(setq modus-vivendi-theme-scale-headings nil)

;; Font scale that should apply to headings.  These are the default values.
(setq modus-operandi-theme-scale-1 1.05)
(setq modus-operandi-theme-scale-2 1.1)
(setq modus-operandi-theme-scale-3 1.15)
(setq modus-operandi-theme-scale-4 1.2)

(setq modus-vivendi-theme-scale-1 1.05)
(setq modus-vivendi-theme-scale-2 1.1)
(setq modus-vivendi-theme-scale-3 1.15)
(setq modus-vivendi-theme-scale-4 1.2)

;; Make the fringes visible.  This renders them in a different
;; background than the main buffer.
(setq modus-operandi-theme-visible-fringes nil)

(setq modus-vivendi-theme-visible-fringes nil)

;; Use a distinct background for Org's source blocks and extend their
;; headings until the edge of the window (the "extend" part is for Emacs
;; versions >= 27, whereas before they would extend anyhow).  The
;; default is to use the same background as the rest of the buffer,
;; while beginning and end lines do not extend to the end of the window
;; (again, the extend is for Emacs 27 or higher).
(setq modus-operandi-theme-distinct-org-blocks nil)

(setq modus-vivendi-theme-distinct-org-blocks nil)
#+END_SRC

*NOTE* that all customisation options must be declared /before/ loading
the theme, else they will not be parsed and have no effect.

* Face coverage
  :PROPERTIES:
  :CUSTOM_ID: h:944a3bdf-f545-40a0-a26c-b2cec8b2b316
  :END:

This list will always be updated to reflect the current state of the
project.  The idea is to offer an overview of the /known status/ of all
affected face groups.

** Full support
   :PROPERTIES:
   :CUSTOM_ID: h:5ea98392-1376-43a4-8080-2d42a5b690ef
   :END:

The items with an appended asterisk =*= tend to have lots of extensions, so
the "full support" may not be 100% true…

+ ace-window
+ alert
+ all-the-icons
+ annotate
+ anzu
+ apropos
+ apt-sources-list
+ artbollocks-mode
+ auctex and TeX
+ auto-dim-other-buffers
+ avy
+ bm
+ breakpoint (provided by built-in gdb-mi.el)
+ buffer-expose
+ calendar and diary
+ calfw
+ centaur-tabs
+ change-log and log-view (=vc-print-log= and =vc-print-root-log=)
+ cider
+ column-enforce-mode
+ company-mode*
+ company-posframe
+ compilation-mode
+ completions
+ counsel*
+ counsel-css
+ counsel-notmuch
+ counsel-org-capture-string
+ cov
+ custom (=M-x customize=)
+ dap-mode
+ dashboard (emacs-dashboard)
+ deadgrep
+ define-word
+ deft
+ diff-hl
+ diff-mode
+ dim-autoload
+ dired
+ dired-async
+ dired-git
+ dired-git-info
+ dired-narrow
+ dired-subtree
+ diredfl
+ disk-usage
+ doom-modeline
+ dynamic-ruler
+ easy-jekyll
+ easy-kill
+ ebdb
+ ediff
+ eldoc-box
+ elfeed
+ elfeed-score
+ emms
+ enhanced-ruby-mode
+ epa
+ equake
+ erc
+ ert
+ eshell
+ evil* (evil-mode)
+ evil-goggles
+ evil-visual-mark-mode
+ eww
+ eyebrowse
+ fancy-dabbrev
+ flycheck
+ flycheck-indicator
+ flycheck-posframe
+ flymake
+ flyspell
+ flyspell-correct
+ freeze-it
+ frog-menu
+ focus
+ fold-this
+ font-lock (generic syntax highlighting)
+ fountain (fountain-mode)
+ geiser
+ git
+ git-gutter (and variants)
+ git-lens
+ git-timemachine
+ git-walktree
+ gnus
+ helm* (also see [[#h:e4408911-e186-4825-bd4f-4d0ea55cd6d6][section below on Helm's grep-related functions]])
+ helm-ls-git
+ helm-switch-shell
+ helm-xref
+ highlight-blocks
+ highlight-defined
+ highlight-escape-sequences (=hes-mode=)
+ highlight-numbers
+ highlight-thing
+ hl-fill-column
+ hl-line-mode
+ hl-todo
+ hydra
+ ido-mode
+ iedit
+ imenu-list
+ info
+ info-colors
+ interaction-log
+ ioccur
+ isearch, occur, etc.
+ ivy*
+ ivy-posframe
+ jira (org-jira)
+ js2-mode
+ julia
+ jupyter
+ kaocha-runner
+ keycast
+ line numbers (=display-line-numbers-mode= and global variant)
+ lsp-mode
+ lsp-ui
+ magit
+ markdown-mode
+ markup-faces (=adoc-mode=)
+ mentor
+ messages
+ modeline
+ mood-line
+ mu4e
+ mu4e-conversation
+ multiple-cursors
+ neotree
+ num3-mode
+ org*
+ org-journal
+ org-noter
+ org-pomodoro
+ org-recur
+ org-roam
+ org-superstar
+ org-treescope
+ origami
+ outline-mode
+ outline-minor-faces
+ package (=M-x list-packages=)
+ paradox
+ paren-face
+ pass
+ persp-mode
+ perspective
+ powerline
+ powerline-evil
+ proced
+ prodigy
+ rainbow-blocks
+ rainbow-identifiers
+ rainbow-delimiters
+ regexp-builder (also known as =re-builder=)
+ rg (rg.el)
+ ripgrep
+ rmail
+ ruler-mode
+ sallet
+ selectrum
+ sesman
+ shell-script-mode
+ show-paren-mode
+ side-notes
+ skewer-mode
+ smart-mode-line
+ smartparens
+ smerge
+ speedbar
+ stripes
+ suggest
+ swiper
+ sx
+ symbol-overlay
+ syslog-mode
+ telephone-line
+ term
+ transient (pop-up windows like Magit's)
+ treemacs
+ undo-tree
+ vc (built-in mode line status for version control)
+ vc-annotate (=C-x v g=)
+ visual-regexp
+ volatile-highlights
+ web-mode
+ wgrep
+ which-function-mode
+ which-key
+ whitespace-mode
+ window-divider-mode
+ writegood-mode
+ xah-elisp-mode
+ xref
+ xterm-color (and ansi-colors)
+ yaml-mode
+ ztree

Plus many other miscellaneous faces that are provided by the out-of-the-box
Emacs distribution.

** Covered but not styled explicitly
   :PROPERTIES:
   :CUSTOM_ID: h:8ada963d-046d-4c67-becf-eee18595f902
   :END:

These do not require any extra styles because they are configured to
inherit from some basic faces.  Please confirm.

+ comint
+ bongo
+ edit-indirect

** Help needed
   :PROPERTIES:
   :CUSTOM_ID: h:bcc3f6f9-7ace-4e2a-8dbb-2bf55574dae5
   :END:

These are face groups that I am aware of but do not know how to access
or do not actively use.  I generally need to see how a face looks in its
context before assessing its aesthetics or specific requirements.

Use =M-x list-faces-display= to get these.

+ tty-menu

Note that the themes do provide support for =org-mode=, but some of
these interfaces have been decided based on indirect experience.  If you
encounter anything that does not "feel right", please let me know.

** Will NOT be supported
   :PROPERTIES:
   :CUSTOM_ID: h:46756fcc-0d85-4f77-b0e3-64f890e1c2ea
   :END:

I have thus far identified a single package that does fit into the
overarching objective of this project: [[https://github.com/hlissner/emacs-solaire-mode][solaire]].  It basically tries to
cast a less intense background on the main file-visiting buffers, so
that secondary elements like sidebars can have the default (pure
white/black) background.

/I will only support this package if it ever supports the inverse
effect/: less intense colours (but still accessible) for supportive
interfaces and the intended styles for the content you are actually
working on.

** Note for HELM users of grep or grep-like functions
   :PROPERTIES:
   :CUSTOM_ID: h:e4408911-e186-4825-bd4f-4d0ea55cd6d6
   :END:

There is one face from the Helm package that is meant to highlight the
matches of a grep or grep-like command (=ag= or =ripgrep=).  It is
=helm-grep-match=.  However, this face can only apply when the user does
not pass =--color=always= as a command-line option for their command.

Here is the docstring for that face, which is defined in the
=helm-grep.el= library (view a library with =M-x find-library=).

#+begin_quote
Face used to highlight grep matches. Have no effect when grep backend
use "--color="
#+end_quote

The user must either remove =--color= from the flags passed to the grep
function, or explicitly use =--color=never= (or equivalent).  Helm
provides user-facing customisation options for controlling the grep
function's parameters, such as =helm-grep-default-command= and
=helm-grep-git-grep-command=.

When =--color=always= is in effect, the grep output will use red text in
bold letter forms to present the matching part in the list of
candidates.  *That style still meets the contrast ratio target of >= 7:1*
(accessibility standard WCAG AAA), because it draws the reference to
ANSI colour number 1 (red) from the already-supported array of
=ansi-color-names-vector=.

I presented [[https://gitlab.com/protesilaos/modus-themes/-/issues/21#note_302748582][some screen shots of this in issue 21]].

** Note on VC-ANNOTATE-BACKGROUND-MODE
   :PROPERTIES:
   :CUSTOM_ID: h:5b5d4420-50cc-4d53-a9f8-825cba6b68f1
   :END:

Due to the unique way =vc-annotate= (=C-x v g=) applies colours, support for
its background mode (=vc-annotate-background-mode=) is disabled at the
theme level.

Normally, such a drastic measure should not belong in a theme: assuming
the user's preferences is bad practice.  However, it has been deemed
necessary in the interest of preserving colour contrast accessibility
while still supporting a useful built-in tool.

If there actually is a way to avoid such a course of action, without
prejudice to the accessibility standard of this project, then please
report as much (or contribute as per the information in the [[#h:25ba8d6f-6604-4338-b774-bbe531d467f6][Contributing]]
section).

* Contributing
  :PROPERTIES:
  :CUSTOM_ID: h:25ba8d6f-6604-4338-b774-bbe531d467f6
  :END:

A few tasks you can help me with, sorted from the most probable to the
least likely:

+ Suggest refinements to packages that are covered.
+ Report packages not covered thus far.
+ Report bugs, inconsistencies, shortcomings.
+ Help expand the documentation of covered-but-not-styled packages.
+ Suggest refinements to the colour palette.
+ Help expand this document or any other piece of documentation.

It would be great if your feedback also includes some screenshots, GIFs,
or short videos.  Though this is not a requirement.

Whatever you do, please bear in mind the overarching objective of the
Modus themes: to keep a contrast ratio that is greater or equal to 7:1
between background and foreground colours.  If a compromise is ever
necessary between aesthetics and accessibility, it shall always be made
in the interest of the latter.

** Code contributions require copyright assignment to the FSF
   :PROPERTIES:
   :CUSTOM_ID: h:d3fb2fc7-6c34-4e68-b2d6-6048849b0319
   :END:

I accept code contributions as well (send merge requests!).  But for any
major contribution (more than 15 lines, or so), you need to make a
copyright assignment to the Free Software Foundation.  This is necessary
because the themes are distributed through the official GNU ELPA
repository and the FSF must be in a position to enforce the GNU General
Public License.

Copyright assignment /is a simple process/ that I had to follow as well.
Check the [[https://git.savannah.gnu.org/cgit/gnulib.git/tree/doc/Copyright/request-assign.future][request form]].  You must send an email to the address mentioned
in the form and then wait for the FSF to send you a legal agreement.
Sign the document and file it back to them.  This should all happen via
email and take about a week.

I encourage you to go through this process.  You only need to do it
once.  It will allow you to make contributions to Emacs in general.

* Meta
  :PROPERTIES:
  :CUSTOM_ID: h:4c338a51-509e-42c0-8820-1f5014fb477b
  :END:

If you interested in the principles that govern the development of this
project, read my article [[https://protesilaos.com/codelog/2020-03-17-design-modus-themes-emacs/][On the design of the Modus themes]] (2020-03-17).

* COPYING
  :PROPERTIES:
  :CUSTOM_ID: h:66652183-2fe0-46cd-b4bb-4121bad78d57
  :END:

The Modus Themes are distributed under the terms of the GNU General
Public License version 3 or, at your choice, any later version.  See the
COPYING file distributed in the [[https://gitlab.com/protesilaos/modus-themes][project's Git repository]].
