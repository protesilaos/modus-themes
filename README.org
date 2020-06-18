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

The themes are *highly customisable* and can be made to look much
different than their default austere aesthetic.  Make sure to read the
section on the [[#h:d414ca47-6dce-4905-9f2e-de1465bf23bb][customisation options]].

** Links with demo content
   :PROPERTIES:
   :CUSTOM_ID: h:3b1b8ad9-f08f-4329-b9ee-d817b610708f
   :END:

Check the [[https://gitlab.com/protesilaos/modus-themes/wikis/Screenshots][Wiki page with the screen shots]].  There are lots of scenaria
on display that draw attention to details and important aspects in the
design of the themes.  They also showcase the numerous customisation
options.

Also note that I use these themes in [[https://protesilaos.com/code-casts][my Emacs-related screen casts]]
(although older videos contain earlier, "alpha" versions).

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

*Remember to refresh your package lists* (=M-x package-refresh-contents= or
=M-x list-packages=), in case Emacs complains that a package is no longer
available.

*** With `use-package'
    :PROPERTIES:
    :CUSTOM_ID: h:3ab0ac39-38fb-405b-8a15-771cbd843b6d
    :END:

For a declarative approach with =use-package=, you can write something
like this (also see [[#h:d414ca47-6dce-4905-9f2e-de1465bf23bb][the customisation options]] for how to expand this):

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

Make sure to /remove any other theme/ that is being loaded, otherwise you
might run into unexpected issues (you can also =M-x disable-theme=).

*** Load at a given time or at sunset/sunrise
    :PROPERTIES:
    :CUSTOM_ID: h:4e936e31-e9eb-4b50-8fdd-45d827a03cca
    :END:

This is a neat trick contributed by [[https://www.reddit.com/r/emacs/comments/gdtqov/weekly_tipstricketc_thread/fq9186h/][b3n on r/emacs]].  It will select the
appropriate theme based on the time of the day and it will also switch
themes when the time comes.

#+begin_src emacs-lisp
;; Light for the day
(load-theme 'modus-operandi t t)
(run-at-time "05:00" (* 60 60 24)
             (lambda () (enable-theme 'modus-operandi)))

;; Dark for the night
(load-theme 'modus-vivendi t t)
(run-at-time "21:00" (* 60 60 24)
             (lambda () (enable-theme 'modus-vivendi)))
#+end_src

A modified version of the above technique was contributed by [[https://gitlab.com/aadcg][André
Alexandre Gomes]].  If you set =calendar-latitude= and =calendar-longitude=
(defined in the built-in =solar.el= library) in your =init.el=, you can
automatically switch between both themes at sunrise and sunset.  Note
that /those calendar variables need to be set before loading the themes/.

#+begin_src emacs-lisp
;; Light at sunrise
(load-theme 'modus-operandi t t)
(run-at-time (nth 1 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda () (enable-theme 'modus-operandi)))

;; Dark at sunset
(load-theme 'modus-vivendi t t)
(run-at-time (nth 4 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda () (enable-theme 'modus-vivendi)))
#+end_src

For the sake of completeness, the =load-theme= call in these snippets is
slightly different than the one in the section right above, because it
does not enable the theme directly: the subsequent =enable-theme= does
that when needed.

* Customisation options
  :PROPERTIES:
  :CUSTOM_ID: h:d414ca47-6dce-4905-9f2e-de1465bf23bb
  :END:

Both of the Modus themes expose variables that allow users to tweak how
certain styles are configured.  Check the [[https://gitlab.com/protesilaos/modus-themes/wikis/Screenshots][Wiki with the screen shots]] to
see how these will change things.

By default, all variables are deactivated (=nil=), meaning that *you need
to explicitly opt in* like this:

#+begin_src emacs-lisp
(setq CUSTOMISATION-OPTION-NAME t)

;; example with "rainbow" headings for Modus Operandi:
(setq modus-operandi-theme-rainbow-headings t)
#+end_src

Put the point (cursor) exactly to the right of the closing parenthesis
and use =C-x C-e= to evaluate each =setq= form individually, or the entire
expression that holds it (e.g. a function or =use-package= declaration).

All customisation *options must be declared before loading the theme*,
else they will not be parsed and have no immediate effect.  To [re-]load
a theme, you can evaluate either of these:

#+begin_src emacs-lisp
(load-theme 'modus-operandi t)
(load-theme 'modus-vivendi t)
#+end_src

Consult the section below with the [[#h:0e3b8a62-8d72-4439-be2d-cb12ed98f4cb][complete example configuration]] for a
fully fledged =use-package= declaration.

** Option for distinct Org source blocks
   :PROPERTIES:
   :CUSTOM_ID: h:ca57a3af-6f79-4530-88c0-e35eda9d3bf7
   :END:

+ =modus-operandi-theme-distinct-org-blocks=
+ =modus-vivendi-theme-distinct-org-blocks=

Use a distinct background for Org's source blocks and extend the
background of their beginning and end lines to the edge of the window
(the "extend" part is for Emacs versions >= 27, whereas before they
would extend regardless).

The default is to use the same background as the rest of the buffer for
the contents of the block, while beginning and end lines do not extend
to the end of the window (again, the "extend" is for Emacs 27 or
higher).

** Option for colourful "rainbow" headings
   :PROPERTIES:
   :CUSTOM_ID: h:1be42afb-bcd2-4425-b956-0ba93eb960c2
   :END:

+ =modus-operandi-theme-rainbow-headings=
+ =modus-vivendi-theme-rainbow-headings=

Apply more saturated colours to headings in =org-mode= and =outline-mode=
while retaining all other heading properties (such as a bold weight and
the optional scaled height ---see relevant customisation toggle).  The
colours follow the rainbow's spectrum.  In Org headings, some additional
tweaks are made to adapt keywords (like "TODO") to the more vivid
presentation.

The default uses a more frugal aesthetic for headings, letting their
bold typography and the nuances between the various heading levels
provide the elements of differentiation.

** Option for sectioned headings
   :PROPERTIES:
   :CUSTOM_ID: h:c1c9a380-7a05-4c0d-b714-2acac88f10ad
   :END:

+ =modus-operandi-theme-section-headings=
+ =modus-vivendi-theme-section-headings=

Uses a background colour and an overline to mark section headings in
=org-mode= and =outline-mode=.  These attributes are applied in addition to
the existing foreground colour and the bold weight and can, therefore,
be combined with the "rainbow" headings option (as well as all the other
options pertaining to headings).  For Org several additional faces are
configured accordingly, such as TODO keywords which gain a box style.

The default is to use only a subtle foreground and a bold weight for
headings, while no boxes or {under,over}-line effects are present.

Note that the background for sectioned headings will extend to the edge
of the window.  To make it cover just the area occupied by the heading's
text, use this:

#+begin_src emacs-lisp
(setq org-fontify-whole-heading-line nil)
#+end_src

** Option for scaled headings
   :PROPERTIES:
   :CUSTOM_ID: h:db0275ea-11c2-47c9-82a9-10b65d8df0f8
   :END:

+ =modus-operandi-theme-scale-headings=
+ =modus-vivendi-theme-scale-headings=

Make headings larger in size relative to the main text.  This is
noticeable in modes like Org.  The default is to use the same size for
headers and body copy.

In addition to toggles for enabling scaled headings, users can also
specify a number of their own.

+ If it is a *floating point*, say, =1.5=, it is interpreted as a multiple
  of the base font size (there are many ways to set the main font in
  Emacs, such as those [[https://gitlab.com/protesilaos/modus-themes/-/issues/43#note_337308289][I documented in issue 43]], though the most robust
  method is covered below in [[#h:ea30ff0e-3bb6-4801-baf1-d49169d94cd5][Font configurations for Org (and others)]]).
+ If it is an *integer*, it is read as an absolute font height.  The
  number is basically the point size multiplied by a hundred.  So if you
  want it to be =18pt= you must pass =180=.

Below are the variables in their default values, using the floating
point paradigm.  The numbers are very conservative, but you are free to
change them to your liking, such as =1.2=, =1.4=, =1.6=, =1.8=, =2.0=---or use a
resource for finding a consistent scale, like [[https://www.modularscale.com][modularscale.com]]:

#+begin_src emacs-lisp
(setq modus-operandi-theme-scale-1 1.05
      modus-operandi-theme-scale-2 1.1
      modus-operandi-theme-scale-3 1.15
      modus-operandi-theme-scale-4 1.2
      modus-operandi-theme-scale-5 1.3)

(setq modus-vivendi-theme-scale-1 1.05
      modus-vivendi-theme-scale-2 1.1
      modus-vivendi-theme-scale-3 1.15
      modus-vivendi-theme-scale-4 1.2
      modus-vivendi-theme-scale-5 1.3)
#+end_src

Note that in Org, scaling only increases the size of the heading, but
not of keywords that are added to it, like "TODO".  This is outside the
control of the themes and I am not aware of any way to make such
keywords scale accordingly (see [[*Font configurations for Org (and others)][issue 37]]).

Also note that in the latest tagged release (=0.8.1=) an Org file's
=#+TITLE= will not scale at all.  This has been fixed in =master= and will
be available in version =0.9.0=, which is expected in early June 2020
(the fix pertains to the introduction of =*-scale-5=).

** Option for visible fringes
   :PROPERTIES:
   :CUSTOM_ID: h:d989f116-7559-40bc-bf94-ef508d480960
   :END:

+ =modus-operandi-theme-visible-fringe=
+ =modus-vivendi-theme-visible-fringe=

When enabled, this will render the fringes in a subtle background
colour.

The default is to use the same colour as that of the main background,
meaning that the fringes are not obvious though they still occupy the
space given to them by =fringe-mode= (8px on either side by default).

** Option for more slanted constructs
   :PROPERTIES:
   :CUSTOM_ID: h:cb327797-b303-47c5-8171-4587a911ccc2
   :END:

+ =modus-operandi-theme-slanted-constructs=
+ =modus-vivendi-theme-slanted-constructs=

Choose to render more faces in slanted text (italics).  This typically
affects documentation strings and code comments.

The default is to not use italics unless it is absolutely necessary.

** Option for more bold constructs
   :PROPERTIES:
   :CUSTOM_ID: h:9a77e814-5eca-488f-9a67-119a95c2d28a
   :END:

+ =modus-operandi-theme-bold-constructs=
+ =modus-vivendi-theme-bold-constructs=

Display several constructs in bold weight.  This concerns keywords and
other important aspects of code syntax.  It also affects certain mode
line indicators.

The default is to only use a bold weight when it is necessary.

** Option for three-dimensional focused mode line
   :PROPERTIES:
   :CUSTOM_ID: h:ce155208-fdd6-4ada-9e0c-54aab7e2aff8
   :END:

+ =modus-operandi-theme-3d-modeline=
+ =modus-vivendi-theme-3d-modeline=

Use a three-dimensional, "released button" effect for the focused
window's mode line.  When enabled, this option will also affect the
styles of any inactive mode lines, making them slightly less intense in
order to accommodate the added element of depth.

The default is to present the mode lines as rectangles with a border
around them and with the active one having more intense colours than any
inactive ones.

** Option for subtle diffs
   :PROPERTIES:
   :CUSTOM_ID: h:e3933a53-cbd9-4e44-958a-1d6d133f0816
   :END:

+ =modus-operandi-theme-subtle-diff=
+ =modus-vivendi-theme-subtle-diff=

Display =diff-mode=, =ediff=, =smerge-mode=, =magit= diff buffers with fewer
and/or less intense background colours or, where possible, with no
background colours applied to the presentation of the added and removed
lines.  Concerning =magit=, an extra set of tweaks are introduced for the
effect of highlighting the current diff hunk, so as to remain consistent
with the overall experience of that mode.

The default is to use colour-coded backgrounds for line-wise highlights.
"Refined" changes (word-wise highlights) always use a background value
which is, nonetheless, more subtle with this option than with its
default equivalent.

** Option for intense standard completions
   :PROPERTIES:
   :CUSTOM_ID: h:5b0b1e66-8287-4f3f-ba14-011c29320a3f
   :END:

+ =modus-operandi-theme-intense-standard-completions=
+ =modus-vivendi-theme-intense-standard-completions=

Display faces for built-in completion frameworks, such as =icomplete=,
with a combination of background and foreground colours.  This covers
every completion interface that either is part of the upstream Emacs
distribution or extends some built-in library.  For example, =orderless=
is a powerful completion style that can be used with core Emacs.  So it
also is covered by this customisation option.

With this enabled, Icomplete and others will use similar UI metaphors to
those of =ivy=, =helm=, =selectrum= (among others).

The default is to only use foreground colour values for the various
matching characters or items of standard completion tools.

** Option for proportional fonts in headings
   :PROPERTIES:
   :CUSTOM_ID: h:33023fa6-6482-45d4-9b5e-3c73c945718f
   :END:

+ =modus-operandi-theme-proportional-fonts=
+ =modus-vivendi-theme-proportional-fonts=

Choose to apply a proportionately-spaced typeface to headings (such as
in Org mode).  The default is to use whatever the main typeface is,
typically a monospaced family.

Though also read [[#h:ea30ff0e-3bb6-4801-baf1-d49169d94cd5][Font configurations for Org (and others)]] as the themes
are designed to cope well with more prose-friendly typeface
configurations (e.g. using a proportionately-spaced sans-serif font for
the main text, while letting inline code and some other space-sensitive
constructs use a monospaced font).

** Complete example configuration for the above
   :PROPERTIES:
   :CUSTOM_ID: h:0e3b8a62-8d72-4439-be2d-cb12ed98f4cb
   :END:

This is a complete =use-package= declaration with Modus Operandi as an
example.  You can modify it to your preferences.  Here we enable all
variables /before/ loading the theme.  You can also see a different form
of =setq= that sets the value of multiple variables at once: use one =setq=
expression for each variable, if in doubt.

*Do not forget* to =M-x package-refresh-contents= to get your package list
up-to-date, else the initial download may fail due to a newer version
being available.

#+begin_src emacs-lisp
(use-package modus-operandi-theme
  :ensure t
  :init
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-visible-fringes t
        modus-operandi-theme-3d-modeline t
        modus-operandi-theme-subtle-diffs t
        modus-operandi-theme-intense-standard-completions t
        modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-proportional-fonts t
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-section-headings t
        modus-operandi-theme-scale-headings t
        modus-operandi-theme-scale-1 1.05
        modus-operandi-theme-scale-2 1.1
        modus-operandi-theme-scale-3 1.15
        modus-operandi-theme-scale-4 1.2
        modus-operandi-theme-scale-5 1.3)
  :config
  (load-theme 'modus-operandi t))
#+end_src

Need more ideas?  Check the [[https://protesilaos.com/dotemacs/#h:b7444e76-75d4-4ae6-a9d6-96ff9408efe6][Modus themes section of my dotemacs]] (though
do not try to interpret the values of the variables, as I always test
different combinations and scenaria).

** Full access to the palette for further tweaks (advanced)
   :PROPERTIES:
   :CUSTOM_ID: h:b7282635-4fe9-415a-abdf-962b736ff5b6
   :END:

Unlike the previous options which follow a straightforward pattern of
allowing the user to quickly select their preference, the themes also
provide a more powerful, albeit difficult, mechanism of controlling
things with precision.

*** Option 1 to redefine colour values
    :PROPERTIES:
    :CUSTOM_ID: h:149e23b6-ada1-480f-95cd-c56fb40999b5
    :END:

The variables are:

+ =modus-operandi-theme-override-colors-alist=
+ =modus-vivendi-theme-override-colors-alist=

Users can specify an association list that maps the names of colour
variables to hexadecimal RGB values (in the form of =#RRGGBB=).  This
means that it is possible to override the entire palette or subsets
thereof (see the source code for the actual names and values).

Example:

#+begin_src emacs-lisp
;; Redefine the values of those three variables for the given theme
(setq modus-vivendi-theme-override-colors-alist
      '(("magenta" . "#ffaabb")
        ("magenta-alt" . "#ee88ff")
        ("magenta-alt-other" . "#bbaaff")))
#+end_src

You can then use this as a basis for creating a spin-off theme, such as
what [[https://gitlab.com/protesilaos/modus-themes/-/issues/45][Vincent Foley did in issue 45]].  The customisations were used to
emulate the aesthetic of Vincent's [[https://github.com/gnuvince/purp][purp theme]].

Excerpt of what was used in issue 45:

#+begin_src emacs-lisp
(use-package modus-operandi-theme
  :ensure t
  :config
  (defun customize-modus-operandi ()
    (when (member 'modus-operandi custom-enabled-themes)
      (setq modus-operandi-theme-override-colors-alist
            '(("magenta" . "#ff00ff"))) ; Redefine the value of the `magenta' variable
      (modus-operandi-theme-with-color-variables
        (custom-theme-set-faces
         'modus-operandi
         `(font-lock-builtin-face              ((t (:background ,bg-main :foreground ,fg-main))))
         `(font-lock-comment-delimiter-face    ((t (:background ,bg-main :foreground ,green))))
         `(font-lock-comment-face              ((t (:background ,bg-main :foreground ,green))))
         `(font-lock-constant-face             ((t (:background ,bg-main :foreground ,fg-main))))
         `(font-lock-doc-face                  ((t (:background ,bg-main :foreground ,green))))
         `(font-lock-function-name-face        ((t (:background ,bg-main :foreground ,magenta)))) ; gets the value from the above alist
         `(font-lock-keyword-face              ((t (:background ,bg-main :foreground ,fg-main))))
         `(font-lock-negation-char-face        ((t (:background ,bg-main :foreground ,fg-main))))
         `(font-lock-preprocessor-face         ((t (:background ,bg-main :foreground ,fg-main))))
         `(font-lock-regexp-grouping-backslash ((t (:background ,bg-main :foreground ,fg-main))))
         `(font-lock-regexp-grouping-construct ((t (:background ,bg-main :foreground ,fg-main))))
         `(font-lock-string-face               ((t (:background ,bg-main :foreground ,yellow))))
         `(font-lock-type-face                 ((t (:background ,bg-main :foreground ,fg-main))))
         `(font-lock-variable-name-face        ((t (:background ,bg-main :foreground ,fg-main))))
         `(font-lock-warning-face              ((t (:weight bold :background ,bg-main :foreground ,red))))
         ))))
  (add-hook 'after-load-theme-hook 'customize-modus-operandi))
#+end_src

The code for the bespoke =after-load-theme-hook= could be something like
the following (courtesy of the [[https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-funcs.el][Centaur Emacs project]]):

#+begin_src emacs-lisp
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(advice-add #'load-theme :after #'run-after-load-theme-hook)
#+end_src

*** Option 2 to apply colour variables to faces
    :PROPERTIES:
    :CUSTOM_ID: h:9754abfd-c890-4af3-91a8-1a2cb2b5be44
    :END:

The macro symbols are:

+ =modus-operandi-theme-with-color-variables=
+ =modus-vivendi-theme-with-color-variables=

Users can wrap face customisation snippets inside this macro in order to
pass the variables that the themes use and map them to face attributes.
This means that one can essentially override or extend the original
design (also in tandem with option 1).

Len Trigg who proposed [[https://gitlab.com/protesilaos/modus-themes/-/issues/39][the whole idea in issue 39]] uses this method to
tweak how a couple of Magit faces will look in GUI and terminal Emacs
respectively (follow the link for screen shots and details).  This is
Len's sample package declaration (with comments by me):

#+begin_src emacs-lisp
(use-package modus-vivendi-theme
  :init
  ;; enable some of the customisation options before loading the theme
  (setq modus-vivendi-theme-visible-fringe t
        modus-vivendi-theme-3d-modeline t)
  :config
  (defun customize-modus-vivendi ()
    "Customize modus-vivendi theme"
    (if (member 'modus-vivendi custom-enabled-themes)
        ;; this macro allows us to access the colour palette
        (modus-vivendi-theme-with-color-variables
         (custom-theme-set-faces
          'modus-vivendi
          `(magit-branch-current
            ((;; use a box property if the current display terminal
              ;; supports it and also apply a background
              ((supports :box t))
              (:foreground ,blue-alt-other :background ,bg-alt :box t))
             (t
              ;; use an underline if instead of a box for other
              ;; terminals
              (:foreground ,blue-alt-other :background ,bg-alt :underline t))))
          `(magit-branch-remote-head
            ((((supports :box t))
              (:foreground ,magenta-alt-other :background ,bg-alt :box t))
             (t
              (:foreground ,magenta-alt-other :background ,bg-alt :underline t))))
          ))))
  ;; invoke the above function when appropriate in order to override the
  ;; styles of the desired faces
  (add-hook 'after-load-theme-hook 'customize-modus-vivendi)
  ;; load the theme
  (load-theme 'modus-vivendi t))
#+end_src

Perhaps you want something simpler, such as a nice style for the cursor:

#+begin_src emacs-lisp
(modus-operandi-theme-with-color-variables
  (custom-theme-set-faces
   'modus-operandi
   `(cursor ((t (:background ,blue-alt))))))

(modus-vivendi-theme-with-color-variables
  (custom-theme-set-faces
   'modus-vivendi
   `(cursor ((t (:background ,green-alt))))))
#+end_src

Remember that the =after-load-theme-hook= is not built into Emacs.  The
code for it was shown under heading [[#h:149e23b6-ada1-480f-95cd-c56fb40999b5][Option 1 to redefine colour values]].

If you need more ideas check how I configure the themes in [[https://gitlab.com/protesilaos/dotemacs][my dotemacs]].
If something is not clear or not working as intended, please let me
know.

*** Further considerations
    :PROPERTIES:
    :CUSTOM_ID: h:4acda0f1-564e-48ff-8998-ebf7618377dd
    :END:

Please understand that these customisation methods are meant for
advanced users or those who are prepared to do their own research.  If
you think that the themes do not work well in some context you can
inform me about it: maybe you do not need to carry your own
customisations.  We can just fix the issue in its source.

To harness the potential of this method you will need to study the
source code of the themes.  You can always open an issue in case you
need some help.  To support you in this task, try the =rainbow-mode=
package which offers live colour previews.  This is how I configure it:

#+begin_src emacs-lisp
(use-package rainbow-mode
  :ensure
  :diminish                             ; optional if you use `diminish'
  :commands rainbow-mode                ; optional
  :config
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))
#+end_src

As for the means to check the contrast in perceived luminance between a
foreground colour and its expected background combination, refer to the
methods documented in my [[https://protesilaos.com/codelog/2020-05-10-modus-operandi-palette-review/][Modus Operandi theme subtle palette review]]
(2020-05-10).

** Font configurations for Org (and others)
   :PROPERTIES:
   :CUSTOM_ID: h:ea30ff0e-3bb6-4801-baf1-d49169d94cd5
   :END:

The themes are designed to cope well with mixed font settings.
Currently this applies to Org mode (courtesy of [[https://gitlab.com/protesilaos/modus-themes/-/issues/40][Ben in issue 40]]), though
it may be extended to other major modes as well (e.g. markdown).

In practice it means that some parts of a buffer will use a monospaced
font even when the user opts for a proportionately-spaced typeface as
their default (such as by enabling =variable-pitch-mode=).  This is to
ensure that code blocks, tables, and other relevant elements use the
appropriate type settings and are positioned correctly.

*To make everything use your desired font families*, you need to configure
the =variable-pitch= (proportional spacing) and =fixed-pitch= (monospaced)
faces respectively.  Otherwise you may get unintended combinations (such
as those experienced by Mark in [[https://gitlab.com/protesilaos/modus-themes/-/issues/42][issue 42]]).

Put something like this in your initialisation file:

#+begin_src emacs-lisp
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :height 110)
(set-face-attribute 'fixed-pitch nil :family "Source Code Pro" :height 110)
#+end_src

You can also set your standard font the same way.  For example:

#+begin_src emacs-lisp
(set-face-attribute 'default nil :family "Fira Code" :height 120)
#+end_src

The value of the =:height= attribute essentially is the point size × 100.
So if you want to use Fira Code at point size =12=, you set the height to
=120=.  Values do not need to be rounded to multiples of ten, so the likes
of =125= are perfectly valid.

If any Org power user is reading this section, I encourage you to
recommend some other /minimal/ tweaks and customisations that could
improve the user experience.

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
+ ag
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
+ circe
+ color-rg
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
+ csv-mode
+ ctrlf
+ custom (=M-x customize=)
+ dap-mode
+ dashboard (emacs-dashboard)
+ deadgrep
+ debbugs
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
+ eglot
+ el-search
+ eldoc-box
+ elfeed
+ elfeed-score
+ emms
+ enhanced-ruby-mode
+ epa
+ equake
+ erc
+ eros
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
+ flx
+ freeze-it
+ frog-menu
+ focus
+ fold-this
+ font-lock (generic syntax highlighting)
+ forge
+ fountain (fountain-mode)
+ geiser
+ git
+ git-gutter (and variants)
+ git-lens
+ git-timemachine
+ git-walktree
+ gnus
+ golden-ratio-scroll-screen
+ helm* (also see [[#h:e4408911-e186-4825-bd4f-4d0ea55cd6d6][section below on Helm's grep-related functions]])
+ helm-ls-git
+ helm-switch-shell
+ helm-xref
+ helpful
+ highlight-blocks
+ highlight-defined
+ highlight-escape-sequences (=hes-mode=)
+ highlight-indentation
+ highlight-numbers
+ highlight-symbol
+ highlight-thing
+ hl-fill-column
+ hl-line-mode
+ hl-todo
+ hydra
+ hyperlist
+ ibuffer
+ icomplete
+ icomplete-vertical
+ ido-mode
+ iedit
+ iflipb
+ imenu-list
+ indium
+ info
+ info-colors
+ interaction-log
+ ioccur
+ isearch, occur, etc.
+ ivy*
+ ivy-posframe
+ jira (org-jira)
+ journalctl-mode
+ js2-mode
+ julia
+ jupyter
+ kaocha-runner
+ keycast
+ line numbers (=display-line-numbers-mode= and global variant)
+ lsp-mode
+ lsp-ui
+ magit
+ magit-imerge
+ man
+ markdown-mode
+ markup-faces (=adoc-mode=)
+ mentor
+ messages
+ minibuffer-line
+ minimap
+ modeline
+ mood-line
+ mu4e
+ mu4e-conversation
+ multiple-cursors
+ neotree
+ no-emoji
+ num3-mode
+ nxml-mode
+ orderless
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
+ page-break-lines
+ paradox
+ paren-face
+ parrot
+ pass
+ persp-mode
+ perspective
+ phi-grep
+ phi-search
+ pomidor
+ powerline
+ powerline-evil
+ proced
+ prodigy
+ rainbow-blocks
+ rainbow-identifiers
+ rainbow-delimiters
+ rcirc
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
+ spell-fu
+ stripes
+ suggest
+ switch-window
+ swiper
+ swoop
+ sx
+ symbol-overlay
+ syslog-mode
+ telephone-line
+ term
+ tomatinho
+ transient (pop-up windows like Magit's)
+ trashed
+ treemacs
+ tuareg
+ undo-tree
+ vc (built-in mode line status for version control)
+ vc-annotate (=C-x v g=)
+ vdiff
+ vimish-fold
+ visible-mark
+ visual-regexp
+ volatile-highlights
+ vterm
+ wcheck-mode
+ web-mode
+ wgrep
+ which-function-mode
+ which-key
+ whitespace-mode
+ window-divider-mode
+ winum
+ writegood-mode
+ woman
+ xah-elisp-mode
+ xref
+ xterm-color (and ansi-colors)
+ yaml-mode
+ yasnippet
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
+ swift-mode

** Help needed
   :PROPERTIES:
   :CUSTOM_ID: h:bcc3f6f9-7ace-4e2a-8dbb-2bf55574dae5
   :END:

These are face groups that I am aware of but do not know how to access
or do not actively use.  I generally need to see how a face looks in its
context before assessing its aesthetics or specific requirements.

Use =M-x list-faces-display= to get these.

+ tty-menu

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

** Note about colour-coded ORG source blocks
   :PROPERTIES:
   :CUSTOM_ID: h:9ef7e899-63f4-4eb1-958c-1a1dd999fa35
   :END:

It is possible to apply unique coloured backgrounds to Org's source
blocks on a per-language basis.  The customisation option is
=org-src-block-faces=.

Because this is an inherently subjective choice, it is impossible to
reach a consensus of what colour should be assigned to each language.
Furthermore, there are so many languages to choose from, making it
impractical to apply a unique background to each of them without adding
disproportionate complexity to the themes.

The least we could do is provide a set of background values that have
been tested with all colours that highlight code syntax.

These approved colour variants are as follows:

| Background | Modus Operandi | Modus Vivendi |
|------------+----------------+---------------|
| red        | #fef2f2        | #180505       |
| yellow     | #fcf6f1        | #18140a       |
| magenta    | #fff4fc        | #160616       |
| green      | #f4faf4        | #061206       |
| blue       | #f4f4ff        | #070722       |
| cyan       | #f0f6fa        | #091620       |

The differences between those colour values are subtle, but quite
noticeable when applied to large, contiguous areas (such as code
blocks).

Pick the one you wish to use for your language of choice.  Here is an
example:

#+begin_src emacs-lisp
;; Modus Operandi
(setq org-src-block-faces '(("emacs-lisp" (:background "#fef2f2"))
                            ("python" (:background "#f4f4ff"))))

;; Modus Vivendi
(setq org-src-block-faces '(("emacs-lisp" (:background "#180505"))
                            ("python" (:background "#070722"))))
#+end_src

For versions of Emacs >= 27, also add the =:extend t= property, like this:

#+begin_src emacs-lisp
;; Modus Operandi
(setq org-src-block-faces '(("emacs-lisp" (:background "#fef2f2" :extend t))
                            ("python" (:background "#f4f4ff" :extend t))))

;; Modus Vivendi
(setq org-src-block-faces '(("emacs-lisp" (:background "#180505" :extend t))
                            ("python" (:background "#070722" :extend t))))
#+end_src

Feel free to contribute any concrete proposals on how to improve support
for this at the theme level (see [[#h:25ba8d6f-6604-4338-b774-bbe531d467f6][the "Contributing" section]]).

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

A similar scenario was [[https://gitlab.com/protesilaos/modus-themes/-/issues/49][also encountered in issue 49]] which was promptly
fixed by the Helm maintainer.

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

A few tasks you can help me with:

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
major contribution (more than 15 lines, or so, in aggregate), you need
to make a copyright assignment to the Free Software Foundation.  This is
necessary because the themes are distributed through the official GNU
ELPA repository and the FSF must be in a position to enforce the GNU
General Public License.

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
