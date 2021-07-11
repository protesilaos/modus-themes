# Modus themes for GNU Emacs (Modus Operandi and Modus Vivendi)

A pair of highly accessible themes that conform with the WCAG AAA
standard for colour contrast between background and foreground
combinations (a minimum contrast of 7:1---the highest standard of its
kind).  The Modus themes also strive to empower users with red-green
color deficiency: this is achieved through customisation options that
replace all relevant instances of green with blue, as well as the
overall design of the themes which relies mostly on colours that cover
the blue-cyan-magenta side of the spectrum.

The themes are built into GNU Emacs 28 (development target).  They are
also distributed in several packages formats.

+ `modus-operandi` is light.
+ `modus-vivendi` is dark.

For some demo content, check:

+ The screenshots of the themes <https://protesilaos.com/modus-themes-pictures/>.
+ My videos on Emacs <https://protesilaos.com/code-casts/>.

## Quick setup for the latest version

```elisp
(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))
```

Note: make sure that you **do not customise** `custom-theme-load-path`
or `custom-theme-directory` _after_ the themes' package declaration.
That will lead to failures in loading the files.  If you must change
those variables, do it before the package declaration.

## All customisations in short

While you should read the manual for all the details (see next section),
here is a snippet with all current customisation options and their
possible values.  Note that those settings are only for purposes of
demonstration:

```elisp
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-no-mixed-fonts nil
      modus-themes-subtle-line-numbers nil
      modus-themes-success-deuteranopia t
      modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

      modus-themes-fringes nil ; {nil,'subtle,'intense}

      ;; Options for `modus-themes-lang-checkers': nil,
      ;; 'straight-underline, 'subtle-foreground,
      ;; 'subtle-foreground-straight-underline, 'intense-foreground,
      ;; 'intense-foreground-straight-underline, 'colored-background
      modus-themes-lang-checkers nil

      ;; Options for `modus-themes-mode-line' are either nil, or a list
      ;; that can combine any of `3d' OR `moody', `borderless',
      ;; `accented'.  The variable's doc string shows all possible
      ;; combinations.
      modus-themes-mode-line '(3d accented)

      ;; Options for `modus-themes-syntax' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
      modus-themes-syntax nil

      ;; Options for `modus-themes-hl-line' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `accented', `underline', `intense'
      modus-themes-hl-line '(underline accented)

      ;; Options for `modus-themes-paren-match' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `bold', `intense', `underline'
      modus-themes-paren-match '(bold intense)

      ;; Options for `modus-themes-links': nil, 'faint,
      ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
      ;; 'underline-only, 'neutral-underline-only
      modus-themes-links 'neutral-underline

      ;; Options for `modus-themes-prompts' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `background', `bold', `gray', `intense', `italic'
      modus-themes-prompts '(intense bold)

      modus-themes-completions 'moderate ; {nil,'moderate,'opinionated}

      modus-themes-mail-citations nil ; {nil,'faint,'monochrome}

      ;; Options for `modus-themes-region' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `no-extend', `bg-only', `accented'
      modus-themes-region '(bg-only no-extend)

      ;; Options for `modus-themes-diffs': nil, 'desaturated,
      ;; 'bg-only, 'deuteranopia, 'fg-only-deuteranopia
      modus-themes-diffs 'fg-only-deuteranopia

      modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

      modus-themes-org-agenda ; this is an alist: read the manual or its doc string
      '((header-block . (variable-pitch scale-title))
        (header-date . (grayscale workaholic bold-today))
        (scheduled . uniform)
        (habit . traffic-light-deuteranopia))

      modus-themes-headings ; this is an alist: read the manual or its doc string
      '((1 . (overline background))
        (2 . (rainbow overline))
        (t . (no-bold)))

      modus-themes-variable-pitch-ui nil
      modus-themes-variable-pitch-headings t
      modus-themes-scale-headings t
      modus-themes-scale-1 1.1
      modus-themes-scale-2 1.15
      modus-themes-scale-3 1.21
      modus-themes-scale-4 1.27
      modus-themes-scale-title 1.33)
```

## Further information

Read the [Info manual HTML](https://protesilaos.com/modus-themes)
version for how to install, load, enable, and customise the themes.  If
you are using the latest version of the themes, you already have the
manual installed: evaluate `(info "(modus-themes) Top")` to start
reading it.

The themes cover a broad range of packages and are highly customisable.

## NOTE about old packages

Users coming from version `0.13.0` or earlier, are advised to read the
announcement on the emacs-devel mailing list:
<https://lists.gnu.org/archive/html/emacs-devel/2021-03/msg00300.html>.
Or read issue 174: <https://gitlab.com/protesilaos/modus-themes/-/issues/174>.

The web page of the change log is also available:
<https://protesilaos.com/modus-themes-changelog/>.

An Info manual should be distributed with the `modus-themes` package.
Evaluate this form to access it directly:

    (info "(modus-themes) Top")

Or visit it at: <https://protesilaos.com/modus-themes>.
