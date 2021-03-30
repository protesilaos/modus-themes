# Modus themes (Modus Operandi and Modus Vivendi)

A pair of highly accessible themes that conform with the WCAG AAA
standard for colour contrast between background and foreground
combinations (a minimum contrast of 7:1---the highest standard of its
kind).

The themes are built into GNU Emacs 28 (development target).  They are
also distributed in several packages formats.

+ `modus-operandi` is light
+ `modus-vivendi` is dark

## Quick setup for the latest version

```elisp
(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region 'no-extend)

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

## Further information

Read the [Info manual HTML](https://protesilaos.com/modus-themes)
version for how to install, load, enable, and customise the themes.  If
you are using the latest version of the themes, you already have the
manual installed: evaluate `(info "(modus-themes) Top")` to start
reading it.

The themes cover a broad range of packages and are highly customisable.

For some demo content, check:

+ the screenshots https://protesilaos.com/modus-themes-pictures/
+ my videos https://protesilaos.com/code-casts/

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
