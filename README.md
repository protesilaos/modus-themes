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
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))
```

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
