# Modus themes for GNU Emacs

IMAGES HERE: <https://protesilaos.com/emacs/modus-themes-pictures>.

Highly accessible themes, conforming with the highest standard for
colour contrast between background and foreground values (WCAG AAA).
They also are optimised for users with red-green colour deficiency.

The themes are very customisable and provide support for a very wide
range of packages.  Their manual is detailed so that new users can get
started, while it also provides custom code for all sorts of more
advanced customisations.

Since August 2020, the original Modus themes (`modus-operandi`,
`modus-vivendi`) are built into Emacs version 28 or higher.  Emacs 28
ships with `modus-themes` version `1.6.0`.  Emacs 29 includes version
`3.0.0`.  Emacs 30 provides a newer, refactored version that
thoroughly refashions how the themes are implemented and customized.
Such major versions are not backward-compatible due to the limited
resources at my disposal to support multiple versions of Emacs and of
the themes across the years.

Starting with version `5.0.0` of the `modus-themes`, other packages
can be built on top to provide their own "Modus" derivative themes.
The manual has a section about building on top of Modus. My
`ef-themes` and `standard-themes` are done in this way (versions
`2.0.0` and `3.0.0`, respectively).

+ Package name (GNU ELPA): `modus-themes`
+ Official manual: <https://protesilaos.com/emacs/modus-themes>
+ Change log: <https://protesilaos.com/emacs/modus-themes-changelog>
+ Colour palette: <https://protesilaos.com/emacs/modus-themes-colors>
+ Sample pictures: <https://protesilaos.com/emacs/modus-themes-pictures>
+ Git repositories:
  + GitHub: <https://github.com/protesilaos/modus-themes>
  + GitLab: <https://gitlab.com/protesilaos/modus-themes>
+ Backronym: My Old Display Unexpectedly Sharpened ... themes.

```elisp
(use-package modus-themes
  :ensure t
  :demand t
  :init
  ;; Starting with version 5.0.0 of the `modus-themes', other packages
  ;; can be built on top to provide their own "Modus" derivatives.
  ;; For example, this is what I do with my `ef-themes' and
  ;; `standard-themes' (starting with versions 2.0.0 and 3.0.0,
  ;; respectively).
  ;;
  ;; The `modus-themes-include-derivatives-mode' makes all Modus
  ;; commands that act on a theme consider all such derivatives, if
  ;; their respective packages are available and have been loaded.
  ;;
  ;; Note that those packages can even completely take over from the
  ;; Modus themes such that, for example, `modus-themes-rotate' only
  ;; goes through the Ef themes (to this end, the Ef themes provide
  ;; the `ef-themes-take-over-modus-themes-mode' and the Standard
  ;; themes have the `standard-themes-take-over-modus-themes-mode'
  ;; equivalent).
  ;;
  ;; If you only care about the Modus themes, then (i) you do not need
  ;; to enable the `modus-themes-include-derivatives-mode' and (ii) do
  ;; not install and activate those other theme packages.
  (modus-themes-include-derivatives-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; Your customizations here:
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-to-rotate modus-themes-items
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides nil)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'modus-operandi))
```
