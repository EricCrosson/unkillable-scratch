# unkillable-scratch [![Build Status](https://travis-ci.org/EricCrosson/unkillable-scratch.svg?branch=master)](https://travis-ci.org/EricCrosson/unkillable-scratch) [![MELPA Stable](https://stable.melpa.org/packages/unkillable-scratch-badge.svg)](https://stable.melpa.org/#/unkillable-scratch) [![MELPA](http://melpa.org/packages/unkillable-scratch-badge.svg)](http://melpa.org/#/unkillable-scratch)

> Disallow the \*scratch\* buffer from being killed

## Install

From [MELPA](https://melpa.org/)

``` {.sourceCode .lisp}
(use-package unkillable-scratch
  :ensure t
  :pin melpa-stable
  :config (unkillable-scratch t))
```

Or manually, after downloading into your `load-path`

``` {.sourceCode .lisp}
(require 'unkillable-scratch)
(unkillable-scratch t)
```

## Use

The variable `unkillable-scratch-behavior` defines the action taken when a kill
is attempted on a buffer matching one or more of the regexp's in the list
`unkillable-buffers`. This list will only match one buffer by default, the
`*scratch*` buffer.

This package treats the `*scratch*` buffer specially; in the event of a call to
`kill-buffer` the buffer contents will be replaced with
`initial-scratch-message`.

The following values of `unkillable-scratch-behavior` are supported

|       Setting | Meaning                                             |
|--------------:|-----------------------------------------------------|
|       `'bury` | bury the buffer instead of killing it (default)     |
| `'do-nothing` | disallow the attempted kill from occurring          |
|       `'kill` | kill the buffer -- same as disabling the minor mode |

<!-- ## Example -->

<!-- ![TODO: set hover-text](https://raw.githubusercontent.com/EricCrosson/unkillable-scratch/master/img/demo.{TODO: set filetype png,gif}) -->

## Related

[persistent-scratch](https://github.com/Fanael/persistent-scratch) is
definitely worth a look.

## Acknowledgements

The inspiration to make the `unkillable-buffers` list came from
[Donald Curtis (milkypostman)](http://emacswiki.org/emacs/RecreateScratchBuffer)

## Regrets

If I were the type of person to proliferate breaking-changes, I would
rename the `unkillable-scratch` minor-mode to
`unkillable-scratch-mode` for conventions' sake.

Making this change would necessitate re-publishing the package to
MELPA and only consume the time of the MELPA maintainers and every
user of this package without making any functional changes, so for now
just note that said function is enabling a global minor-mode.

## License

GPL 2 (or higher) © [Free Software Foundation, Inc](http://www.fsf.org/about).
