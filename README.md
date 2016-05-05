unkillable-scratch
==================
![Travis CI](https://travis-ci.org/EricCrosson/unkillable-scratch.svg?branch=master)
[![MELPA](http://melpa.org/packages/unkillable-scratch-badge.svg)](http://melpa.org/#/unkillable-scratch)
[![GNU GPL](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Overview
--------

This package disallows the `*scratch*` buffer from being killed. It can also be
used generally to prevent an arbitrary buffer from being destroyed.

Installation
------------

Install from [melpa] with `M-x package-install RET unkillable-scratch RET`.

Usage
-----

The variable `unkillable-scratch-behavior` defines the action taken when a kill
is attempted on a buffer matching one or more of the regexp's in the list
`unkillable-buffers`. This list will only match one buffer by default, the
`*scratch*` buffer.

This package treats the `*scratch*` buffer specially; in the event of a call to
`kill-buffer` the buffer contents will be replaced with
`initial-scratch-message`. Removing the regexp matching `*scratch*` from
`unkillable-buffers` disables this behavior.

The following values of `unkillable-scratch-behavior` are supported

| Setting       | Meaning                                             |
|--------------:|-----------------------------------------------------|
| `'do-nothing` | disallow the attempted kill from occurring          |
| `'bury`       | bury the buffer instead of killing it               |
| `'kill`       | kill the buffer -- same as disabling the minor mode |


  [melpa]: https://github.com/milkypostman/melpa
