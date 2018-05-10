# godoctor.el

[![MELPA](https://melpa.org/packages/godoctor-badge.svg)](https://melpa.org/#/godoctor)

Emacs frontend for [godoctor](https://github.com/godoctor/godoctor).

## Installation

The package is available on [MELPA](https://melpa.org). Run `M-x package-install <RET> godoctor <RET>` to install.

## Quickstart

```emacs-lisp
(use-package go-mode
  :config (use-package godoctor))
```

## Commands

- `godoctor-rename`
- `godoctor-extract`
- `godoctor-toggle`
- `godoctor-godoc`
- `godoctor-rename-dry-run`
- `godoctor-extract-dry-run`
- `godoctor-toggle-dry-run`
- `godoctor-godoc-dry-run`

See [Getting Started](http://gorefactor.org/starting.html) for more details.

## Screenshots

### `godoctor-rename`

![godoctor-rename](https://cloud.githubusercontent.com/assets/1378791/19587359/510c200c-97ba-11e6-9d36-fdc07a583f56.gif)

### `godoctor-extract`

![godoctor-extract](https://cloud.githubusercontent.com/assets/1378791/19587358/50c8ef8a-97ba-11e6-88fd-2360351df50f.gif)

### `godoctor-toggle`

![godoctor-toggle](https://cloud.githubusercontent.com/assets/1378791/19587361/5131b920-97ba-11e6-9555-b81bf2695221.gif)

### `godoctor-godoc`

![godoctor-godoc](https://cloud.githubusercontent.com/assets/1378791/19587360/510e99cc-97ba-11e6-8029-acfdbe015a45.gif)

## Contributors

- [@syohex](https://github.com/syohex)
- [@grafov](https://github.com/grafov)
- [@kreg](https://github.com/kreg)

## License

GNU General Public License version 3
