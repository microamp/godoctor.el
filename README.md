# godoctor.el

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

![godoctor-rename](https://cloud.githubusercontent.com/assets/1378791/19228157/e784c042-8f1e-11e6-944c-c00483c65398.gif)

### `godoctor-extract`

![godoctor-extract](https://cloud.githubusercontent.com/assets/1378791/19228164/fb5755c6-8f1e-11e6-83e8-763722622a38.gif)

### `godoctor-toggle`

![godoctor-toggle](https://cloud.githubusercontent.com/assets/1378791/19228165/04883782-8f1f-11e6-8aed-2fe42d4dd9c7.gif)

### `godoctor-godoc`

![godoctor-godoc](https://cloud.githubusercontent.com/assets/1378791/19228166/049195ca-8f1f-11e6-88f3-246db64927d7.gif)

## License

GNU General Public License version 3
