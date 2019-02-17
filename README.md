# helm-codesearch.el [![MELPA](http://melpa.org/packages/helm-codesearch-badge.svg)](http://melpa.org/#/helm-codesearch)

helm interface for codesearch

## ScreenShot

- **find pattern**

<img align="center" src="https://raw.github.com/youngker/helm-codesearch.el/master/helm-codesearch-pattern.png">

- **find file**

<img align="center" src="https://raw.github.com/youngker/helm-codesearch.el/master/helm-codesearch-file.png">

## Installation

It's available on [Melpa](https://melpa.org/):

    M-x package-install helm-codesearch

## Requirements

- **Google codesearch**

  [https://github.com/youngker/codesearch](https://github.com/youngker/codesearch)


You can add these lines to your init file.

```elisp
(define-key global-map (kbd "C-c h f") 'helm-codesearch-find-file)
(define-key global-map (kbd "C-c h t") 'helm-codesearch-find-pattern)
(define-key global-map (kbd "C-c h I") 'helm-codesearch-create-csearchindex)
```

If you want to search for multiple projects, you need to set the csearchindex.

```elisp
(setq helm-codesearch-global-csearchindex "~/.csearchindex")
```

## Key bindings

Key | Function
--- | --------
<kbd>C-c f</kbd> | helm-codesearch-run-set-filename
<kbd>C-c i</kbd> | helm-codesearch-run-ignore-case
<kbd>C-c s</kbd> | helm-codesearch-run-save-buffer

## Saved buffer Key bindings

helm-codesearch now supports saving search results to a new buffer.

Key | Function
--- | --------
<kbd>RET</kbd> | helm-codesearch-jump-to-source
<kbd>n</kbd> | helm-codesearch-next-line
<kbd>p</kbd> | helm-codesearch-previous-line
<kbd>q</kbd> | helm-codesearch-quit-window

## License

Copyright (C) 2019 Youngjoo Lee

Author: Youngjoo Lee <youngker@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
