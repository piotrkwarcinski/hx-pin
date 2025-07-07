# hx-pin [WIP]

A Helix plugin for pinning files.

## Features

* Pinned files are persisted locally and opened at startup.
* Allows for switching between pinned files using configured shortcuts or a custom picker.

## File picker bindings

* Move up the list using `k`
* Move down the list using `j`
* Reordering is possible using `(` and `)`.
* Unpinning a file is possible through `D`.

## Install
```
forge pkg install --git https://github.com/piotrkwarcinski/hx-pin.git
```

Configure keybindings
```
(keymap (global)
        (normal
          (space (space ":pin.pin-open-selector")
               (n ":pin.pin-add")
               ("0" ":pin.pin-goto 0")
               ("1" ":pin.pin-goto 1")
               ("2" ":pin.pin-goto 2")
               ("3" ":pin.pin-goto 3")
               ("4" ":pin.pin-goto 4")
               ("5" ":pin.pin-goto 5")
               (N ":pin.pin-refresh")
               (O ":pin.pin-close-others"))))
```

## Tips

* You can use the plugin to reorder your pinned buffers, but all other opened buffers are going to be closed during the process.
