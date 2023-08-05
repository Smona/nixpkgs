#!/usr/bin/env python3
import subprocess

SWITCH_KEYBOARD_ENABLED = [
    "dconf",
    "write",
    "/org/gnome/desktop/a11y/applications/screen-keyboard-enabled",
]


def main():
    p = subprocess.Popen(
        # Line-buffer the output of `libinput debug-events`
        ["stdbuf", "-oL", "-eL", "libinput", "debug-events"],
        stdout=subprocess.PIPE,
    )

    assert p.stdout is not None
    for line in iter(p.stdout.readline, b""):
        if b"tablet-mode state 1" in line:
            subprocess.call(SWITCH_KEYBOARD_ENABLED + ["true"])
            print("tablet mode enabled")

        if b"tablet-mode state 0" in line:
            subprocess.call(SWITCH_KEYBOARD_ENABLED + ["false"])
            print("tablet mode disabled")


if __name__ == "__main__":
    main()
