#!/usr/bin/env bash

kill -SIGUSR1 $(pgrep kitty) || true
emacsclient --eval "(load-theme 'noctalia :no-confirm)" || true
# `pywalfox update` can be used to have clients update colors as well, but it seems to be
# going automatically for now
