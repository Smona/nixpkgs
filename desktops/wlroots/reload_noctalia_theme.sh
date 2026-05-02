#!/usr/bin/env bash

kill -SIGUSR1 $(pgrep kitty) || true
emacsclient --eval "(load-theme 'noctalia :no-confirm)" || true
