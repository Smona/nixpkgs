#!/usr/bin/env python3
import os
import json
import re
import sys
import subprocess
from dataclasses import dataclass
from socket import AF_UNIX, SOCK_STREAM, socket

import gi

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk

instance_signature = os.environ.get("HYPRLAND_INSTANCE_SIGNATURE")
SOCKET_PATH = f"/tmp/hypr/{instance_signature}/.socket2.sock"
s = socket(AF_UNIX, SOCK_STREAM)

icon_theme = Gtk.IconTheme.get_default()


def get_icon(icon_name):
    icon_info = icon_theme.lookup_icon(icon_name, 48, 0) or ""
    return icon_info.get_filename() if icon_info else ""


@dataclass
class State:
    active_window: str = ""
    active_window_icon: str = ""
    num_workspaces: int = 1
    active_workspace: int = 1


def report(state: State):
    print(json.dumps(state.__dict__))
    # Make sure messages go through immediately so UI is instantly updated
    sys.stdout.flush()


def get_command_output(cmd: str):
    return subprocess.check_output(cmd, shell=True).decode("utf8")


def parse_hyprctl_object(output: str):
    result = {}
    for line in output.splitlines():
        # Skip header lines & padding
        if line.startswith("\t"):
            key, value = line.strip().split(": ", 1)
            result[key] = value

    return result


def set_initial_active_window(state: State):
    output = get_command_output("hyprctl activewindow")
    active_window = parse_hyprctl_object(output)
    state.active_window = active_window.get("title", "")
    window_class = active_window.get("class", "")
    state.active_window_icon = get_icon(window_class) if window_class else ""


def set_initial_workspaces(state: State, active_window: str):
    result = get_command_output("hyprctl workspaces")
    workspaces = [parse_hyprctl_object(w) for w in result.strip().split("\n\n")]

    state.num_workspaces = len(workspaces)
    for index, workspace in enumerate(workspaces):
        if workspace["lastwindowtitle"] == active_window:
            state.active_workspace = index + 1
            break


def main():
    s.connect(SOCKET_PATH)

    state = State()
    set_initial_active_window(state)
    set_initial_workspaces(state, state.active_window)
    report(state)

    while True:
        payload = s.recv(4096).decode("utf8").strip()

        # Message may be buffered
        for msg in payload.split("\n"):
            dirty = False
            msg_type, data = msg.split(">>")

            if msg_type == "activewindow":
                window_class, title = data.split(",")
                state.active_window = title
                state.active_window_icon = get_icon(window_class)
                dirty = True

            if msg_type == "createworkspace":
                state.num_workspaces += 1
                dirty = True

            if msg_type == "destroyworkspace":
                state.num_workspaces -= 1
                dirty = True

            if msg_type == "workspace":
                state.active_workspace = int(data)
                dirty = True

            if dirty:
                report(state)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        s.close()
