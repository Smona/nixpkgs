#!/usr/bin/env python3
import os
import json
import re
import sys
import subprocess
from dataclasses import dataclass
from socket import AF_UNIX, SOCK_STREAM, socket

instance_signature = os.environ.get("HYPRLAND_INSTANCE_SIGNATURE")
SOCKET_PATH = f"/tmp/hypr/{instance_signature}/.socket2.sock"
s = socket(AF_UNIX, SOCK_STREAM)


@dataclass
class State:
    active_window: str = ""
    num_workspaces: int = 1
    active_workspace: int = 1


def report(state: State):
    print(json.dumps(state.__dict__))
    # Make sure messages go through immediately so UI is instantly updated
    sys.stdout.flush()


def get_instantaneous_title():
    result = subprocess.check_output("hyprctl activewindow", shell=True)
    for line in result.decode("utf8").splitlines():
        if re.match(r"\s+title:", line):
            return line.strip().split(" ")[1]
    return ""


def main():
    s.connect(SOCKET_PATH)

    result = subprocess.check_output("hyprctl workspaces", shell=True).decode("utf8")

    state = State(
        active_window=get_instantaneous_title(),
        num_workspaces=len(result.split("\n\n")) - 1,
    )
    report(state)

    while True:
        payload = s.recv(4096).decode("utf8").strip()

        # Message may be buffered
        for msg in payload.split("\n"):
            dirty = False
            msg_type, data = msg.split(">>")

            if msg_type == "activewindow":
                window_class, title = data.split(",")
                state.active_window = re.sub(r"(.*) — Mozilla Firefox", r" \1", title)
                dirty = True

            if msg_type == "createworkspace":
                state.num_workspaces += 1
                dirty = True

            if msg_type == "destroyworkspace":
                state.num_workspaces -= 1
                dirty = True

            if msg_type == "workspace":
                state.active_workspace = data
                dirty = True

            if dirty:
                report(state)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        s.close()
