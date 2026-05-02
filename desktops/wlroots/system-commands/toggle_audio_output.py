#!/usr/bin/env python

import subprocess
import re

found_audio = False
found_sinks = False
sinks = []

status = subprocess.run("wpctl status", shell=True, capture_output=True).stdout

for line in status.splitlines():
    line = line.decode("utf8").rstrip()

    if not found_audio and line != "Audio":
        continue
    found_audio = True

    if not found_sinks:
        if "Sinks:" in line:
            found_sinks = True
        continue

    match = re.match(r"[│\s]+(\*\s*)?(\d+)\. (.*)\[", line)
    if match:
        sinks.append(match.groups())
    else:
        break

for i in range(len(sinks)):
    is_default = sinks[i][0]
    if is_default:
        next_index = (i + 1) % len(sinks)
        next_default = sinks[next_index]
        subprocess.call(
            [
                "noctalia-shell",
                "ipc",
                "call",
                "toast",
                "send",
                f'{{ "title": "Audio output changed", "icon": "device-speaker", "body": "{next_default[2].strip()}" }}',
            ]
        )

        exit(subprocess.call(f"wpctl set-default {next_default[1]}", shell=True))
