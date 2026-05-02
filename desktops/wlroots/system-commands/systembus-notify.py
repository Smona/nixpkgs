#!/usr/bin/env python3

"""Listen for earlyoom notifications on the system D-Bus and forward them as Noctalia toasts."""
import subprocess

import dbus
from dbus.mainloop.glib import DBusGMainLoop
from gi.repository import GLib


def on_signal(*args):
    kill_message_parts = args[1].split(" ")
    pid, pname = kill_message_parts[-2:]
    subprocess.run(
        [
            "noctalia-shell",
            "ipc",
            "call",
            "toast",
            "send",
            f'{{ "title": "RAM critically low!", "body": "Killed process “{pname}” ({pid}) to prevent OOM", "type": "error" }}',
        ]
    )


DBusGMainLoop(set_as_default=True)

bus = dbus.SystemBus()
bus.add_signal_receiver(
    on_signal,
    dbus_interface="net.nuetzlich.SystemNotifications",
)

# Run the event loop
try:
    loop = GLib.MainLoop()
    loop.run()
except:
    pass
