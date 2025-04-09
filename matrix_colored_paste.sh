#!/bin/bash

# Overview:
#   This script will help you convert your clipboard text (copied from Konsole) to colored text that is ready to paste into Matrix chat.
#   For example, users could copy the terminal output of a problem happening when building projects with kde-builder, and then send it to developers on Matrix.

# Setup:
#   It is assumed you use Wayland session.
#   Install wl-clipboard with your package manager.
#   In system settings, in keyboard shortcuts, add a new custom shortcut pointing to this script. Let's say it is Meta+U.

# Usage:
#   Select some text in Konsole, press Copy in context menu or with keyboard shortcut.
#   Press your custom shortcut Meta+U.
#   In the chat room in matrix, paste your clipboard and send message.

# Explanation:
#   When copying text from Konsole, it can be pasted not just as usual plain text, but as text/html.
#   This by itself however is not useful, because Matrix does not allow "style" attribute in "span" tags, see:
#   https://spec.matrix.org/v1.14/client-server-api/#mroommessage-msgtypes
#   We remove style attributes, while converting colors to matrix attributes, and we remove "uncolored" background and font
#   (otherwise appeared as forced to black text on white background), so it looks nice in chat with both dark and light themes.

echo "/html $(wl-paste --type text/html)" | \
sed 's/font-weight:bold;//g' | \
sed -E 's/style="color:#([0-9A-Fa-f]{6});background-color:#([0-9A-Fa-f]{6});"/data-mx-color="#\1" data-mx-bg-color="#\2"/g' | \
sed 's/ data-mx-bg-color="#ffffff"//g' | \
sed 's/ data-mx-color="#000000"//g' | \
wl-copy
