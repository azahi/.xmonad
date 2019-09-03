#!/bin/sh

PIC="${HOME}/pictures/screenshots/$(date +%Y-%m-%d_%H-%M-%S).png"

maim --format=png --hidecursor --quality 8 --select "${PIC}"
