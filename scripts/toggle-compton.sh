#!/bin/sh

if ! pgrep compton > /dev/null 2>&1
then
    compton -b
else
    pkill compton
fi
