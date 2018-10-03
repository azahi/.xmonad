#!/usr/bin/env bash

if [[ $(pgrep 'compton') ]]
then
    pkill "compton"
else
    compton -b --config "${XDG_CONFIG_HOME}/compton.conf"
fi
