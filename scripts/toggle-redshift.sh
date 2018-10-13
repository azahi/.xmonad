#!/usr/bin/env bash

if [[ $(pgrep 'redshift') ]]
then
    pkill "redshift"
else
    redshift -c "${XDG_CONFIG_HOME}/redshift.conf" &
    disown redshift
fi
