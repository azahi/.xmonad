#!/usr/bin/env bash

if ! pgrep redshift > /dev/null 2>&1
then
    redshift &
    disown redshift
else
    pkill redshift
fi
