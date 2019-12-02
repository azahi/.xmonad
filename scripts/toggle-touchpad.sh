#!/bin/sh

ID=$(xinput list | grep -Eo 'TouchPad\s*id\=[0-9]{1,2}' | grep -Eo '[0-9]{1,2}')
STATE=$(xinput list-props "$ID" | awk '/Device Enabled/ {print $4}')

if [ "$STATE" = 1 ]
then
    xinput disable "$ID"
else
    xinput enable "$ID"
fi
