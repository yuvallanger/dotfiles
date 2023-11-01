#!/bin/sh

if [ -z "$(command -v gsettings)" ]; then
    printf "No \"gsettings\" command!\n";
    exit 1;
fi

name="$1"
schema="$2"
key="$3"

current_value="$(gsettings get $schema $key)"

if [ "$current_value" = "true" ]; then
    printf "%s set to false\n" "$name"
    gsettings set "$schema" "$key" false
elif [ "$current_value" = "false" ]; then
    printf "%s set to true\n" "$name"
    gsettings set "$schema" "$key" true
else
    printf "Something's wrong! %s not changed.  Not a valid %s value: %s\n" \
           "$name" \
           "$key" \
           "$current_value"
fi
