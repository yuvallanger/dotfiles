#!/bin/sh

if [ -z "$(command -v gsettings)" ]; then
    echo "No gsettings.";
    exit 1;
fi

current_value="$(gsettings get org.gnome.settings-daemon.plugins.color night-light-enabled)"

if [ "$current_value" = "true" ]; then
    echo "Night light turn off."
    gsettings set org.gnome.settings-daemon.plugins.color night-light-enabled false
elif [ "$current_value" = "false" ]; then
    echo "Night light turn on."
    gsettings set org.gnome.settings-daemon.plugins.color night-light-enabled true
else
    echo "Something's wrong! Not a valid value: $current_value"
fi
