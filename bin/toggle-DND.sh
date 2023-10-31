#!/bin/sh

if [ -z "$(command -v gsettings)" ]; then
    echo "No gsettings.";
    exit 1;
fi

show_banners_value="$(gsettings get org.gnome.desktop.notifications show-banners)"

if [ "$show_banners_value" = "true" ]; then
    echo "DND turn on."
    gsettings set org.gnome.desktop.notifications show-banners false
elif [ "$show_banners_value" = "false" ]; then
    echo "DND turn off."
    gsettings set org.gnome.desktop.notifications show-banners true
else
    echo "Something's wrong! Not a valid show-banner value: $show_banners_value"
fi
