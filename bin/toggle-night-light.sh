#!/bin/sh

toggle-gsetting.sh 'Night light' \
                   org.gnome.settings-daemon.plugins.color \
                   night-light-enabled
