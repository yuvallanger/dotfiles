#!/bin/sh

set -e

cleanup () {
    if [ "$2" = "--debug" ]; then
        printf "\nRemoving \"%s\"\n" "$temp_file" 1>&2
    fi
    rm "$temp_file"
}

help_message () {
    printf "Usage:
\t%s -p [--debug]
\t%s -s [--debug]
\t%s -b [--debug]
\t%s [FILENAME] [--debug]

Flags:
  -p, --primary         Operate on the PRIMARY selection.
  -s, --secondary       Operate on the SECONDARY selection.
  -b, --clipboard       Operate on the CLIPBOARD selection.
  --debug               Show debug information.
" "$0" "$0" "$0" "$0" # XXX: That's a dumb looking way of providing
                      # this one "$0" argument many times.
    exit 1

}

if [ -z "$(command -v xsel)" ] || [ -z "$(command -v curl)" ]; then
    printf "Must install xsel and curl.\n"
fi

if [ "$#" -ne 1 ] &&
       [ "$#" -ne 2 ]; then
    help_message
fi

if [ "$1" = "-p" ] ||
       [ "$1" = "-s" ] ||
       [ "$1" = "-b" ]; then
    xsel_flag="$1"

    trap cleanup EXIT

    temp_file=$(mktemp)

    xsel "$xsel_flag" > "$temp_file"

    if [ "$2" = "--debug" ]; then
        cat "$temp_file"
    else
        curl -F"file=@${temp_file}" 0x0.st
    fi
else
    filename="$1"

    if [ "$2" = "--debug" ]; then
        cat "$filename"
    else
        curl -F"file=@${filename}" 0x0.st
    fi
fi
