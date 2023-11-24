#!/bin/sh

set -e

if [ -z "$(command -v xsel)" ] || [ -z "$(command -v curl)" ]; then
    printf "Must install xsel and curl.\n"
    exit 1
fi

# https://www.baeldung.com/linux/bash-parse-command-line-arguments

OUR_ARGUMENTS="$(getopt -o hpsb --long help,debug,primary,secondary,clipboard -- "$@" )"

help_message () {
    printf "Usage:
\t%s [--debug] -p / --primay
\t%s [--debug] -s / --secondary
\t%s [--debug] -b / --clipboard
\t%s [--debug] [FILENAME]

Flags:
  -p, --primary         Operate on the PRIMARY selection.
  -s, --secondary       Operate on the SECONDARY selection.
  -b, --clipboard       Operate on the CLIPBOARD selection.
  --debug               Show debug information and do not upload to the 0x0.st.
" "$0" "$0" "$0" "$0"
    # XXX: That's a dumb looking way of providing
    # this one "$0" argument many times.
}

eval set -- "$OUR_ARGUMENTS"
while [ -n "$*" ]; do
    case "$1" in
        -h | --help)
            help_message
            exit 0
            shift
            ;;
        --debug)
            debug_flag=1
            shift
            ;;
        -p | primary)
            xsel_flag_primary="$1"
            if [ -n "$xsel_flag_secondary" ] || [ -n "$xsel_flag_clipboard" ]; then
                printf "Provide only none or one of -p / --primary, -s / --secondary, -b / --clipboard flags.\n"
                exit 1
            fi
            shift
            ;;
        -s | --secondary)
            xsel_flag_secondary="$1"
            if [ -n "$xsel_flag_primary" ] || [ -n "$xsel_flag_clipboard" ]; then
                printf "Provide only none or one of -p / --primary, -s / --secondary, -b / --clipboard flags.\n"
                exit 1
            fi
            shift
            ;;
        -b | clipboard)
            xsel_flag_clipboard="$1"
            if [ -n "$xsel_flag_primary" ] || [ -n "$xsel_flag_secondary" ]; then
                printf "Provide only none or one of -p / --primary, -s / --secondary, -b / --clipboard flags.\n"
                exit 1
            fi
            shift
            ;;
        --)
            shift
            filename="$1"
            if [ -z "$filename$xsel_flag_primary$xsel_flag_secondary$xsel_flag_clipboard" ]; then
                printf "Either provide a clipboard flag or a filename:\n\n"
                help_message
                exit 1
            fi
            break
            ;;
    esac
done

xsel_flag="$xsel_flag_primary"
xsel_flag="$xsel_flag_secondary"
xsel_flag="$xsel_flag_clipboard"

cleanup () {
    if [ -n "$debug_flag" ]; then
        printf "\nRemoving \"%s\"\n" "$temp_file" 1>&2
    fi
    rm "$temp_file"
}

if [ "$#" -ne 1 ] &&
       [ "$#" -ne 2 ]; then
    help_message
fi

if [ -n "$xsel_flag" ]; then
    trap cleanup EXIT

    temp_file=$(mktemp)

    xsel "$xsel_flag" > "$temp_file"

    if [ -n "$debug_flag" ]; then
        cat "$temp_file"
    else
        curl -F"file=@${temp_file}" 0x0.st
    fi
else
    if [ -n "$debug_flag" ]; then
        cat "$filename"
    else
        curl -F"file=@${filename}" 0x0.st
    fi
fi
