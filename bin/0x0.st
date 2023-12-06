#!/usr/bin/bash

set -e

if [ -z "$(command -v xsel)" ] || [ -z "$(command -v curl)" ]; then
    printf "Must install xsel and curl.\n"
    exit 1
fi

help_message () {
    printf "Usage:
\t%s [--debug] -p, --primay
\t%s [--debug] -s, --secondary
\t%s [--debug] -b, --clipboard
\t%s [--debug] -u URL, --url URL
\t%s [--debug] FILENAME

Flags:
  -p, --primary         Operate on the PRIMARY selection.
  -s, --secondary       Operate on the SECONDARY selection.
  -b, --clipboard       Operate on the CLIPBOARD selection.
  -u, --url             Tell 0x0.st to mirror the file at the URL.
  --debug               Show debug information and do not upload to the 0x0.st.
" "$0" "$0" "$0" "$0"
    # XXX: That's a dumb looking way of providing
    # this one "$0" argument many times.
}

print_provide_one_and_only_one_flag_message_flag () {
    printf "Provide only none or one of -p / --primary, -s / --secondary, -b / --clipboard, -u / --url flags.\n"
}

# https://www.baeldung.com/linux/bash-parse-command-line-arguments
OUR_ARGUMENTS="$(getopt -o hpsbu: --long help,debug,primary,secondary,clipboard,url: -- "$@" )"
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
        -p | --primary)
            xsel_flag_primary="$1"
            if [ -n "$xsel_flag_secondary" ] || [ -n "$xsel_flag_clipboard" ] || [ -n "$url_flag" ]; then
                print_provide_one_and_only_one_flag_message
                help_message
                exit 1
            fi
            shift
            ;;
        -s | --secondary)
            xsel_flag_secondary="$1"
            if [ -n "$xsel_flag_primary" ] || [ -n "$xsel_flag_clipboard" ] || [ -n "$url_flag" ]; then
                print_provide_one_and_only_one_flag_message
                help_message
                exit 1
            fi
            shift
            ;;
        -b | --clipboard)
            xsel_flag_clipboard="$1"
            if [ -n "$xsel_flag_primary" ] || [ -n "$xsel_flag_secondary" ] || [ -n "$url_flag" ]; then
                print_provide_one_and_only_one_flag_message
                help_message
                exit 1
            fi
            shift
            ;;
        -u | --url)
            url_flag="$1"
            if [ -n "$xsel_flag_primary" ] || [ -n "$xsel_flag_secondary" ] || [ -n "$xsel_flag_clipboard" ]; then
                print_provide_one_and_only_one_flag_message
                help_message
                exit 1
            fi
            target_url="$2"
            shift 2
            ;;
        --)
            shift
            filename="$1"
            if [ -z "$filename$xsel_flag_primary$xsel_flag_secondary$xsel_flag_clipboard$url_flag" ]; then
                print_provide_one_and_only_one_flag_message
                help_message
                exit 1
            fi
            xsel_flag="$xsel_flag_primary$xsel_flag_secondary$xsel_flag_clipboard"
            break
            ;;
        *)
            printf "Something is wrong." 1>&2
            exit 1
            ;;
    esac
done

remove_temporary_file () {
    if [ -n "$debug_flag" ]; then
        printf "\nRemoving \"%s\"\n" "$temp_file" 1>&2
    fi
    rm "$temp_file"
}

if [ -n "$xsel_flag" ]; then
    trap remove_temporary_file EXIT

    temp_file=$(mktemp)

    xsel "$xsel_flag" > "$temp_file"

    if [ -n "$debug_flag" ]; then
        cat "$temp_file"
    else
        curl -F"file=@${temp_file}" 0x0.st
    fi
elif [ -n "$url_flag" ]; then
    if [ -n "$debug_flag" ]; then
        curl "$target_url"
    else
        curl -F"url=$target_url" 0x0.st
    fi
else
    if [ -n "$debug_flag" ]; then
        cat "$filename"
    else
        curl -F"file=@${filename}" 0x0.st
    fi
fi
