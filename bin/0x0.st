#!/usr/bin/bash

set -e

if [ -z "$(command -v xsel)" ] || [ -z "$(command -v curl)" ]; then
    printf "Must install xsel and curl.\n"
    exit 1
fi

if [ -z "${URL_FOR_0X0_DOT_ST}" ]; then
    URL_FOR_0X0_DOT_ST="0x0.st"
fi

self_executable="$0"

help_message () {
    printf "Usage:\n"
    printf "\t%s [--debug] -p, --primay\n" "$self_executable"
    printf "\t%s [--debug] -s, --secondary\n" "$self_executable"
    printf "\t%s [--debug] -b, --clipboard\n" "$self_executable"
    printf "\t%s [--debug] -u URL, --url URL\n" "$self_executable"
    printf "\t%s [--debug] FILENAME\n" "$self_executable"
    printf "\n"
    printf "Flags:
    -p, --primary         Operate on the PRIMARY selection.
    -s, --secondary       Operate on the SECONDARY selection.
    -b, --clipboard       Operate on the CLIPBOARD selection.
    -u, --url             Tell 0x0.st to mirror the file at the URL.
    --debug               Show debug information and do not upload to the 0x0.st.\n"
    printf "\n"
    printf "Environment Variables:
    URL_FOR_0X0_DOT_ST (all upper case)
        Its value will be used for the 0x0.st instance address.
        If URL_FOR_0X0_DOT_ST is empty, 0x0.st will be used by default.\n"
}

print_variables() {
    printf "Self executable: '%s'\n" "$self_executable" 1>&2

    printf "Flags:\n" 1>&2
    printf "    xsel_flag: '%s'\n" "$xsel_flag" 1>&2
    printf "    target_url: '%s'\n" "$target_url" 1>&2

    printf "Environment Variables:\n" 1>&2
    printf "    URL_FOR_0X0_DOT_ST: '%s'\n" "$URL_FOR_0X0_DOT_ST" 1>&2
}

print_provide_one_and_only_one_flag_message () {
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

if [ -n "$debug_flag" ]; then
    print_variables
fi

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
        curl -F"file=@${temp_file}" "$URL_FOR_0X0_DOT_ST"
    fi
elif [ -n "$url_flag" ]; then
    if [ -n "$debug_flag" ]; then
        curl "$target_url"
    else
        curl -F"url=$target_url" "$URL_FOR_0X0_DOT_ST"
    fi
else
    if [ -n "$debug_flag" ]; then
        cat "$filename"
    else
        curl -F"file=@${filename}" "$URL_FOR_0X0_DOT_ST"
    fi
fi
