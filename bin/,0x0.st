#!/usr/bin/bash

set -e

if [ -z "$(command -v xsel)" ] || [ -z "$(command -v curl)" ]; then
    printf "Must install xsel and curl.\n"
    exit 1
fi

if [ -z "${URL_FOR_0X0_DOT_ST}" ]; then
    # Default URL:
    url_for_0x0_dot_st="0x0.st"
else
    url_for_0x0_dot_st="$URL_FOR_0X0_DOT_ST"
fi

self_executable="$0"

help_message () {
    printf "Usage:\n"
    printf "\t%s [--debug] -p, --primay\n" "$self_executable"
    printf "\t%s [--debug] -s, --secondary\n" "$self_executable"
    printf "\t%s [--debug] -b, --clipboard\n" "$self_executable"
    printf "\t%s [--debug] -u URL, --url URL\n" "$self_executable"
    printf "\t%s [--debug] -x URL, --null-dot-st-url URL\n" "$self_executable"
    printf "\t%s [--debug] FILENAME\n" "$self_executable"
    printf "\n"
    printf "Flags:
    -p, --primary          Operate on the PRIMARY selection.
    -s, --secondary        Operate on the SECONDARY selection.
    -b, --clipboard        Operate on the CLIPBOARD selection.
    -u, --url              Tell 0x0.st to mirror the file at the URL.
    -x, --null-dot-st-url  The 0x0.st instance address, instead of 0x0.st itself.
    --debug                Show debug information and do not upload to 0x0.st.\n"
    printf "\n"
    printf "Environment Variables:
    URL_FOR_0X0_DOT_ST (all upper case)
        Its value will be used for the 0x0.st instance address.
        If URL_FOR_0X0_DOT_ST is empty, 0x0.st will be used by default.\n"
}

print_variables() {
    printf "Self executable: '%s'\n" "$self_executable" 1>&2

    printf "Flags:\n" 1>&2
    printf "    null_dot_st_url_flag: '%s'\n" "$null_dot_st_url_flag"
    printf "    null_dot_st_url: '%s'\n" "$null_dot_st_url"
    printf "    xsel_flag: '%s'\n" "$xsel_flag" 1>&2
    printf "    target_url: '%s'\n" "$target_url" 1>&2

    printf "Environment Variables:\n" 1>&2
    printf "    URL_FOR_0X0_DOT_ST: '%s'\n" "$URL_FOR_0X0_DOT_ST" 1>&2
}

print_provide_one_and_only_one_flag_error_message_and_exit() {
    printf "Provide only none or one of -p / --primary, -s / --secondary, -b / --clipboard, -u / --url flags.\n"
    help_message
    if [ -n "$debug_flag" ]; then
        print_variables
    fi
    exit 1
}

# https://www.baeldung.com/linux/bash-parse-command-line-arguments
flags_flag=""
OUR_ARGUMENTS="$(getopt -o hpsbu:x: --long help,debug,primary,secondary,clipboard,url:,null-dot-st-url: -- "$@" )"
eval set -- "$OUR_ARGUMENTS"
while [ -n "$*" ]; do
    case "$1" in
        -h | --help)
            help_message
            exit 0
            ;;
        --debug)
            debug_flag=1
            shift
            ;;
        -p | --primary)
            if [ -z "$1" ]; then
                continue;
            elif [ -n "$flags_flag" ]; then
                print_provide_one_and_only_one_flag_error_message_and_exit
            else
                flags_flag=1
            fi

            xsel_flag="$1"
            shift
            ;;
        -s | --secondary)
            if [ -z "$1" ]; then
                continue;
            elif [ -n "$flags_flag" ]; then
                print_provide_one_and_only_one_flag_error_message_and_exit
            else
                flags_flag=1
            fi

            xsel_flag="$1"
            shift
            ;;
        -b | --clipboard)
            if [ -z "$1" ]; then
                continue;
            elif [ -n "$flags_flag" ]; then
                print_provide_one_and_only_one_flag_error_message_and_exit
            else
                flags_flag=1
            fi

            xsel_flag="$1"
            shift
            ;;
        -u | --url)
            if [ -z "$1" ]; then
                continue;
            elif [ -n "$flags_flag" ]; then
                print_provide_one_and_only_one_flag_error_message_and_exit
            else
                flags_flag=1
            fi

            url_flag="$1"
            target_url="$2"
            shift 2
            ;;
        -x | --null-dot-st-url )
            null_dot_st_url_flag="$1"
            null_dot_st_url="$2"
            shift 2
            ;;
        --)
            shift
            if [ -z "$1" ]; then
                break
            elif [ -n "$flags_flag" ]; then
                print_provide_one_and_only_one_flag_error_message_and_exit
            fi

            filename="$1"
            break
            ;;
        *)
            printf "Something is wrong." 1>&2
            exit 1
            ;;
    esac
done

if [ -z "$null_dot_st_url_flag" ]; then
    null_dot_st_url="$url_for_0x0_dot_st"
fi

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
        curl -F"file=@${temp_file}" "$null_dot_st_url"
    fi
elif [ -n "$url_flag" ]; then
    if [ -n "$debug_flag" ]; then
        curl "$target_url"
    else
        curl -F"url=$target_url" "$null_dot_st_url"
    fi
else
    if [ -n "$debug_flag" ]; then
        cat "$filename"
    else
        curl -F"file=@${filename}" "$null_dot_st_url"
    fi
fi
