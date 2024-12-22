export LANG=en_US.UTF-8
export LC_ALL=en_IL.UTF-8

export INFOPATH="/usr/share/info:$INFOPATH"

PATH="$HOME/.local/bin:$PATH"

# shellcheck source=.bash_guix_profile
GUIX_PROFILE="$HOME/.guix-profile"
if [ -f "$GUIX_PROFILE/etc/profile" ]; then
    # shellcheck source=.guix-profile/etc/profile
    source "$GUIX_PROFILE/etc/profile"
fi

# Point at the Guix libraries.
#if [ -f "$HOME/.config/guix/current/share/guile/site/3.0" ]; then
#    export GUILE_LOAD_PATH="$HOME/.config/guix/current/share/guile/site/3.0:$GUILE_LOAD_PATH"
#fi
#if [ -f "$HOME/.config/guix/current/lib/guile/3.0/site-ccache" ]; then
#    export GUILE_LOAD_COMPILED_PATH="$HOME/.config/guix/current/lib/guile/3.0/site-ccache/:$GUILE_LOAD_COMPILED_PATH"
#fi

GUIX_PROFILE="$HOME/.config/guix/current"
if [ -f "$GUIX_PROFILE/etc/profile" ]; then
    # shellcheck source=.config/guix/current/etc/profile
    source "$GUIX_PROFILE/etc/profile"
fi
if [ -d "$GUIX_PROFILE/share/info" ]; then
    export INFOPATH="$GUIX_PROFILE/share/info:$INFOPATH"
fi

if [ -d "$HOME/guixstuff/bin" ]; then
    export PATH="$HOME/guixstuff/bin:$PATH"
fi

if [ -d "$HOME/infopath" ]; then
	export INFOPATH="$HOME/infopath:$INFOPATH"
fi

if [ -d "$HOME/bin" ]; then
	export PATH="$HOME/bin:$PATH"
fi

export VISUAL=emacsclient
export EDITOR="$VISUAL"

# shellcheck source=.bashrc
source "$HOME/.bashrc"
