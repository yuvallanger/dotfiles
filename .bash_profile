export LANG=en_US.UTF-8
export LC_ALL=en_IL.UTF-8

export INFOPATH="/usr/share/info:$INFOPATH"

### Executables installed at home.

if [ -d ~/.local/bin ]; then export PATH="$HOME/.local/bin:$PATH"; fi

### Guix stuff.

if [ -d ~/.guix-profile/lib/locale ]; then export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"; fi

if [ -d ~/.guix-profile ]; then eval "$( guix package --search-paths=prefix -p ~/.guix-profile )"; fi

if [ -d ~/.config/guix/current ]; then eval "$( guix package --search-paths=prefix -p ~/.config/guix/current )"; fi

if [ -d "$HOME/.config/guix/current/share/info" ]; then export INFOPATH="$HOME/.config/guix/current/share/info:$INFOPATH"; fi

if [ -d "$HOME/guixstuff/bin" ]; then export PATH="$HOME/guixstuff/bin:$PATH"; fi

### Info path.

if [ -d "$HOME/infopath" ]; then export INFOPATH="$HOME/infopath:$INFOPATH"; fi

### Artisanal executables.

if [ -d "$HOME/bin" ]; then export PATH="$HOME/bin:$PATH"; fi

### Editory pagery stuff.

export VISUAL=emacsclient
export EDITOR="$VISUAL"
export PAGER="less"

### And we continue with bashrc for some reason.

# shellcheck source=.bashrc
source "$HOME/.bashrc"
