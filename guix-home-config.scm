(use-modules
 (ice-9 textual-ports)

 (gnu home services dotfiles)
 (gnu home services shells)
 (gnu home)

 (gnu packages autogen)
 (gnu packages autotools)
 (gnu packages bison)
 (gnu packages books)
 (gnu packages c)
 (gnu packages calendar)
 (gnu packages certs)
 (gnu packages check)
 (gnu packages chez)
 (gnu packages ci)
 (gnu packages code)
 (gnu packages compression)
 (gnu packages curl)
 (gnu packages databases)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages entr)
 (gnu packages fonts)
 (gnu packages fontutils)
 (gnu packages fsf)
 (gnu packages games)
 (gnu packages gcc)
 (gnu packages ghostscript)
 (gnu packages gnome-xyz)
 (gnu packages gnu-doc)
 (gnu packages gnupg)
 (gnu packages groff)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages haskell-xyz)
 (gnu packages irc)
 (gnu packages libreoffice)
 (gnu packages librewolf)
 (gnu packages lisp)
 (gnu packages mail)
 (gnu packages man)
 (gnu packages maths)
 (gnu packages matrix)
 (gnu packages mes)
 (gnu packages ncdu)
 (gnu packages networking)
 (gnu packages pdf)
 (gnu packages python-check)
 (gnu packages python-xyz)
 (gnu packages scheme)
 (gnu packages shells)
 (gnu packages shellutils)
 (gnu packages skribilo)
 (gnu packages speech)
 (gnu packages sqlite)
 (gnu packages tcl)
 (gnu packages terminals)
 (gnu packages tex)
 (gnu packages texinfo)
 (gnu packages tls)
 (gnu packages tmux)
 (gnu packages toys)
 (gnu packages version-control)
 (gnu packages video)
 (gnu packages web)
 (gnu packages web-browsers)
 (gnu)

 (guix profiles)
 (guix)

 (kakafarm packages emacs-xyz)
 (kakafarm packages guile-xyz)
 )

(home-environment
 (packages
  (list
   ;; book-faif
   ;; book-sparc
   ;; chez-scheme
   ;; emacs-geiser-chez
   ;; fontconfig
   ;; fortunes-jkirchartz
   ;; guile-picture-language
   ;; guile-websocket-next
   ;; scheme48-prescheme
   ;; scsh
   autogen
   automake
   bibata-cursor-theme
   bison
   bsd-games
   c-intro-and-ref
   cdecl
   clisp
   cowsay
   curl
   darkhttpd
   emacs
   emacs-ansi
   emacs-arei
   emacs-benchmark-init
   emacs-buttercup
   emacs-casual-avy
   emacs-casual-calc
   emacs-casual-dired
   emacs-casual-info
   emacs-cider
   emacs-corfu
   emacs-corfu-doc
   emacs-darkroom
   emacs-denote
   emacs-dictionary
   emacs-diff-hl
   emacs-direnv
   emacs-djvu
   emacs-eat
   emacs-ef-themes
   emacs-eglot
   emacs-elfeed
   emacs-elfeed-goodies
   emacs-elfeed-org
   emacs-elfeed-protocol
   emacs-elfeed-score
   emacs-elpher
   emacs-ement
   emacs-eval-in-repl-racket
   emacs-flycheck
   emacs-flycheck-haskell
   emacs-flycheck-package
   emacs-fzf
   emacs-geiser-guile
   emacs-geiser-racket
   emacs-ggtags
   emacs-gnosis
   emacs-greader-mode
   emacs-gruvbox-theme
   emacs-guix
   emacs-helm
   emacs-helpful
   emacs-howm
   emacs-htmlize
   emacs-hy-mode
   emacs-hyperbole
   emacs-hyperbole
   emacs-iedit
   emacs-jabber
   emacs-magit
   emacs-magit-todos
   emacs-markdown-mode
   emacs-mastodon
   emacs-multi-vterm
   emacs-multiple-cursors
   emacs-nano-tts-minor-mode
   emacs-nginx-mode
   emacs-nov-el
   emacs-olivetti
   emacs-orderless
   emacs-org-roam
   emacs-package-build
   emacs-package-lint
   emacs-paredit
   emacs-parinfer-mode
   emacs-peg
   emacs-perspective
   emacs-rainbow-blocks
   emacs-rainbow-delimiters
   emacs-rec-mode
   emacs-request
   emacs-rust-mode
   emacs-sly
   emacs-synosaurus
   emacs-tco-el
   emacs-transmission
   emacs-treemacs
   emacs-undo-tree
   emacs-use-package
   emacs-vterm
   emacs-w3m
   emacs-wgrep
   emacs-whitespace-cleanup-mode
   emacs-wisp-mode
   emacs-writeroom
   emacs-yasnippet
   emacs-zotxt
   entr
   espeak-ng
   festival
   flite
   font-dejavu
   font-ghostscript
   font-gnu-freefont
   fzf
   fzf-tab
   git
   git-lfs
   glibc
   glibc-locales
   gnu-c-manual
   gnu-make
   gnu-standards
   gnupg
   gnutls
   groff
   guile-3.0
   guile-ac-d-bus
   guile-clipboard-speaker
   guile-colorized
   guile-config
   guile-fibers
   guile-filesystem
   guile-git
   guile-gnutls
   guile-goblins
   guile-hall
   guile-hoot
   guile-json-4
   guile-lib
   guile-pipe
   guile-png
   guile-reader
   guile-readline
   guile-simple-zmq
   guile-srfi-128
   guile-srfi-133
   guile-srfi-145
   guile-srfi-146
   guile-srfi-158
   guile-srfi-159
   guile-srfi-180
   guile-srfi-189
   guile-srfi-197
   guile-srfi-232
   guile-srfi-235
   guile-srfi-253
   guile-srfi-89
   guile-websocket
   guile-wisp
   guile-zlib
   guilescript
   harmonist
   haunt
   hebcal
   hello
   hut
   ii
   info-reader
   laminar
   le-certs
   libhdate
   librewolf
   links
   man-pages
   man-pages-posix
   mes
   mpv
   mumi
   mythes
   ncdu
   nethack
   nss-certs
   pandoc
   poppler
   python-flake8
   python-hissp
   python-pep8
   python-pycodestyle
   python-pyflakes
   python-pylama
   python-pylint
   r7rs-small-texinfo
   recutils
   sbcl
   scsh
   sicp
   skribilo
   slib
   socat
   speech-dispatcher
   sqlite
   tcl
   texinfo
   texlive-amsmath
   texlive-ebproof
   tig
   tk
   tmux
   units
   universal-ctags
   zstd
   ))

 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (environment-variables
              '(
                #;
                ("PS1" . "\\[\\e[1;32m\\]\\u \\[\\e[1;34m\\]\\w \\[\\e[0m\\]λ ")
                ("EDITOR" . "emacsclient")
                #;
                ("LC_ALL" . "en_US.UTF-8")
                ))
             (aliases
              '(
                ("gs" . "git status")
                ("wgi" . "wget -m -np -i-")
                ))
             (bash-profile (list (local-file "regular-non-guix-home-files/.bash_profile" "bash_profile")))
             (bashrc (list (local-file "regular-non-guix-home-files/.bashrc" "bashrc")))
             ))

   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (directories '("guix-home-files"))))
   )))
