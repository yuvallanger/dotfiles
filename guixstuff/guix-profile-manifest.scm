(import
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
 (gnu packages lisp)
 (gnu packages mail)
 (gnu packages man)
 (gnu packages maths)
 (gnu packages matrix)
 (gnu packages mes)
 (gnu packages ncdu)
 (gnu packages networking)
 (gnu packages package-management)
 (gnu packages pdf)
 (gnu packages python-check)
 (gnu packages python-xyz)
 (gnu packages racket)
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

 (kakafarm packages emacs-xyz)
 (kakafarm packages guile-xyz)

 (guix)
 )

(define kakafarm-glibc-locales
  (make-glibc-utf8-locales
   glibc
   #:locales
   '("en_US" "en_GB" "en_IL" "he_IL")
   #:name
   "glibc-kakafarm-utf8-locales"))

(packages->manifest
 (list
  autoconf
  c-intro-and-ref
  curl
  darkhttpd
  emacs
  emacs-ansi
  emacs-benchmark-init
  emacs-buttercup
  emacs-casual-avy
  emacs-casual-calc
  emacs-casual-dired
  emacs-casual-info
  emacs-cider
  emacs-consult
  emacs-corfu
  emacs-corfu-doc
  emacs-darkroom
  emacs-denote
  emacs-dictionary
  emacs-diff-hl
  emacs-direnv
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
  emacs-geiser-racket
  emacs-ggtags
  emacs-gnosis
  emacs-go-mode
  emacs-greader
  emacs-gruvbox-theme
  emacs-guix
  emacs-helm
  emacs-helpful
  emacs-howm
  emacs-htmlize
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
  emacs-next
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
  emacs-racket-mode
  emacs-rainbow-blocks
  emacs-rainbow-delimiters
  emacs-request
  emacs-rust-mode
  emacs-scribble-mode
  emacs-sly
  emacs-subed
  emacs-synosaurus
  emacs-tco-el
  emacs-tempel
  emacs-transmission
  emacs-treemacs
  emacs-undo-tree
  emacs-use-package
  emacs-vertico
  emacs-vterm
  emacs-w3m
  emacs-wgrep
  emacs-whitespace-cleanup-mode
  emacs-wisp-mode
  emacs-writeroom
  emacs-yasnippet
  emacs-zotxt
  espeak-ng
  fzf
  fzf-tab
  glibc
  gnu-make
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
  guix
  haunt
  kakafarm-glibc-locales
  le-certs
  nss-certs
  r7rs-small-texinfo
  racket
  sicp
  skribilo
  slib
  zstd
  ))
