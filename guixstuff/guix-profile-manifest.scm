(import
 (gnu packages autogen)
 (gnu packages autotools)
 (gnu packages bison)
 (gnu packages c)
 (gnu packages calendar)
 (gnu packages certs)
 (gnu packages check)
 (gnu packages chez)
 (gnu packages ci)
 (gnu packages code)
 (gnu packages curl)
 (gnu packages databases)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages entr)
 (gnu packages fonts)
 (gnu packages fontutils)
 (gnu packages games)
 (gnu packages gcc)
 (gnu packages ghostscript)
 (gnu packages gnu-doc)
 (gnu packages gnupg)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages haskell-xyz)
 (gnu packages libreoffice)
 (gnu packages mail)
 (gnu packages man)
 (gnu packages maths)
 (gnu packages mes)
 (gnu packages ncdu)
 (gnu packages pdf)
 (gnu packages python-check)
 (gnu packages python-xyz)
 (gnu packages scheme)
 (gnu packages shells)
 (gnu packages speech)
 (gnu packages sqlite)
 (gnu packages tcl)
 (gnu packages tex)
 (gnu packages texinfo)
 (gnu packages tls)
 (gnu packages tmux)
 (gnu packages toys)
 (gnu packages version-control)
 (gnu packages video)
 (gnu packages web-browsers)

 (kakafarm packages emacs-xyz)
 (kakafarm packages guile-xyz))

(packages->manifest
 (list
  ;; guile-websocket-next
  autogen
  automake
  bison
  bsd-games
  c-intro-and-ref
  cdecl
  chez-scheme
  curl
  emacs
  emacs-ansi
  emacs-arei
  emacs-benchmark-init
  emacs-buttercup
  emacs-corfu
  emacs-corfu-doc
  emacs-darkroom
  emacs-denote
  emacs-dictionary
  emacs-diff-hl
  emacs-direnv
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
  emacs-geiser-chez
  emacs-geiser-guile
  emacs-geiser-racket
  emacs-ggtags
  emacs-greader-mode
  emacs-gruvbox-theme
  emacs-guix
  emacs-helm
  emacs-helpful
  emacs-htmlize
  emacs-hy-mode
  emacs-hyperbole
  emacs-magit
  emacs-magit-todos
  emacs-markdown-mode
  emacs-mastodon
  emacs-multi-vterm
  emacs-multiple-cursors
  emacs-nano-tts-minor-mode
  emacs-nov-el
  emacs-olivetti
  emacs-orderless
  emacs-org-roam
  emacs-paredit
  emacs-parinfer-mode
  emacs-peg
  emacs-rainbow-blocks
  emacs-rainbow-delimiters
  emacs-rec-mode
  emacs-request
  emacs-synosaurus
  emacs-tco-el
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
  font-dejavu
  font-ghostscript
  font-gnu-freefont
  fontconfig
  fortunes-jkirchartz
  glibc
  glibc-locales
  gnu-c-manual
  gnu-make
  gnu-standards
  gnupg
  gnutls
  guile-3.0
  guile-ac-d-bus
  guile-clipboard-speaker
  guile-colorized
  guile-config
  guile-fibers
  guile-filesystem
  guile-git
  guile-gnutls
  guile-hall
  guile-hoot
  guile-json-4
  guile-lib
  guile-picture-language
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
  info-reader
  laminar
  le-certs
  libhdate
  links
  man-pages
  man-pages-posix
  mes
  mpv
  mumi
  mythes
  ncdu
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
  scsh
  sicp
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
  ))
