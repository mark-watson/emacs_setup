# My Emacs setup with Straight.el

Do the following:

- move aside ~/.emacs and ~/.emacs.d
- cp ./.emacs ~/

The first time you open Emacs, git repos of all used packages will appear in ~/.emacs.d/ and there is not reason to edit that or back it up ever.

This is specific to my setup (Common Lisp, etc.)

## Aspell and Ispell:

    brew install aspell

When in Ispell mode, put cursor on miss-spelled word and:

    M-x ispell-word

and hit "i" character to install a word into the personal Aspell dictionary:

    ~/.aspell.en.pws

