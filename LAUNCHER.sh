#!/usr/bin/env zsh
# shellcheck disable=all
# Lightweight wrapper to invoke the LISP-based launcher (LAUNCHER.lisp)
# Forwards all arguments to the LISP script.

exec sbcl --script ./main.lisp "$@"
