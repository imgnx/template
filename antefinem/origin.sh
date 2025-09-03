#!/bin/sh
basedir=$(dirname "$(echo "$0" | sed -e 's,\\,/,g')")

case `uname` in
    *CYGWIN*) basedir=`cygpath -w "$basedir"`;;
esac

export PATH="/Users/donaldmoore/.bvm/nodejs/22.14.0/bin:$PATH"
"/Users/donaldmoore/.bvm/nodejs/22.14.0/bin/node"  "$basedir/../.bvm/links/bit/node_modules/@teambit/bit/bin/bit" "$@"
exit $?
