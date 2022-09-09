#! /usr/bin/env bash

toolPath=$(dirname $0)/../../../target/scala-2.13/jsrt-out

function indent {
  sed 's/^/  /'
}

if [ -e "${toolPath}" ] ; then
    for example in example{,2,3} ; do
        echo "testing: $example"
        example="$(dirname $0)/${example}"
        "${toolPath}" "--config=$example.conf" "$example.json"          | indent
        "${toolPath}" "$example.json.js"                                | indent
        if ! diff -w "$example.json" "$example.out.json" ; then
            echo "error: they are different"                            | indent
            exit 1
        else
            echo "success: they are the same (ignoring all whitespace)" | indent
        fi
    done
    exit 0
else
   >&2 echo "could not find tool at: ${toolPath}.  Did you build it?"
   exit 1
fi
