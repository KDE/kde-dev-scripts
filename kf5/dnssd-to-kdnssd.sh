#!/bin/sh

sed -i -e 's/\<DNSSD::/KDNSSD::/g' -e 's/\(namespace\s\+\)DNSSD\>/\1KDNSSD/g' \{\} \; "$@"
