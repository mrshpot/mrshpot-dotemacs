#!/bin/sh

cd `dirname $0` || exit 1
rm -rf pylint-deps/
mkdir -p pylint-deps/logilab || exit 1
echo '"""This is an autogenerated dummy file"""' > pylint-deps/logilab/__init__.py || exit 1
cp -r logilab-common-*/build/lib/logilab/common/ pylint-deps/logilab/ || exit 1
cp -r logilab-astng-*/build/lib/logilab/astng/ pylint-deps/logilab/ || exit 1
