#!/bin/sh

version="The Caml Light to Objective Caml translator, version 2.0 (Nov 1st 1996)"

libdir=INSTALLDIR
clib="-clib LIBNAME"
includes="-I $libdir"
options=""
output="a.zlc"

while : ; do
  case $1 in
    "")
      break;;
    -bare)
      clib="";;
    -clib)
      clib="-clib $2"
      shift;;
    -I|-include)
      includes="$includes -I $2"
      shift;;
    -o)
      output=$2
      shift;;
    -O|-open)
      options="$options -O $2"
      shift;;
    -L|-verbose)
      options="$options -L";;
    -W|nowarning)
      options="$options -W";;
    -c)
      camlrun $libdir/caml2csl \
            $clib $includes $options -o $output -c $2 || exit 2
      shift;;
    -v)
      echo $version;;
    -*)
      echo "Unknown option \"$1\", ignored" >&2;;
    *)
      camlrun $libdir/caml2csl \
            $clib $includes $options $1 || exit 2 ;;
  esac
  shift
done

