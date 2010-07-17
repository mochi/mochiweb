#!/bin/sh
cd `dirname $0`

MAKE=make
case `uname` in
*BSD)
	MAKE=gmake
	;;
esac

"${MAKE}"
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s skel
