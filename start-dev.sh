#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -DTEST -boot start_sasl -s reloader -s inets -s couchbeam -s ebot
