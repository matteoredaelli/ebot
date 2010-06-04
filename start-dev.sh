#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -DTEST -config ebot_local -boot start_sasl -s reloader -s inets -s ebot 
