#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin $PWD/deps/*/deps/*/ebin -DNOTEST -config sys  -boot start_sasl -s inets -s ssl -s ebot
