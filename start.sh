#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -DNOTEST -boot start_sasl -s inets -s ebot
