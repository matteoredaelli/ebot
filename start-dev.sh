#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -DTEST -config log4e -s reloader -s inets -s ebot 
