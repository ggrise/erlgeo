#!/bin/sh
exec /opt/local/bin/erl -pa ebin deps/*/ebin -boot start_sasl -sname geo_dev -s erlgeo
