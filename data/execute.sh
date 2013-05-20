#!/usr/bin/env bash

set -e

psql -a -w \
 -U $DATABASE_USER \
 -h $DATABASE_HOST \
 -p $DATABASE_PORT \
 -d $DATABASE \
 -c "$1"
