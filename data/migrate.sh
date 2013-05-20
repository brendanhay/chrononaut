#!/usr/bin/env bash

psql -a -w \
 -U $DATABASE_USER \
 -h $DATABASE_HOST \
 -p $DATABASE_PORT \
 -d $DATABASE \
 -f $1

