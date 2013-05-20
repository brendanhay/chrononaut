#!/usr/bin/env bash

psql \
 -U $DATABASE_USER \
 -h $DATABASE_HOST \
 -p $DATABASE_PORT \
 -d $DATABASE \
 > /dev/null 2>&1
