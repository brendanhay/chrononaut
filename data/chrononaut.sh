#!/usr/bin/env bash
set -e

command="$1"; shift 1
argument="$@"

exec_string() {
    psql -q -A -w -t \
        -U $DATABASE_USER \
        -h $DATABASE_HOST \
        -p $DATABASE_PORT \
        -d $DATABASE \
        -c "$argument"
}

exec_file() {
    psql -a -w \
        -U $DATABASE_USER \
        -h $DATABASE_HOST \
        -p $DATABASE_PORT \
        -d $DATABASE \
        -f "$argument"
}

case "$command" in
    "migrate" | "rollback" )
        exec_file
        ;;
    "query" )
        exec_string
        ;;
    "" | * )
        echo "Unknown command: $command"
        echo "Usage: $(basename $0) <command> [<args>]"
        exit 1
        ;;
esac
