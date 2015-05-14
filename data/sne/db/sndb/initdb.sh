#!/bin/sh

createdb sndb
psql sndb -c "create schema sn"
psql sndb -c "grant usage on schema sn to public"

psql sndb -f references.sql
psql sndb -f sncat.sql
