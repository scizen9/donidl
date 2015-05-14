#!/bin/sh

createdb asncat
psql asncat -c "create schema sn"
psql asncat -c "grant usage on schema sn to public"

psql asncat -f sncat.sql
