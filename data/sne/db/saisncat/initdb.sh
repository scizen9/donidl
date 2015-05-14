#!/bin/sh


createdb saisncat
psql saisncat -c "create schema sn"
psql saisncat -c "grant usage on schema sn to public"

psql saisncat -f references.sql
psql saisncat -f sncat.sql
