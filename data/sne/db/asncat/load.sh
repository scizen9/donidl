#!/bin/sh

psql asncat -f sn.dump

psql asncat -f grant.sql
psql asncat -f index.sql
