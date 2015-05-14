#!/bin/sh

psql -A -t saisncat -c "select 'Total number of supernovae in sn_cat: '||count(*)  from sn.sn_cat" 
