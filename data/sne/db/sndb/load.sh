#!/bin/sh

for dump in band.dump gcats.dump sncats.dump optmaglim.dump qualification.dump \
	    sntypes.dump survey.dump ; do
	psql sndb -f $dump;
done

#psql sndb -f view.sql
psql sndb -f grant.sql
#psql sndb -f index.sql
