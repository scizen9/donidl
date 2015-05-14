#!/bin/sh

for dump in band.dump cats.dump optmaglim.dump properties.dump qualification.dump \
	    sntypes.dump survey.dump \
            galaxies.dump sn.dump map.dump ; do
	psql saisncat -f $dump;
done

psql saisncat -f view.sql
psql saisncat -f grant.sql
psql saisncat -f index.sql
