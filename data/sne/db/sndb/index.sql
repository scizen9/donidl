SET search_path = sn, public, pg_catalog;

CREATE INDEX sn_band_idx ON sn(bandid);
CREATE INDEX sn_id_idx ON sn(id);
CREATE INDEX sn_name_idx ON sn(name);
CREATE INDEX gal_band_idx ON galaxies(bandid);
CREATE INDEX gal_name_idx ON galaxies(name);
vacuum analyze;
