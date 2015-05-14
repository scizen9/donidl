SET search_path = sn, public, pg_catalog;

CREATE INDEX sn_band_idx ON sn(bandid);
CREATE INDEX sn_id_idx ON sn(id);
CREATE INDEX sn_name_idx ON sn(name);
CREATE INDEX sn_new_bandid_idx ON sn(bandid);
CREATE INDEX sn_new_gid_idx ON sn(gid);
CREATE INDEX sn_new_lid_idx ON sn(lid);
CREATE INDEX gal_band_idx ON galaxies(bandid);
CREATE INDEX sn_map_cid_idx ON map(cid);
vacuum analyze;