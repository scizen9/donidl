SET search_path = sn, public, pg_catalog;

CREATE INDEX sn_id_idx ON sn(id);
CREATE INDEX sn_name_idx ON sn(name);
vacuum analyze;
