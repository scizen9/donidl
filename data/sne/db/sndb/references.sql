SET search_path = sn, public, pg_catalog;

CREATE TABLE sntypes
(
	id integer PRIMARY KEY,
        name text,
        mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE sntypes IS 'SN types';


CREATE TABLE survey
(
	id integer PRIMARY KEY,
	name text,
	info text,
	mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE survey IS 'SN surveys';


CREATE TABLE band
(
        id integer PRIMARY KEY,
        name text,
	info text,
	mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE band IS 'Magnitudes bands';

CREATE TABLE qualification
(
	id integer PRIMARY KEY,
	name text,
	mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE qualification IS 'Data uncertaincy/pecularity';

CREATE TABLE optmaglim
(
        id integer PRIMARY KEY,
        name text,	-- equality, upper,lower limits
	mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE optmaglim IS 'Stellar magnitudes limits';

CREATE TABLE gcats (
        id integer PRIMARY KEY,
        name text,
        mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE gcats IS 'Galaxy Sources ( database names ) ';

CREATE TABLE sncats (
        id integer PRIMARY KEY,
        name text,
        mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE gcats IS 'SN Sources ( database names ) ';
