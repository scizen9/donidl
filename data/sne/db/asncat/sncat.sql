SET search_path = sn, public, pg_catalog;


CREATE TABLE sn
(
 	id      text PRIMARY KEY,
	name	text,
	host	text,
	hra	double precision,
	hdec	double precision,
	ra	double precision,  -- decimal ra J2000 (in degrees!!!)
	dec	double precision,
	gtype	text,
	gtc	real,
	ginc	real,
	gpa	real,
	gbt	real,
	d25	real,
	cz	real, -- recession velocity (kms)
        off_ew	real, -- offset EW, + for west 
        off_ns	real, -- offset NS, + for north
        snmag     real, -- stellar magnitude
	snfilt	text,
	mtyp	text,
	sntype	text,
	ddate	text,
	discoverer text, -- discoverer(s)
	mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE sn IS 'Asiago Supernova Catalog';
COMMENT ON COLUMN sn.host IS 'Host galaxy of SN';
COMMENT ON COLUMN sn.hra IS 'Host Right Ascension (2000.0)';
COMMENT ON COLUMN sn.hdec IS 'Host Declination (2000.0)';
COMMENT ON COLUMN sn.ra	IS 'SN Right Ascension (2000.0)';
COMMENT ON COLUMN sn.dec IS 'SN Declination (2000.0)';
COMMENT ON COLUMN sn.gtype IS 'Host galaxy morphological type';
COMMENT ON COLUMN sn.gtc IS 'Host galaxy morphological type code';
COMMENT ON COLUMN sn.ginc IS 'Host galaxy inclination: 0. -face on';
COMMENT ON COLUMN sn.gpa IS 'Host galaxy position angle';
COMMENT ON COLUMN sn.gbt IS 'Host galaxy integrated B mag';
COMMENT ON COLUMN sn.d25 IS 'Host galaxy diameter (arcsec)';
COMMENT ON COLUMN sn.cz IS 'Recession velocity of SN (km/s)';
COMMENT ON COLUMN sn.off_ew	IS 'E/W offset of SN in arcseconds from the nucleus of the host galaxy, with positive sign for eastern direction';
COMMENT ON COLUMN sn.off_ns	IS 'N/S offset of SN in arcseconds from the nucleus of the host galaxy, with positive sign for northern direction';
COMMENT ON COLUMN sn.snmag IS 'SN Magnitude';
COMMENT ON COLUMN sn.snfilt IS 'Filter of SN Magnitude';
COMMENT ON COLUMN sn.mtyp IS 'Type of SN mag: * -discovery, otherwise max';
COMMENT ON COLUMN sn.sntype IS 'Type of SN';
COMMENT ON COLUMN sn.ddate IS 'Date of discovery';
COMMENT ON COLUMN sn.discoverer IS 'Names of SN discoverers';
COMMENT ON COLUMN sn.mod_date IS 'Date of data modifications';
