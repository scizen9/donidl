SET search_path = sn, public, pg_catalog;


 CREATE TABLE galaxies (
         id integer PRIMARY KEY,
         name text,
         ra     double precision,     -- alpha, delta (2000.0)
         dec    double precision,
         mag    real,                 -- galaxy magnitude
         bandid  integer,             -- band
         posang  real,                -- pos. angle
         z       double precision,    -- redshift 
         logab   real,   -- gaxial ratio (log a/b)
         log10d  real,   -- giso diam  (log 10d)
         type    text,   -- As in LEDA (examples Sa, Sb, E, S0+, I, SBa, SBbc)
         qidt    integer,-- type galaxy (S?, E?,...) uncertainty
         t       real,   --  Numerical code of morphological type as in LEDA
         e_t     real,    -- Error of numerical code of morphological type as in LEDA
         comment text,
         mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE galaxies IS 'Host galaxies';

COMMENT ON COLUMN galaxies.ra	IS 'Right Ascension (2000.0)';
COMMENT ON COLUMN galaxies.dec IS 'Declination (2000.0)';
COMMENT ON COLUMN galaxies.mag IS 'Magnitude';
COMMENT ON COLUMN galaxies.bandid IS 'The name of the standard band in which the magnitude was derived';
COMMENT ON COLUMN galaxies.posang IS 'Position angle in degrees measured from the North to the East';
COMMENT ON COLUMN galaxies.z IS 'Redshift';
COMMENT ON COLUMN galaxies.logab IS 'Decimal log of axial ratio (major axis divided by minor axis)';
COMMENT ON COLUMN galaxies.log10d IS 'Decimal log of apparent isophotal major diameter in units of 0.1 arcminute';
COMMENT ON COLUMN galaxies.type IS 'Morphological type';
COMMENT ON COLUMN galaxies.qidt IS 'Uncertainty in determination of SN type';
COMMENT ON COLUMN galaxies.t IS 'Numerical code of morphological type as in LEDA';
COMMENT ON COLUMN galaxies.e_t IS 'Error of numerical code of morphological type as in LEDA';
COMMENT ON COLUMN galaxies.comment IS 'Comments';
COMMENT ON COLUMN galaxies.mod_date IS 'Date of data modifications';



CREATE TABLE sn
(
 	id      text PRIMARY KEY,
	name	text,
	gid	integer REFERENCES galaxies(id),
	qsn	integer REFERENCES qualification(id), --  supernova uncertainty
	qgid    integer REFERENCES qualification(id), --  galaxy identification uncertainty
	type	integer REFERENCES sntypes(id),       -- SN type
	qtype	integer REFERENCES qualification(id), -- sn type uncertainty
	ra	double precision,  -- decimal ra J2000 (in degrees!!!)
	dec	double precision,
        ew	real, -- offset EW, + for west 
        ns	real, -- offset NS, + for north
        mag     real, -- stellar magnitude
	qmag	integer REFERENCES qualification(id), --  magnitude uncertainty 
	lid	integer REFERENCES optmaglim(id),
	bandid	integer	REFERENCES band(id),	 -- band
	optdisc	integer,	 --  photo(1)/ccd(2) discovery
	discdate	date, -- date of discovery
	maxdate         date, -- date of maximum in textual representation
        survey  integer[], -- surveys, discovered SN
	discoverer text, -- discoverer(s)
	comment text,
	mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE sn IS 'Supernovae';
COMMENT ON COLUMN sn.gid IS 'Identificator of the host galaxy in table galaxies';
COMMENT ON COLUMN sn.qsn IS 'Uncertainty of SN';
COMMENT ON COLUMN sn.qgid IS 'Uncertainty of host galaxy identification';
COMMENT ON COLUMN sn.type IS 'Type of SN';
COMMENT ON COLUMN sn.qtype IS 'Uncertainty in determination of SN type';
COMMENT ON COLUMN sn.ra	IS 'Right Ascension (2000.0)';
COMMENT ON COLUMN sn.dec IS 'Declination (2000.0)';
COMMENT ON COLUMN sn.ew	IS 'E/W offset of SN in arcseconds from the nucleus of the host galaxy, with positive sign for western direction';
COMMENT ON COLUMN sn.ns	IS 'N/S offset of SN in arcseconds from the nucleus of the host galaxy, with positive sign for northern direction';
COMMENT ON COLUMN sn.mag IS 'Magnitude';
COMMENT ON COLUMN sn.qmag IS 'Quality of magnitude';
COMMENT ON COLUMN sn.lid IS 'Description of magnitude: code 1 is for magnitude at maximum light, code 2 is for magnitude estimated at any phase of SN evolution';
COMMENT ON COLUMN sn.bandid IS 'The name of the standard band in which the magnitude was derived';
COMMENT ON COLUMN sn.optdisc IS 'Method of SN discovery: code 1 is for SNe discovered photographically, code 2 is for all other kinds of discoveries';
COMMENT ON COLUMN sn.discdate IS 'Date of discovery';
COMMENT ON COLUMN sn.maxdate  IS 'Date of maximum light';
COMMENT ON COLUMN sn.survey  IS 'Code for SN search programme or observatory where SN was discovered';
COMMENT ON COLUMN sn.discoverer IS 'Names of SN discoverers';
COMMENT ON COLUMN sn.comment IS 'Comments on SN';
COMMENT ON COLUMN sn.mod_date IS 'Date of data modifications';


CREATE TABLE map (
	gid integer REFERENCES galaxies(id),	-- galaxy id
	pid integer REFERENCES properties(id),  -- property id
	cid integer REFERENCES cats(id),	-- catalog id
	cidid integer,  			-- id in catalog cid
        mod_date timestamp DEFAULT now(), -- modification date, trigger ts_row
	UNIQUE(gid,pid)
);
COMMENT ON TABLE map IS 'Map';
COMMENT ON COLUMN map.gid IS 'Identificator of galaxy in table galaxies';
COMMENT ON COLUMN map.pid IS 'Identificator of property of galaxy';
COMMENT ON COLUMN map.cid IS 'Identificator of Catalog';
COMMENT ON COLUMN map.cidid IS 'Identificator of galaxy in Catalog';
COMMENT ON COLUMN map.mod_date IS 'Date of data modifications';