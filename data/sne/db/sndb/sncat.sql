SET search_path = sn, public, pg_catalog;


CREATE TABLE galex_obs (
	id integer PRIMARY KEY,
	tile		text,	-- GALEX tile name
	imname		text,	-- GALEX image name
	ra	double precision,	-- J2000 coords of center
	dec	double precision,
	path		text,	-- path to data in pipeline filesystem
	lpath		text,	-- path to subimage in local filesystem
	survey		text,	-- GALEX survey
	eclipse		integer,	-- eclipse number
	subvis		integer,	-- sub-visit number
	band		integer,	-- GALEX band: 0 - FUV, 1 - NUV
	exptime		real,		-- Exposure time in seconds
	source		integer,	-- 0 -visit, 1 -main, 2 -AIS 2ndary, 3 - custom
	pub		integer,	-- GI data public? 0 - no, 1 - yes
	nimage		integer, 	-- Number of images coadded
	mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE galex_obs IS 'GALEX observations of SNe and their hosts';
COMMENT ON COLUMN galex_obs.tile IS 'GALEX tile name';
COMMENT ON COLUMN galex_obs.imname IS 'GALEX image name';
COMMENT ON COLUMN galex_obs.ra IS 'J2000 RA of image center';
COMMENT ON COLUMN galex_obs.dec	IS 'J2000 Dec of image center';
COMMENT ON COLUMN galex_obs.path IS 'path to data in pipeline filesystem';
COMMENT ON COLUMN galex_obs.lpath IS 'path to subimage in local filesystem';
COMMENT ON COLUMN galex_obs.survey IS 'GALEX survey';
COMMENT ON COLUMN galex_obs.eclipse IS 'Eclipse number';
COMMENT ON COLUMN galex_obs.subvis IS 'Sub-visit number';
COMMENT ON COLUMN galex_obs.band IS 'GALEX band: 0 - FUV, 1 - NUV';
COMMENT ON COLUMN galex_obs.exptime IS 'Exposure time in seconds';
COMMENT ON COLUMN galex_obs.source IS '0 - visit, 1 - main, 2 - AIS 2ndary, 3 - custom';
COMMENT ON COLUMN galex_obs.pub	 IS 'GI data public? 0 - no, 1 - yes';
COMMENT ON COLUMN galex_obs.nimage IS 'Number of images coadded';
COMMENT ON COLUMN galex_obs.mod_date IS 'Date of data modifications';


CREATE TABLE sitemeas (
         id integer PRIMARY KEY,
         snid			text,	-- YYYY.id designation for SN measured
	 snname			text,	-- SN name
	 gxid			integer,-- index into table galex_obs
	 fexptime		real,	-- GALEX FUV exposure time
	 nexptime		real,	-- GALEX NUV exposure time
	 ap_res			real,	-- radius of GALEX resolution aperture
	 ap_500pc		real,	-- radius of 500pc aperture in arcsec
	 ap_1kpc		real,	-- radius of 1kpc aperture in arcsec
	 ap_2kpc		real,	-- radius of 2kpc aperture in arcsec
	 r_iso			real,	-- Isophotal radius of SN in host
	 r_equiv		real,	-- Equivalent radius of SN in host
	 fuv_frac		real,	-- Fraction of FUV host light inside SN
	 nuv_frac		real,	-- Fraction of NUV host light inside SN
	 sdss_u_frac		real,	-- Fraction of SDSS u light inside SN
	 sdss_g_frac		real,	-- Fraction of SDSS g light inside SN
	 sdss_r_frac		real,	-- Fraction of SDSS r light inside SN
	 sdss_i_frac		real,	-- Fraction of SDSS i light inside SN
	 sdss_z_frac		real,	-- Fraction of SDSS z light inside SN
	 J_frac			real,	-- Fraction of J host light inside SN
	 H_frac			real,	-- Fraction of H host light inside SN
	 K_frac			real,	-- Fraction of K host light inside SN
	 fuv_res_mag		real,	-- GALEX UV site res ap mags
	 fuv_res_magerr		real,
	 nuv_res_mag		real,
	 nuv_res_magerr		real,
	 sdss_u_res_mag		real,	-- SDSS ugriz res ap mags
	 sdss_u_res_magerr	real,
	 sdss_g_res_mag		real,
	 sdss_g_res_magerr	real,
	 sdss_r_res_mag		real,
	 sdss_r_res_magerr	real,
	 sdss_i_res_mag		real,
	 sdss_i_res_magerr	real,
	 sdss_z_res_mag		real,
	 sdss_z_res_magerr	real,
	 J_res_mag		real,	-- 2MASS JHK res ap mags
	 J_res_magerr		real,
	 H_res_mag		real,
	 H_res_magerr		real,
	 K_res_mag		real,
	 K_res_magerr		real,
	 h12m_res_mag		real,	-- IRAS 12 to 100 micron res ap mags
	 h12m_res_magerr	real,
	 h25m_res_mag		real,
	 h25m_res_magerr	real,
	 h60m_res_mag		real,
	 h60m_res_magerr	real,
	 h100m_res_mag		real,
	 h100m_res_magerr	real,
	 fuv_500pc_mag		real,	-- GALEX UV site 500pc ap mags
	 fuv_500pc_magerr	real,
	 nuv_500pc_mag		real,
	 nuv_500pc_magerr	real,
	 sdss_u_500pc_mag	real,	-- SDSS ugriz 500pc ap mags
	 sdss_u_500pc_magerr	real,
	 sdss_g_500pc_mag	real,
	 sdss_g_500pc_magerr	real,
	 sdss_r_500pc_mag	real,
	 sdss_r_500pc_magerr	real,
	 sdss_i_500pc_mag	real,
	 sdss_i_500pc_magerr	real,
	 sdss_z_500pc_mag	real,
	 sdss_z_500pc_magerr	real,
	 J_500pc_mag		real,	-- 2MASS JHK 500pc ap mags
	 J_500pc_magerr		real,
	 H_500pc_mag		real,
	 H_500pc_magerr		real,
	 K_500pc_mag		real,
	 K_500pc_magerr		real,
	 h12m_500pc_mag		real,	-- IRAS 12 to 100 micron 500pc ap mags
	 h12m_500pc_magerr	real,
	 h25m_500pc_mag		real,
	 h25m_500pc_magerr	real,
	 h60m_500pc_mag		real,
	 h60m_500pc_magerr	real,
	 h100m_500pc_mag	real,
	 h100m_500pc_magerr	real,
	 fuv_1kpc_mag		real,	-- GALEX UV site 1kpc ap mags
	 fuv_1kpc_magerr	real,
	 nuv_1kpc_mag		real,
	 nuv_1kpc_magerr	real,
	 sdss_u_1kpc_mag	real,	-- SDSS ugriz 1kpc ap mags
	 sdss_u_1kpc_magerr	real,
	 sdss_g_1kpc_mag	real,
	 sdss_g_1kpc_magerr	real,
	 sdss_r_1kpc_mag	real,
	 sdss_r_1kpc_magerr	real,
	 sdss_i_1kpc_mag	real,
	 sdss_i_1kpc_magerr	real,
	 sdss_z_1kpc_mag	real,
	 sdss_z_1kpc_magerr	real,
	 J_1kpc_mag		real,	-- 2MASS JHK 1kpc ap mags
	 J_1kpc_magerr		real,
	 H_1kpc_mag		real,
	 H_1kpc_magerr		real,
	 K_1kpc_mag		real,
	 K_1kpc_magerr		real,
	 h12m_1kpc_mag		real,	-- IRAS 12 to 100 micron 1kpc ap mags
	 h12m_1kpc_magerr	real,
	 h25m_1kpc_mag		real,
	 h25m_1kpc_magerr	real,
	 h60m_1kpc_mag		real,
	 h60m_1kpc_magerr	real,
	 h100m_1kpc_mag		real,
	 h100m_1kpc_magerr	real,
	 fuv_2kpc_mag		real,	-- GALEX UV site 2kpc ap mags
	 fuv_2kpc_magerr	real,
	 nuv_2kpc_mag		real,
	 nuv_2kpc_magerr	real,
	 sdss_u_2kpc_mag	real,	-- SDSS ugriz 2kpc ap mags
	 sdss_u_2kpc_magerr	real,
	 sdss_g_2kpc_mag	real,
	 sdss_g_2kpc_magerr	real,
	 sdss_r_2kpc_mag	real,
	 sdss_r_2kpc_magerr	real,
	 sdss_i_2kpc_mag	real,
	 sdss_i_2kpc_magerr	real,
	 sdss_z_2kpc_mag	real,
	 sdss_z_2kpc_magerr	real,
	 J_2kpc_mag		real,	-- 2MASS JHK 2kpc ap mags
	 J_2kpc_magerr		real,
	 H_2kpc_mag		real,
	 H_2kpc_magerr		real,
	 K_2kpc_mag		real,
	 K_2kpc_magerr		real,
	 h12m_2kpc_mag		real,	-- IRAS 12 to 100 micron 2kpc ap mags
	 h12m_2kpc_magerr	real,
	 h25m_2kpc_mag		real,
	 h25m_2kpc_magerr	real,
	 h60m_2kpc_mag		real,
	 h60m_2kpc_magerr	real,
	 h100m_2kpc_mag		real,
	 h100m_2kpc_magerr	real,
         comment text,
         mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE sitemeas IS 'SN site measurements';
COMMENT ON COLUMN sitemeas.snid IS 'SN id: YYYY.id';
COMMENT ON COLUMN sitemeas.snname IS 'SN name';
COMMENT ON COLUMN sitemeas.gxid	IS 'index into table galex_obs';
COMMENT ON COLUMN sitemeas.fexptime IS 'GALEX FUV exposure time';
COMMENT ON COLUMN sitemeas.nexptime IS 'GALEX NUV exposure time';
COMMENT ON COLUMN sitemeas.ap_res IS 'radius of GALEX resolution aperture';
COMMENT ON COLUMN sitemeas.ap_500pc IS 'radius of 500pc aperture in arcsec';
COMMENT ON COLUMN sitemeas.ap_1kpc IS 'radius of 1kpc aperture in arcsec';
COMMENT ON COLUMN sitemeas.ap_2kpc IS 'radius of 2kpc aperture in arcsec';
COMMENT ON COLUMN sitemeas.r_iso IS 'Isophotal radius of SN in host';
COMMENT ON COLUMN sitemeas.r_equiv IS 'Equivalent radius of SN in host';
COMMENT ON COLUMN sitemeas.fuv_frac IS 'Fraction of FUV host light inside SN';
COMMENT ON COLUMN sitemeas.nuv_frac IS 'Fraction of NUV host light inside SN';
COMMENT ON COLUMN sitemeas.sdss_u_frac IS 'Fraction of SDSS u light inside SN';
COMMENT ON COLUMN sitemeas.sdss_g_frac IS 'Fraction of SDSS g light inside SN';
COMMENT ON COLUMN sitemeas.sdss_r_frac IS 'Fraction of SDSS r light inside SN';
COMMENT ON COLUMN sitemeas.sdss_i_frac IS 'Fraction of SDSS i light inside SN';
COMMENT ON COLUMN sitemeas.sdss_z_frac IS 'Fraction of SDSS z light inside SN';
COMMENT ON COLUMN sitemeas.J_frac IS 'Fraction of J host light inside SN';
COMMENT ON COLUMN sitemeas.H_frac IS 'Fraction of H host light inside SN';
COMMENT ON COLUMN sitemeas.K_frac IS 'Fraction of K host light inside SN';
COMMENT ON COLUMN sitemeas.fuv_res_mag IS 'GALEX FUV site res ap mag';
COMMENT ON COLUMN sitemeas.fuv_res_magerr IS 'Error on GALEX FUV site res ap mag';
COMMENT ON COLUMN sitemeas.nuv_res_mag IS 'GALEX NUV site res ap mag';
COMMENT ON COLUMN sitemeas.nuv_res_magerr IS 'Error on GALEX NUV site res ap mag';
COMMENT ON COLUMN sitemeas.mod_date IS 'Date of data modifications';


CREATE TABLE hostmeas (
         id integer PRIMARY KEY,
         name text,
	 gxid		integer,-- index into table of GALEX observations
	 fexptime	real,	-- GALEX FUV exposure time
	 nexptime	real,	-- GALEX NUV exposure time
	 fuv_mag	real,	-- GALEX UV integrated mags
	 fuv_magerr	real,
	 fuv_gcid	integer REFERENCES gcats(id),
	 nuv_mag	real,
	 nuv_magerr	real,
	 nuv_gcid	integer REFERENCES gcats(id),
	 UJ_mag		real,	-- Johnson UBV integrated mags
	 UJ_magerr	real,
	 UJ_gcid	integer REFERENCES gcats(id),
	 BJ_mag		real,
	 BJ_magerr	real,
	 BJ_gcid	integer REFERENCES gcats(id),
	 VJ_mag		real,
	 VJ_magerr	real,
	 VJ_gcid	integer REFERENCES gcats(id),
	 sdss_u_mag	real,	-- SDSS ugriz integrated mags
	 sdss_u_magerr	real,
	 sdss_u_gcid	integer REFERENCES gcats(id),
	 sdss_g_mag	real,
	 sdss_g_magerr	real,
	 sdss_g_gcid	integer REFERENCES gcats(id),
	 sdss_r_mag	real,
	 sdss_r_magerr	real,
	 sdss_r_gcid	integer REFERENCES gcats(id),
	 sdss_i_mag	real,
	 sdss_i_magerr	real,
	 sdss_i_gcid	integer REFERENCES gcats(id),
	 sdss_z_mag	real,
	 sdss_z_magerr	real,
	 sdss_z_gcid	integer REFERENCES gcats(id),
	 J_mag		real,	-- 2MASS JHK integrated mags
	 J_magerr	real,
	 J_gcid		integer REFERENCES gcats(id),
	 H_mag		real,
	 H_magerr	real,
	 H_gcid		integer REFERENCES gcats(id),
	 K_mag		real,
	 K_magerr	real,
	 K_gcid		integer REFERENCES gcats(id),
	 h12m_mag	real,	-- IRAS 12 to 100 micron integrated mags
	 h12m_magerr	real,
	 h12m_gcid	integer REFERENCES gcats(id),
	 h25m_mag	real,
	 h25m_magerr	real,
	 h25m_gcid	integer REFERENCES gcats(id),
	 h60m_mag	real,
	 h60m_magerr	real,
	 h60m_gcid	integer REFERENCES gcats(id),
	 h100m_mag	real,
	 h100m_magerr	real,
	 h100m_gcid	integer REFERENCES gcats(id),
	 OAbd		real,	-- Oxygen Abundance from SDSS
         comment text,
         mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE hostmeas IS 'SN Host measurements';
COMMENT ON COLUMN hostmeas.name IS 'SN Host name';
COMMENT ON COLUMN hostmeas.gxid	IS 'Index into table galex_obs';
COMMENT ON COLUMN hostmeas.fexptime IS 'GALEX FUV exposure time';
COMMENT ON COLUMN hostmeas.nexptime IS 'GALEX NUV exposure time';
COMMENT ON COLUMN hostmeas.fuv_mag IS 'GALEX FUV integrated mag';
COMMENT ON COLUMN hostmeas.fuv_magerr IS 'Error on GALEX FUV integrated mag';
COMMENT ON COLUMN hostmeas.fuv_gcid IS 'Index into table gcats (source)';
COMMENT ON COLUMN hostmeas.nuv_mag IS 'GALEX NUV integrated mag';
COMMENT ON COLUMN hostmeas.nuv_magerr IS 'Error on GALEX NUV integrated mag';
COMMENT ON COLUMN hostmeas.nuv_gcid IS 'Index into table gcats (source)';
COMMENT ON COLUMN hostmeas.OAbd IS 'Oxygen Abundance from Prieto et al. (2008)';
COMMENT ON COLUMN hostmeas.mod_date IS 'Date of data modifications';


CREATE TABLE galaxies (
         id integer PRIMARY KEY,
         name	text,
         ra	double precision,     -- alpha, delta (2000.0)
         dec	double precision,
	 scid	integer REFERENCES gcats(id), -- catalog source of position
         gbt	real,    -- galaxy magnitude
         z	double precision,    -- redshift 
	 d25	real,   -- Diameter in arcsec
	 dmod	real,   -- Distance modulus
	 linscl	real,   -- Linear scale at galaxy (pc / arcsec)
	 mwebmv	real,   -- Milky Way E(B-V)
	 inc	real,   -- Inclination: 0 is face on
         pa	real,   -- pos. angle (degrees E of N)
         logab	real,   -- gaxial ratio (log a/b)
         log10d	real,   -- giso diam  (log 10d)
         type	text,   -- As in LEDA (examples Sa, Sb, E, S0+, I, SBa, SBbc)
         qidt	integer,-- type galaxy (S?, E?,...) uncertainty
         tyn	real,   --  Numerical code of morphological type as in LEDA
         e_t	real,    -- Error of numerical code of morphological type as in LEDA
         comment text,
	 hid	integer REFERENCES hostmeas(id), -- host measures index
         mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE galaxies IS 'Host galaxies';
COMMENT ON COLUMN galaxies.ra	IS 'Right Ascension (2000.0)';
COMMENT ON COLUMN galaxies.dec IS 'Declination (2000.0)';
COMMENT ON COLUMN galaxies.scid IS 'Identificator of the source catalog for position';
COMMENT ON COLUMN galaxies.gbt IS 'Host galaxy integrated B mag';
COMMENT ON COLUMN galaxies.z IS 'Redshift';
COMMENT ON COLUMN galaxies.d25 IS 'Diameter of galaxy in arcseconds';
COMMENT ON COLUMN galaxies.dmod IS 'Distance modulus';
COMMENT ON COLUMN galaxies.linscl IS 'Linear scale at galaxy (pc / arcsec)';
COMMENT ON COLUMN galaxies.mwebmv IS 'Milky Way E(B-V)';
COMMENT ON COLUMN galaxies.inc IS 'Host inclination 0 - face on';
COMMENT ON COLUMN galaxies.pa IS 'Position angle in degrees measured from the North to the East';
COMMENT ON COLUMN galaxies.logab IS 'Decimal log of axial ratio (major axis divided by minor axis)';
COMMENT ON COLUMN galaxies.log10d IS 'Decimal log of apparent isophotal major diameter in units of 0.1 arcminute';
COMMENT ON COLUMN galaxies.type IS 'Morphological type';
COMMENT ON COLUMN galaxies.qidt IS 'Uncertainty in determination of SN type';
COMMENT ON COLUMN galaxies.tyn IS 'Numerical code of morphological type as in LEDA';
COMMENT ON COLUMN galaxies.e_t IS 'Error of numerical code of morphological type as in LEDA';
COMMENT ON COLUMN galaxies.comment IS 'Comments';
COMMENT ON COLUMN galaxies.hid IS 'Identificator of host measures in table hostmeas';
COMMENT ON COLUMN galaxies.mod_date IS 'Date of data modifications';


CREATE TABLE lcfit (
	id integer PRIMARY KEY,
	snid		text,	-- SN id: YYYY.id
	snname		text,	-- SN name
	smpl_str	real,	-- Simplfit Stretch
	smpl_strerr	real,
	smpl_clr	real,	-- color
	smpl_clrerr	real,
	smpl_zcmb	real,	-- zCMB
	smpl_zcmberr	real,
	smpl_bmag	real,	-- Bmag
	smpl_bmagerr	real,
	smpl_ucos	integer,-- Use for cosmo 0 - no, 1 - yes
	smpl_ustr	integer,-- Use for stretch 0 - no, 1 - yes
	mlcs2k_delta	real,	-- MLCS2K Delta (JRK07)
	mlcs2k_deltaerr	real,
	mlcs2k_a0v	real,	-- A0(V)
	mlcs2k_a0verr	real,
	mlcs2k_m0v	real,	-- M0(V)
	mlcs2k_m0verr	real,
	mlcs2k_mub	real,	-- mu(B)
	mlcs2k_muberr	real,
	mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE lcfit IS 'SN light curve fit parameters';
COMMENT ON COLUMN lcfit.snid IS 'Identificator in table sn';
COMMENT ON COLUMN lcfit.snname IS 'SN name';
COMMENT ON COLUMN lcfit.smpl_str IS 'SiFTO fitted stretch';
COMMENT ON COLUMN lcfit.smpl_strerr IS 'Error in SiFTO fitted stretch';
COMMENT ON COLUMN lcfit.smpl_clr IS 'SiFTO fitted color';
COMMENT ON COLUMN lcfit.smpl_clrerr IS 'Error in SiFTO fitted color';
COMMENT ON COLUMN lcfit.smpl_zcmb IS 'SiFTO fitted CMB redshift';
COMMENT ON COLUMN lcfit.smpl_zcmberr IS 'Error in SiFTO fitted CMB redshift';
COMMENT ON COLUMN lcfit.smpl_bmag IS 'SiFTO fitted B magnitude';
COMMENT ON COLUMN lcfit.smpl_bmagerr IS 'Error in SiFTO fitted B magnitude';
COMMENT ON COLUMN lcfit.smpl_ucos IS 'Use in cosmo fits flag: 0 - no, 1 - yes';
COMMENT ON COLUMN lcfit.smpl_ustr IS 'Use in stretch plots flag: 0 - no, 1 - yes';
COMMENT ON COLUMN lcfit.mlcs2k_delta IS 'MLCS2K fitted Delta';
COMMENT ON COLUMN lcfit.mlcs2k_deltaerr IS 'Error in MLCS2K fitted Delta';
COMMENT ON COLUMN lcfit.mlcs2k_a0v IS 'MLCS2K fitted A0(V)';
COMMENT ON COLUMN lcfit.mlcs2k_a0verr IS 'Error in MLCS2K fitted A0(V)';
COMMENT ON COLUMN lcfit.mlcs2k_m0v IS 'MLCS2K fitted M0(V)';
COMMENT ON COLUMN lcfit.mlcs2k_m0verr IS 'Error in MLCS2K fitted M0(V)';
COMMENT ON COLUMN lcfit.mlcs2k_mub IS 'MLCS2K fitted mu(B)';
COMMENT ON COLUMN lcfit.mlcs2k_muberr IS 'Error in MLCS2K fitted mu(B)';
COMMENT ON COLUMN lcfit.mod_date IS 'Date of data modifications';


CREATE TABLE sn
(
 	id      text PRIMARY KEY,	-- SN id: YYYY.id
	name	text,			-- SN name
	tyn	integer,-- Integer SN type code (index into saisncat.sn.sntypes)
	type	text, -- Type of SN
	cz	real, -- recession velocity (kms)
	ra	double precision,  -- decimal ra J2000 (in degrees)
	dec	double precision,
	off_ew	real, -- E/W offset of SN from host center in arcsec
	off_ns	real, -- N/W offset of SN from host center in arcsec
	snmag	real, -- SN Magnitude
	snfilt	text, -- Filter of SN Magnitude
	mtyp	text, -- Type of SN mag: * -discovery, otherwise max
	ddate	text, -- Date of discovery
	discoverer	text, -- Names of SN discoverers
	scid	integer REFERENCES sncats(id), -- catalog source of position
	gid	integer REFERENCES galaxies(id),-- host galaxy
	sid	integer REFERENCES sitemeas(id), -- SN host site measurements
	lcid	integer REFERENCES lcfit(id), -- SN light curve fit data
	mod_date timestamp DEFAULT now() -- modification date, trigger ts_row
);
COMMENT ON TABLE sn IS 'Supernovae';
COMMENT ON COLUMN sn.name IS 'SN name';
COMMENT ON COLUMN sn.tyn IS 'Type code of SN';
COMMENT ON COLUMN sn.type IS 'Type of SN';
COMMENT ON COLUMN sn.cz IS 'Recession velocity of SN (km/s)';
COMMENT ON COLUMN sn.ra	IS 'Right Ascension (2000.0)';
COMMENT ON COLUMN sn.dec IS 'Declination (2000.0)';
COMMENT ON COLUMN sn.off_ew IS 'E/W offset of SN in arcseconds from the nucleus of the host galaxy, with positive sign for eastern direction';
COMMENT ON COLUMN sn.off_ns IS 'N/S offset of SN in arcseconds from the nucleus of the host galaxy, with positive sign for northern direction';
COMMENT ON COLUMN sn.snmag IS 'SN Magnitude';
COMMENT ON COLUMN sn.snfilt IS 'Filter of SN Magnitude';
COMMENT ON COLUMN sn.mtyp IS 'Type of SN mag: * -discovery, otherwise max';
COMMENT ON COLUMN sn.ddate IS 'Date of discovery';
COMMENT ON COLUMN sn.discoverer IS 'Names of SN discoverers';
COMMENT ON COLUMN sn.scid IS 'Identificator of the source catalog for SN position';
COMMENT ON COLUMN sn.gid IS 'Identificator of the host galaxy in table galaxies';
COMMENT ON COLUMN sn.sid IS 'Identificator of the site measurements in the table sitemeas';
COMMENT ON COLUMN sn.lcid IS 'Identificator of light curve data catalog';
COMMENT ON COLUMN sn.mod_date IS 'Date of data modifications';
