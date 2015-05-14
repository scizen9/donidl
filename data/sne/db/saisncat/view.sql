SET search_path = sn, public, pg_catalog;

begin;

-- new view 

 create or replace view sn_cat AS SELECT s.name as sn_name, 
     g.name  as gal_name,

     s.qgid as sn_gal_uncert,

     g.ra as gal_ra, 
     g.dec as gal_dec,
         g.mag as gal_mag,  
         bg.name as gal_mband,

      c.name    as gal_mag_ref, 

      abs(g.posang) as gal_posang, 

      case when substr(g.type,1,1)='E' then NULL
           when substr(g.type,1,1)='I' then NULL
           when g.type ISNULL then NULL else
         case when ((0.1^g.logab)^2-0.2^2)/(1-0.2^2) > 0 then

floor(acos(sqrt(((0.1^g.logab)^2-0.2^2)/(1-0.2^2) ))*180./3.1415926536)::real

         else case when g.logab NOTNULL then 90.0::real else NULL end
         end  end as gal_inclination, 

         g.z as redshift, 

         g.type as gal_type,
         g.logab as gal_logab, g.log10d as gal_log10d,  
         g.t  as gal_type_code,
         g.e_t as gal_type_code_prec,

	 s.ew as sn_ew, s.ns as sn_ns, 
 
         s.mag as sn_mag, 
         s.lid as sn_lid,
         b.name as sn_mband,

         s.qmag as sn_qmag,

	s.optdisc as sn_optdisc,

    --     s.maxdate as sn_max_date, 

         --case when s.maxdate  ISNULL then NULL else
         --substr(s.maxdate,7,4)||'-'||substr(s.maxdate,1,5)

         s.maxdate  as sn_max_date,
         

       --  case when s.discdate ISNULL then NULL else
       --  to_char(s.discdate,'MM-DD-YYYY') end as sn_disc_date,

         s.discdate as sn_disc_date,

 ---     sntype
      
         s.type as sn_type,
         s.qtype as sn_qtype,         
   
         s.ra as sn_ra,
         s.dec as sn_dec,

         s.qsn as sn_uncert,        

         s.survey,

         s.discoverer as sn_discoverer,
         s.comment as sn_comment,
         g.comment as gal_comment,

         string_to_array(s.mod_date||','||g.mod_date,',') as mod_date_arr


         FROM 
         sn as s
              LEFT OUTER JOIN band as b on (b.id = s.bandid) 
              LEFT OUTER JOIN sntypes as st on (st.id=s.type)

              LEFT OUTER JOIN galaxies as g  on ( s.gid=g.id)
              LEFT OUTER JOIN band as bg on (bg.id=g.bandid)

              LEFT OUTER JOIN map  on (map.gid=s.gid and s.gid NOTNULL)
              LEFT OUTER JOIN cats as c on (c.id=map.cid) --c.name for gal.magnitude
                                                          --c.name=gal_mag_ref
         WHERE  map.pid=4 or map.pid ISNULL;  -- map.pid=4 : 
                                              -- pull out a CID for gal.magnitude 
                                              -- (pid(mag)=4)
--         ORDER BY s.name;

COMMENT ON COLUMN sn_cat.sn_name IS 'SN designation';
COMMENT ON COLUMN sn_cat.gal_name IS 'Galaxy designation';
COMMENT ON COLUMN sn_cat.sn_gal_uncert IS 'Uncertainty of host galaxy identification is indicated by code 1';
COMMENT ON COLUMN sn_cat.gal_ra IS 'Right Ascension of the host galaxy (2000.0)';
COMMENT ON COLUMN sn_cat.gal_dec IS 'Declination of the host galaxy (2000.0)';
COMMENT ON COLUMN sn_cat.gal_mag IS 'Magnitude of the host galaxy';
COMMENT ON COLUMN sn_cat.gal_mag_ref IS 'Source of magnitude';
COMMENT ON COLUMN sn_cat.gal_mband IS 'Magnitude band';
COMMENT ON COLUMN sn_cat.gal_posang IS 'Position angle in degrees measured from the North to the East';
COMMENT ON COLUMN sn_cat.gal_inclination IS 'Inclination angle in degrees for disk-like systems (i = 0 for face-on systems)';
COMMENT ON COLUMN sn_cat.redshift IS 'Redshift';
COMMENT ON COLUMN sn_cat.gal_type IS 'Morphological type of the host galaxy';
COMMENT ON COLUMN sn_cat.gal_logab IS 'Decimal log of axial ratio (major axis divided by minor axis)';
COMMENT ON COLUMN sn_cat.gal_log10d IS 'Decimal log of apparent isophotal major diameter in units of 0.1 arcminute';
COMMENT ON COLUMN sn_cat.gal_type_code IS 'Numerical code of morphological type as in LEDA';
COMMENT ON COLUMN sn_cat.gal_type_code_prec IS 'Error of numerical code of morphological type as in LEDA';
COMMENT ON COLUMN sn_cat.sn_ew IS 'E/W offset of SN in arcseconds from the nucleus of the host galaxy, with positive sign for eastern direction';
COMMENT ON COLUMN sn_cat.sn_ns IS 'N/S offset of SN in arcseconds from the nucleus of the host galaxy, with positive sign for northern direction';
COMMENT ON COLUMN sn_cat.sn_mag IS 'Magnitude of SN';
COMMENT ON COLUMN sn_cat.sn_lid IS 'Description of magnitude: code 1 is for magnitude at maximum light, code 2 is for magnitude estimated at any phase of SN evolution';
COMMENT ON COLUMN sn_cat.sn_mband IS 'The name of the standard band in which the magnitude was derived';
COMMENT ON COLUMN sn_cat.sn_qmag IS 'Quality of magnitude: code 1 is for uncertain magnitude estimate';
COMMENT ON COLUMN sn_cat.sn_optdisc IS 'Method of SN discovery: code 1 is for SNe discovered  photographically, code 2 is for all other kinds of discoveries '; 
COMMENT ON COLUMN sn_cat.sn_max_date IS 'Date of maximum light';
COMMENT ON COLUMN sn_cat.sn_disc_date IS 'Date of discovery';
COMMENT ON COLUMN sn_cat.sn_type IS 'Type of supernova';
COMMENT ON COLUMN sn_cat.sn_qtype IS 'Uncertainty in determination of SN type. Code 1 is  for uncertain SN type, code 2 is for peculiar SN type, code 3 is for uncertain and peculiar SN type';
COMMENT ON COLUMN sn_cat.sn_ra IS 'Right Ascension of SN (2000.0)';
COMMENT ON COLUMN sn_cat.sn_dec IS 'Declination of SN (2000.0)';
COMMENT ON COLUMN sn_cat.sn_uncert IS 'Uncertainty of SN. Doubtful SNe are indicated by code 1';
COMMENT ON COLUMN sn_cat.survey IS 'Code for SN search programme or observatory where SN was discovered';
COMMENT ON COLUMN sn_cat.sn_discoverer IS 'Names of SN discoverers';
COMMENT ON COLUMN sn_cat.sn_comment IS 'Comment on SN';
COMMENT ON COLUMN sn_cat.gal_comment IS 'Comment on the host galaxy';
COMMENT ON COLUMN sn_cat.mod_date_arr IS 'Dates of data modifications (SN,host galaxy)';

grant select on sn_cat to public;

end;
