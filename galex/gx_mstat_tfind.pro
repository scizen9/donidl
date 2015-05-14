pro gx_mstat_tfind,ms,ra,dec,diam,tiles,rads,vdirs,svis,cra,cdec,fexpt,nexpt, $
	base,surv,eclipse,planid,obsdate,rlim=rlim,ow=ow
;+
; GX_MSTAT_TFIND - find tiles in the mission status file containing an
;			objects at ra,dec with size diam
;
; INPUTS:
;
;	ms	- mission status struct read with mrdfits
;	ra,dec	- position of object of interest (decimal degrees)
;	diam	- diameter of object in arcsec
;
; OUTPUTS:
;	tiles	- tile names that fully contain the object
;	rads	- radii to center of object for each tile
;	vdirs	- visit directories (where images can be found)
;	svis	- sub-visits (AIS, otherwise = 0)
;	cra	- RA of observation
;	cdec	- Dec of observation
;	fexpt	- FUV exposure time
;	nexpt	- NUV exposure time
;	base	- base name of image
;	surv	- survey type
;	eclipse	- eclipse number
;	planid	- Plan id number
;	obsdate - Start of observation
;
; KEYWORDS:
;	rlim	- radial limit (default 0.5 degrees)
;	ow	- optical wheel ('g', or 'd', 'd' is default), set to
;		 something other than 'g' or 'd' to get all visits
;
; HISTORY:
;	22oct07 jdn - initial revision
;-
; check inputs
if n_params(0) lt 4 then begin
	print,'Usage - gx_mstat_tfind,mstat,ra,dec,diam,tiles,rads,vdirs,svis,rlim=rlim,ow=ow'
	return
endif
;
; default (not found)
tiles = ''
rads  = -9.
vdirs = ''
svis  = 0
;
; check keywords
if keyword_set(rlim) then $
	rlm = rlim $
else	rlm = 0.5
if keyword_set(ow) then $
	opw = ow $
else	opw = 'd'
;
; find distances (in arcsec)
gcirc,2,ra,dec,ms.asp_ave_ra_rta,ms.asp_ave_dec_rta,dis
;
; which distances include entire object
if opw ne 'd' and opw ne 'g' then $
	t=where(dis le (rlm*3600.d0-(diam/2.)) and $
		( strpos(ms.qa_grade,'PASS') ge 0 or $
		  strpos(ms.surv_type,'AIS') ge 0 ) , n) $
else	t=where(dis le (rlm*3600.d0-(diam/2.)) and ms.ow eq opw and $
		( strpos(ms.qa_grade,'PASS') ge 0 or $
		  strpos(ms.surv_type,'AIS') ge 0 ) , n)
if n gt 0 then begin
	tiles = ms(t).tile
	rads  = dis(t)/3600.d0
	svis  = ms(t).sub_visit
	vdirs = strtrim(ms(t).info_str,2)
	cra   = ms(t).asp_ave_ra_rta
	cdec  = ms(t).asp_ave_dec_rta
	fexpt = ms(t).fuv_exptime
	nexpt = ms(t).nuv_exptime
	base  = strtrim(ms(t).base,2)
	surv  = strtrim(ms(t).surv_type,2)
	eclipse = ms(t).eclipse
	planid= ms(t).plan_id
	obsdate = strtrim(ms(t).ecl_start,2)

;
; sort on radial distance
	s     = sort(rads)
	rads  = rads(s)
	tiles = tiles(s)
	svis  = svis(s)
	vdirs = vdirs(s)
	cra   = cra(s)
	cdec  = cdec(s)
	fexpt = fexpt(s)
	nexpt = nexpt(s)
	base  = base(s)
	surv  = surv(s)
	eclipse = eclipse(s)
	planid = planid(s)
	obsdate = obsdate(s)
endif 
;
return
end
