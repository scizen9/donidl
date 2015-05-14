pro gx_mstat_tfind_best,ms,ra,dec,diam,tile,rad,vdir,svis,rlim=rlim,ow=ow
;+
; GX_MSTAT_TFIND_BEST - find the best tile in the mission status file 
;		containing an object at ra,dec
;
; INPUTS:
;	ms	- mission status struct read with mrdfits
;	ra,dec	- position of object of interest (decimal degrees)
;	diam	- diameter of object in arcsec
;
; OUTPUTS:
;	tile 	- tile name
;	rad	- radius to center of object for tile
;	vdir 	- visit directory where images can be found
;	svis	- subvisit (for AIS, otherwise 0)
;
; KEYWORDS:
;	rlim	- radial limit (default 0.5 degrees)
;	ow	- optical wheel ('g' or 'd', 'd' is default), set to
;		something other than 'g' or 'd' to get all visits
;
; HISTORY:
;	22oct07 jdn - initial revision
;-
; check inputs
if n_params(0) lt 4 then begin
	print,'Usage - gx_mstat_tfind_best,mstat,ra,dec,diam,tile,rad,vdir,svis,rlim=rlim,ow=ow'
	return
endif
;
; default (not found)
tile = ''
rad  = -9.
vdir = ''
svis = 0
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
	t=where(dis le (rlm*3600.d0-diam/2.),n) $
else	t=where(dis le (rlm*3600.d0-diam/2.) and ms.ow eq opw,n)
if n le 0 then return
;
tlist = ms(t).tile
elist = ms(t).exptime
flist = ms(t).fuv_exptime
nlist = ms(t).nuv_exptime
rlist = dis(t)/3600.d0
slist = ms(t).sub_visit
vlist = strtrim(ms(t).info_str,2)
;
; get unique tile names
ulist = tlist(uniq(tlist(sort(tlist))))
;
; reverse sort to put AIS last
ulist = ulist(sort(ulist,/reverse))
;
; max exposure time
mexp = 0.
rat  = 0.
;
; loop over uniqe tiles
for i=0,n_elements(ulist)-1 do begin
	p=where(strpos(tlist,ulist(i)) ge 0)
	texp = total(elist(p))
	fexp = total(flist(p))
	nexp = total(nlist(p))
	if texp gt mexp and (fexp/nexp) ge rat then begin
		mexp = texp
		tile = ulist(i)
		rad  = rlist(i)
		svis = slist(i)
		vdir = vlist(i)
	endif
endfor
;
return
end
