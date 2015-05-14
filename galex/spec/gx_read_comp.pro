pro gx_read_comp,ifil,vis,svis,ext,sid,gra,xrms,mdrs,frat,fsig,nrat,nsig
;+
; GX_READ_COMP - read in standard comparison results
;
;	gx_read_comp,ifil,vis,svis,ext,sid,gra,xrms,mdrs,frat,fsig,nrat,nsig
;
; OUTPUTS:
;
;	vis	- visit number
;	svis	- subvisit number
;	ext	- exposure time (seconds)
;	sid	- GALEX object id for standard
;	gra	- grism angle (degrees)
;	xrms	- rms of x offset solution
;	mdrs	- median x offset
;	frat	- FUV flux ratio (GALEX/REF)
;	fsig	- stddev of FUV flux ratio
;	nrat	- NUV flux ratio (GALEX/REF)
;	nsig	- stddev of NUV flux ratio
;
; HISTORY:
;
;	07jun10 jdn	- initial version
;-
; get standard info
	readcol,ifil,vis,svis,ext,sid,gra,xrms,mdrs,frat,fsig,nrat,nsig, $
		format='i,i,f,l,f,f,f,f,f,f,f',comment='#',/silent
;
return
end
