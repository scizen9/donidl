function gx_mcat,mfil,ra0,dec0,rad,status=status,time=time
;+
;	gx_mcat - read mcat file and select records based on ra0,dec0 and
;		search radius, rad.
;
; INPUTS:
;	ra0,dec0 - coords of interest (decimal degrees)
;	rad	 - radius of search (arcseconds)
;
; RETURNS:
;	mcat structure including only objects that match
;
; KEYWORDS:
;	status	- variable to contain 0 if all is well, 1 otherwise
;	time	- variable to contain unix seconds of mid-exposure
;
; HISTORY:
;	28-oct-2010 - Initial Revision, neill@srl.caltech.edu
;-
status = 0
hdr0 = headfits(mfil)
t0 = sxpar(hdr0,'NEXPSTAR')
t1 = sxpar(hdr0,'NEXPEND')
time = (t0 + t1) / 2.d0
;
mcat = mrdfits(mfil,1,mhdr,/silent)
;
gcirc,2,ra0,dec0,mcat.alpha_j2000,mcat.delta_j2000,r
;
t = where(r le rad, nt)
;
if nt gt 0 then $
	ocat = mcat[t] $
else	begin
	ocat = -1
	status=1
endelse
;
return,ocat
end
