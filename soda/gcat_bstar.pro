pro gcat_bstar,flim,blim,slim
a=0
;+
;	gcat_bstar - get a bright star catalog from gcat
;-
gc = mrdfits('/home/lin/gcat/data/chunks/168/asc/SP_168-asc-xd-pricat.fits.gz',1,hdr)
good = where(gc.mag_nuv le flim and gc.mag_nuv gt blim and $
	     gc.flux50_radius_nuv le slim, ngood)


