pro update_galdat_from_glga,row,ifile,verbose=verbose
;+
;	update_galdat_from_glga - update galdat using glga data file
;-
COMMON galdb_info, galdat, gphsrc
;
tnams = tag_names(galdat)
;
; GLGA source numbers
filts = ['FUV','NUV','U','G','R','I','Z','J','H','K','W1','W2','W3','W4']
phsrc = ['GALEX','GALEX','SDSS','SDSS','SDSS','SDSS','SDSS','2MASS','2MASS','2MASS','WISE','WISE','WISE','WISE']
exc0  = [8.24, 8.24, 5.155, 3.793, 2.751, 2.086, 1.479, 0.900, 0.576, 0.365, 0.0274, 0.0165, 0.0100, 0.0010]
exc1  = [ 0., 0.67, 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.]
;
; get info from file name
fdecomp,ifile,disk,dir,rute,qual
sta = strsplit(dir,'/',/extract)
ddir = sta[4]				; data directory
sta = strsplit(rute,'_',/extract)
id = sta[0]				; host id
filt = strupcase(sta[1])		; filter
do_uv = (strpos(filt,'UV') ge 0)	; are we GALEX data?
if do_uv then begin
	if strpos(filt,'F') ge 0 then begin	; are we FUV?
		glxf = '-fd-int'
		etag = 'FUV_EXPTIME'
	endif else begin			; nope, NUV
		glxf = '-nd-int'
		etag = 'NUV_EXPTIME'
	endelse
	imfil = !GLGA_ROOT+'data/'+ddir+'/uv/fits/'+id+glxf+'.fits.gz'
	if file_exist(imfil) then begin		; do we exist?
		hdr = headfits(imfil)
		expt = sxpar(hdr,'EXPTIME')
	endif else expt = -99.99
endif else expt = -99.99
;
; get source
f=where(filts eq filt, nf)
if nf ne 1 then begin
	if keyword_set(verbose) then $
		print,'Error - returning, unknown filter: ',filt
	return
endif
f = f[0]
src = galdb_phsrc('GLGA '+phsrc[f[0]])
;
; get tag number
t = where(tnams eq filt+'_INT_MAG', nt)
if nt ne 1 then begin
	if keyword_set(verbose) then $
		print,'ERROR - returning, illegal tag: ',filt+'_INT_MAG'
	return
endif
t = t[0]
;
; update photometry
readcol,ifile,a,mg,merr, format='f,f,f',/silent,comment='#'
if finite(mg) then begin
	if galdat[row].mwebmv gt -1. then $
		extin = exc0[f[0]] * galdat[row].mwebmv - $
			exc1[f[0]] * galdat[row].mwebmv^2 $
	else	extin = 0.
	galdat[row].(t)   = mg - extin
	galdat[row].(t+1) = merr
	galdat[row].(t+2) = src
endif
;
; update GALEX exposure time
if do_uv then begin
	t = where(strpos(tnams,etag) ge 0, nt)
	if nt lt 1 then $
		print,'No GALEX exposure time tag?: ',etag $
	else	galdat[row].(t[0]) = expt

endif
;
; ellipse file
efile = repstr(ifile,'aperture','ellipsepar')
;
; read measured position, shape
readcol,efile,ra,dec,smaj,smin,spa,form='d,d,f,f,f',/silent,comment='#'
;
; update ra, dec, shape
; measured
galdat[row].m_ra = ra
galdat[row].m_dec = dec
galdat[row].m_majax = smaj * 2.0
galdat[row].m_minax = smin * 2.0
galdat[row].m_pa = spa
; measured becomes cononical
galdat[row].ra = ra
galdat[row].dec = dec
galdat[row].majax = smaj * 2.0
galdat[row].minax = smin * 2.0
galdat[row].pa = spa
; source
galdat[row].coo_src = 'G'
;
; sample
galdat[row].sample = gadd_sample(galdat[row].sample,'glga')
;
; time stamp
galdat[row].mod_time = systime(1)
;
return
end
