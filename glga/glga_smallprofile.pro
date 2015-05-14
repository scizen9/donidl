pro glga_smallprofile, id, ra, dec, majdiam, mindiam, pa, $
        galex=galex,sdss=sdss,twomass=twomass,csp=csp, verbose=verbose
;+
; glga_batchprofile - run radial profile photometry on objects in lfile
;
; list of objects with these columns:
;       id
;       ned_id
;       ra,dec  - degrees
;       majdiam,mindiam - arcmin
;       pa      - degrees
;
;-
; set defaults and check keywords
ddir='uv/'		; data directory
bands = ['f','n']	; image filters
dtype='galex'		; data type
fpx=''			; file postfix
if keyword_set(sdss) then begin
	ddir='sdss/'
	bands=['u','g','r','i','z']
	dtype='sdss'
	fpx='sdss_'
endif
if keyword_set(galex) then begin
	ddir='uv/'
	bands = ['f','n']
	dtype='galex'
	fpx=''
endif
if keyword_set(twomass) then begin
	ddir='2mass/'
	bands = ['j','h','k']
	dtype='2mass'
	fpx='2mass_'
endif
if keyword_set(csp) then begin
	ddir='csp/'
	bands = ['U','B','V','g','r','i','z','y']
	dtype='csp'
	fpx='csp_'
endif
nbands = n_elements(bands)
;
; read in sample data
;readcol,lfile, id, ned_id, ra, dec, majdiam, mindiam, pa, format='a,a,f,f,f,f,f'
;
; deal with negative sizes
a = where(majdiam le 0.0, count)
if count gt 0 then majdiam[a] = 0.5
if count gt 0 then mindiam[a] = 0.5
a = where(mindiam le 0.0,  count)
if count gt 0 then mindiam[a] = majdiam[a]
;
; deal with negative PA
w=where(pa lt 0. and pa gt -360., nw)
if nw gt 0 then pa[w] = pa[w] + 180.
w=where(pa lt 0. and pa gt -180., nw)
if nw gt 0 then pa[w] = pa[w] + 180.
;
; define top level directory
deg = string(floor(ra), format='(i3.3)')+'D'
;
; loop over object list
for i=0,n_elements(id)-1 do begin
	print,i+1,'/',n_elements(id),': ',id[i]
;
; directories
	auxpath=!GLGA_ROOT+'/data/'+deg[i]+'/aux/'
	dsspath=!GLGA_ROOT+'data/'+deg[i]+'/dss/fits/'
	photpath='./'
	plotpath='./'
;	photpath=!GLGA_ROOT+'data/'+deg[i]+'/photometry/'
;	plotpath=!GLGA_ROOT+'data/'+deg[i]+'/plots/'
	fpath=!GLGA_ROOT+'data/'+deg[i]+'/'+ddir+'/fits/'
	jpath=!GLGA_ROOT+'data/'+deg[i]+'/'+ddir+'/jpg/'
;
; get object data
	r=ra[i]
	d=dec[i]
	p=pa[i]>0.
	mjdiam=majdiam[i]
	mndiam=mindiam[i]
;
; get object file names
	masklistfile=auxpath+id[i]+'_'+fpx+'mask.dat'
	ellipsefile=auxpath+id[i]+'_ellipse.dat'
	dssfile=dsspath+id[i]+'_dss2_red.fits*'
	for j=0,nbands-1 do begin
		;
		; GALEX data
		if strpos(dtype,'galex') ge 0 then begin
			galex_radprof,id[i],r,d,$
 				mjdiam,mndiam,p,$
				fpath+id[i]+'-'+bands[j]+'d-int.fit*',$
				fpath+id[i]+'-'+bands[j]+'d-rrhr.fit*',$
				fpath+id[i]+'-'+bands[j]+'d-cnt.fit*',$
 				ellipsefile=ellipsefile,annuli_size=3.0, $
 				diam_units='arcmin', masklistfile=masklistfile,$
 				/verbose, /extend, outpath=photpath
		;
		; SDSS data
		endif else if strpos(dtype,'sdss') ge 0 then begin
			sdss_radprof,id[i],r,d, $
				mjdiam,mndiam,p, $
				fpath+id[i]+'_'+bands[j]+'.fit*', $
				ellipsefile=ellipsefile,annuli_size=1.0, $
				diam_units='arcmin', masklistfile=masklistfile,$
				/verbose, /extend, outpath=photpath
		;
		; 2MASS data
		endif else if strpos(dtype,'2mass') ge 0 then begin
			twomass_radprof,id[i],r,d, $
				mjdiam,mndiam,p, $
				fpath+id[i]+'_'+bands[j]+'.fit*', $
				ellipsefile=ellipsefile, annuli_size=2.0, $
				diam_units='arcmin', masklistfile=masklistfile,$
				/verbose, /extend, outpath=photpath
		;
		; CSP data
		endif else if strpos(dtype,'csp') ge 0 then begin
			print,'Photometry not implemented yet for: ',dtype
		endif
	endfor	; loop over bands
	print,i+1,'/',n_elements(id),': ',id[i],'  DONE'
	;
	; create plots
	if strpos(dtype,'galex') ge 0 then begin
		galex_plotradprof, id[i], $
			pathtoprofile=photpath, $
    			fintfile=fpath+id[i]+'-fd-int.fit*', $
			nintfile=fpath+id[i]+'-nd-int.fit*', $
    			masklistfile=masklistfile,$
    			uvjpgpath=jpath, dssfile=dssfile, $
			outpath=plotpath
	endif else if strpos(dtype,'sdss') ge 0 then begin
		sdss_plotradprof, id[i], $
			pathtoprofile=photpath, $
			intfile=fpath+id[i]+'_r.fit*', $
			masklistfile=masklistfile, $
			jpgpath=jpath, outpath=plotpath
	endif else if strpos(dtype,'2mass') ge 0 then begin
		twomass_plotradprof, id[i], $
			pathtoprofile=photpath, $
			intfile=fpath+id[i]+'_h.fit*', $
			masklistfile=masklistfile, $
			jpgpath=jpath, outpath=plotpath
	endif else if strpos(dtype,'csp') ge 0 then begin
		print,'Photometry not implemented yet for: ',dtype
	endif

endfor

return
end
