pro glga_deploy,ilist,sdss=sdss,twomass=twomass,csp=csp,silent=silent, $
	wise=wise,atlas=atlas,drizzle=drizzle
;+
;	glga_deploy - put built image into glga data directory structure
;-
;
; directories
mdir='sdss/fits/'
idir=!GLGA_SDSS_DATA + 'data/sort/'
sdir=''
type=''
if keyword_set(twomass) then begin
	mdir='2mass/fits/'
	idir=!GLGA_2MASS_DATA + 'data/sort/'
	sdir=''
	type=''
endif
if keyword_set(wise) then begin
	mdir='wise/fits/'
	idir=!GLGA_WISE_DATA + 'data/sort/'
	if keyword_set(atlas) then begin
		sdir='atlas/'
		type='atlas_'
	endif else begin
		sdir='driz?/'
		type='driz_'
	endelse
endif
if keyword_set(sdss) then begin
	mdir='sdss/fits/'
	idir=!GLGA_SDSS_DATA + 'data/sort/'
	sdir=''
	type=''
endif
;
odirpre = !GLGA_ROOT + 'data/'
;
; read list
readcol,ilist,id,ra,dec,mjx,mnx,pa,ty,form='a,d,d,f,f,f,a',silent=silent, $
	comment='#'
nf=n_elements(id)
;
; script file
sfile=type+'glga_deploy_cp'
filestamp,sfile,/arch
openw,sl,sfile,/get_lun
;
; processed file
pfile=type+'processed.glga'
filestamp,pfile,/arch
openw,pl,pfile,/get_lun
printf,pl,'# GLGA_DEPLOY - Done. '+systime(0)
printf,pl,'# DATA: '+idir
;
; not processed file
nfile=type+'notprocessed.glga'
filestamp,nfile,/arch
openw,nl,nfile,/get_lun
printf,nl,'# GLGA_DEPLOY - Not yet processed. '+systime(0)
printf,nl,'# DATA: '+idir
;
; astrometry error log
lfile=type+'astromerr.glga'
filestamp,lfile,/arch
openw,el,lfile,/get_lun
printf,el,'# GLGA_DEPLOY - Astrometry errors. '+systime(0)
printf,el,'# DATA: '+idir
;
; loop
ncop=0L
nprc=0L
nerr=0L
nnpc=0L
for i=0,nf-1 do begin

    obj=id[i]
    print,string(13B),i+1,'/',nf,obj,format='($,a1,i7,a1,i7,2x,a-25)'
;
; list of files to copy
    flist=file_search(idir+'/'+obj+'/'+sdir+obj+'_*.fits.gz',count=nc)
    if nc gt 0 then begin
;
; exclude coverage, mask, and standard deviation images (for now)
	good = where(strpos(flist,'_cov') lt 0, nc)
	if nc gt 0 then flist = flist[good]
	good = where(strpos(flist,'_msk') lt 0, nc)
	if nc gt 0 then flist = flist[good]
	good = where(strpos(flist,'_std') lt 0, nc)
	if nc gt 0 then flist = flist[good]
	if nc gt 0 then begin
;
; loop over list
	    perr = (1 eq 1)
	    for j=0,nc-1 do begin
;
; check astrometry
		hdr=headfits(flist[j])
		extast,hdr,astr,npars
		ast0 = astr.cd[0] * astr.cdelt[0]
		astr_good = (ast0 lt 0.)	; ra incr. with decr. im x
;
; right handed, or no astrometry
		if not astr_good or npars le 0 then begin
			if perr then begin
				perr = (1 eq 0)
				printf,el,obj,ra[i],dec[i],mjx[i],mnx[i], $
					pa[i],ty[i], $
					form='(a-25,2f13.8,3f9.2,2x,a)'
				nerr=nerr+1L
			endif
;
; get destination
		endif else begin
			ddir=string(ra[i], format='(i3.3)')+'D/'
			dest=odirpre+ddir+mdir
			printf,sl,'cp '+flist[j]+' '+dest
			if j eq 0 then begin
			    printf,pl,obj,ra[i],dec[i],mjx[i],mnx[i], $
				pa[i],ty[i],form='(a-25,2f13.8,3f9.2,2x,a)'
			    nprc=nprc+1L
		        endif
			ncop=ncop+1L
		endelse
	    endfor	; loop over file list
	endif else begin
		printf,nl,obj,ra[i],dec[i],mjx[i],mnx[i], $
			pa[i],ty[i],form='(a-25,2f13.8,3f9.2,2x,a)'
		nnpc=nnpc+1L
	endelse
    endif else begin
	printf,nl,obj,ra[i],dec[i],mjx[i],mnx[i], $
		pa[i],ty[i],form='(a-25,2f13.8,3f9.2,2x,a)'
	nnpc=nnpc+1L
    endelse

endfor	; loop over directories
;
; final status
print,' '
print,'Processed: ',nprc,', number of files: ',ncop
print,'Need Imgs: ',nnpc
print,'AstromErr: ',nerr
;
free_lun,sl,el,nl,pl
;
return
end
