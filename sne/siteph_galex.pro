pro siteph_galex
;+
; Perform site photometry and analysis on galex GLGA sub images.
;-
;
common sndb_info	; sndat structure
common galdb_info
common glgadb_info
;
; phot files
ofile = !SNE_DATA + 'galex_site.dat'
pfile = !SNE_DATA + 'galex_prof.dat'
;
; calibrations
sbrto = 3.4787		; mag offset to convert to mu (SurfBrt)
fzo = 6.18		; FUV zeropoint offset (25 - 18.82)
nzo = 4.92		; NUV zeropoint offset (25 - 20.08)
;
; get host image list
gims = where(glgadat.nuv_img gt 0, ng)
;
; open analysis log files
filestamp,ofile,/arch
openw,ol,ofile,/get_lun
printf,ol,'# SITEPH_GALEX SITE PHOTOMETRY - '+systime(0)
printf,ol,'# SN           mF  mFerr   mF.5  mFerr    mF1  mFerr    mF2  mFerr    sbF     mN  mNerr   mN.5  mNerr    mN1  mNerr    mN2  mNerr    sbN  host                           fexpt    nexpt     Fbackg   Nbackg     snx     sny'
;
filestamp,pfile,/arch
openw,pl,pfile,/get_lun
printf,pl,'# SITEPH_GALEX PROFILE DATA - '+systime(0)
printf,pl,'# SN        host                         FUVfrac   dFUV  dFUVerr    NUVfrac   dNUV  dNUVerr'
;
; set up filter list
flts = ['FUV','NUV']
nflts = n_elements(flts)
;
; loop over hosts
for i=0,ng-1 do begin
;
; host name
  l = gims[i]
  host=strtrim(glgadat[l].id,2)
;
; get sne and loop
  w = snhfind(host,nsne,/silent)
  if nsne gt 0 then begin
;
; host ra
    h = gfind(host)
    if galdat[h].ra lt 0. then $
	ra = glgadat[l].ra $
    else ra = galdat[h].ra
    dd = !GLGA_ROOT+'data/'+string(floor(ra), format='(i3.3)')+'D/'
    ppath = dd + 'photometry/'
;
; get aperture sizes in arcsec
    aprasec = [ galdat[h].ap_res, $	; radius of one NUV resolution element
    galdat[h].ap_500pc, $		; 500pc diameter aperture
    galdat[h].ap_1kpc,  $		; 1kpc diameter aperture
    galdat[h].ap_2kpc ]			; 2kpc diameter aperture
    napr=n_elements(apr)
;
; files
    fimf=file_search(dd+'galex/fits/'+host+'_FUV_cnt.fits*',count=nfimf)
    nimf=file_search(dd+'galex/fits/'+host+'_NUV_cnt.fits*',count=nnimf)
    fbkf=file_search(dd+'photometry/'+host+'_FUV_background.dat', $
	  count=nfbkf)
    nbkf=file_search(dd+'photometry/'+host+'_NUV_background.dat', $
	  count=nnbkf)
    if nfimf eq 1 and nnimf eq 1 and nfbkf eq 1 and nnbkf eq 1 then begin
;
; read glga images
      fim=mrdfits(fimf[0],0,hdr,/silent,/fscale)
      fexp = sxpar(hdr,'EXPTIME')
      readcol,fbkf[0],fbk,form='f',/silent
      fim = fim - fbk[0]*fexp
      fim = fim+1000.	; create count image (with 1000 count bias)
      nim=mrdfits(nimf[0],0,hdr,/silent,/fscale)
      nexp = sxpar(hdr,'EXPTIME')
      readcol,nbkf[0],nbk,form='f',/silent
      nim = nim - nbk[0]*nexp
      nim = nim+1000.	; create count image (with 1000 count bias)
;
; get header info
      nx = sxpar(hdr,'NAXIS1')
      ny = sxpar(hdr,'NAXIS2')
      extast,hdr,astr
      getrot,hdr,rot,cdelt,/silent
      as_pix = abs(cdelt[0])*3600.
      apr = aprasec / as_pix
;
; loop over sne
      for j=0,nsne-1 do begin
	p=w[j]
;
; sn name,type
	sn=sndat[p].id
	snty = sndat[p].type
;
; analyze image
;
; fix any bad apers
	bap=where(apr le 0.,nbap)
	if nbap gt 0 then apr(bap)=1.
;
; get sn coords in image
	ad2xy,sndat[p].ra,sndat[p].dec,astr,snx,sny
;
; make sure we're not off the image
	if snx ge 0. and snx lt nx-1 and sny ge 0. and sny lt ny-1 then begin
		aper,fim,snx,sny,mg,mge,sky,skerr,1.,apr,[0.,0.],[0.,0.], $
	    		setskyval=1000.,/silent,/exact
    		mgf = mg - fzo + 2.5*alog10(fexp)
    		mgfe = mge
		b=where(mgf gt 50., nb)
		if nb gt 0 then begin
			for k=0,nb-1 do $
				mgf(b(k))=gx_limag(fexp,fbk,apr(b(k)),/fuv)
			mgfe(b)= -9.
		endif
    		if mgf(0) gt 0. then $
			sbf = mgf(0) + sbrto $
		else	sbf = -99.
		aper,nim,snx,sny,mg,mge,sky,skerr,1.,apr,[0.,0.],[0.,0.], $
	    		setskyval=1000.,/silent,/exact
    		mgn = mg - nzo + 2.5*alog10(nexp)
    		mgne = mge
		b=where(mgn gt 50., nb)
		if nb gt 0 then begin
			for k=0,nb-1 do $
				mgn(b(k))=gx_limag(nexp,nbk,apr(b(k)),/nuv)
			mgne(b)= -9.
		endif
    		if mgn(0) gt 0. then $
			sbn = mgn(0) + sbrto $
		else	sbn = -99.
;
; now check for bad apers
		bap = where(aprasec le 0., nbad)
		if nbap gt 0 then begin
			mgf[bap] = -99.
			mgfe[bap]= -9.
			mgn[bap] = -99.
			mgne[bap]= -9.
		endif
;
; analyze profile
		snprofan,sn,host,'FUV',ppath,sbf,mgfe[0],ffrac,fdel,fdele
		snprofan,sn,host,'NUV',ppath,sbn,mgne[0],nfrac,ndel,ndele
;
; print out results
    		print,i+1,ng,sn,host,snty,snx,sny,sbf,sbn,fexp,nexp, $
			format='(i6,i6,2x,a-8,a-14,a-5,2f9.2,2f7.2,2f9.1)'
    		printf,ol,sn,mgf(0),mgfe(0),mgf(1),mgfe(1),mgf(2),mgfe(2), $
	    	mgf(3),mgfe(3),sbf,mgn(0),mgne(0),mgn(1),mgne(1),mgn(2),mgne(2), $
	    	mgn(3),mgne(3),sbn,host,fexp,nexp,fbk,nbk,snx,sny, $
		format='(a-10,18f7.2,2x,a-25,2x,2f9.1,2x,2e9.2,2f8.1)'
		printf,pl,sn,host,ffrac,fdel,fdele,nfrac,ndel,ndele, $
			format='(a-10,2x,a-25,f9.3,2f9.2,f9.3,2f9.2)'
;
; end if we are on the image
    	endif else print,i+1,ng,sn,host,snty,'OFF IMAGE', $
		form='(i6,i6,2x,a-8,a-14,a-5,2x,a)'
;
; end loop over sne
      endfor
;
; no files found
    endif else print,'Missing files for: ',host,'  ',nfimf,nnimf,nfbkf,nnbkf
;
; no sne
  endif else print,i+1,ng,'NONE',host,'-',format='(i6,i6,2x,a-8,a-14,a-5,2x)'
;
; end loop over hosts
endfor
;
; close files
free_lun,ol,pl
;
return
end
