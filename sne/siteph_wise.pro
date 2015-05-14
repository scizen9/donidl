pro siteph_wise
;
; Perform photometry and analysis on WISE sub images.
;
; get master data
COMMON sndb_info
COMMON galdb_info
COMMON glgadb_info
;
; phot files
ofile = !SNE_DATA + 'wise_site.dat'
pfile = !SNE_DATA + 'wise_prof.dat'
;
; get host image list
wims = where(glgadat.w1_img gt 0, ng)
;
; open analysis log files
filestamp,ofile,/arch
openw,ol,ofile,/get_lun
printf,ol,'# SITEPH_WISE SITE PHOTOMETRY - '+systime(0)
printf,ol,'# SN          mw1 mw1err  mw1.5 mw1err   mw11 mw1err   mw12 mw1err   sbw1    mw2 mw2err  mw2.5 mw2err   mw21 mw2err   mw22 mw2err   sbw2    mw3 mw3err  mw3.5 mw3err   mw31 mw3err   mw32 mw3err   sbw3    mw4 mw4err  mw4.5 mw4err   mw41 mw4err   mw42 mw4err   sbw4  host                          snx     sny'
;
filestamp,pfile,/arch
openw,pl,pfile,/get_lun
printf,pl,'# SITEPH_WISE PROFILE DATA - '+systime(0)
printf,pl,'# SN        host                        w1Frac      dw1   dw1err   w2Frac      dw2   dw2err   w3Frac      dw3   dw3err   w4Frac      dw4   dw4err'
;
; set up filter list
flts = ['w1','w2','w3','w4']
nflts = n_elements(flts)
;
; AB conversion from Table 3 in Section IV.3.h.i.3 of
; WISE All-Sky Release Explanatory Suppl.
aboff=[2.699, 3.339, 5.174, 6.620]
;
; loop over hosts
for i=0,ng-1 do begin
;
; host name
  l = wims[i]
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
    aprasec = [galdat[h].ap_res, $	; radius of one NUV resolution element
    galdat[h].ap_500pc, $		; 500pc diameter aperture
    galdat[h].ap_1kpc, $		; 1kpc diameter aperture
    galdat[h].ap_2kpc ]			; 2kpc diameter aperture
    napr=n_elements(aprasec)
;
; loop over sne
    for j=0,nsne-1 do begin
      p=w[j]
;
; sn name, type
      sn=sndat[p].id
      snty = sndat[p].type
;
; set up mag arrays
      mags = fltarr(napr+1,nflts) - 99.	; napr+1 by nflts
      merr = fltarr(napr+1,nflts) - 9.	; extra 1 for surface brightness
;
; loop over filters
      for k=0,nflts-1 do begin
;
; find files
        imf=file_search(dd+'wise/fits/'+host+'_'+flts[k]+'.fits.gz',count=nimf)
        bkf=file_search(dd+'photometry/'+host+'_'+flts[k]+'_background.dat', $
	      count=nbkf)
        if nimf eq 1 and nbkf eq 1 then begin
	  im=mrdfits(imf[0],0,hdr,/silent,/fscale)
	  readcol,bkf[0],bkg,form='f',/silent
	  im = im - bkg[0]
	  im = im + 1000.
;
; get header info
	  nx = sxpar(hdr,'NAXIS1')
	  ny = sxpar(hdr,'NAXIS2')
	  hzp = sxpar(hdr,'MAGZP') + aboff[k]
	  hzperr = sxpar(hdr,'MAGZPUNC')
	  extast,hdr,astr
	  getrot,hdr,rot,cdelt,/silent
	  as_pix = abs(cdelt[0])*3600.
	  AD2XY,sndat[p].ra,sndat[p].dec,astr,snx,sny
;
; make sure we're not off the image
	  if snx ge 0. and snx lt nx-1 and sny ge 0. and sny lt ny-1 then begin
	    sbrto = 3.4787	; mag off to convert GALEX rezap to surf bright
	    apr = aprasec / as_pix
;
; fix any bad apers
	    bap = where(apr le 0., nbap)
	    if nbap gt 0 then apr(bap) = 1.
	    aper,im,snx,sny,mg,mge,sky,skerr,1.,apr,[0.,0.],[0.,0.], $
		    setskyval = 1000., /silent, /exact, /flux
	    mge= 2.5/alog(10.)*(mge/mg)
	    mg = -2.5 * alog10(mg) + hzp
;
; check for bad mags
	    b=where(mg lt 0. or mg gt 50. or finite(mg) eq 0, nb)
	    if nb gt 0 then begin
		    mg[b] = -99.
		    mge[b]= -9.
	    endif
;
; now check for bad apers
	    bap = where(aprasec le 0., nbad)
	    if nbad gt 0 then begin
		    mg[bap] = -99.
		    mge[bap]= -9.
	    endif
;
; fill arrays
	    mags(0:(napr-1),k) = mg(*)
	    merr(0:(napr-1),k) = mge(*) > 0.01	; minimum error 1%
	    if mags(0,k) gt 0 and mags(0,k) lt 50. then $
		    mags(napr,k) = mags(0,k) + sbrto
;
; we're off the image
	  endif else begin
		  snx = -99.
		  sny = -99.
	  endelse
;
; no files found
	endif else begin
		print,'Missing files for: ',host,' ',flts[k],' ',nimf,nbkf
		snx = -99.
		sny = -99.
	endelse
;
; end loop over filters
      endfor
;
; do we have good data?
      if snx ge 0. and sny ge 0. then begin
;
; analyze profile
        snprofan,sn,host,'w1',ppath,mags(napr,0),merr(0,0),w1frac,w1del,w1dele
        snprofan,sn,host,'w2',ppath,mags(napr,1),merr(0,1),w2frac,w2del,w2dele
        snprofan,sn,host,'w3',ppath,mags(napr,2),merr(0,2),w3frac,w3del,w3dele
        snprofan,sn,host,'w4',ppath,mags(napr,3),merr(0,3),w4frac,w4del,w4dele
;
; print out results
        print,i+1,ng,sn,host,snty,snx,sny,mags(0,0),merr(0,0),mags(4,0), $
		format='(i6,i6,2x,a-8,a-14,a-5,2f9.2,f7.2,f5.2,f7.2)'
        printf,ol,sn,mags(0,0),merr(0,0),mags(1,0),merr(1,0),$
		mags(2,0),merr(2,0),mags(3,0),merr(3,0),mags(4,0), $	; w1
		mags(0,1),merr(0,1),mags(1,1),merr(1,1),mags(2,1),merr(2,1),$
		mags(3,1),merr(3,1),mags(4,1), $			; w2
		mags(0,2),merr(0,2),mags(1,2),merr(1,2),mags(2,2),merr(2,2),$
		mags(3,2),merr(3,2),mags(4,2), $			; w3
		mags(0,3),merr(0,3),mags(1,3),merr(1,3),mags(2,3),merr(2,3),$
		mags(3,3),merr(3,3),mags(4,3), $			; w4
		host,snx,sny, $
		format='(a-10,36f7.2,2x,a-25,2f8.1)'
	printf,pl,sn,host,w1frac,w1del,w1dele,w2frac,w2del,w2dele, $
		w3frac,w3del,w3dele,w4frac,w4del,w4dele, $
		format='(a-10,2x,a-25,4(f9.3,2f9.2))'
      endif else print,i+1,ng,sn,host,snty,'OFF IMAGE', $
	      form='(i6,i6,2x,a-8,a-14,a-5,2x,a)'
;
; end loop over sne
    endfor
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
