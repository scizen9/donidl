pro gx_snan,nga_background=nga_background
;
; Perform photometry and analysis on galex sub images.
;
common lowz_sne_info	; sndat structure
;
; get list of sn host galaxy images to work on
flist=file_search('*/*int.fits',count=nf)
if nf le 0 then begin
	print,'No files found.'
	return
endif
;
; get NGA data
if keyword_set(nga_background) then $
	readcol,!NGA_DATA+'table2.dat',gal2,dobs,exp2,tiles, $
	fbck,fbms,fbsm,nbck,format='a,a,f,a,f,f,f,f',/silent
;
; open analysis log file
lfile='snan.log'
filestamp,lfile
openw,ol,lfile,/get_lun
printf,ol,'# SNAN run on '+systime(0)
printf,ol,'# SN          mF   mFerr   mF.5  mFerr    mF1  mFerr    mF2  mFerr   sbF     mN   mNerr   mN.5  mNerr    mN1  mNerr    mN2  mNerr   sbN   host           surv     expt       snx      sny  backg      file'
;
; loop over files
for i=0,nf-1 do begin
;
; header
    hdr = headfits(flist(i))
    ftile = sxpar(hdr,'tile')
    while strpos(ftile,'_') ge 0 do strput,ftile,'-',strpos(ftile,'_')
    band = sxpar(hdr,'band')
    srv = sxpar(hdr,'mpstype')
;
; sn name
    sta=strsplit(flist(i),'/',/extract)
    sn=sta(0)
;
; file name
    fn=sta(1)
;
; is sn in sndat?
    p=snfind(sn)
    if p ge 0 then begin
	host = sndat(p).host
	snty = sndat(p).type
;
; get background
	backg = 0.d0	; default to zero background
;
; get NGA background if requested (unless AIS image)
	if strpos(srv,'AIS') lt 0 and keyword_set(nga_background) then begin
    	  gal=get_nga_name(host,/silent)
    	  h=where(strpos(gal2,gal) ge 0, nh)
    	  if nh gt 0 then begin
	    tilist=tiles(h)
	    fsky=fbck(h)
	    nsky=nbck(h)
	    m=where(strpos(tilist,ftile) ge 0, nm)
	    if nm gt 0 then begin
		if band eq 1 and nsky(m(0)) gt 0. then backg = nsky(m(0))
		if band eq 2 and fsky(m(0)) gt 0. then backg = fsky(m(0))
	    endif
    	  endif
	endif	; get NGA background
;
; use background subtracted image if not using NGA background
	if backg le 0. then begin
		nfn = strmid(fn,0,strpos(fn,'.'))+'bgsub.fits'
		if file_test(sn+'/'+nfn) then $
			fn = nfn
	endif
;
; analyze images
	im=mrdfits(sn+'/'+fn,0,hdr,/silent)
	expt=sxpar(hdr,'EXPTIME')
	im  = im - backg	; subtract background
	ima = im*expt+1000.	; create count image (with 1000 count bias)
;
; get aperture sizes in arcsec
	apr = [ sndat(p).ap_res, $	; radius of one NUV resolution element
	    sndat(p).ap_500pc, $	; 500pc diameter aperture
	    sndat(p).ap_1kpc,  $	; 1kpc diameter aperture
	    sndat(p).ap_2kpc ]		; 2kpc diameter aperture
;
; convert aper sizes to pixels using 1.5 arcsec/pix
	apr = apr / 1.5
	napr=n_elements(apr)
;
; fix any bad apers
	bap=where(apr le 0.,nbap)
	if nbap gt 0 then apr(bap)=1.
;
; get sexigesimal strings
	rastr=''
	decstr=''
	coostr=adstring(sndat(p).ra,sndat(p).dec,1)
	for j=0,2 do $
	    if j ne 2 then $
		    rastr=rastr+gettok(coostr,' ')+':' $
	    else    rastr=rastr+gettok(coostr,' ')
	for j=0,2 do $
	    if j ne 2 then $
		    decstr=decstr+gettok(coostr,' ')+':' $
	    else    decstr=decstr+gettok(coostr,' ')
;
; get sn coords in image
	cmd='sky2xy '+flist(i)+' '+rastr+' '+decstr
	spawn,cmd,res
;
; make sure we're not off the image
	if strpos(res,'ff') lt 0 then begin
		stb=strsplit(res,/extract)
		snx=float(stb(4))
		sny=float(stb(5))
		sbrto = 3.4787		; mag offset to convert to mu (SurfBrt)
		fzo = 6.18		; FUV zeropoint offset (25 - 18.82)
		nzo = 4.92		; NUV zeropoint offset (25 - 20.08)
		aper,ima,snx,sny,mg,mge,sky,skerr,1.,apr,[0.,0.],[0.,0.], $
	    		setskyval=1000.,/silent,/exact
        	if band eq 1 then begin	; NUV
	    		mgn = mg - nzo + 2.5*alog10(expt)
	    		mgne = mge
	    		sbn = mgn(0) + sbrto
	    		mgf = replicate(-99.,napr)
	    		mgfe = replicate(-9.,napr)
	    		sbf = -9.
		endif else begin	; FUV
	    		mgf = mg - fzo + 2.5*alog10(expt)
	    		mgfe = mge
	    		sbf = mgf(0) + sbrto
	    		mgn = replicate(-99.,napr)
	    		mgne = replicate(-9.,napr)
	    		sbn = -9.
		endelse
;
; we are off the image
    	endif else begin
		snx=-9.
		sny=-9.
		mgf=replicate(-99.,napr)
		mgfe=replicate(-9.,napr)
		sbf=-9.
		mgn=replicate(-99.,napr)
		mgne=replicate(-9.,napr)
		sbn=-9.
    	endelse
;
; print out results
    	print,sn,host,snty,mgf(0),mgfe(0),sbf,mgn(0),mgne(0),sbn,srv,expt,fn, $
		format='(a-10,a-15,a-10,6f7.2,2x,a-5,f9.1,2x,a)'
    	printf,ol,sn,mgf(0),mgfe(0),mgf(1),mgfe(1),mgf(2),mgfe(2), $
	    mgf(3),mgfe(3),sbf,mgn(0),mgne(0),mgn(1),mgne(1),mgn(2),mgne(2), $
	    mgn(3),mgne(3),sbn,host,srv,expt,snx,sny,backg,fn,$
	format='(a-10,18f7.2,2x,a-15,a-5,3f9.1,2x,e9.3,2x,a)'
    endif	; did we find sn in sndat?
endfor	; loop over files
;
; close files
free_lun,ol
;
return
end
