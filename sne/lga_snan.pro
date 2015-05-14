pro lga_snan,silent=silent,ps=ps,czlimit=czlimit
;
; Perform photometry and analysis on 2mass LGA images.
;
; get master data
COMMON sndb_info
COMMON galdb_info
;
nsne=n_elements(sndat)
;
; check keywords
if keyword_set(czlimit) then $
	czlim = czlimit $
else	czlim = 1.e9
;
; open analysis log file
lfile=!SNE_DATA+'lga_snan.log'
filestamp,lfile
openw,ol,lfile,/get_lun
printf,ol,'# LGA_SNAN run on '+systime(0)
printf,ol,'# SN          mJ   mJerr   mJ.5  mJerr    mJ1  mJerr    mJ2  mJerr   sbJ   Jfrac  Jdelp Jdelpe    mH   mHerr   mH.5  mHerr    mH1  mHerr    mH2  mHerr   sbH   Hfrac  Hdelp Hdelpe    mK   mKerr   mK.5  mKerr    mK1  mKerr    mK2  mKerr   sbK   Kfrac  Kdelp Kdelpe  host                 snx      sny'
;
; open error log file
efile=!SNE_DATA+'lga_snan.err'
filestamp,efile
openw,el,efile,/get_lun
printf,el,'# LGA_SNAN run on '+systime(0)
printf,el,'# SN    Host                    cz     hinc  err'
;
; set up filter list
flts = ['j','h','k']
nflts = n_elements(flts)
;
; plot setups
th=3
si=1.75
csi=1.25
font_store=!p.font
!p.multi=[0,1,2]
;
; loop over sne
for i=0,nsne-1 do begin
;
; get SN name
    tmp=strtrim(sndat(i).id,2)
    snnm=gettok(tmp,'?')
;
; SN type?
    snty=strtrim(sndat(i).type,2)
;
; do we have LGA data?
    host=strtrim(sndat(i).host,2)
    gal =get_lga_name(host,flist,/silent)
    nf  =n_elements(flist)
;
; host data
    hind = gfind(host)
;
; do we have data a type and coords? are we below czlim?
    if nf gt 1 and snty ne '-' and sndat(i).cz le czlim and $
	sndat(i).ra ge 0. and sndat(i).dec ge -90. and hind ge 0L then begin
;
; get sexigesimal strings
	rastr=''
	decstr=''
	coostr=adstring(sndat(i).ra,sndat(i).dec,1)
	for j=0,2 do $
	    if j ne 2 then $
		    rastr=rastr+gettok(coostr,' ')+':' $
	    else    rastr=rastr+gettok(coostr,' ')
	for j=0,2 do $
	    if j ne 2 then $
		    decstr=decstr+gettok(coostr,' ')+':' $
	    else    decstr=decstr+gettok(coostr,' ')
;
; get aperture sizesin arcsec
; aper sizes are already in pixels because 2MASS scale is 1.0 arcsec/pix
	apr = [galdat[hind].ap_res, $	; radius of one NUV resolution element
	    galdat[hind].ap_500pc, $	; 500pc diameter aperture
	    galdat[hind].ap_1kpc, $		; 1kpc diameter aperture
	    galdat[hind].ap_2kpc ]		; 2kpc diameter aperture
	napr=n_elements(apr)
;
; fix any bad aps
	bap=where(apr le 0., nbap)
	if nbap gt 0 then apr(bap)=1.
;
; set up mag arrays
	mags = fltarr(napr+1,nflts) - 99.	; napr+1 by nflts
	merr = fltarr(napr+1,nflts) - 9.	; extra 1 for surface brightness
;
; loop over filters
	for j=0,nflts-1 do begin
;
; find fits file
	    g=where(strpos(flist,'_'+flts(j)+'.fits') ge 0, ng)
	    if ng eq 1 then begin
;
; get sn coords in image
		g=g(0)
		cmd='sky2xy '+flist(g)+' '+rastr+' '+decstr
		spawn,cmd,res
;
; make sure we're not off the image
		snx = -99.
		sny = -99.
		if strpos(res,'ff') lt 0 then begin
;
; read in image
		    im=mrdfits(flist(g),0,hdr,/silent)
;
; get psf kernel assuming 1 arcsec seeing
		    psf=psf_gaussian(npixel=17,fwhm=5.5,/norm)
		    ima=convol(im,psf) + 1000.	; avoid zero sky
; get photometric zeropoint
		    zpstr=strupcase(flts(j))+'MAGZP'
		    zp = sxpar(hdr,zpstr) - 25.0
;
; get coords
		    stb=strsplit(res,/extract)
		    snx=float(stb(4))
		    sny=float(stb(5))
		    sbrto = 3.4787	; mag off to convert to surf brightness
		    aper,ima,snx,sny,mg,mge,sky,skerr,1.,apr, $
			    [0.,0.],[0.,0.],setskyval=1000.,/silent,/exact
		    mags(0:(napr-1),j) = mg(*) + zp
		    merr(0:(napr-1),j) = mge(*)
		    mags(napr,j) = mags(0,j) + sbrto
		endif	; off image?
	    endif else if not keyword_set(silent) then $
		    print,'No file found for filter: ',flts(j),' for SN: ',$
		    	sndat(i).id
;
; end loop over filters
	endfor
;
; flag bad mags
	bad = where(mags gt 50., nbad)
	if nbad gt 0 then begin
		mags(bad) = -99.
		merr(bad) = -9.
	endif
;
; set up profile data arrays
	frac = fltarr(nflts) -9.		; profile fraction
	delp = fltarr(nflts) -99.		; delta profile
	delperr = fltarr(nflts) -9.		; error in delta profile
	rmag = fltarr(nflts) -99.		; profile surface brightness
;
; analyze profile
	g=where(strpos(flist,'-SB.tbl') ge 0, ng)
	if ng eq 1 then begin	; do we have profile data?
;
; read in profile data
		g=g(0)
		readcol,flist(g),rmid,jmn,jsd,jmd,jmag,jimg, $
				      hmn,hsd,hmd,hmag,himg, $
				      kmn,ksd,kmd,kmag,kimg,jmk,skipline=12, $
			format='f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f',/silent
;
; get mag errs
		jmge	   = 1.0857362d0 * (jsd/jmn)
		hmge	   = 1.0857362d0 * (hsd/hmn)
		kmge	   = 1.0857362d0 * (ksd/kmn)
;
; good isophotal radius?
		if sndat(i).r_iso ge 0. then begin
;
; get good points
			gj = where(jimg gt 0.)
			jimg = jimg(gj)
			rj = rmid(gj)
			gh = where(himg gt 0.)
			himg = himg(gh)
			rh = rmid(gh)
			gk = where(kimg gt 0.)
			kimg = kimg(gk)
			rk = rmid(gk)
;
; get cumulative light
			cntJ = 10.0^(-0.4*(jimg-20.))	; arbitrary zp
			cntH = 10.0^(-0.4*(himg-20.))
			cntK = 10.0^(-0.4*(kimg-20.))
			cumJ = cntJ - cntJ
			cumH = cntH - cntH
			cumK = cntK - cntK
			for j=1,n_elements(jimg)-1 do $
				cumJ(j) = cumJ(j-1)+cntJ(j-1)*(!pi*rj(j)^2 - $
						               !pi*rj(j-1)^2)
			for j=1,n_elements(himg)-1 do $
				cumH(j) = cumH(j-1)+cntH(j-1)*(!pi*rh(j)^2 - $
						               !pi*rh(j-1)^2)
			for j=1,n_elements(kimg)-1 do $
				cumK(j) = cumK(j-1)+cntK(j-1)*(!pi*rk(j)^2 - $
						               !pi*rk(j-1)^2)
			cumJ = cumJ / max(cumJ)
			cumH = cumH / max(cumH)
			cumK = cumK / max(cumK)
;
; where is sn on galaxy?
			t=where(rmid gt sndat(i).r_iso, n)
;
; inside galaxy profile?
			if n gt 0 then begin
			    frac(0) = interpol(cumJ,rj,sndat(i).r_iso)
			    rmag(0) = interpol(jimg,rj,sndat(i).r_iso)
			    frac(1) = interpol(cumH,rh,sndat(i).r_iso)
			    rmag(1) = interpol(himg,rh,sndat(i).r_iso)
			    frac(2) = interpol(cumK,rk,sndat(i).r_iso)
			    rmag(2) = interpol(kimg,rk,sndat(i).r_iso)
;
; get delta mag
			    if mags(napr,0) gt 0. then begin	; J
				delp(0) = mags(napr,0)-rmag(0)
				mge = interpol(jmge(gj),rmid(gj),sndat(i).r_iso)
				delperr(0) = sqrt(mge^2+merr(0,0)^2)
			    endif
			    if mags(napr,1) gt 0. then begin	; H
				delp(1)= mags(napr,1)-rmag(1)
				mge = interpol(hmge(gh),rmid(gh),sndat(i).r_iso)
				delperr(1) = sqrt(mge^2+merr(0,1)^2)
			    endif
			    if mags(napr,2) gt 0. then begin	; K
				delp(2)= mags(napr,2)-rmag(2)
				mge = interpol(kmge(gk),rmid(gk),sndat(i).r_iso)
				delperr(2) = sqrt(mge^2+merr(0,2)^2)
			    endif
;
; outside galaxy profile
			endif else begin
			    frac(0) = 1.
			    frac(1) = 1.
			    frac(2) = 1.
			endelse
;
; output to ps file?
		if keyword_set(ps) then begin
			tmp=flist(g)
			pfl=gettok(tmp,'_')+'_'+strtrim(snnm,2)+'_IRprof'
			psfile,pfl
			!p.font=1
			ylab=textoidl('\mu (mag arcsec^{-2})')
			yflab=textoidl('Fraction of light < Radius')
			xlab=textoidl('Radius (arcsec)')
		endif else begin
			ylab='!9n!3 (mag arcsec!U-2!N)'
			yflab='Fraction of light < Radius'
			xlab='Radius (arcsec)'
		endelse
;
; plot profile
		tar = [jimg,himg,kimg,reform(mags(napr,*))]
		good=where(tar gt 0)
		mxmag = max(tar(good))
		mnmag = min(tar(good))
		maxrad= max([rmid,sndat(i).r_iso])+5.
		bx=[0.,0.]
		by=[0.,0.]
		plot,bx,by,thick=th,charsi=csi,xthick=th,ythick=th,$
			yran=[mxmag,mnmag],ytitle=ylab,ysty=1, $
			xran=[-2,maxrad],xtitle=xlab,xsty=1, $
			charthi=th, /nodata, $
			title=snnm+', '+sndat(i).host+' '+systime(0)
		oplot,[sndat(i).r_iso,sndat(i).r_iso],[-100,100]
; J
		oplot,rmid,jimg,psym=5,thick=th
		errplot,rmid,jimg-jmge,jimg+jmge,thick=th
		plots,sndat(i).r_iso,mags(napr,0),psym=5,thick=th,symsi=si*2.
		errplot,[sndat(i).r_iso,sndat(i).r_iso], $
			[mags(napr,0),mags(napr,0)]-delperr(0), $
			[mags(napr,0),mags(napr,0)]+delperr(0), $
			thick=th
; H
		oplot,rmid,himg,psym=6,thick=th
		errplot,rmid,himg-hmge,himg+hmge,thick=th
		plots,sndat(i).r_iso,mags(napr,1),psym=6,thick=th,symsi=si*2.
		errplot,[sndat(i).r_iso,sndat(i).r_iso], $
			[mags(napr,1),mags(napr,1)]-delperr(1), $
			[mags(napr,1),mags(napr,1)]+delperr(1), $
			thick=th
; K
		oplot,rmid,kimg,psym=4,thick=th
		errplot,rmid,kimg-kmge,kimg+kmge,thick=th
		plots,sndat(i).r_iso,mags(napr,2),psym=4,thick=th,symsi=si*2.
		errplot,[sndat(i).r_iso,sndat(i).r_iso], $
			[mags(napr,2),mags(napr,2)]-delperr(2), $
			[mags(napr,2),mags(napr,2)]+delperr(2), $
			thick=th
		legend,['!9D!3J = '+string(delp(0),form='(f6.2)'), $
			'!9D!3H = '+string(delp(1),form='(f6.2)'), $
			'!9D!3K = '+string(delp(2),form='(f6.2)')], $
			box=0,charthi=th,charsi=csi,/bottom
		legend,['J','H','K'],psym=[5,6,4],charthi=th,charsi=csi,$
			thick=[th,th,th],/right,/clear
;
; plot cumulate light
		plot,bx,by,thick=th,charsi=csi,xthick=th,ythick=th,$
			yran=[-0.05,1.1],ytitle=yflab,ysty=1, $
			xran=[-2,maxrad],xtitle=xlab,xsty=1, $
			charthi=th,/nodata
		oplot,[sndat(i).r_iso,sndat(i).r_iso],[-100,100]
		oplot,rmid,cumJ,psym=5,thick=th
		oplot,rmid,cumH,psym=6,thick=th
		oplot,rmid,cumK,psym=4,thick=th
		legend,['J LFRAC = '+string(frac(0),form='(f6.3)'), $
			'H LFRAC = '+string(frac(1),form='(f6.3)'), $
			'K LFRAC = '+string(frac(2),form='(f6.3)')], $
			charsi=csi,charthi=th,box=0,/right,/bottom
;
; next plot?
		q=''
		if keyword_set(ps) then $
			psclose $
		else	read,'next: ',q
		if strupcase(strmid(q,0,1)) eq 'Q' then begin
			free_lun,ol,el
			!p.font=font_store
			!p.multi=0
			return
		endif
;
; bad isophotal radius
		endif else print,'Bad r_iso for: ',sndat(i).id
;
	endif	; do we have profile data?
;
; print out results
    	print,sndat(i).id,sndat(i).host,snty, $
		mags(0,2),merr(0,2),mags(4,2),frac(2),delp(2),delperr(2), $
		format='(a-10,a-15,a-10,3f7.2,f7.3,2f7.2)'
    	printf,ol,sndat(i).id,mags(0,0),merr(0,0),mags(1,0),merr(1,0),$
		mags(2,0),merr(2,0),mags(3,0),merr(3,0),mags(4,0), $
		frac(0),delp(0),delperr(0), $				; J
		mags(0,1),merr(0,1),mags(1,1),merr(1,1),mags(2,1),merr(2,1),$
		mags(3,1),merr(3,1),mags(4,1), $
		frac(1),delp(1),delperr(1), $				; H
		mags(0,2),merr(0,2),mags(1,2),merr(1,2),mags(2,2),merr(2,2),$
		mags(3,2),merr(3,2),mags(4,2), $
		frac(2),delp(2),delperr(2), $				; K
		sndat(i).host,snx,sny, $
		format='(a-10,3(9f7.2,f7.3,2f7.2),2x,a-15,2f9.1)'
;
; do we have data, a type, and coords?
    endif else begin
	estr=''
	if sndat(i).ra lt 0. or sndat(i).dec lt -90. then estr = 'Bad coords '
	if nf le 1 then estr = estr + 'No files '
	if snty eq '-' then estr = estr + 'No SN type '
	hinc = -99.0
	if hind lt 0 then $
		estr = estr + 'No Host index' $
	else	hinc = galdat[hind].inc
	if sndat(i).cz le czlim and sndat(i).cz ge -999.9 then $
		printf,el,sndat(i).id,sndat(i).host,sndat(i).cz, $
			hinc,estr,format='(a-8,a-15,2x,2f9.1,2x,a)'
    endelse
endfor	; loop over SNe
;
; close files
free_lun,ol,el
;
!p.font=font_store
!p.multi=0
;
return
end
