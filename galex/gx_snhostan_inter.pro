pro gx_snhostan_inter,outmask=outmask
;
; Perform photometry and analysis on galex sub images INTERACTIVELY.
;
common lowz_sne_info	; sndat structure
;
; get list of sn host galaxy images to work on
flist=file_search('*-nd-intbgsub.fits',count=nf)
if nf le 0 then begin
	print,'No files found.'
	return
endif
nonais=where(strpos(flist,'AIS') ne 0, nnais)
if nnais le 0 then begin
	print,'No non-AIS files found.'
	return
endif
flist=flist(nonais)
nf=nnais
;
; sn name
cfile=file_search('*.log',count=nc)
if nc gt 0 then $
	sn=gettok(cfile(0),'.') $
else    begin
	print,'Cannot find *.log file to determine sn name.'
	return
endelse
;
; is sn in sndat?
p=snfind(sn)
if p ge 0 then begin
	host = sndat(p).host
	snty = sndat(p).type
endif	else begin
	print,'sn not in database: ',sn
	return
endelse
;
; get default aperture sizes in pixels
apr = [ sndat(p).hd25/1.5 ]	; D25 in arcsec / 1.5 arcsec per pix
;
; convert diameter to radius plus 33% fudge
apr = apr / 1.5
napr=n_elements(apr)
;
; fix any bad apers
bap=where(apr le 0.,nbap)
if nbap gt 0 then apr(bap)=13.	; default minimum aperture
;
; sky radii
skrads=[apr(0)+1.,apr(0)+25.]
;
; get host coords in image
rastr = string(sndat(p).hra, form='(f13.8)')
decstr = string(sndat(p).hdec, form='(f13.8)')
cmd='sky2xy '+flist(0)+' '+rastr+' '+decstr
spawn,cmd,res
;
; make sure we're not off the image
if strpos(res,'ff') lt 0 then begin
	stb=strsplit(res,/extract)
	hox=float(stb(4))
	hoy=float(stb(5))
endif else begin
	hox=float(nx)/2.
	hoy=float(ny)/2.
endelse
;
; open analysis log file
lfile='snhostani.log'
filestamp,lfile
openw,ol,lfile,/get_lun
printf,ol,'# SNHOSTANI run on '+systime(0)
printf,ol,'# host             mF   mFerr    mN   mNerr  SN       surv      Fexpt    Nexpt      hx       hy   apr(px)    Fbackg     Nbackg     file'
;
; loop over files
for i=0,nf-1 do begin
	nfil=flist(i)
	ffil=nfil
	strput,ffil,'f',strpos(ffil,'-nd-')+1
	fe = file_test(ffil)
;
; image base name
	base=strmid(nfil,0,strpos(nfil,'-'))
;
; background
	nbckg=0.d0	; default to zero background
	fbckg=0.d0
;
; analyze NUV image
	nim=mrdfits(nfil,0,nhdr,/silent)
;	sky,nim,nbckg,nskysig,/silent
;
; header
	srv = sxpar(nhdr,'mpstype')
	nx = sxpar(nhdr,'NAXIS1')
	ny = sxpar(nhdr,'NAXIS2')
	nexpt=sxpar(nhdr,'EXPTIME')
;
; create count image (with 1000 count bias)
	nima = nim*nexpt+1000.
;
; create mask image the first time around
	if i eq 0 then mskim=intarr(nx,ny)
;
; analyze FUV image
	fexpt = 0.
	if fe then begin
		fim=mrdfits(ffil,0,fhdr,/silent)
;		sky,fim,fbckg,fskysig,/silent
		fbckg=0.d0
;
; header
		fexpt = sxpar(fhdr,'EXPTIME')
;
; create count image (1000 count bias)
		fima = fim*fexpt+1000.	
	endif else begin
		fima = nima
		fim  = nim
	endelse
;
; mask image if needed
	mask=where(mskim eq 1, nmsk)
	if nmsk gt 0 then begin
		nima(mask) = 1000.
		nim(mask) = 0.
		fima(mask) = 1000.
		fim(mask) = 0.
	endif
;
; display image
	scales=[40.,40.,200.]
	window,0,xpos=1000,ypos=1200,xsize=nx,ysize=ny,title=sn+' '+host
	if fe then $
		rgb_asinh_disp,nim,fim,scales=scales, $
			aprad=apr(0), appos=[hox,hoy] $
	else	rgb_asinh_disp,nim,scales=scales, /nobsmooth, $
			aprad=apr(0), appos=[hox,hoy]
	print,' '
	print,i+1,'/',nf,': ',sn,fexpt,base, $
			form='(i3,a,i3,a,2x,a,f9.1,2x,a)'
;
; reset or go with it?
	cx = 128.
	cy = 128.
	if i gt 0 then begin
		q=''
		read,'<cr> - accept mask, else redo: ',q
		if strlen(q) le 0 then begin
			cx = 0.
			cy = 0.
		endif
	endif
;
; get star coords
	while cx gt 10. or cy gt 10. do begin
		rdpix,nima
		cursor,cx,cy,/nowait,/dev
;
; adjust aperture?
		if (nx-cx) lt 10. and (ny-cy) lt 10. then begin
			wait,0.5
			print,'Mark galaxy center'
			rdpix,nima
			cursor,cx,cy,/nowait,/dev
			wait,0.5
			plots,cx,cy,psym=1,/dev,symsi=3.0
			hox=cx
			hoy=cy
			print,'Mark radial extent'
			rdpix,nima
			cursor,cx,cy,/nowait,/dev
			wait,0.5
			apr(0) = sqrt((cx-hox)^2+(cy-hoy)^2)
			skrads=[apr(0)+1.,apr(0)+25.]
			if fe then $
				rgb_asinh_disp,nim,fim,scales=scales, $
				    aprad=apr(0),appos=[hox,hoy] $
			else	rgb_asinh_disp,nim,scales=scales,/nobsmooth,  $
				    aprad=apr(0),appos=[hox,hoy]
		endif else if cx gt 10. or cy gt 10. then begin
			ccx=cx
			ccy=cy
			cntrd,nima,ccx,ccy,ccx,ccy,3.7,/silent
			if ccx lt 0. or ccy lt 0. or $
			   not finite(ccx) or not finite(ccy) then begin
				print,'Centroid failed'
			endif else begin
				cx = ccx
				cy = ccy
			endelse
			mrad = 6.0
			ssim = nima( (cx-16)>0:(cx+15)<(nx-1), $
				     (cy-16)>0:(cy+15)<(ny-1) )
			res = gauss2dfit(ssim,a,/tilt)
			if a(2) gt 0 and a(3) gt 0 and $
			   a(2) lt 15.0 and a(3) lt 15.0 then begin
			   	see = (a(2) + a(3)) / 2.0
				mrad = see*3.0 > 4.0 < 6.0
			endif
			dist_circle,dis,[nx,ny],cx,cy
;
; mask 4 pixels in radius
			mask=where(dis le mrad, nmsk)
			if nmsk gt 0 then begin
				nima(mask) = 1000.
				nim(mask) = 0.
				fima(mask) = 1000.
				fim(mask) = 0.
				mskim(mask) = 1
				print,'Masking at: ',cx,' ',cy,' ',' ',mrad
				if fe then $
					rgb_asinh_disp,nim,fim,scales=scales, $
				    		aprad=apr(0),appos=[hox,hoy] $
				else	rgb_asinh_disp,nim,scales=scales, $
				    		aprad=apr(0),appos=[hox,hoy], $
						/nobsmooth  
			endif	; nmsk gt 0
			wait,0.5
		endif
	endwhile
;
; output mask file if requested
	if keyword_set(outmask) then begin
	    	mnfil=base+'-nd-intbgsub_masked.fits'
	    	mffil=base+'-fd-intbgsub_masked.fits'
	    	mwrfits,nima,mnfil,nhdr,/lscale,/silent
	    	if fe then mwrfits,fima,mffil,fhdr,/lscale,/silent
		print,'Wrote: ',mnfil,' and ',mffil
	endif
;
; make sure we're not off the image
	if hox gt apr(0) and hoy gt apr(0) and $
	   hox lt nx-apr(0) and hoy lt ny-apr(0) then begin
;
; plot NUV sky histogram
;		window,1,title='NUV SKY'
;		if nexpt gt 500. then $
;			gx_background,nima,nbckg,aprad=apr(0),appos=[hox,hoy],$
;				/mmm $;,/hplot
;		else	gx_background,nima,nbckg,aprad=apr(0),appos=[hox,hoy]
		
;
; take measurement
		fzo = 6.18		; FUV zeropoint offset (25 - 18.82)
		nzo = 4.92		; NUV zeropoint offset (25 - 20.08)
		aper,nima,hox,hoy,nmg,nmge,nsky,nskerr,1.,apr,[0.,0.],[0.,0.], $
	    		/silent,/exact,setskyval=1000.
;		aper,nima,hox,hoy,nmg,nmge,nsky,nskerr,1.,apr,skrads,[0.,0.], $
;	    		/silent,/exact,/meanback
	    	mgn = nmg - nzo + 2.5*alog10(nexpt)
		if finite(nmge) then $
	    		mgne = nmge $
		else	mgne = 0.1
		if fe then begin
;			q=''
;			read,'FUV? ',q
;			window,1,title='FUV SKY'
;			gx_background,fima,fbckg,aprad=apr(0),appos=[hox,hoy],/h
;			read,'next: ',q
			aper,fima,hox,hoy,fmg,fmge,fsky,fskerr,1.,apr,[0.,0.], $
				[0.,0.],/silent,/exact,setskyval=1000.
;			aper,fima,hox,hoy,fmg,fmge,fsky,fskerr,1.,apr,skrads, $
;				[0.,0.],/silent,/exact,/meanback
	    		mgf = fmg - fzo + 2.5*alog10(fexpt)
			if finite(fmge) then $
	    			mgfe = fmge $
			else	mgfe = 0.1
		endif else begin
			mgf = replicate(-99.,napr)
			mgfe= replicate(-9.,napr)
			fsky=0.0
			fskerr=0.0
		endelse
;
; we are off the image
    	endif else begin
		hox=-9.
		hoy=-9.
		mgf=replicate(-99.,napr)
		mgfe=replicate(-9.,napr)
		mgn=replicate(-99.,napr)
		mgne=replicate(-9.,napr)
    	endelse
;
; print out results
;	print,fbckg*fexpt+1000.,fsky,fskerr,nbckg*nexpt+1000.,nsky,nskerr,$
;		format='(6f9.3)'
    	print,sn,host,snty,mgf(0),mgfe(0),mgn(0),mgne(0),srv,fexpt,nexpt, $
		format='(a-8,a-15,a-6,4f7.2,2x,a-4,2f9.1)'
    	printf,ol,host,mgf(0),mgfe(0),mgn(0),mgne(0),sn,srv,fexpt,nexpt, $
		hox,hoy,apr(0),0.,0.,nfil, $
		format='(a-15,4f7.2,2x,a-10,a-5,5f9.1,2x,2e11.3,2x,a)'
endfor	; loop over files
;
; close files
free_lun,ol
wait,1.
wdelete,0
;
return
end
