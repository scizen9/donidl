pro sdss_inter,ffile,append=append
;
; Perform photometry and analysis on galex sub images INTERACTIVELY.
;
common lowz_sne_info	; sndat structure
;
; set up filter list
filters='ugriz'
flts = ['u','g','r','i','z']
nflts = n_elements(flts)
;
; pixel scale
pxscl = 0.396	; arcsec per pixel
;
; get sdss image list
readcol,ffile, ra,dec,run,rerun,camcol,field,rowc,colc,filter, $
	format='d,d,i,i,i,i,f,f,a',/silent
ns=n_elements(ra)
;
; open analysis log file
lfile='sdsshostani.log'
if keyword_set(append) then begin
	openw,ol,lfile,/get_lun,/append
	printf,ol,'# SDSS_INTER run on '+systime(0)
	printf,ol,'# File: ',ffile
endif else begin
	filestamp,lfile
	openw,ol,lfile,/get_lun
	printf,ol,'# SDSS_INTER run on '+systime(0)
	printf,ol,'# File: ',ffile
	printf,ol,'# SN          mu     muerr    mg     mgerr    mr     mrerr    mi     mierr    mz     mzerr  host                 hox      hoy'
endelse
;
; loop over sne
for i=0,ns-1 do begin
;
; find SN
    gcirc,2,ra(i),dec(i),sndat.ra,sndat.dec,dis
    t=where(dis eq min(dis))
    t=t(0)
    if dis(t) le 5. then begin
;
; SN and type
	sn=strtrim(sndat(t).id,2)
	snty=strtrim(sndat(t).type,2)
;
; host
	host=strtrim(sndat(t).host,2)
;
; get default aperture sizes in pixels
	apr = [ sndat(t).hd25/pxscl ]	; D25 in arcsec / 1.5 arcsec per pix
;
; convert diameter to radius plus 33% fudge
	apr = apr / 1.5
	napr=n_elements(apr)
;
; fix any bad apers
	bap=where(apr le 0.,nbap)
	if nbap gt 0 then apr(bap)=13.	; default minimum aperture
;
; do we have SDSS calibration?
	cspec = 'tsField-' + string(run(i),form='(i06)') + '-' + $
			     string(camcol(i),form='(i1)') + '-' + $
			     string(rerun(i),form='(i02)') + '-' + $
			     string(field(i),form='(i04)') + '.fit'
	clist = file_search(!SNE_HOSTS+'sdss/das/'+cspec,count=nc)
;
; do we have SDSS images?
	fspec = 'fpC-' + string(run(i),form='(i06)') + '-??-' + $
			 string(field(i),form='(i04)') + '.fit'
	flist = file_search(!SNE_HOSTS+'sdss/das/'+fspec,count=nf)
;
; do we have SDSS object list?
	ospec = 'tsObj-' + string(run(i),form='(i06)') + '-' + $
			     string(camcol(i),form='(i1)') + '-' + $
			     string(rerun(i),form='(i02)') + '-' + $
			     string(field(i),form='(i04)') + '.fit'
	olist = file_search(!SNE_HOSTS+'sdss/das/'+ospec,count=nb)
;
; do we have the files we need?
	if nf gt 0 and nc gt 0 then begin
;
; get calibration
	    cdat=mrdfits(clist(0),1,ch,/silent)
;
; get obj list
	    if nb gt 0 then $
		    odat=mrdfits(olist(0),1,oh,/silent)
;
; get sexigesimal strings
	    rastr=''
	    decstr=''
	    coostr=adstring(sndat(t).hra,sndat(t).hdec,1)
	    for j=0,2 do $
		if j ne 2 then $
			rastr=rastr+gettok(coostr,' ')+':' $
	        else    rastr=rastr+gettok(coostr,' ')
	    for j=0,2 do $
		if j ne 2 then $
			decstr=decstr+gettok(coostr,' ')+':' $
	        else    decstr=decstr+gettok(coostr,' ')
;
; set up mag arrays
	    mags = fltarr(nflts) - 99.
	    merr = fltarr(nflts) - 9.
;
; loop over images (filters)
		print,flist(0)
		for j=0,nf-1 do begin
;
; get host coords in image
		    cmd='sky2xy '+flist(j)+' '+rastr+' '+decstr
		    spawn,cmd,res
;
; make sure we're not off the image
		    hox = -99.
		    hoy = -99.
		    if strpos(res,'ff') lt 0 then begin
;
; get coords
		    stb=strsplit(res,/extract)
		    hox=float(stb(4))
		    hoy=float(stb(5))
;
; get filter number
		    fp=strpos(flist(j),'fpC-')+11
		    fstr=strmid(flist(j),fp,1)
		    fi=strpos(filters,fstr)
;
; get seeing
		    if nb gt 0 then begin
			    parm='SEEING_'+strupcase(fstr)
			    see =sxpar(oh,parm)
		    endif else see = 2.0	; default to 2 arcsec
		    spx = see / pxscl
;
; saturation level
		    sat = float(cdat.saturation_level(fi))
;
; read in image
		    im=mrdfits(flist(j),0,hdr,/fscale,/silent)
;
; get size
		    nx=sxpar(hdr,'NAXIS1')
		    ny=sxpar(hdr,'NAXIS2')
;
; get subimage
		    xx0 = ( hox - (apr[0]+50.) ) > 0
		    xx1 = xx0 + 2.*(apr[0]+50.) < (nx-1)
		    yy0 = ( hoy - (apr[0]+50.) ) > 0
		    yy1 = yy0 + 2.*(apr[0]+50.) < (ny-1)
		    xx0 = nint(xx0) & xx1 = nint(xx1)
		    yy0 = nint(yy0) & yy1 = nint(yy1)
		    sim = im(xx0:xx1,yy0:yy1)
		    nx = xx1-xx0 + 1
		    ny = yy1-yy0 + 1
		    hx = hox - xx0
		    hy = hoy - yy0
;
; exposure time
		    ext = float(sxpar(hdr,'EXPTIME'))
;
; zero point
		    zp = ( 2.5 * alog10(ext) - cdat.aa(fi) - $
			    cdat.kk(fi) * cdat.airmass(fi) ) - 25.0
;
; get sky
		    sky,im,skymd,skysg,/silent
;
; check sky
		    if skymd le 0. or skysg le 0. then begin
			ims,im,skymdi,skysgi
			if skymd le 0. then skymd = skymdi
			if skysg le 0. then skysg = skysgi
		    endif
		    ska = [skymd,skysg,10000.]
		    l0 = skymd-3.*skysg
		    l1 = skymd+8.*skysg
		    print,'Sky: ',ska,form='(a,f7.2,f6.2,f9.1)'
;
; create mask image the first time around
		    mskim=intarr(nx,ny)
;
; display image
		    window,0,xpos=1000,ypos=1200,xsize=nx,ysize=ny,title=sn+' '+host+' '+fstr
		    tvscl,sim>l0<l1
		    tvcircle,apr(0),hx,hy
;		    tvcircle,hox,hoy,apr(0),nx,ny
		    print,' '
		    print,i+1,'/',ns,': ',j+1,'/',nf,': ',sn,'  ',fstr, $
			    form='(i3,a,i3,a,i3,a,i3,a,2x,a,a,a)'
;
		    cx = 128.
		    cy = 128.
		    pcx = -1.
		    pcy = -1.
;
; get star coords
		    while cx gt 10. or cy gt 10. do begin
			rdpix,sim
			cursor,cx,cy,/nowait,/dev
;
; adjust aperture?
			if (nx-cx) lt 10. and (ny-cy) lt 10. then begin
				wait,0.5
				q=''
				read,'a - adjust, r - reset mask, s - scale image, q - quit: ',q
				case strtrim(strupcase(q),2) of
				'A': begin
				print,'Mark galaxy center'
				rdpix,im
				cursor,cx,cy,/nowait,/dev
				wait,0.5
				plots,cx,cy,psym=1,/dev,symsi=3.0
				hx=cx
				hy=cy
				print,'Mark radial extent'
				rdpix,im
				cursor,cx,cy,/nowait,/dev
				wait,0.5
				apr(0) = sqrt((cx-hx)^2+(cy-hy)^2)
		    		tvscl,sim>l0<l1
		    		tvcircle,apr(0),hx,hy
				end
				'R': begin
		    			sim = im(xx0:xx1,yy0:yy1)
		    			tvscl,sim>l0<l1
		    			tvcircle,apr(0),hx,hy
				end
				'S': begin
					scl=8.0
					read,'Scale: ',scl
		    			l1 = skymd+scl*skysg
		    			tvscl,sim>l0<l1
		    			tvcircle,apr(0),hx,hy
				end
				'Q': goto, finis
				endcase
			endif else if cx gt 10. or cy gt 10. then begin
				ccx=cx
				ccy=cy
				cntrd,sim,ccx,ccy,ccx,ccy,3.7,/silent
				if ccx lt 0. or ccy lt 0. or $
			   	  not finite(ccx) or not finite(ccy) then begin
					print,'Centroid failed'
				endif else begin
					cx = ccx
					cy = ccy
				endelse
				if sqrt((cx-pcx)^2 + (cy-pcy)^2) lt 2 then begin
					mrad = mrad + 2.
				endif else begin
					mrad = 6.0
					ssim = sim( (cx-16)>0:(cx+15)<(nx-1), $
				     	(cy-16)>0:(cy+15)<(ny-1) )
					res = gauss2dfit(ssim,a,/tilt)
					if a(2) gt 0 and a(3) gt 0 and $
			   		a(2) lt 15.0 and a(3) lt 15.0 then begin
			   			see = (a(2) + a(3)) / 2.0
						mrad = see*3.0 > (2.*spx) < 20.0
					endif
				endelse
;
; mask
				dist_circle,dis,[nx,ny],cx,cy
				mask=where(dis le mrad, nmsk)
				if nmsk gt 0 then begin
					sim(mask) = ska[0]
					mskim(mask) = 1
		    			tvscl,sim>l0<l1
		    			tvcircle,apr(0),hx,hy
					print,'Masking at: ',cx,' ',cy,' ',' ',mrad
				endif	; nmsk gt 0
				pcx = cx
				pcy = cy
				wait,0.5
			endif
		    endwhile
;
; make sure we're not off the image
		    if hx gt apr(0) and hy gt apr(0) and $
	   		hx lt nx-apr(0) and hy lt ny-apr(0) then begin

		        aper,sim,hx,hy,mg,mge,sky,skerr,1.,apr,[0.,0.], $
				[0.,sat],setskyval=ska,/exact,/silent
		        mags(fi) = mg + zp
		        merr(fi) = mge > 0.01	; minimum error 1%
			print,fi,mags(fi),merr(fi),form='(i3,2f9.3)'
;
; we are off the image
    		    endif else begin
			hox=-9.
			hoy=-9.
    		    endelse
;
	    	    endif else $	; off image?
			    print,'Off image: ',sn,' ',host
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
; print out results
    	    print,sndat(t).id,sndat(t).host,snty, $
		mags(0),merr(0),mags(1),merr(1),mags(2),merr(2), $
		mags(3),merr(3),mags(4),merr(4), $
		format='(a-10,a-15,a-10,2x,10f7.2)'
    	    printf,ol,sndat(t).id,mags(0),merr(0),mags(1),merr(1),$
		mags(2),merr(2),mags(3),merr(3),mags(4),merr(4), $
		sndat(t).host,hox,hoy, $
		format='(a-10,10f8.3,2x,a-15,2f9.1)'
;
; do we have data, a type, and coords?
        endif else begin
	    estr=''
	    if nf lt 1 then estr = estr + 'No im files '
	    if nc lt 1 then estr = estr + 'No cal file'
	    if sndat(t).cz le czlim and sndat(t).cz ge -999.9 then $
		print,sndat(t).id,sndat(t).host,sndat(t).cz, $
			sndat(t).hinc,estr,format='(a-8,a-15,2x,2f9.1,2x,a)'
        endelse
    endif else $
	print,'No record found for SN at ra,dec: ',ra(i),dec(i), $
		format='(a,2f13.8)'
endfor	; loop over SNe
finis:
;
; close files
free_lun,ol
wait,1.
wdelete,0
;
return
end
