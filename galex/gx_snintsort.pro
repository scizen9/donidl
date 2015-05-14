pro gx_snintsort,imfil
;+
;	gx_snintsort - analyze intensity correlation with SN position
;			by getting the sn intesity in relation to the
;			range of intensities in the galaxy
;
; INPUTS/OUTPUTS:
;	imfil - input image filename
;
; KEYWORDS:
;	NONE
;
; HISTORY:
;	v1.0, neill@srl.caltech.edu 18-dec-06
;
;-
; check inputs
nn=1
p=0
if n_params(0) lt 1 then begin
	flist=file_search('*-nd-intbgsub.fits',count=nn)
	if nn le 0 then begin
		print,'Error no NUV ims found'
		return
	endif else if nn gt 1 then begin
		for i=0,nn-1 do print,i,' ',flist(i)
		p=-1
		while p lt 0 or p ge nn do read,'im #: ',p
		imfil=flist(p)
	endif else imfil=flist(0)
endif
;
; setups
mskval=-999.00
scl = 20.0
q='g'
th=3
si=1.25
!p.background=colordex('white')
!p.color=colordex('black')
if !d.name ne 'X' then !p.font=1 else !p.font=-1
;
; set up usersym (circle)
a=[findgen(16)*(!pi*2/16.),0.]
usersym,cos(a),sin(a)
;
; get directory info
cd,'./',current=cwd
dirs=strsplit(cwd,'/',/extract,count=nd)
snnm=dirs(nd-1)
srvy=dirs(nd-2)
type=dirs(nd-3)
tlab=snnm+': '+type+', '+srvy+', '+imfil
;
; get file info
tmp=imfil
rute=gettok(tmp,'.')
;
; get fuv image
i=strpos(imfil,'-nd-')
if i ge 0 then begin
	fuvfil=imfil
	strput,fuvfil,'-fd',i
	list=file_search(fuvfil,count=nf)
	if nf eq 1 then begin
		fuvim=readfits(fuvfil,hfuv)
		fxdim=sxpar(hfuv,'NAXIS1')
		fydim=sxpar(hfuv,'NAXIS2')
		if fxdim ne 512 or fydim ne 512 then begin
			print,'Warning: FUV image of wrong dims: ',fxdim,fydim
			fmed=-9.999
			favg=-9.999
			fval=-9.999
			frms=-9.999
			fuvsky=-9.999
			fuvsig=-9.999
		endif else begin
			sky,fuvim,fuvsky,fuvsig,/silent	; get sky mode and sigma
			fmed=median(fuvim(252:258,252:258))
			fval=fuvim(255,255)
			favg=mean(fuvim(254:256,254:256))
			frms=stdev(fuvim(254:256,254:256))
		endelse
	endif else begin
		print,'Warning: no FUV image found: ',fuvfil
		fmed=-9.999
		favg=-9.999
		fval=-9.999
		frms=-9.999
		fuvsky=-9.999
		fuvsig=-9.999
	endelse
endif else begin
	print,'Error: NUV image not found: ',imfil
	return
endelse
;
; get image info
iim = readfits(imfil,h1)
iim(0)=mskval
xdim = sxpar(h1,'NAXIS1')
ydim = sxpar(h1,'NAXIS2')
if xdim ne 512 or ydim ne 512 then begin
	print,'Warning: NUV image of wrong dims: ',xdim,ydim
	if xdim lt 259 or ydim lt 259 then begin
		print,'Returning...'
		return
	endif
endif
oim = iim
sky,oim,smode,ssig,/silent		; get sky mode and sigma
print,' sky: ',smode,' +- ',ssig	; display sky values
if ssig le 0 then $
	s=0.01 $
else	s=ssig
m0=smode+15.0*s
print,'VAL: ',oim(255,255)
print,'AVG: ',mean(oim(254:256,254:256))
print,'MED: ',median(oim(252:258,252:258))
print,'RMS: ',stddev(oim(254:256,254:256))
;
; zoomed image
zim=rebin(oim(252:258,252:258),210,210,/sample)
z0=m0-scl*s
z1=m0+scl*s
zim(0) = z0
zim(1) = z1
window,2,xpos=0+(xdim*2+10),ypos=600+(ydim*2-210),title='ZOOM',xsi=210,ysi=210
tvscl,zim>z0<z1
;
window,0,xpos=0,ypos=600,xsize=xdim*2,ysize=ydim*2,title=tlab
;
tvscl,rebin(oim,xdim*2,ydim*2,/sample)>z0<z1
plot,[511,511],[511,511],psym=8,symsi=4,thick=5,/device,/noerase, $
	xsty=1,ysty=1,xran=[0,xdim*2-1],yran=[0,ydim*2-1], $
	pos=[0,0,xdim*2-1,ydim*2-1],color=128

stops = 'xq'			; keys that break out of this loop

while strpos(stops,q) lt 0 do begin

	q=strlowcase(get_kbrd(1)) ; get command

	case q of
		'-':	begin	; rescale with lower mean
			m0 = m0 - s
			z0=m0-scl*s
			z1=m0+scl*s
			print,m0
			tvscl,rebin(oim,xdim*2,ydim*2,/sample)>z0<z1
plot,[511,511],[511,511],psym=8,symsi=4,thick=5,/device,/noerase, $
	xsty=1,ysty=1,xran=[0,xdim*2-1],yran=[0,ydim*2-1], $
	pos=[0,0,xdim*2-1,ydim*2-1],color=128
			end

		'+':	begin	; rescale with higher mean
			m0 = m0 + s
			z0=m0-scl*s
			z1=m0+scl*s
			print,m0
			tvscl,rebin(oim,xdim*2,ydim*2,/sample)>z0<z1
plot,[511,511],[511,511],psym=8,symsi=4,thick=5,/device,/noerase, $
	xsty=1,ysty=1,xran=[0,xdim*2-1],yran=[0,ydim*2-1], $
	pos=[0,0,xdim*2-1,ydim*2-1],color=128
			end

		'a':	begin
				print,' '
				print,'Mark lower left corner'
				cursor,x0,y0,/device
				oplot,[0,xdim*2],[y0,y0]
				oplot,[x0,x0],[0,ydim*2]
				x0=x0/2
				y0=y0/2
				wait,0.3
				print,'Mark upper right corner'
				cursor,x1,y1,/device
				oplot,[0,xdim*2],[y1,y1]
				oplot,[x1,x1],[0,ydim*2]
				x1=x1/2
				y1=y1/2
				wait,0.3
				aim=oim(x0:x1,y0:y1)
				snval = oim(255,255)
				snmvl = median(oim(252:258,252:258))
				snavg = mean(oim(254:256,254:256))
				snrms = stddev(oim(254:256,254:256))
				w=where(aim gt mskval, nw)
				if nw gt 0 then begin
					vec=aim(w)
					vec=vec(sort(vec))
					acc=vec-vec
					acc(0)=vec(0)
					for i=1L,nw-1L do acc(i)=vec(i)+acc(i-1)
					nacc=n_elements(acc)
			    		window,1,xsize=900,ysize=600,$
			    			xpos=0+(xdim*2+10),ypos=600,$
			    			title=tlab
					plot,acc/max(acc),charsi=1.5,charthi=3,$
    						title=tlab,xthick=3,ythick=3, $
    						xtitle='Pixel #',xsty=1,ysty=1,$
					      xran=[-100,nacc+nacc/10],$
    				       ytitle='Normalized DN/Cumulative Value',$
				       		yran=[min(acc/max(acc)),1.05]
					oplot,vec/max(vec),psym=4
					oplot,[-100,1.d7],[0,0],linesty=1
					t=where(vec eq snval, nt)
					u=where(vec eq snmvl, nu)
					v=where(vec ge snavg, nv)
					if nt gt 0 then begin
						cv=acc(t(0))/max(acc)
						pv=t(0)
						snv=vec(pv)/max(vec)
					endif else begin
						cv=0.
						pv=0
						snv=0.
					endelse
					oplot,[pv,1.d7],[cv,cv],thick=3
					oplot,[pv,pv],[snv,cv],thick=3
					oplot,[pv,pv],[snv,snv],$
							psym=4,symsi=3,thick=3
					vlab=string(cv,form='(f6.3)')+' Pix 1x1'
    					print,'VAL: ',cv,snval, $
						form='(a,1x,f6.3,f9.5)'
					if nu gt 0 then begin
						cm=acc(u(0))/max(acc)
						pm=u(0)
						snm=vec(pm)/max(vec)
					endif else begin
						cm=0.
						pm=0
						snm=0.
					endelse
					oplot,[pm,1.d7],[cm,cm],thick=3, $
						linesty=2
					oplot,[pm,pm],[snm,cm],thick=3, $
						linesty=2
					oplot,[pm,pm],[snm,snm],psym=6,symsi=3,$
						thick=3
					mlab=string(cm,form='(f6.3)')+' Med 7x7'
    					print,'MED: ',cm,snmvl, $
						form='(a,1x,f6.3,f9.5)'
					if nv gt 0 then begin
						ca=acc(v(0))/max(acc)
						pa=v(0)
						sna=vec(pa)/max(vec)
					endif else begin
						ca=0.
						pa=0
						sna=0.
					endelse
					oplot,[pa,1.d7],[ca,ca],thick=3, $
						linesty=3
					oplot,[pa,pa],[sna,ca],thick=3, $
						linesty=3
					oplot,[pa,pa],[sna,sna],psym=5,symsi=3,$
						thick=3
					alab=string(ca,form='(f6.3)')+' Avg 3x3'
    					print,'AVG: ',ca,snavg, $
						form='(a,1x,f6.3,f9.5)'
					legend,[vlab,mlab,alab], $
						linesty=[0,2,3],$
						psym=[-4,-6,-5],symsi=[3,3,3],$
				      		thick=3,charsi=1.5,$
						charthi=3,box=0,spac=3.5
					wset,0
				endif else print,'No pixels left after masking.'
				wset,0
			end

		'd':	begin	; display masked image
			tvscl,rebin(oim,xdim*2,ydim*2,/sample)>z0<z1
plot,[511,511],[511,511],psym=8,symsi=4,thick=5,/device,/noerase, $
	xsty=1,ysty=1,xran=[0,xdim*2-1],yran=[0,ydim*2-1], $
	pos=[0,0,xdim*2-1,ydim*2-1],color=128
			end

		'i':	begin	; initialize mask
			oim=iim
			tvscl,rebin(oim,xdim*2,ydim*2,/sample)>z0<z1
plot,[511,511],[511,511],psym=8,symsi=4,thick=5,/device,/noerase, $
	xsty=1,ysty=1,xran=[0,xdim*2-1],yran=[0,ydim*2-1], $
	pos=[0,0,xdim*2-1,ydim*2-1],color=128
			end

		'c':	begin	; cursor mode
			print,'cursor mode: x,y < 10 to exit'
			cursor,x,y,/device
			wait,0.3
			while x gt 10 and y gt 10 do begin
				xout = x/2
				yout = y/2
				val=oim(xout,yout)
				print,'xyz: ',xout,yout,val,form='(a,i6,i6,f13.7)'
				cursor,x,y,/device
				wait,0.3
			endwhile
			print,'exit cursor mode'
			end

		'm':	begin	; mask a star profile
			cursor,x,y,/device,/nowait
			xim = x/2
			yim = y/2
			if xim ge 7 and yim ge 7 and $
			   xim lt (xdim - 16) and $
			   yim lt (ydim - 16) then begin
			    msim = iim(xim-7:xim+8,yim-7:yim+8)
			    cntrd,msim,7,7,xc,yc,5,/silent
			    dist_circle,r,16,xc,yc
			    xc = xc + float(xim-7)
			    yc = yc + float(yim-7)
			    sky,msim,mmn,msig,/silent
			    print,string(13B),x,y,xim,yim,xc,yc,mmn,msig, $
			    	form='(a1,i6,i6,i6,i6,f9.3,f9.3,2f10.5)'
			    dsim = msim - mmn
			    w=where(msim ge mmn+2.*msig and r lt 8., nm)
			    if nm gt 0 then begin
			    	msim(w)=mskval
				oim(xim-7:xim+8,yim-7:yim+8)=msim(*,*)
			    endif
;			    wset,0
			    tvscl,rebin(oim,xdim*2,ydim*2,/sample)>z0<z1
plot,[511,511],[511,511],psym=8,symsi=4,thick=5,/device,/noerase, $
	xsty=1,ysty=1,xran=[0,xdim*2-1],yran=[0,ydim*2-1], $
	pos=[0,0,xdim*2-1,ydim*2-1],color=128
			endif else print,'ERROR: cursor out of bounds'
			end

		'p':	begin	; plot a star profile
			cursor,x,y,/device,/nowait
			xim = x/2
			yim = y/2
			if xim ge 15 and yim ge 15 and $
			   xim lt (xdim - 32) and $
			   yim lt (ydim - 32) then begin
			    dsim = reform(iim(xim-15:xim+16, $
			    	yim-15:yim+16),32,32)
			    cntrd,dsim,15,15,xc,yc,5,/silent
			    dist_circle,r,32,xc,yc
			    xc = xc + float(xim-15)
			    yc = yc + float(yim-15)
			    lab=string(fix(xc+.5),form='(i4)') + ',' + $
				string(fix(yc+.5),form='(i4)')
			    print,x,y,xim,yim,xc,yc, $
			    	form='(i6,i6,i6,i6,f9.3,f9.3)'
			    sky,dsim,dmn,dsig,/silent
			    dsim = dsim - dmn
			    yr0 = min(dsim) - (max(dsim)-min(dsim))*0.1
			    yr1 = max(dsim) + (max(dsim)-min(dsim))*0.1
			    window,1,xsize=1024,ysize=900,$
			    	xpos=0+(xdim*2+10),ypos=600,$
			    	title=tlab
			    pwin = 0
			    plot,r,dsim,psym=1, $
			    	xtitle='R(pix)',ytitle='DN',$
				xran=[-1,20],xsty=1, $
				yran=[yr0,yr1],ysty=1,$
				title='Profile xy: '+ $
				lab+' Sky: '+string(dmn,form='(f7.1)'),$
				charsize=1.5,xthick=2,ythick=2, $
				charthick=2
			    oplot,[-1,20],[0,0],linesty=2
			    wset,0
			endif else print,'ERROR: cursor out of bounds'
			end

		'r':	begin	; rescale with larger stretch
			scl = scl + 1.0
			z0=m0-scl*s
			z1=m0+scl*s
			print,scl
			tvscl,rebin(oim,xdim*2,ydim*2,/sample)>z0<z1
plot,[511,511],[511,511],psym=8,symsi=4,thick=5,/device,/noerase, $
	xsty=1,ysty=1,xran=[0,xdim*2-1],yran=[0,ydim*2-1], $
	pos=[0,0,xdim*2-1,ydim*2-1],color=128
			end

		's':	begin	; rescale with smaller stretch
			scl = scl - 1.0
			z0=m0-scl*s
			z1=m0+scl*s
			print,scl
			tvscl,rebin(oim,xdim*2,ydim*2,/sample)>z0<z1
plot,[511,511],[511,511],psym=8,symsi=4,thick=5,/device,/noerase, $
	xsty=1,ysty=1,xran=[0,xdim*2-1],yran=[0,ydim*2-1], $
	pos=[0,0,xdim*2-1,ydim*2-1],color=128
			end
		'z':	begin
			wset,2
			zim=rebin(oim(252:258,252:258),210,210,/sample)
			z0=m0-scl*s
			z1=m0+scl*s
			zim(0) = z0
			zim(1) = z1
			tvscl,zim>z0<z1
			rdpix,zim
			wset,0
			end

	else:	; ignore other keys

	endcase
endwhile
;
; log info?
read,'Log analysis? (y/n): ',q
if strmid(strlowcase(q),0,1) eq 'y' then begin
;
; log file exists?
	lfil='~/snsites/'+type+'_'+srvy+'.pxpos'
	if not file_test(lfil) then begin
		openw,lu,lfil,/get_lun
		printf,lu,'# SN        Ty  Srv  cumval  cummed  cumavg  nuvpxval  nuvpxmed  nuvpxavg  nuvpxrms  nuvskymod nuvskysig fuvpxval  fuvpxmed  fuvpxavg  fuvpxrms  fuvskymod fuvskysig im'
	endif else openw,lu,lfil,/append,/get_lun
	printf,lu,snnm,type,srvy,cv,cm,ca,snval,snmvl,snavg,snrms,smode,ssig, $
		fval,fmed,favg,frms,fuvsky,fuvsig,imfil, $
		format='(a8,a6,a5,3f8.4,12f10.6,2x,a)'
	free_lun,lu
	psfile,rute+'_pxpos'
	!p.font=1
	plot,acc/max(acc),charsi=1.5,charthi=3,title=tlab,xthick=3,ythick=3, $
		xtitle='Pixel #', xran=[-100,nacc+nacc/10],xsty=1, $
		ytitle='Normalized DN/Cumulative Value', $
		yran=[min(acc/max(acc)),1.05],ysty=1
	oplot,vec/max(vec),psym=4
	oplot,[-100,1.d7],[0,0],linesty=2
	t=where(vec eq snval, nt)
	if nt gt 0 then begin
		cv=acc(t(0))/max(acc)
		pv=t(0)
    		oplot,[pv,1.d7],[cv,cv],thick=3
		oplot,[pv,pv],[snv,cv],thick=3
		oplot,[pv,pv],[snv,snv],psym=4,symsi=3,thick=3
	endif
	m=where(vec eq snmvl, nm)
	if nm gt 0 then begin
		cm=acc(m(0))/max(acc)
		pm=m(0)
		oplot,[pm,1.d7],[cm,cm],thick=3,linesty=2
		oplot,[pm,pm],[snm,cm],thick=3,linesty=2
		oplot,[pm,pm],[snm,snm],psym=6,symsi=3,thick=3
	endif
	v=where(vec ge snavg, nv)
	if nv gt 0 then begin
		ca=acc(v(0))/max(acc)
		pa=v(0)
		sna=vec(pa)/max(vec)
		oplot,[pa,1.d7],[ca,ca],thick=3,linesty=3
		oplot,[pa,pa],[sna,ca],thick=3,linesty=3
		oplot,[pa,pa],[sna,sna],psym=5,symsi=3,thick=3
	endif
	legend,[vlab,mlab,alab],linesty=[0,2,3],psym=[-4,-6,-5], $
		symsi=[3,3,3],thick=3,charsi=1.5,charthi=3,box=0,spac=3.5
	psclose
	!p.font=-1
endif
;
print,'Image: ',p+1,' / ',nn
;
return
end
