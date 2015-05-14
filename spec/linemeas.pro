pro linemeas,ifl,wid,llist,help=help
;
; measure emission lines
;
; check keywords and parameters
if keyword_set(help) then begin
	print,'Usage: linemeas,file,zoom_width,line_list'
	return
endif
;
if n_params(0) lt 1 then begin
	ifl=''
	read,'filename: ',ifl
endif
if strpos(ifl,'.') lt 0 then begin
	rfl = ifl+'.fits'
	lfl = ifl+'.llog'
endif else begin
	rfl = ifl
	tmp = ifl
	rute = gettok(tmp,'.')
	lfl = rute+'.llog'
endelse
if n_params(0) lt 2 then begin
	wid=50
	read,'zoom width (pixels): ',wid
endif
if n_params(0) lt 3 then begin
	llist=''
	read,'Line list: ',llist
endif
;
; read line list
if file_test(llist) then $
	readcol,llist,lwav,lnam,form='f,a',/silent $
else	begin
	print,'***Line list file not found: ',llist
	return
endelse
;
; read spectrum
spec=mrdfits(rfl,0,h,/silent)
;
; get wavelength solution from header
w0=sxpar(h,'CRVAL1')
dw=sxpar(h,'CDELT1')
nw=sxpar(h,'NAXIS1')
wave=findgen(nw)*dw + w0
;
; get errors per pixel
; blue side
g=where(wave lt 5075., ng)
if ng gt 0 then begin
	ims,spec(g),bspmn,bspsg
	print,'BLUE - Avg Flux error/pixel: ',bspsg,format='(a,g11.4)'
endif
g=where(wave gt 5590., ng)
if ng gt 0 then begin
	ims,spec(g),rspmn,rspsg
	print,'RED  - Avg Flux error/pixel: ',rspsg,format='(a,g11.4)'
endif
;
; data units
bu=sxpar(h,'BUNIT')
;
; plot ranges
mxsp = max(spec)
mnsp = -(mxsp/10.)
mxsp = mxsp + mxsp/10.
;
; check log file
cents=fltarr(1000)	; line centers
nln = 0l		; number of lines measured
if file_test(lfl) then begin
	q=''
	read,'Log file exists, append? (Y/n): ',q
	if strupcase(strmid(q,0,1)) eq 'N' then $
		openw,ol,lfl,/get_lun $
	else begin
		readcol,lfl,jnk1,jnk2,incents,jnk3,format='a,f,f,f',/silent
		nln = n_elements(incents)
		cents(0:(nln-1)) = incents(*)
		openw,ol,lfl,/get_lun,/append
	endelse
endif else openw,ol,lfl,/get_lun
;
; instructions
print,'click on line to measure, then set blue limit then red limit'
print,'click on left end of plot to abort line or quit.'
;
; print header
printf,ol,'# LINEMEAS: '+systime(0)
printf,ol,'# file: ',rfl,', line list: ',llist
printf,ol,'# BLUE - Avg Flux error/pixel: ',bspsg,format='(a,g11.4)'
printf,ol,'# RED  - Avg Flux error/pixel: ',rspsg,format='(a,g11.4)'
printf,ol,'# Flux unit: ',bu
printf,ol,'#     Line    Centr1    Centr2    LabCentr   V(kms)   Npx  Flux       Error        S/N'
print,'      Line    Centr1    Centr2    LabCentr   V(kms)   Npx  Flux       Error        S/N'
;
; plot setups
th=3
si=1.75
;
; loop until Quit
q = ''
while strupcase(strtrim(q,2)) ne 'Q' do begin
;
; plot spectrum
    plot,wave,spec,xsty=1,xtitle='Wavelength(A)',yran=[mnsp,mxsp],ysty=1, $
	ytitle='Flux: '+bu,charthi=th,charsi=si,xthick=th,ythick=th,titl=rfl, $
	position=[0.10,0.1,0.98,0.95]
;
; overplot measured lines
    if nln gt 0 then $
	for i=0,nln-1 do $
		oplot,[cents(i),cents(i)],[-1,1],linesty=5
;
; read line selection
    cursor,x,y,1,/data
;
; check for Quit
    if x-w0 gt 25. then begin
	xx=fix((x-w0)/dw)	; convert to pixels
	l0=xx-wid>0		; set measurement window
	l1=xx+wid<(nw-1)
	mwv=wave(l0:l1)		; extract wavelengths, spectrum
	mw0=mwv(0)
	msp=spec(l0:l1)
;
; plot zoomed spectrum
	plot,mwv,msp,xsty=1,xtitle='Wavelength(A)',titl=rfl,ytitle='Flux: '+bu,$
		charthick=th,charsi=si,xthick=th,ythick=th,$
		position=[0.12,0.1,0.98,0.95]
	oplot,[-1.e6,1.e6],[0,0]
	wait,1	; avoid double click
;
; read limits
	cursor,xb,yb,1,/data
	xxb=fix(0.5+(xb-mw0)/dw)
;
; check for abort
	if xxb gt 0 then begin
	    oplot,[xb,xb],[-1.,1],linesty=5
	    wait,1
	    cursor,xr,yr,1,/data
	    xxr=fix(0.5+(xr-mw0)/dw)
	    oplot,[xr,xr],[-1.,1],linesty=5
;
; re-order correctly
	    if xxb gt xxr then begin
		tmp=xxb
		xxb=xxr
		xxr=tmp
	    endif
;
; convert to pixels
	    mwv=mwv(xxb:xxr)
	    msp=msp(xxb:xxr)
;
; check for positive values
	    t=where(msp gt 0., n)

	    if n ge 1 then begin
;
; calculate line fluxes and centroids
		cent2=total(mwv(t)*msp(t)^(1.5)) / total(msp(t)^(1.5))
		line=total(msp)*(max(mwv)-min(mwv))/float(xxr-xxb)
		cent1=total(msp*mwv)/total(msp)
;
; identify line
		dif = cent2-lwav
		t=where(abs(dif) lt 10.,nf)
		if nf eq 1 then begin
			t=where(abs(dif) eq min(abs(dif)))
			name = lnam(t)
			lcent = lwav(t)
			v = !phys_c * (dif(t)/lcent)
		endif else if nf gt 1 then begin
			for i=0,nf-1 do print,i,lwav(t(i)),lnam(t(i)), $
				format='(i3,f10.3,2x,a)'
			lnum = 0
			read,'Enter line number: ',lnum
			name = lnam(t(lnum))
			lcent = lwav(t(lnum))
			v = !phys_c * (dif(t(lnum))/lcent)
		endif else begin
			name = 'UnK'
			lcent = -999.
			v = -999.
		endelse
		npx = (xxr - xxb) + 1
;
; get flux error
		dif = abs(wave - cent2)
		cpx = where(dif eq min(dif))
		vec = [spec((cpx-(npx+wid))>0:(cpx-npx)), $
		       spec((cpx+npx):(cpx+(npx+wid))<(nw-1))]
		ims,vec,vmn,vsg
		flxe = sqrt(float(npx) * vsg^2)
;
; plot centers
		oplot,[cent1,cent1],[0,max(msp)],linesty=2
		oplot,[cent2,cent2],[0,max(msp)],linesty=1
		fmt='(a10,2x,4f10.3,i5,2g11.4,f7.1)'
		printf,ol,name,cent1,cent2,lcent,v,npx,line,flxe,line/flxe, $
			form=fmt
		print,name,cent1,cent2,lcent,v,npx,line,flxe,line/flxe, $
			format=fmt
		cents(nln) = cent1
		nln = nln + 1
	    endif else begin
		print,'No positive points in line'
	    endelse
	endif		; check for abort line
	wait,0.5	; wait to avoid double click
    endif else q='Q'	; check for Quit
endwhile		; loop
;
free_lun,ol
;
return
end

