pro cldsrvy, blink=blink, playback=playback, start=start, $
	scales=scales
;+
;	cldsrvy - routine to display nova survey data and allow interactive
;		blinking and recording of nova candidates to OFILE.
;
; KEYWORDS:
;	blink - set to begin each subimage blinking, value is blink rate
;		in seconds (defaults to 0.4 sec if no value given)
;	playback - set to enter 'playback' mode to visit all candidates in
;			file
;	start - set to the sub-field to start on (1-100)
;
;	read - set to read in data, otherwise use data from input
;
; HISTORY:
;	v1.0, jdn@amnh.org, 08jan01.
;
;-
; check inputs
;if n_params(0) lt 1 then begin
;	print,'NSURVEY: Usage - cldsrvy'
;	return
;endif
;
; check keywords
start_blink = keyword_set(blink)
start_blink = (1 eq 0)
;
if keyword_set(blink) then $
	blinkrate = blink $
else	blinkrate = 1.5
;
play_back = keyword_set(playback)
;
; 
; variables
xin = [0] & yin = [0]
cmntin = ['']
tittin = ['']
fldin = [0]
rec = ''
cfmt = '(i6,i6,i6,3x,a,2x,a)'
cfmt2 = '(i6,i6,3x,a,2x,a,1x,a)'
cfmt3 = '(i6,i6,i6,i6,3x,a,2x,a)'
;
; get input image list
flist=['']
rec=''
openr,1,'list'
while not eof(1) do begin
	readf,1,rec
	if strmid(rec,0,1) ne '#' then flist = [flist,rec]
endwhile
close,1
flist = flist(1:*)
fldno = fix(stregex(flist(0),'[0-9]+',/extract))
if strpos(flist(0),'lmc') ge 0 then begin
	cld='LMC' 
	starts = [ 1, 134,  427, 438, 328, 235, 899, 709, 301, $
		      334, 1259, 481, 339, 210, 444, 235, 202 ]
endif else begin
	cld='SMC'
	starts = [ 1, 948, 763, 1183, 615 ]
endelse
;
do_start = keyword_set(start)
if do_start then begin
	if start le 1 then begin
		start=starts(fldno)
	endif
endif
;
s = size(flist)
if s(0) ne 1 and s(1) ne 2 and s(2) ne 7 then begin
	print,'NSURVEY: Error - malformed image list: ',flist
	return
endif
nim = s(3)
ntim = nim
tlist = flist
;
; set titles
titles = tlist
;
; get scale factors
if keyword_set(scales) then begin
	s = size(scales)
	if s(0) eq 1 and s(1) eq ntim then begin
		pscls = float(scales)
	endif else begin
		print,'NSURVEY: Error - malformed scales vector: ',scales
		print,'Using default of unit scaling.'
		pscls = fltarr(ntim) + 1.0
	endelse
endif else $
	pscls = fltarr(ntim) + 1.0
;
; set up usersym (circle)
a=[findgen(16)*(!pi*2/16.),0.]
usersym,cos(a),sin(a)
;
; input/output file
ofile = 'cldsrvy_f'+string(fldno,form='(i02)')+'.dat'
list=findfile(ofile,count=nfiles)	; does current file exist?

nin = 0					; number of objects input from file

if nfiles le 0 then $			; no current file, so open a new one
	openw,olun,ofile,/get_lun $

else	begin				; current file exists
	openu,olun,ofile,/get_lun

	while not eof(olun) do begin	; read in existing objects
		readf,olun,rec

		if strpos(rec,'#') lt 0 then begin	; skip comments
			xin = [xin,fix(gettok(rec,' '))]
			yin = [yin,fix(gettok(rec,' '))]
			temp = gettok(rec,' ')
			fldin = [fldin,fix(stregex(temp,'[0-9]+',/extract))]
			tittin = [tittin,temp]
			cmntin = [cmntin,rec]
		endif
	endwhile

	nin = n_elements(xin) - 1	; how many objects?

	if nin gt 0 then begin		; reformat vectors
		xin = xin(1:nin)
		yin = yin(1:nin)
		fldin = fldin(1:nin)
		tittin = tittin(1:nin)
		cmntin = cmntin(1:nin)
					; print them out
		for i=0,nin-1 do print,xin(i),yin(i),fldin(i),tittin(i), $
			cmntin(i), format=cfmt
	endif

	shown=intarr(nin)		; keep track of what has been shown

endelse
;
; can we play anything back?
if play_back and nin eq 0 then begin
	free_lun,olun
	print,'NSURVEY: No objects to play back, returning.'
	return
endif
;
; read fits or imh?
if strpos(tlist(0),'fits') gt 0 then begin
;
; get image info
	im = readfits(tlist(0),h1)
	xdim = sxpar(h1,'NAXIS1')
	ydim = sxpar(h1,'NAXIS2')
	data=fltarr(ntim,xdim,ydim)
	data(0,*,*) = im
;
endif else begin
    	irafread,im,h1,tlist(0)
	xdim = sxpar(h1,'NAXIS1')
	ydim = sxpar(h1,'NAXIS2')
	data=fltarr(ntim,xdim,ydim)
	data(0,*,*) = im
endelse
;
; read in images
for i=1,ntim-1 do begin
	if strpos(tlist(i),'fits') gt 0 then $
		data(i,*,*) = readfits(tlist(i),h) $
	else begin
		irafread,im,h,tlist(i)
		data(i,*,*) = im
	endelse
endfor
;
; get spacing
space = 450
zspace = 2 * space
grid = get_grid(space, xdim, ydim, ngrid)

;
; zoomed windows
zwins = intarr(ntim) - 1
pwin  = -1
;
; get starting object or sub-field

ip = 0					; sub-field, or list pointer

if play_back then begin			; start with first object

	if do_start then begin		; unless start candidate specified
	    if start le nin then begin
		while ip lt start-1 do begin
			shown(ip) = 1
			ip = ip + 1	; loop to start
		endwhile
	    endif else begin
		print,'NSURVEY: Error - start out of range: ',start
		return
	    endelse
	endif 

	ix = fix(xin(ip)+0.5)
	iy = fix(yin(ip)+0.5)
	grd = get_index(space,xdim,ydim,ix,iy)
	x0 = grd(0)
	x1 = grd(1)
	y0 = grd(2)
	y1 = grd(3)

endif	else begin			; start with first field

	if do_start then begin		; unless start field specified
	    if start le ngrid then begin
		while ip lt start-1 do ip = ip + 1	; loop to start
	    endif else begin
		print,'NSURVEY: Error - start out of range: ',start
		return
	    endelse
	endif 

	x0 = grid(0,ip)
	x1 = grid(1,ip)
	y0 = grid(2,ip)
	y1 = grid(3,ip)
	ix = grid(4,ip)
	iy = grid(5,ip)

endelse

;
; loop over x,y sections or playback list
done = (1 eq 0)				; tells us when to quit
do_win = (1 eq 1)			; tells us when to create windows
while not done do begin
	nc = 0
;
; extract subims
	sub = rebin(data(*,x0:x1,y0:y1),ntim,zspace,zspace,/sample)
;
; check for marked objects in frame
	if nin gt 0 then begin		; objects were read in

					; find out which ones are in subfield
		tin = where(xin lt x1 and xin gt x0 and $
			  yin lt y1 and yin gt y0 and fldin eq fldno,nm)

 		if nm gt 0 then begin	; we've got some in the subfield
					; so print them out
			for i=0,nm-1 do $
				print,tin(i)+1,xin(tin(i)),yin(tin(i)), $
					fldin(tin(i)), tittin(tin(i)), $
					cmntin(tin(i)), format=cfmt3
			shown(tin) = 1		; mark these as shown

			; check for nova candidates
			tmp = strpos(cmntin(tin),'nova')
			cin = where(tmp ge 0, nc)
			if nc gt 0 then begin
				xnv = xin(tin(cin))
				ynv = yin(tin(cin))
			endif
			; check for var candidates
			tmp = strpos(cmntin(tin),'var?')
			cin = where(tmp ge 0, nv)
			if nv gt 0 then begin
				xva = xin(tin(cin))
				yva = yin(tin(cin))
			endif
		endif

	endif	else nm = 0		; no objects in subfield
;
; print starting position
print,' '
if play_back then $
	print,'cand:'+string(ip+1,form='(i4)')+'/'+string(nin,form='(i4)') $
else	print,'sub-field:'+string(ip+1,form='(i3)')+$
		'/'+string(ngrid,form='(i3)')
	print,'x,y: ',ix,iy
;
; display subims

	md=fltarr(nim)			; vector of median sky values
	sg=fltarr(nim)			; vector of sky sigma values

	scl = fltarr(nim) + 7.0	; initial stretch for display
	;scl(0) = 5.0
	;scl(1) = 4.0
	;scl(2) = 5.0	;13.33

	for i=(nim-1),0,-1 do begin	; loop over subims

		if do_win then $	; create a window
			window,i,xsize=zspace,ysize=zspace, $
				 xpos=0,ypos=150,title=titles(i),retain=2 $
		else	begin		; just set and show the current window
			wset,i	
			wshow,i
		endelse

		im = sub(i,*,*)			; get current subim
		t = where(im gt 10 and im lt 1.e6,ngood) ; ignore bad pixels
		if ngood gt 3 then begin
			sky,im(t),m,s,/silent	; get sky mode and sigma
			if s lt 0.0 then $	; check sigma
				ims,im(t),m,s,siglim=3.1
		endif else begin
			m = 0
			s = 1.0
		endelse

		md(i) = m & sg(i) = s	; store them in vectors

					; display image
		;tvscl,sub(i,*,*)>(md(i)-(scl(i)/5.0)*sg(i))<(md(i)+scl(i)*sg(i))
		tvscl,sub(i,*,*)>(md(i)-(scl(i))*sg(i))<(md(i)+scl(i)*sg(i))

		print,titles(i),' sky: ',m,' +- ',s	; display sky values

		if nm gt 0 then begin	; plot input objects

			xplt = ( xin(tin) - x0 ) * 2.0	; rescale to subim
			yplt = ( yin(tin) - y0 ) * 2.0

			plot,xplt,yplt,/device,/noerase, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
		pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=7, $
			color=colordex('white')

			; plot nova candidates in yellow
			if nc gt 0 then begin
				xnplt = (xnv - x0 ) * 2.0
				ynplt = (ynv - y0 ) * 2.0
				oplot,xnplt,ynplt,psym=8,symsize=7, $
					color=colordex('yellow')
			endif

			; plot var candidates in green
			if nv gt 0 then begin
				xnplt = (xva - x0 ) * 2.0
				ynplt = (yva - y0 ) * 2.0
				oplot,xnplt,ynplt,psym=8,symsize=7, $
					color=colordex('green')
			endif

		endif else $		; no objects, but set plot scale anyway

			plot,[0,0],[0,0],/device,/noerase,/nodata, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
		pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=1

	endfor				; loop over subims
	print,' '
;	do_win = (1 eq 0)		; turn off window making

;
; loop and get keyboard input

	if start_blink then begin		; should we start by blinking?
		q = 'b'
		blinking = (1 eq 1)
	endif else begin
		q = ' '
		blinking = (1 eq 0)
	endelse

	stops = 'nlxg'			; keys that break out of this loop

	while strpos(stops,q) lt 0 do begin

		if not blinking then q=strlowcase(get_kbrd(1)) ; get command

		case q of
			'-':	begin	; rescale with lower mean
				i = !D.WINDOW
				md(i) = md(i) - sg(i)
	tvscl, sub(i,*,*)>(md(i)-scl(i)*sg(i))<(md(i)+scl(i)*sg(i))
	if nm gt 0 then begin
		xplt = ( xin(tin) - x0 ) * 2.0
		yplt = ( yin(tin) - y0 ) * 2.0
		plot,xplt,yplt,/device,/noerase, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
			pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=7
	endif else $
		plot,[0,0],[0,0],/device,/noerase,/nodata, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
			pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=1
				end

			'+':	begin	; rescale with higher mean
				i = !D.WINDOW
				md(i) = md(i) + sg(i)
	tvscl, sub(i,*,*)>(md(i)-scl(i)*sg(i))<(md(i)+scl(i)*sg(i))
	if nm gt 0 then begin
		xplt = ( xin(tin) - x0 ) * 2.0
		yplt = ( yin(tin) - y0 ) * 2.0
		plot,xplt,yplt,/device,/noerase, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
			pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=7
	endif else $
		plot,[0,0],[0,0],/device,/noerase,/nodata, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
			pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=1
				end

			'1':	begin	; show frame 1
				wset,0
				wshow,0
				end

			'2':	begin	; show frame 2
				wset,1
				wshow,1
				end
					; show frame 3 if exists
			'3':	if nim ge 3 then begin
					wset,2
					wshow,2
				endif
					; show frame 4 if exists
			'4':	if nim ge 4 then begin
					wset,3
					wshow,3
				endif
					; show frame 5 if exists
			'5':	if nim ge 5 then begin
					wset,4
					wshow,4
				endif
					; show frame 6 if exists
			'6':	if nim ge 6 then begin
					wset,5
					wshow,5
				endif

			'b':	begin	; blink images
				q = ''
				i = 0
				blinking = (1 eq 1)
				while blinking do begin
					wset,i
					wshow,i
					wait,blinkrate
					i = i + 1
					if i eq nim then i = 0
					q=strlowcase(get_kbrd(0))
					if q eq 'b' or q eq 'x' or $
					   q eq 'n' or q eq 'l' then $
						blinking = (1 eq 0)
					if q eq 'z' then break
				endwhile
				wset,i
				wshow,i
				end

			'c':	begin	; get cursor x,y
				i = !D.WINDOW
				print,'cursor mode: x,y < 10 to exit'
				cursor,x,y,/device
				wait,0.3
				while x gt 10 and y gt 10 do begin
					xout = x0 + x/2
					yout = y0 + y/2
					print,'xyz: ',xout,yout, $
						data(i,xout,yout), $
						form='(a,i6,i6,f12.2)'
					cursor,x,y,/device
					wait,0.3
				endwhile
				print,'exit cursor mode'
				end

			'g':	begin	; go to specific location
				inr=''
				read,'x,y: ',inr
				if inr ne '' then begin
					ix = fix(gettok(inr,' '))
					iy = fix(gettok(inr,' '))
				endif
				end

			'h':	begin	; print help
				print,' '
				print,'1-5: show frame #n, b: toggle blink' + $
				', c: cursor, g: goto xy, h: help'
				print,'m: mark, p: plot, r: rescale +, '+ $
				's: rescale -, z: zoom, n: next, l:last, x - exit'
				end

			'i':	begin	; get photometry data
				cursor,x,y,/device,/nowait
				xx = x0 + x/2
				yy = y0 + y/2
				;read,'Aper radius (px): ',apr
				apr=2.5
				print,' '
				print,'      im      x       y     mag   err      sky    err'
				pfmt = '(a8,1x,2f8.2,f7.2,a2,f4.2,f9.2,a2,f5.2)'
				for j = 0,ntim-1 do begin
				    temp=reform(data(j,*,*),xdim,ydim)*pscls(j)
				    cntrd,temp,xx,yy,xc,yc,apr,/silent
				    aper,temp,xc,yc,mags,errap,sky,$
				    	skyerr,1.0,apr,[10.,15.],[-1000,20000],$
				    	/silent
				  print,titles(j),xc,yc,mags(0),'+-',errap(0), $
				    	sky(0),'+-',skyerr(0),format=pfmt
				endfor
				print,'Done.'
				end

			'm':	begin	; mark an object
				print,'mark object'
				cursor,x,y,/device,/nowait
				xout = x0 + x/2
				yout = y0 + y/2
				print,'xy: ',xout,yout,form='(a,i6,i6)'
				cmnt = ''
				read,'comment: ',cmnt
				if strtrim(cmnt,2) ne '' then begin
					i = !D.WINDOW
					print,xout,yout,titles(i),cmnt, $
						'('+strn(ip+1)+')', format=cfmt2
					printf,olun,xout,yout,titles(i),cmnt, $
						'('+strn(ip+1)+')', format=cfmt2
					if strpos(cmnt,'nova') ge 0 then $
						plots,[x,x],[y,y],/dev,psym=8, $
						symsiz=7, $
						color=colordex('yellow') $
					else	plots,[x,x],[y,y],/dev,psym=8, $
						symsiz=7,$
						color=colordex('white')
					xin=[xin,xout]
					yin=[yin,yout]
					fldin=[fldin,fldno]
					tittin=[tittin,titles(i)]
					cmntin=[cmntin,cmnt]
					nin=nin+1
				endif
				end
				
			'p':	begin	; plot a star profile
				i = !D.WINDOW
				cursor,x,y,/device,/nowait
				xim = x0 + x/2
				yim = y0 + y/2
				if xim ge 15 and yim ge 15 and $
				   xim lt (xdim - 32) and $
				   yim lt (ydim - 32) then begin
				    dsim = reform(data(i,xim-15:xim+16, $
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
				    window,(ntim*2),xsize=600,ysize=600,$
				    	xpos=(zspace+10),ypos=150,$
				    	title=titles(i)
				    pwin = 0
				    plot,r,dsim,psym=1, $
				    	xtitle='R(pix)',ytitle='DN',$
					xran=[-1,20],xsty=1, $
					yran=[yr0,yr1],ysty=1,$
					title='Profile: '+titles(i)+' xy: '+ $
					lab+' Sky: '+string(dmn,form='(f7.1)'),$
					charsize=1.5,xthick=2,ythick=2, $
					charthick=2
				    oplot,[-1,20],[0,0],linesty=2
				    wset,i
				endif else print,'ERROR: cursor out of bounds'
				end

			'r':	begin	; rescale with larger stretch
				i = !D.WINDOW
				scl(i) = scl(i) + 1.0
				;if scl(i) gt 10.0 then scl(i) = 5.0
	tvscl, sub(i,*,*)>(md(i)-scl(i)*sg(i))<(md(i)+scl(i)*sg(i))
	if nm gt 0 then begin
		xplt = ( xin(tin) - x0 ) * 2.0
		yplt = ( yin(tin) - y0 ) * 2.0
		plot,xplt,yplt,/device,/noerase, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
			pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=7
	endif else $
		plot,[0,0],[0,0],/device,/noerase,/nodata, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
			pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=1
				end

			's':	begin	; rescale with smaller stretch
				i = !D.WINDOW
				scl(i) = scl(i) - 1.0
				if scl(i) lt 1.0 then scl(i) = 7.0
	tvscl, sub(i,*,*)>(md(i)-scl(i)*sg(i))<(md(i)+scl(i)*sg(i))
	if nm gt 0 then begin
		xplt = ( xin(tin) - x0 ) * 2.0
		yplt = ( yin(tin) - y0 ) * 2.0
		plot,xplt,yplt,/device,/noerase, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
			pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=7
	endif else $
		plot,[0,0],[0,0],/device,/noerase,/nodata, $
		xran=[0,(zspace-1)], yran=[0,(zspace-1)], xsty=1, ysty=1, $
			pos=[0,0,(zspace-1),(zspace-1)], psym=8, symsize=1
				end

			'z':	begin	; zoom and display ims
				i = !D.WINDOW
				cursor,x,y,/device,/nowait
				if x ge 31 and y ge 31 and $
				   x lt (zspace-32) and $
				   y lt (zspace-32) then begin
				   sup = rebin(sub(*,x-31:x+32,y-31:y+32),ntim,$
				   	128,128,/sample)
				   for j = 0,ntim-1 do begin
				   	window,j+ntim,xsize=128,ysize=128, $
				    	   xpos=zspace+9, $
					   ypos=(zspace-4) - (j*158), $
				   	   title=titles(j)
				   	supim = sup(j,*,*)
					t = where(supim gt 0)
					sky,supim(t),m,s,/silent
					lo = (m - 7.0*s)
					hi = (m + 7.0*s)
					tvscl,supim>lo<hi
					zwins(j) = j+ntim
				   endfor
			        endif else print,'ERROR: cursor out of bounds'
				wset,i
				if blinking then q='b'
				end

		else:	; ignore other keys

		endcase
	endwhile
;
; check our ending conditions
	if q eq 'x' then $	; get outta dodge
		done = (1 eq 1) $

	else begin
		for k = 0,ntim-1 do begin
			if zwins(k) ge 0 then begin
				wdelete,zwins(k)
				zwins(k) = -1
			endif
		endfor
		if pwin ge 0 then begin
			wdelete,(ntim*2)
			pwin = -1
		endif
		if q ne 'g' then begin	; go to next object/sub-frame

			if q eq 'n' then begin
			    ip = ip + 1
			    if ip lt nin then $
				while play_back and shown(ip) eq 1 do $
					if ip ge nin-1 then $
						break $
					else	ip = ip + 1
			endif else ip = ip - 1 > 0

			if play_back then begin	; next object
				if ip lt nin then begin
					ix = fix(xin(ip)+0.5)
					iy = fix(yin(ip)+0.5)
					grd = get_index(space,xdim,ydim,ix,iy)
					x0 = grd(0)
					x1 = grd(1)
					y0 = grd(2)
					y1 = grd(3)
				endif else done = (1 eq 1)	; all done

			endif else begin	; next subframe
				if ip gt ngrid - 1 then $	;finished
					done = (1 eq 1) $
				else	begin
					x0 = grid(0,ip)
					x1 = grid(1,ip)
					y0 = grid(2,ip)
					y1 = grid(3,ip)
					ix = grid(4,ip)
					iy = grid(5,ip)
				endelse
			endelse
		endif else begin
			grd = get_index(space,xdim,ydim,ix,iy)
			x0 = grd(0)
			x1 = grd(1)
			y0 = grd(2)
			y1 = grd(3)
		endelse
	endelse
endwhile
;
; close up file
free_lun,olun
;
return
end	; pro cldsrvy
