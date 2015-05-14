pro daocmd, field, id, m1filt, m1c, m1ce, m2filt, m2c, m2ce, cands=cands
; m1
file1=''
read,'phot file 1 (no ext): ',file1

field=strmid(file1,0,6)

root=gettok(file1,'.')

; get m1 filter

m1filt = strmid(root,6,1)

; get m1 photometry

openr,lun,root+'.dat',/get_lun
m1ndates = 0L
m1nstars = 0L
readu,lun,m1ndates,m1nstars
m1jd = dblarr(m1ndates)
readu,lun,m1jd
;
m1id	= lonarr(m1nstars)
m1x	= fltarr(m1nstars)
m1y	= fltarr(m1nstars)
m1	= fltarr(m1nstars)
m1e	= fltarr(m1nstars)
m1nfr	= intarr(m1nstars)
m1chi	= fltarr(m1nstars)
m1sharp	= fltarr(m1nstars)
m1var	= fltarr(m1nstars)
m1blund	= fltarr(m1nstars)
readu,lun,m1id,m1x,m1y,m1,m1e,m1nfr,m1chi,m1sharp,m1var,m1blund
free_lun,lun

; get limits

; chi limit

h = histogram(m1chi,omin=omin,bins=0.01)
xx = findgen(n_elements(h)) * 0.01 + omin

t = where(h eq max(h))	; find peak of histogram
hmax = h(t(0))		; get peak value
htlow = h
htlow(0:t(0)) = 1000000L; set lower half of histogram to high value
t =where(htlow lt hmax*0.05,nlow); get 5% of peak value
if nlow gt 0 then $
	chilim = xx(t(0)) $	; set chi limit
else	chilim = 0.5

; sharp limits

h = histogram(m1sharp, omin=omin)
xx = findgen(n_elements(h)) + omin

t = where(h eq max(h))	; find peak of histogram
hmax = h(t(0))		; get peak value
htlow = h
htlow(t(0):*) = 1000000L; set upper half to high value
hthi = h
hthi(0:t(0)) = 1000000L	; set lower half to high value

tl = where(htlow lt hmax*0.01,nlow) ; get 1% of peak value
th = where(hthi  lt hmax*0.01,nhi ) ; get 1% of peak value

if nlow gt 0 and nhi gt 0 then $
	sharpran = [xx(tl(n_elements(tl)-1)),xx(th(0))] $
else	sharpran = [-20., 20.]

; blunder limits

h = histogram(m1blund, omin=omin,bins=0.01)
xx = findgen(n_elements(h)) * 0.01 + omin

t = where(h eq max(h))
hmax = h(t(0))		; get peak value
htlow = h
htlow(t(0):*) = 1000000L; set upper half to high value
hthi = h
hthi(0:t(0)) = 1000000L	; set lower half to high value

tl = where(htlow lt hmax*0.01,nlow) ; get 1% of peak value
th = where(hthi  lt hmax*0.01,nhi ) ; get 1% of peak value

if nlow gt 0 and nhi gt 0 then $
	blundran = [xx(tl(n_elements(tl)-1)),xx(th(0))] $
else	blundran = [0.2, 0.8]

if blundran(1) - blundran(0) lt 0.1 then $
	blundran=[0.2,0.8]

; error limits

h = histogram(m1e,omin=omin,omax=omax,bins=0.01)
xx = findgen(n_elements(h)) * 0.01 + omin

t = where(h eq max(h))	; find peak of histogram
hmax = h(t(0))		; get peak value
htlow = h
t =where(htlow lt hmax*0.05,nlow); get 5% of peak value
if nlow gt 0 then $
	errlim = xx(t(0)) $	; set error limit
else	errlim = 0.2

chilim1 = chilim
sharpran1 = sharpran
blundran1 = blundran
errlim1 = errlim

print,'Chi       upper limit: ',chilim1,form='(a,f9.3)'
print,'Sharp   range [lo,hi]: ',sharpran1,form='(a,2f9.3)'
print,'Blunder range [lo,hi]: ',blundran1,form='(a,2f9.3)'
print,'Error     upper limit: ',errlim1,form='(a,f9.3)'

; apply limits

go1 = where(m1chi lt chilim1 and m1e lt errlim1 $
	and m1sharp gt sharpran1(0) and m1sharp lt sharpran1(1) $
	and m1blund gt blundran1(0) and m1blund lt blundran1(1), ngo1)

if ngo1 lt 5 then begin
	print,'ERROR: limits too strict, not enough points: ',ngo1
	return
endif

print,'M1 stars: ',m1nstars
print,' '

;
; m2
file2=''
read,'phot file 2 (no ext): ',file2
root=gettok(file2,'.')

; get m2 filter

m2filt = strmid(root,6,1)

; get m2 photometry

openr,lun,root+'.dat',/get_lun
m2ndates = 0L
m2nstars = 0L
readu,lun,m2ndates,m2nstars
m2jd = dblarr(m2ndates)
readu,lun,m2jd
;
m2id	= lonarr(m2nstars)
m2x	= fltarr(m2nstars)
m2y	= fltarr(m2nstars)
m2	= fltarr(m2nstars)
m2e	= fltarr(m2nstars)
m2nfr	= intarr(m2nstars)
m2chi	= fltarr(m2nstars)
m2sharp	= fltarr(m2nstars)
m2var	= fltarr(m2nstars)
m2blund	= fltarr(m2nstars)
readu,lun,m2id,m2x,m2y,m2,m2e,m2nfr,m2chi,m2sharp,m2var,m2blund
free_lun,lun

print,'M2 stars: ',m2nstars
print,' '

; get color

col = fltarr(m1nstars) - 1000.0
colerr = fltarr(m1nstars) - 1000.0
id= [0L]
m1c = [0.]
m1ce = [0.]
m2c = [0.]
m2ce = [0.]
nfound = 0L
;
for i=0,m1nstars-1 do begin
    if m1id(i) lt 200000L then begin
	t = where(m2id eq m1id(i), count)
	if count eq 1 then begin
		col(i) = m1(i) - m2(t(0))
		colerr(i) = sqrt(m1e(i)^2 + m2e(t(0))^2)
		nfound = nfound + 1L
	endif
    endif 
;    else begin
;    	r = sqrt( (m1x(i) - m2x)^2 + (m1y(i) - m2y)^2 )
;	t = where(r lt 2., count)
;	if count ge 1 then begin
;		s = sort(r(t))
;		t = t(s)
;		col(i) = m1(i) - m2(t(0))
;		colerr(i) = sqrt(m1e(i)^2 + m2e(t(0))^2)
;		nfound = nfound + 1L
;	endif
;    endelse
endfor
;

print,'M1,M2 matches: ',nfound
print,' '

;
xran=[min(col(go1)),max(col(go1))]
yran=[max(m1(go1)),min(m1(go1))]
plot,col,m1,psym=3,yran=yran,xran=xran,xsty=1,ysty=1,charsize=2, $
        xtitl=m1filt+'-'+m2filt, ytitl=m1filt,titl=field+' '+systime(0)
x0 = !x.crange(0) + (!x.crange(1) - !x.crange(0) ) * 0.90
x1 = !x.crange(0) + (!x.crange(1) - !x.crange(0) ) * 0.93
y0 = !y.crange(1) + (!y.crange(0) - !y.crange(1) ) * 0.1
yinc = (!y.crange(0) - !y.crange(1)) * 0.1

if keyword_set(cands) then begin
	rec=''
	cid = [0L]
	ccl = [0.]
	cm1 = [0.]
	cq  = [0]
	openr,ilun,root+'.cnd',/get_lun
	readf,ilun,rec			; header
	while not eof(ilun) do begin
		readf,ilun,rec
		cid = [cid,  long(gettok(rec,' '))]
		ccl = [ccl, float(gettok(rec,' '))]
		cm1 = [cm1, float(gettok(rec,' '))]
		cq  = [ cq,   fix(gettok(rec,' '))]
	endwhile
	free_lun,ilun
	xyouts,x0,y0,'qual'
	y0 = y0 + yinc*0.5
	for i=1,4 do begin
		oplot,[x0,x0],[y0,y0],psym=i+3
		xyouts,x1,y0+(yinc*0.1),strtrim(i,2)
		y0 = y0 + yinc
		t = where(cq eq i, nfound)
		if nfound gt 0 then $
			oplot,ccl(t),cm1(t),psym=i+3
	endfor
	for i=11,14 do begin
		t = where(cq eq i, nfound)
		if nfound gt 0 then $
			for j=0,nfound-1 do begin
				f = where(id eq cid(t(j)), nid)
				if nid eq 1 then begin
					oplot,[m1m2(f(0)),m1m2(f(0))], $
					      [m1c(f(0)), m1c(f(0))], $
					      psym=(i-10)+3,symsi=3,thick=2
					print,'id, m1-m2, m1: ',cid(t(j)),$
						m1m2(f(0)), m1c(f(0)), $
						form = '(a,i6,2f9.2)'
				endif else $
					print,'id: ',cid(t(j)),' not found'
			endfor
	endfor
	return
endif
; open mark file

openw,olun,root+'.cnd',/get_lun,/append

;
q = ''	; input action
print,'keys: c - cursor, f - find id, m - mark, z - zoom: '
repeat begin
	q = get_kbrd(1)		; get action letter
	case q of

	'c':	begin
		print,'cursor mode: '
		cursor,xx,yy
		print,xx,yy
		end

	'f':	begin
		fid = 0L
		read,'Enter id: ',fid
		t=where(m1id eq fid, nfound)
		if nfound le 0 then begin
			print,'Not found: ',fid
		endif else begin
			t = t(0)
			oplot,[col(t),col(t)],[m1(t),m1(t)], $
				psym=2,symsi=3,thick=2
			print,'id,col,m1: ',fid,col(t),m1(t)
		endelse
		end

	'm':	begin
		print,'mark mode: '
		done = (1 eq 0)
		while not done do begin
			cursor,xx,yy
			if xx lt !x.crange(0) or xx gt !x.crange(1) or $
			   yy lt !y.crange(1) or yy gt !y.crange(0) then begin
			   	done = (1 eq 1)
			endif else begin
				r = sqrt( (col-xx)^2 + (m1-yy)^2 )
				t = where(r eq min(r))
				candid = m1id(t(0))
				candcol = col(t(0))
				candm1 = m1(t(0))
				printf,olun,candid, candcol, candm1, $
					form='(i6, f9.3, f9.3 )'
				print,candid, candcol, candm1, $
					form='(i6, f9.3, f9.3 )'
				oplot,[candcol,candcol],[candm1,candm1],psym=4
			endelse
			wait,0.25
		endwhile
		print,'exiting mark mode'
		end

	'z':	begin
		read,'X range (x1, x2): ',xran
		read,'Y range (y1, y2): ',yran
		plot,col,m1,psym=3,yran=yran,xran=xran,xsty=1,ysty=1, $
			charsize=2.5, xtitl=m1filt+'-'+m2filt, ytitl=m1filt, $
			titl=field+' '+systime(0)
		end

	else:
	endcase
endrep until q eq 'q'
;
free_lun,olun
;
return
end
