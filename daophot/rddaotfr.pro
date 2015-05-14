pro rddaotfr, tfile, flist, ids, xs, ys, indx, offsets
;
if n_params(0) lt 2 then begin
	print,'rddaotfr, tfile, alflist, ids, xs, ys, indx, offs'
	return
endif
;
maxstars=100000L
openr,ilun,tfile,/get_lun

offsets = [ 0. ]
flist = ['']
rec = ''

nf = 0
readf,ilun,rec

while strmid(rec,1,1) ne '=' do begin

	flist = [ flist, gettok(rec,' ') ]
	off   = float(gettok(rec,' '))
	offsets = [ offsets, off ]

	nf = nf + 1
	readf,ilun,rec

endwhile
flist = flist(1:nf)
offsets = offsets(1:nf)
print,'Files read in: ',nf, form='(a,i7)'
;
;
fmt = '(i6, 2f9.3, '
ni = 0
while ni lt nf do begin
	if nf-ni gt 18 then begin
		fmt = fmt + '18i6/24x, '
		ni = ni + 18
	endif else begin
		fmt = fmt + strtrim(nf-ni,2)+'i6)'
		ni = nf
	endelse
endwhile
;print,'FMT: ',fmt
;
ids  = lonarr(maxstars)
xs   = fltarr(maxstars)
ys   = fltarr(maxstars)
indx = lonarr(nf,maxstars)	; indx(file, star)
;
i = 0L
x = 0.
y = 0.
iindx = lonarr(nf)
;
star = 0L
while not eof(ilun) do begin

	readf,ilun,i,x,y,iindx,form=fmt

	ids(star)	= i
	xs(star)	= x
	ys(star)	= y
	indx(*,star)	= iindx(*)

	star = star + 1L
	if star mod 100 eq 0 then $
		print,string(13B),'stars read in: ',star, format='($,a1,a,i7)'

endwhile
free_lun,ilun
print,string(13B),'Stars read in: ',star, format='($,a1,a,i7)'
print,' '
;
ids = ids(0:star-1)
xs  =  xs(0:star-1)
ys  =  ys(0:star-1)
indx=indx(*,0:star-1)
;
return
end
