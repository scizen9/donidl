pro rddaocoo,cfile,id,x,y,mags,sharp,rnd,silent=silent
;
if n_params(0) lt 1 then begin
	print,'Usage: rddaocoo, coofile, id, x, y, mags, sharp, round'
	return
endif
;
openr,clun,cfile,/get_lun
rddaohdr,clun

maxstars= 100000L

id	= lonarr(maxstars)
x	= fltarr(maxstars)
y	= fltarr(maxstars)
mags	= fltarr(maxstars)
sharp	= fltarr(maxstars)
rnd	= fltarr(maxstars)

star=0L
while not eof(clun) do begin
	readf,clun,rec
	id(star) = fix(gettok(rec,' '))
	x(star) = float(gettok(rec,' '))
	y(star) = float(gettok(rec,' '))
	mags(star) = float(gettok(rec,' '))
	sharp(star) = float(gettok(rec,' '))
	rnd(star) = float(gettok(rec,' '))

	star = star + 1L
	if star mod 10 eq 0 and not keyword_set(silent) then $
		print,string(13B),'stars read in: ',star, format='($,a1,a,i7)'
endwhile

if not keyword_set(silent) then begin
	print,string(13B),'Stars read in: ',star, format='($,a1,a,i7)'
	print,' '
endif

id	= id(0:star-1)
x	=  x(0:star-1)
y	=  y(0:star-1)
mags	= mags(0:star-1)
sharp	= sharp(0:star-1)
rnd	= rnd(0:star-1)

free_lun,clun

return
end	; pro rddaoap

