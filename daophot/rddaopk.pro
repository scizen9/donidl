pro rddaopk,pfile,id,x,y,mags,merrs,sky,niter,chi,sharp,silent=silent
;
if n_params(0) lt 1 then begin
	print,'Usage: rddaopk, pkfile, id, x, y, mags, merrs, sky, niter, chi, sharp'
	return
endif
;
openr,plun,pfile,/get_lun
rddaohdr,plun

maxstars= 100000L

id	= lonarr(maxstars)
x	= fltarr(maxstars)
y	= fltarr(maxstars)
mags	= fltarr(maxstars)
merrs	= fltarr(maxstars)
sky	= fltarr(maxstars)
chi	= fltarr(maxstars)
niter	= fltarr(maxstars)
sharp	= fltarr(maxstars)

rec=''
star=0L
while not eof(plun) do begin
	readf,plun,rec
	id(star)	= fix(gettok(rec,' '))
	x(star)		= float(gettok(rec,' '))
	y(star)		= float(gettok(rec,' '))
	mags(star)	= float(gettok(rec,' '))
	merrs(star)	= float(gettok(rec,' '))
	sky(star)	= float(gettok(rec,' '))
	niter(star)	= float(gettok(rec,' '))
	chi(star)	= float(gettok(rec,' '))
	sharp(star)	= float(gettok(rec,' '))

	star = star + 1L
	if star mod 10 eq 0 and not keyword_set(silent) then $
		print,string(13B),'stars read in: ',star, format='($,a1,a,i7)'
endwhile

if not keyword_set(silent) then begin
	print,string(13B),'Stars read in: ',star, format='($,a1,a,i7)'
	print,' '
endif

id	=    id(0:star-1)
x	=     x(0:star-1)
y	=     y(0:star-1)
mags	=  mags(0:star-1)
merrs	= merrs(0:star-1)
sky	=   sky(0:star-1)
niter	= niter(0:star-1)
chi	=   chi(0:star-1)
sharp	= sharp(0:star-1)

free_lun,plun

return
end	; pro rddaopk

