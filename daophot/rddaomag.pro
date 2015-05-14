pro rddaomag, mfile, ids, xs, ys, mags, merrs, nframes, chi, sharp, var, blunder
;
if n_params(0) lt 1 then begin
	print,'Usage: rddaomag, mfile, ids, xs, ys, mags, merrs, nframes, chi, sharp, var, blunder'
	return
endif
;
openr,mlun,mfile,/get_lun
rddaohdr,mlun
;
maxstars= 100000L
;
ids	= lonarr(maxstars)
xs	= fltarr(maxstars)
ys	= fltarr(maxstars)
mags	= fltarr(maxstars)
merrs	= fltarr(maxstars)
nframes = intarr(maxstars)
chi 	= fltarr(maxstars)
sharp 	= fltarr(maxstars)
var 	= fltarr(maxstars)
blunder = fltarr(maxstars)

rec=''
star = 0L
while not eof(mlun) do begin

	readf,mlun,rec

	ids(star)	= long(gettok(rec,' '))
	xs(star)	= float(gettok(rec,' '))
	ys(star)	= float(gettok(rec,' '))
	mags(star)	= float(gettok(rec,' '))
	merrs(star)	= float(gettok(rec,' '))

	junk=gettok(rec,' ')

	nframes(star)	= fix(gettok(rec,' '))
	chi(star) 	= float(gettok(rec,' '))
	sharp(star) 	= float(gettok(rec,' '))
	var(star) 	= float(gettok(rec,' '))
	blunder(star)	= float(gettok(rec,' '))

	star = star + 1L
	if star mod 100 eq 0 then $
		print,string(13B),'stars read in: ',star, format='($,a1,a,i7)'

endwhile
free_lun,mlun
print,string(13B),'Stars read in: ',star, format='($,a1,a,i7)'
print,' '

ids	=     ids(0:star-1)
xs	=      xs(0:star-1)
ys	=      ys(0:star-1)
mags	=    mags(0:star-1)
merrs	=   merrs(0:star-1)
nframes = nframes(0:star-1)
chi	=     chi(0:star-1)
sharp	=   sharp(0:star-1)
var	=     var(0:star-1)
blunder = blunder(0:star-1)

return
end	; pro rddaomag
