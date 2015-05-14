pro rddaoap,cfile,id,x,y,mags,merrs,sky,skysig,silent=silent
;
if n_params(0) lt 1 then begin
	print,'Usage: rddaoap, apfile, id, x, y, mags, merrs, sky, skysig'
	return
endif
;
openr,clun,cfile,/get_lun
rddaohdr,clun

; get number of records per star and number of observations per star

rec = ''
readf,clun,rec	; blank line
readf,clun,rec
for i=0,2 do junk = gettok(rec,' ')
nobs = 0
while junk ne '' do begin
	junk = gettok(rec,' ')
	nobs = nobs + 1
endwhile
nrec = 2

if not keyword_set(silent) then begin
	print,'recs per star: ',nrec
	print,'obs  per star: ',nobs-1
endif

free_lun,clun

; open file again and start reading in earnest

openr,clun,cfile,/get_lun
rddaohdr,clun

maxstars= 100000L

id	= lonarr(maxstars)
x	= fltarr(maxstars)
y	= fltarr(maxstars)
mags	= fltarr(nobs, maxstars)
merrs	= fltarr(nobs, maxstars)
sky	= fltarr(maxstars)
skysig	= fltarr(maxstars)

star=0L
while not eof(clun) do begin
	readf,clun,rec	; read blank line
	readf,clun,rec
	id(star) = fix(gettok(rec,' '))
	x(star) = float(gettok(rec,' '))
	y(star) = float(gettok(rec,' '))
	for i = 0,nobs-1 do mags(i,star) = float(gettok(rec,' '))
	readf,clun,rec
	sky(star) = float(gettok(rec,' '))
	skysig(star) = float(gettok(rec,' '))
	junk = gettok(rec,' ')
	for i = 0,nobs-1 do merrs(i,star) = float(gettok(rec,' '))
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
mags	= mags(0:nobs-2,0:star-1)
merrs	= merrs(0:nobs-2,0:star-1)
sky	= sky(0:star-1)
skysig	= skysig(0:star-1)

free_lun,clun

return
end	; pro rddaoap

