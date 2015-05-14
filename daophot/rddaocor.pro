pro rddaocor,cfile,id,x,y,mags,merrs,chi,sharp,silent=silent
;
if n_params(0) lt 6 then begin
	print,'RDDAOCOR: Usage - rddaocor, <file>, id, x, y, mags, merrs, chi, sharp'
	return
endif
;
openr,clun,cfile,/get_lun
rddaohdr,clun

; get number of records per star and number of observations per star

rec = ''
readf,clun,rec
nrec = 1L
nobs = recstars(rec,mags,merrs,id,x,y)

if not eof(clun) then begin
    readf,clun,rec
    while strtrim(strmid(rec,0,6),2) eq '' do begin
	nobs = nobs + recstars(rec,mags,merrs)
	nrec = nrec + 1
	readf,clun,rec
    endwhile
endif

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
chi	= fltarr(maxstars)
sharp	= fltarr(maxstars)

star=0L
while not eof(clun) do begin
	for r = 0, nrec-1 do begin
		readf,clun,rec
		if r eq 0 then begin
			n = recstars(rec,m,e,i,ix,iy)
			id(star)		= i
			x(star)			= ix
			y(star)			= iy
			mags(0:n-1,star)	= m(*)
			merrs(0:n-1,star)	= e(*)
			ob = n
		endif else begin
			n = recstars(rec,m,e)
			if ob+(n-1) lt nobs then begin
				mags(ob:ob+(n-1),star)  = m(*)
				merrs(ob:ob+(n-1),star) = e(*)
				ob = ob + n
			endif else begin	
				print,'ERROR - nobs exceeded: ',ob+(n-1)
				free_lun,clun
				return
			endelse
		endelse
	endfor
	chi(star) = mags(nobs-1,star)
	sharp(star) = merrs(nobs-1,star)
	star = star + 1L
	if star mod 100 eq 0 and not keyword_set(silent) then $
		print,string(13B),'stars read in: ',star, format='($,a1,a,i7)'
endwhile

if not keyword_set(silent) then begin
	print,string(13B),'Stars read in: ',star, format='($,a1,a,i7)'
	print,' '
end

id	= id(0:star-1)
x	=  x(0:star-1)
y	=  y(0:star-1)
chi	= chi(0:star-1)
sharp	= sharp(0:star-1)
mags	= mags(0:nobs-2,0:star-1)
merrs	= merrs(0:nobs-2,0:star-1)

free_lun,clun

return
end	; pro rddaocor

