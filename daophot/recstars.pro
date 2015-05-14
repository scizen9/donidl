function recstars,rec,mags,merrs,id,x,y
;
mags = fltarr(6)
merrs= fltarr(6)
;
if n_params(0) gt 3 then begin
	id = long(gettok(rec,' '))
	x = float(gettok(rec,' '))
	y = float(gettok(rec,' '))
endif
;
n = 0L
;
while strtrim(rec,2) ne '' do begin
	mags(n) = float(gettok(rec,' '))
	merrs(n)= float(gettok(rec,' '))
	n = n + 1L
endwhile

mags = mags(0:n-1)
merrs= merrs(0:n-1)
;
return,n
end	; function recstars
