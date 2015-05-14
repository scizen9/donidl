pro nearneighb,x,y,mags,merrs,mthr,rnei
;
; get the nearest neighbor brighter than mthr mags fainter than the star
;
nstar = n_elements(x)
s = size(mags)
nobs = s(1)
mmag = fltarr(nstar)
rnei = fltarr(nstar)
;
; get mean mags
for i=0,nstar-1 do begin
	mgs = mags(*,i)
	mer = merrs(*,i)
	mmag(i) = wmean(mgs,mer)
endfor
;
; find neighbors for each star
for i=0,nstar-1 do begin
	xoff = x(i) - x
	yoff = y(i) - y
	dmag = mmag - mmag(i)
	r = sqrt(xoff^2 + yoff^2)
	s = sort(r)
;
; skip self
	for j=1,nstar-1 do begin
		rnei(i) = r(s(j))
		if dmag(s(j)) lt mthr then break
	endfor
endfor
;
return
end
