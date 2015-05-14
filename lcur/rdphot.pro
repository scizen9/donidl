pro rdphot,photf,filter,jd,mags,merrs
;
;
rec = ''
nrec = 0
filter = ''

openr,1,photf
readf,1,nrec,filter

filter = strtrim(filter,2)

jd = dblarr(nrec)
mags = fltarr(nrec)
merrs = fltarr(nrec)

for i=0,nrec-1 do begin
	readf,1,rec
	jd(i) = double(gettok(rec,' '))
	mags(i) = float(gettok(rec,' '))
	merrs(i) = float(gettok(rec,' '))
endfor
close,1

;
return
end
