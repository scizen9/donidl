pro edbfits,ifil
a=0
;+
;	edbfits - read an edb file and convert into a fits binary table file
;-
readcol,ifil,name,spty,rastr,decstr,mag,ep,form='a,a,a,a,f,f',delim=','
nrec = n_elements(name)
;
erec = { name: '', type: '', ra: 0.d0, dec: 0.d0, mag: 0., ep: 0. }

edat = replicate(erec, nrec)
;
for i=0L,nrec-1 do begin
	cood,rastr[i] + ' ' + decstr[i], rad, decd,/silent
	edat[i].name = name[i]
	typ = spty[i]
	jnk = gettok(typ,'|')
	edat[i].type = typ
	edat[i].ra = rad
	edat[i].dec = decd
	edat[i].mag = mag[i]
	edat[i].ep = ep[i]
endfor
;
tmp = ifil
rute = gettok(tmp,'.')
ofil = rute + '.fits'
mwrfits,bogus,ofil,/create
mwrfits,edat,ofil
;
return
end
