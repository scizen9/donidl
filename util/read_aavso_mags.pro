pro read_aavso_mags,ifil, $
	jd=jd,mag=mag, merr=merr,hqerr=hqerr,band=band, $
	obcode=obcode,cocode=cocode,valid=valid
;+
;	read_aavso_mags - read mags from AAVSO data files
;-
openr,il,ifil,/get_lun
rec=''
jd=0.d0
mag=0. & merr = 0. & hqerr=0.
band= [''] & obcode = [''] & cocode = [''] & valid = ['']
while not eof(il) do begin
	readf,il,rec
	if strpos(rec,'#') ne 0 then begin
		jd = [jd,double(gettok(rec,','))]
		mag = [mag,float(gettok(rec,','))]
		merr = [merr,float(gettok(rec,','))]
		hqerr = [hqerr,float(gettok(rec,','))]
		band = [band,gettok(rec,',')]
		obcode = [obcode,gettok(rec,',')]
		cocode = [cocode,gettok(rec,',')]
		valid = [valid,gettok(rec,',')]
	endif
endwhile
jd = jd[1:*]
mag = mag[1:*]
merr = merr[1:*]
hqerr = hqerr[1:*]
band = band[1:*]
obcode = obcode[1:*]
cocode = cocode[1:*]
valid = valid[1:*]
;
return
end
