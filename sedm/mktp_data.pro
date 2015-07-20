pro mktp_data
;+
; make a script to get data
;-
flist = file_search('rc*.solved', count=nf)
;
filestamp,'tp_solve.dat'
openw,ol,'tp_solve.dat',/get_lun
printf,ol,'# MKPT_DATA - run: '+systime(0)
;
for i=0,nf-1 do begin
	tmp = flist[i]
	rute = gettok(tmp,'.')
	h=headfits(rute+'.fits')
	ra = sxpar(h,'RA')
	dec = sxpar(h,'DEC')
	spawn,'wcs-xy2rd -w '+rute+'.fits -x 1093 -y 1080',result0
	for j=0,1 do jnk=gettok(result0,'(')
	rad0 = gettok(result0,',')
	decd0 = gettok(result0,')')
	spawn,'wcs-xy2rd -w '+rute+'.wcs -x 1093 -y 1080',result1
	for j=0,1 do jnk=gettok(result1,'(')
	rad1 = gettok(result1,',')
	decd1 = gettok(result1,')')
	printf,ol,rute+' '+ra+' '+dec+' '+rad0+' '+decd0+' '+ $
		rad1+' '+decd1
endfor
free_lun,ol
return
end
