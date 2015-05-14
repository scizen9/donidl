pro sdss_junk
	;
readcol,'sne_survey.dat',snid,form='a'
readcol,'sdss_survey_sn.txt',ssn,ty,iid,rastr,decstr,z,mjd, $
	form='a,a,a,a,a,f,d'
snid = strtrim(snid,2)
ssn = strtrim(ssn,2)
iid = strtrim(iid,2)
;
openw,ol,'new_sdss_survey_sn.dat',/get_lun
;
srv='SDSS'
n=n_elements(ssn)
for i=0,n-1 do begin
	t=where(strcmp(snid,ssn[i]) eq 1, nt)
	if nt le 0 then begin
		radec_parse,rastr[i],decstr[i],':',rad,decd
		coostr=adstring(rad,decd)
		hst='A'+gettok(coostr,' ')
		hst=hst+gettok(coostr,' ')
		hst=hst+gettok(coostr,'.')
		jnk=gettok(coostr,' ')
		hst=hst+gettok(coostr,' ')
		hst=hst+gettok(coostr,' ')
	hrad = -9.99999999D0
	hdecd= -99.99999999D0
	printf,ol,ssn[i],srv,ty[i],iid[i],rad,decd,hst,hrad,hdecd,z[i], $
	    format='(a-16,2x,a-5,2x,a-8,2x,a-8,2x,2f13.8,2x,a-25,2f13.8,f11.5)'
		
	endif
endfor
;
free_lun,ol
return
end
