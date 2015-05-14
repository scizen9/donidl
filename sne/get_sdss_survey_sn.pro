pro get_sdss_survey_sn
;
; output file
    openw,ol,!SNE_DATA+'/sdss_survey_sn.dat',/get_lun
;
; input file
    readcol,!SNE_DATA+'/sdss_fulllist.txt',ssn,rastr,decstr,can,form='a,a,a,a'
    nsn = n_elements(ssn)
    for i=0,nsn-1 do begin
	if stregex(strmid(strtrim(can[i],2),0,1),'[0-9]') lt 0 then begin
		iau='-'
		type='Ia'
	endif else begin
		iau=can[i]
		type='-'
	endelse
	srv = 'SDSS'
	id = ssn[i]
	radec_parse,rastr[i],decstr[i],':',rad,decd
	coostr=rastr[i]+decstr[i]
	while strpos(coostr,':') ge 0 do $
	    strput,coostr,' ',strpos(coostr,':')
	coostr=strcompress(coostr,/remove)
	host = 'A'+strmid(coostr,0,6)+strmid(coostr,10,5)
	hrad = -9.99999999D0
	hdecd= -99.99999999D0
	z = -9.99
	printf,ol,id,srv,type,iau,rad,decd,host,hrad,hdecd,z, $
	    format='(a-16,2x,a-5,2x,a-8,2x,a-8,2x,2f13.8,2x,a-25,2f13.8,f9.3)'
    endfor
;
    free_lun,ol
;
return
end
