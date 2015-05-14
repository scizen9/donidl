pro get_sai_survey_sn
;
; output file
    openw,ol,!SNE_DATA+'/sai_survey_sn.dat',/get_lun
;
; input file
    openr,il,!SNE_DATA+'/sai.txt',/get_lun
    rec=''
    for i=0,1 do readf,il,rec
    while not eof(il) do begin
	readf,il,rec
	sta=strsplit(rec,'|',/extract,count=ni)
	if ni gt 26 and $
	   stregex(strmid(strtrim(sta[0],2),0,1),'[0-9]') lt 0 then begin
	    if strpos(sta[0],'SNLS') ge 0 then srv = 'SNLS'
	    if strpos(sta[0],'HST') ge 0 then srv = 'HST'
	    id = strtrim(sta[0],2)
	    type=strtrim(sta[24],2)
	    iau='-'
	    rad=-9.
	    decd=-99.
	    hrad=-9.
	    hdecd=-99.
	    rastr=strtrim(sta(25),2)
	    decstr=strtrim(sta(26),2)
	    hrastr=strtrim(sta(3),2)
	    hdecstr=strtrim(sta(4),2)
	    if rastr ne '' and decstr ne '' then begin
		radec_parse,rastr,decstr,' ',rad,decd
	    endif else begin
		print,sta(0),' Badd coords: ',rastr,' ',decstr
	    endelse
	    if hrastr ne '' and hdecstr ne '' then begin
		radec_parse,hrastr,hdecstr,' ',hrad,hdecd
	    endif else begin
		print,sta(1),' Badd coords: ',hrastr,' ',hdecstr
	    endelse
	    host=strcompress(sta(1),/remove)
	    if strtrim(host,2) eq '' then begin
		    host = 'A'+strcompress(rastr,/remove)+ $
			       strcompress(decstr,/remove)
	    endif
	    if hrad eq rad and hdecd eq decd then begin
		    hrad = -9.99999999D0
		    hdecd= -99.99999999D0
	    endif
	    zstr = strtrim(sta[10],2)
	    if zstr eq '' then $
		    z = -9.99 $
	    else    z = float(zstr)
	    printf,ol,id,srv,type,iau,rad,decd,host,hrad,hdecd,z, $
	      format='(a-16,2x,a-5,2x,a-8,2x,a-8,2x,2f13.8,2x,a-25,2f13.8,f9.3)'
	endif
    endwhile
;
    free_lun,il,ol
;
return
end
