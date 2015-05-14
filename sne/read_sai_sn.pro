pro read_sai_sn,sn,host,hra,hdec,snra,sndec,ofew,ofns, $
	silent=silent,reread=reread
;
afile=!SNE_DATA+'/sai.dat'
if not keyword_set(reread) and file_test(afile,/read) then begin
	readcol,afile,sn,snra,sndec,ofew,ofns,host,hra,hdec,silent=silent, $
		format='a,d,d,f,f,a,d,d'
endif else begin
;
; move original
    filestamp,afile,/verbose,/arch
;
; output file
    openw,ol,!SNE_DATA+'/sai.dat',/get_lun
;
; input file
    openr,il,!SNE_DATA+'/sai.txt',/get_lun
    rec=''
    for i=0,1 do readf,il,rec
    while not eof(il) do begin
	readf,il,rec
	sta=strsplit(rec,'|',/extract,count=ni)
	if ni gt 26 then begin
	    rad=-9.
	    decd=-99.
	    ofew=-9999.9
	    ofns=-9999.9
	    hrad=-9.
	    hdecd=-99.
	    host=strcompress(sta(1),/remove)
	    if strtrim(host,2) eq '' then host = 'Anon.'
	    rastr=strtrim(sta(25),2)
	    decstr=strtrim(sta(26),2)
	    hrastr=strtrim(sta(3),2)
	    hdecstr=strtrim(sta(4),2)
	    ofewstr=strtrim(sta(15),2)
	    ofnsstr=strtrim(sta(16),2)
	    if ofewstr ne '' then begin
		if strpos(ofewstr,'W') ge 0 then begin
			val=gettok(ofewstr,'W')
			ofew=-float(val)
		endif else if strpos(ofewstr,'E') ge 0 then begin
			val=gettok(ofewstr,'E')
			ofew=float(val)
		endif
	    endif
	    if ofnsstr ne '' then begin
		if strpos(ofnsstr,'S') ge 0 then begin
			val=gettok(ofnsstr,'S')
			ofns=-float(val)
		endif else if strpos(ofnsstr,'N') ge 0 then begin
			val=gettok(ofnsstr,'N')
			ofns=float(val)
		endif
	    endif
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
	    printf,ol,sta(0),rad,decd,ofew,ofns,host,hrad,hdecd, $
		    format='(a,2f13.8,2f9.1,2x,a-30,2f13.8)'
	endif
    endwhile
;
    free_lun,il,ol
endelse
;
return
end
