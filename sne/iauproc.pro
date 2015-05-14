pro iauproc
;
; read in IAU_SN.txt
read_iau_sn,snnm,host,date,hra,hdec,off,mag,dref,snra,sndec,pref,sntype
;
; open output file
openw,1,'IAU_SN.dat'
;
; loop over objects
nsn=n_elements(snnm)
for i=0,nsn-1 do begin
;
; sn name
	osn=strcompress(snnm(i),/remove_all)
	if strtrim(snnm(i),2) eq '' then osn='-'
;
; host name
	ohost=strcompress(host(i),/remove_all)
	if strtrim(ohost,2) eq '' then ohost='-'
;
; date
	odate=strcompress(date(i),/remove_all)
	if not valid_num(odate,/integer) then odate='-'
;
; host ra, dec
if strlen(strtrim(hra(i),2)) gt 0 then begin
        radec_parse,hra(i),hdec(i),' ',rad,decd
        res=adstring(rad,decd)
        l=strpos(res,'  ')
        ohra=strtrim(strmid(res,0,l),2)
        ohdec=strtrim(strmid(res,l,100),2)
        while strpos(ohra,' ') ge 0 do $
                strput,ohra,':',strpos(ohra,' ')
        while strpos(ohdec,' ') ge 0 do $
                strput,ohdec,':',strpos(ohdec,' ')
endif else begin
	ohra='-'
	ohdec='-'
endelse
;
; host offset
	toff=strsplit(off(i),/extract,count=ns)
	if ns eq 0 then begin
		of1='-'
		of2='-'
	endif
	if ns eq 1 then begin
		of1=toff(0)
		of2='-'
	endif
	if ns ge 2 then begin
		of1=toff(0)
		of2=toff(1)
	endif
;
; discovery ref
	odref=strcompress(dref(i),/remove_all)
	while strpos(odref,',') ge 0 do strput,odref,';',strpos(odref,',')
	if strtrim(odref,2) eq '' then odref='-'
;
; SN ra, dec
if strlen(strtrim(snra(i),2)) gt 0 then begin
        ora=snra(i)
        odec=sndec(i)
        while strpos(ora,' ') ge 0 do $
                strput,ora,':',strpos(ora,' ')
        while strpos(odec,' ') ge 0 do $
                strput,odec,':',strpos(odec,' ')
endif else begin
	ora='-'
	odec='-'
endelse
;
; position ref
	opref=strcompress(pref(i),/remove_all)
	while strpos(opref,',') ge 0 do strput,opref,';',strpos(opref,',')
	if strtrim(opref,2) eq '' then opref='-'
;
; Type
	if strlen(strtrim(sntype(i),2)) eq '' then $
		otype='-' $
	else	otype=sntype(i)
;
; output
	printf,1,osn,ohost,odate,ohra,ohdec,of1,of2,mag(i),odref,ora,odec, $
		opref,otype, $
     format='(a-9,a-17,a-9,a-11,a-12,a6,a6,f5.1,1x,a-12,1x,a-13,a-13,a-16,a8)'
	
endfor
close,1
;
return
end
