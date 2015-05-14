pro gx_lga
;
;	retrieve all SN host LGA galaxies
;
openr,il,'snmrg.log',/get_lun
rec=''
while not eof(il) do begin
    readf,il,rec
    if strpos(rec,'#') lt 0 then begin
	sta=strsplit(rec,/extract)
;
; Get host name
	host=strlowcase(sta(19))
	p=stregex(host,'[0-9]')
	cat=strmid(host,0,p)
	if strcmp(cat,'e') then begin	; EOS galaxy catalog
		cat='eso'
		b=stregex(host,'-')
		n1=fix(strmid(host,p,b-p))
		n2=fix(strmid(host,b+2))
		gal=cat+string(n1,form='(i03)')+'-g'+string(n2,form='(i03)')
	endif else begin
		n1=long(strmid(host,p))
		gal=cat+strn(n1)
	endelse
;
; check for messier objects
	mess=mcross(strupcase(gal),/silent)
	if strlen(mess) gt 1 then gal=strlowcase(mess)
	print,'Getting: ',gal
	get_lga,gal,/silent
    endif
;
endwhile
;
free_lun,il
;
return
end
