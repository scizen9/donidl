pro read_lpara,pfile,zphotlib,firlibs
;+
;	read_lpara - read in LePhare configuration
;
; CALLING SEQUENCE:
;	read_lpara,pfile,zphotlib
;
; INPUTS:
;	pfile	- LePhare *.para file used in zphota run
;
; OUTPUTS:
;	zphotlib	- library used for galaxy SED fits
;	firlibs		- library used for FIR SED fits
;
; HISTORY:
;	28-jul-2011, jdn - Initial Version
;-
if not file_test(pfile) then begin
	print,'File not found: ',pfile
	return
endif
;
openr,il,pfile,/get_lun
rec=''
while not eof(il) do begin
	readf,il,rec
	if strpos(rec,'ZPHOTLIB') eq 0 then begin
		jnk = gettok(rec,' ')
		zphotlib = gettok(rec,' ')
	endif
	if strpos(rec,'FIR_LIB') eq 0 then begin
		jnk = gettok(rec,' ')
		rec = strtrim(rec,2)
		firlibs = gettok(rec,',')
		while strlen(rec) gt 3 do $
			firlibs = [firlibs,gettok(rec,',')]
	endif
endwhile
;
free_lun,il
;
return
end
