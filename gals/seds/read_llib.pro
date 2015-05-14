pro read_llib,zphotlib,modlib,workdir=workdir
;+
;	read_llib - read in LePhare model library
;
; CALLING SEQUENCE:
;	read_llib,zphotlib,modlib
;
; INPUTS:
;	zphotlib	- library used for SED fits
;
; KEYWORDS:
;	workdir		- specify LePhare work directory, else use getenv
;
; OUTPUTS:
;	modlib		- model library name
;
; HISTORY:
;	28-jul-2011, jdn - Initial Version
;-
if keyword_set(workdir) then $
	lwdir = workdir + '/lib_mag/' $
else	lwdir = getenv('LEPHAREWORK') + '/lib_mag/'
;
mfile = lwdir + zphotlib + '.doc'
if file_test(mfile) then begin
	openr,il,mfile,/get_lun
	rec=''
	while strpos(rec,'LIB_NAME') lt 0 do readf,il,rec
	jnk = gettok(rec,' ')
	modlib = gettok(rec,' ')
	free_lun,il
endif else $
	print,'Library file not found: ',mfile
;
return
end
