pro read_lmodel,zphotlib,mno,model,workdir=workdir
;+
;	read_lmodel - read in LePhare configuration
;
; CALLING SEQUENCE:
;	read_lmodel,zphotlib,mno,model
;
; INPUTS:
;	zphotlib	- library used for SED fits
;	mno		- model number
;
; KEYWORDS:
;	workdir		- specify LePhare work directory, else use getenv
;
; OUTPUTS:
;	model		- model name
;
; HISTORY:
;	28-jul-2011, jdn - Initial Version
;-
read_llib,zphotlib,modlib
if keyword_set(workdir) then $
	lwdir = workdir + '/lib_bin/' $
else	lwdir = getenv('LEPHAREWORK') + '/lib_bin/'
;
mfile = lwdir + modlib + '.doc'
if file_test(mfile) then begin
	openr,il,mfile,/get_lun
	rec=''
	while strpos(rec,'NUMBER_SED') lt 0 do readf,il,rec
	jnk = gettok(rec,' ')
	nsed = fix(gettok(rec,' '))
	if mno lt 1 or mno gt nsed then begin
		print,'Error: model number outside range: 1 - ' + strn(nsed) + $
			':',mno
	endif else begin
		rno = 0
		while rno ne mno do begin
			readf,il,rec
			jnk = gettok(rec,' ')
			rno = fix(gettok(rec,' '))
		endwhile
		for i=0,1 do jnk = gettok(rec,' ')
		jnk = gettok(rec,'/')
		model = gettok(rec,'.')
	endelse
	free_lun,il
endif else $
	print,'Library file not found: ',mfile
;
return
end
