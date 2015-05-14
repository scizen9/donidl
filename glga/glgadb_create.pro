pro glgadb_create,verbose=verbose,silent=silent,log=log
;+
; glgadb_create - get list and status of stuff in !GLGA_ROOT
;
; KEYWORDS:
;	verbose	- set for extra output
;	log	- set to log operations
;
; HISTORY:
;	$Id: glgadb_create.pro,v 1.10 2014/03/02 23:08:04 neill Exp $
;-
; common variable for glgadat
	COMMON glgadb_info, glgadat
;
; setup
	pre = 'GLGADB_CREATE'
	version = repstr('$Revision: 1.10 $ $Date: 2014/03/02 23:08:04 $','$','')
;
; check if loggin
	if keyword_set(log) then begin
		logfile = !GLGA_DATA + 'glgadb_create.log'
		filestamp,logfile,verbose=verbose
		openw,ll,logfile,/get_lun
		printf,ll,'# '+pre+': '+version
		printf,ll,'# Date: '+systime(0)
	endif
;
; get unique IDs
	if keyword_set(verbose) then print,'Getting a list of ids ...'
	flist=file_search(!GLGA_ROOT+'data/???D/{2mass,irac,sdss,galex,wise}/fits/*.fit*')
	id = extract_ids(flist,degdir=degdir)
	id = strcompress(id,/rem)
	nh = n_elements(id)
	if keyword_set(log) then $
		printf,ll,'# Records examined: ',nh
;
; create struct
	A = {glgadb_data}
	A = struct_init(A)
	newdat = replicate(A,nh)
;
; loop over hosts
	for i=0,nh-1 do begin
		newrec = glgadb_mkrec(id[i],degdir[i])
		newdat[i] = newrec
;
; status
		if not keyword_set(silent) then $
			print,string(13B),i+1,'/',nh,id[i], $
				newrec.ra,newrec.catalog, $
				format='($,a1,i6,a1,i6,2x,a-25,f13.8,2x,a-10)'
;
; log it?
		if keyword_set(log) then $
			printf,ll,i+1,id[i],newrec.ra,newrec.catalog, $
				format='(i6,2x,a-25,f13.8,2x,a-10)'

	endfor
	print,' '
;
; sort on RA and replace old glgadat
	glgadat = newdat[sort(newdat.ra)]
;
; save file
	savfile=!GLGA_DATA + 'glgadb_info.sav'
;
; mv old save file
	filestamp,savfile,verbose=verbose
;
; create save file
	print,'Saving GLGA info database to: ',savfile
	if keyword_set(log) then $
		printf,ll,'Saving GLGA status to: ',savfile
	save,glgadat,filename=savfile,verbose=verbose
;
; close log file
	if keyword_set(log) then free_lun,ll
;
	return
end
