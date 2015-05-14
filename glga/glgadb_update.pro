pro glgadb_update,lfile,verbose=verbose,silent=silent,log=log
;+
; glgadb_update - update hosts in lfile
;
; lfile - list of objects with one line per object with these columns:
;       id
;       ra,dec  - degrees
;       majdiam,mindiam - arcmin
;       pa      - degrees
;	type	- galaxy type
;
; NOTE:
;	Only id and ra are required by this program for lfile
;
; KEYWORDS:
;	verbose	- set for extra output
;	log	- set to log operations
;
; HISTORY:
;	$Id: glgadb_update.pro,v 1.8 2014/03/02 23:19:01 neill Exp $
;-
; common variable for glgadat
	COMMON glgadb_info, glgadat
;
; setup
	pre = 'GLGADB_UPDATE'
	version = repstr('$Revision: 1.8 $ $Date: 2014/03/02 23:19:01 $','$','')
;
; check if loggin
	if keyword_set(log) then begin
		logfile = !GLGA_DATA + 'glgadb_update.log'
		openw,ll,logfile,/get_lun,/append
		printf,ll,'# '+pre+': '+version
		printf,ll,'# Date: '+systime(0)
		printf,ll,'# List file: '+lfile
	endif
;
; get IDs
	if keyword_set(verbose) then $
		print,'Getting a list of ids from: '+lfile
	readcol,lfile,id,ra,format='a,d',comment='#',silent=silent
	nh = n_elements(ra)
	degdir = !GLGA_ROOT + 'data/'+glga_degdir(ra)+'/'
	id = strcompress(id,/rem)
	if keyword_set(log) then $
		printf,ll,'# Records examined: ',nh
;
; initialize counts
	nadd = 0L
	nupd = 0L
;
; in case we have new records
	A = {glgadb_data}
	A = struct_init(A)
;
; get status
	if keyword_set(verbose) then print,'Updating database ... '
	for i=0,nh-1 do begin
		;
		; get new record based on current processing
		newrec = glgadb_mkrec(id[i],degdir[i])
		;
		; are we already in the database?
		l = glfind(id[i],count=ng)
		if ng eq 1 then begin
			glgadat[l] = newrec
			if keyword_set(log) then $
				printf,ll,i+1,id[i],newrec.ra,newrec.catalog, $
					' Updated', $
					format='(i6,2x,a-25,f13.8,2x,a-10,2x,a)'
			nupd += 1
		endif else if ng le 0 then begin
			glgadat = [ glgadat, newrec ]
			if keyword_set(log) then $
				printf,ll,i+1,id[i],newrec.ra,newrec.catalog, $
					' Added', $
					format='(i6,2x,a-25,f13.8,2x,a-10,2x,a)'
			nadd += 1
		endif else begin
			print,pre+': Warning, ambiguous ID: ',id[i]
			if keyword_set(log) then $
				printf,ll,i+1,id[i],' Ambiguous ID', $
					format='(i6,2x,a-25,27x,a)'

		endelse
		;
		; status
		if not keyword_set(silent) then $
			print,string(13B),i+1,'/',nh,id[i], $
				newrec.ra,newrec.catalog, $
				format='($,a1,i6,a1,i6,2x,a-25,f13.8,2x,a-10)'
	endfor
	print,' '
	if not keyword_set(silent) then begin
		print,'Records updated: ',nupd
		print,'Records added  : ',nadd
		if keyword_set(log) then begin
			printf,ll,'Records updated: ',nupd
			printf,ll,'Records added  : ',nadd
		endif
	endif
;
; sort on RA
	glgadat = glgadat[sort(glgadat.ra)]
;
; save file
	savfile=!GLGA_DATA + 'glgadb_info.sav'
;
; mv old save file
	filestamp,savfile,verbose=verbose
;
; create save file
	print,'Saving GLGA status to: ',savfile
	if keyword_set(log) then $
		printf,ll,'Saving GLGA status to: ',savfile
	save,glgadat,filename=savfile,verbose=verbose
;
; close log file
	if keyword_set(log) then free_lun,ll
;
	return
end
