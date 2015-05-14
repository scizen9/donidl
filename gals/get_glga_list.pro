pro get_glga_list,ifile,indices=indices,hldata=hldata, $
	verbose=verbose,missing=missing,inventory=inventory
;+
; get_glga_list - make a list of galaxies suitable for input to glga procs
;			from an input list of host names
;
; INPUTS:
;	ifile		- list of host names one per line
;
; KEYWORDS:
;	indices		- galdat indices from idl struct
;	hldata		- hyperleda catalog idl struct
;	verbose		- extra output
;	missing		- file to put list of missing hosts
;
; NOTE:
;	Deafaults to given values.  Using measured values is dangerous as
;	the size may possibly expand by 1.5 (if it does not have an
;	<host>_ellipse.dat file).
;-
common galdb_info
if keyword_set(hldata) then $
	restore,!CAT_DATA+'hl_master_structs.sav'
;
tmp = ifile
rute = gettok(tmp,'.')
ofile=rute+'.glga'
filestamp,ofile,/arch
openw,ol,ofile,/get_lun
printf,ol,'# GET_GLGA_LIST: '+systime(0)
if keyword_set(indices) then $
	printf,ol,'#    INDEX LIST:',n_elements(indices) $
else	printf,ol,'#    INPUT LIST: '+ifile
;
; check inventory
if not keyword_set(indices) and keyword_set(inventory) then begin
	readcol,ifile,hosts,form='a',comment='#',/silent
	readcol,inventory,id,ra,dec,mjx,mnx,pa,ty,form='a,d,d,f,f,f,a',/silent
	for i=0l,n_elements(hosts)-1l do begin
		t=where(strcmp(id,hosts[i]) eq 1,nt)
		if nt eq 1 then begin
			t=t[0]
			printf,ol,id[t],ra[t],dec[t],mjx[t],mnx[t],pa[t],ty[t],$
				format='(a-25,2f13.8,3f9.3,2x,a)'
		endif else if nt gt 1 then print,'Ambiguous host: ',hosts[i]
	endfor
	free_lun,ol
	return
endif
;
; check keyword
if keyword_set(indices) then begin
	indx = indices
	hosts= strtrim(galdat[indx].id,2)
	lens = strlen(hosts)
	bad  = where(lens eq 0, nbad)
	if nbad gt 0 then $
		hosts[bad] = strtrim(galdat[indx[bad]].id,2)
	nhsts= n_elements(indx)
endif else begin
;
; read in host list
	readcol,ifile,hosts,form='a',comment='#'
	hosts = strtrim(hosts,2)
	nhsts = n_elements(hosts)
endelse
if keyword_set(missing) then begin
	openw,ml,missing,/get_lun
	printf,ml,'# Missing hosts: '+systime(0)
endif
;
for j=0l,nhsts-1l do begin
    i = gfind(hosts[j],count=n,/silent)
    if n gt 0 then begin
	i = i[0]
	;
	; literature values
	ra = galdat[i].ra
	dec = galdat[i].dec
	majdiam = galdat[i].majax / 60.	; convert to arcmin
	mindiam = galdat[i].minax / 60.
	if majdiam le 0. then begin
		majdiam = 0.5
		mindiam = 0.5
	endif
	pa = galdat[i].pa
	if finite(pa) eq 0 then pa = -100.
	type = galdat[i].type
	if strtrim(type,2) eq '' then $
		type = '-'
	printf,ol,hosts[j],ra,dec,majdiam,mindiam,pa,type, $
		format='(a-25,2f13.8,3f9.3,2x,a)'
    endif
    if n le 0 and keyword_set(hldata) then begin
	    hl = hlfind(name=hosts[j],hldata=hldata)
	    if hl ge 0 then begin
	    	id	= hldata[hl].objname
	    	ra	= hldata[hl].al2000*15.d0
	    	dec	= hldata[hl].de2000
	    	majdiam	= 10.^(hldata[hl].logd25) * 0.1	; arcmin
	    	mindiam	= majdiam / 10.^(hldata[hl].logr25)
		if finite(hldata[hl].pa) eq 0 then $
			pa	= -100. $
	    	else	pa	= hldata[hl].pa
		if strlen(strtrim(hldata[hl].type,2)) eq 0 then $
			type = '-' $
	    	else	type	= strtrim(hldata[hl].type,2)
		printf,ol,id,ra,dec,majdiam,mindiam,pa,type, $
			format='(a-25,2f13.8,3f9.3,2x,a)'
	    	n = 1
	    endif
    endif
    if keyword_set(verbose) then $
	    print,string(13B),j+1,'/',nhsts,hosts[j],form='($,a1,i7,a,i7,2x,a-25)'
    if n le 0 then begin
	    print,'Not found: ',hosts[j]
	    if keyword_set(missing) then printf,ml,hosts[j]
    endif
endfor
print,' '
free_lun,ol
if keyword_set(missing) then free_lun,ml
return
end

