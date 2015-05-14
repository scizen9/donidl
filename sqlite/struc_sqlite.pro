pro struc_sqlite,str,tabnam,date_tags=date_tags,index_tags=index_tags, $
	verbose=verbose
;+
; struc_sqlite - analyze an idl structure and convert it to an sqlite table
;
; Usage: struc_sqlite,structure,'tabname',date_tags='date,tags',/verbose
;
; INPUTS:
;	str	- a simple structure array, no dependant structures within
;	tabnam	- string specifying the table name
;
; KEYWORDS:
;	date_tags - a comma separated list of structure tags that are date/time
;	index_tags - a comma separated list of tags that should be indexed
;	verbose	- set for more output
;
; HISTORY:
;	02-jun-2011, jdn, Initial Revision
;	03-jun-2011, jdn, Added keywords
;-
; get list of structure tags
tags = tag_names(str)
nt=n_elements(tags)
;
; arrays for types
ttyp = strarr(nt)	; atomic types
ttab = strarr(nt)	; non-atomic so another table
ttyb = strarr(nt)	; the new table type
indb = intarr(nt)	; index? 0 - no, 1 - yes
;
; base table name
if n_params(0) lt 2 then begin
	tabnam = ''
	while tabnam eq '' do read,'Enter base table name: ',tabnam
endif
;
; which tags are dates or times?
if keyword_set(date_tags) then $
	dtags = strupcase(date_tags) $
else	dtags = ''
;
; which tags should be indexed?
if keyword_set(index_tags) then $
	itags = strupcase(index_tags) $
else	itags = ''
;
; loop over struct tags and analyze
newtab = 0L
for i=0l,nt-1 do begin
	s=size(str.(i))
	;
	; if not atomic, start a new table
	if s[0] ne 1 then begin
		if s[s[0]+1] ne 8 then begin
			newtab = newtab + 1L
			ttab[i] = tags[i]
			ttyb[i] = sqlite_type(s[s[0]+1])
		endif else $
		print,'Non-atomic structure elements are ignored: ',tags[i]
	endif else begin
		;
		; get type
		ttyp[i] = sqlite_type(s[2])
		;
		; check for dates
		if strpos(dtags,tags[i]) ge 0 then $
		   ttyp[i] = 'date'
	   	; check for indices
	   	if strpos(itags,tags[i]) ge 0 then $
		   indb[i] = 1
		if keyword_set(verbose) then $
		print,'Analyzing tag: ',tags[i],ttyp[i],indb[i], $
			format='(a,a-20,2x,a15,2x,i3)'
	endelse
endfor
;
; trim non-atomics
g = where(strlen(ttyp) gt 0, ng)
if ng gt 0 then begin
	tags=tags[g]
	ttyp=ttyp[g]
	indb=indb[g]
endif else begin
	print,'No non-atomic tags, returning.'
	return
endelse
;
ofil = tabnam + '_create.sql'
filestamp,ofil,/arch
openw,ol,ofil,/get_lun
printf,ol,'--- STRUCT_SQLITE: '+systime(0)
printf,ol,'CREATE TABLE '+tabnam+' (key INTEGER PRIMARY KEY,'
tb = '	'
for i=0,ng-2 do $
	if strtrim(ttyp[i],2) ne '' then $
	printf,ol,tb + tags[i] + ' ' + ttyp[i] + ','
printf,ol,tb + tags[i] + ' ' + ttyp[i] + ');'
;
; create any indices
w = where(indb eq 1, nind)
if nind gt 0 then $
	for i=0,nind-1 do $
		printf,ol,'CREATE INDEX '+tabnam+'_'+tags[w[i]]+'_index ON ' + $
			tabnam+' ( '+tags[w[i]]+ ' );'
;
; create any dependant tables
if newtab gt 0 then begin
	t=where(strlen(ttab) gt 0, nnt)
	for i=0,newtab-1 do begin
		printf,ol,'CREATE TABLE ' + tabnam + '_' + ttab[t[i]] + $
			' (key INTEGER PRIMARY KEY,'
		printf,ol,tb + ttab[t[i]] + ' ' + ttyb[t[i]] + ','
		printf,ol,tb + 'fkey integer,'
		printf,ol,tb + 'FOREIGN KEY(fkey) REFERENCES ' + tabnam + $
			'(key) );'
	endfor
endif
;
free_lun,ol
;
return
end
