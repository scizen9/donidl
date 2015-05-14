function snfind,fname,n,items=items,silent=silent
;
common sndb_info
;
ind=-1
;
; trim spaces from canonical ids
ids = strtrim(sndat.id,2)
;
; trim ?
w = strpos(ids,'?')
t = where(w ge 0, nt)
for i=0,nt-1 do $
	ids[t[i]] = strmid(ids[t[i]],0,w[t[i]])
;
; trim query name
w=strpos(fname,'?')
if w ge 0 then $
	name = strmid(fname,0,w) $
else	name = fname
;
; get exact match
t=where(strcmp(ids,strtrim(name,2)) eq 1, n)
;
; if nothing found
if n le 0 then begin
	;
	; trim spaces from survey ids
	ids = strtrim(sndat.srv_id,2)
	;
	; trim ?
	w = strpos(ids,'?')
	t = where(w ge 0, nt)
	for i=0,nt-1 do $
		ids[t[i]] = strmid(ids[t[i]],0,w[t[i]])
	;
	; get exact match
	t=where(strcmp(ids,strtrim(name,2)) eq 1, n)
	;
	; no match
	if n le 0 then begin
		if not keyword_set(silent) then print,name,' not found.'
	;
	; set index
	endif else ind=t[0]
;
; more than one match
endif else if n gt 1 then begin
	g = where(strpos(sndat[t].id,'?') lt 0, ng)
	if ng eq 1 then ind=t[g]
	if not keyword_set(silent) then print,name,' ambiguous.'
endif else ind=t(0)
;
; print items
if ind ge 0 then $
	snprint,ind,items=items
;
return,ind
end
