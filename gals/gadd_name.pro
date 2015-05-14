function gadd_name, inam, nnam
;+
;	gadd_name - add a name to the list of names
;
; INPUTS:
;	inam	- original input name list
;	nnam	- new name to add
;
; RETURNS:
;	bar separated list of names with nnam added on the end
;
; HISTORY:
;	12-jul-2011:	written, jdn
;-
	;
	; trim and canonicalize input
	nam = get_hl_name(strtrim(nnam,2))
	;
	; first time adding a name
	if strlen(inam) le 0 then begin
		onam = '|' + nam + '|'
	;
	; not new, so check for duplicates
	endif else begin
		sta = strsplit(inam,'|',/extract)
		t=where(strcmp(sta,nam) eq 1, nt)
		if nt gt 0 then $
			onam = inam $		; already there
		else	onam = inam + nam + '|'	; add new name
	endelse
	;
	return,onam
end
