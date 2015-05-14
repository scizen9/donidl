function gadd_sample, isam, nsam, delimiter=delimiter
;+
;	gadd_sample - add a sample to the list of samples
;
; INPUTS:
;	isam	- original input sample list
;	nsam	- new sample to add
;
; KEYWORDS:
;	delimiter	- delimiter between items (default is ',')
;
; RETURNS:
;	comma separated list of samples with nsam added on the end
;
; HISTORY:
;	14-mar-2011:	written, jdn
;-
;
; trim inputs
insam  = strtrim(isam,2)
newsam = strtrim(nsam,2)
;
; check inputs
if keyword_set(delimiter) then $
	dlim = delimiter $
else	dlim = ','
;
; first time adding a sample
if strlen(insam) le 0 then begin
	osam = newsam
;
; not new so check for duplicates
endif else begin
	sta = strsplit(insam,dlim,/extract)
	t=where(strcmp(sta,newsam) eq 1, nt)
	if nt gt 0 then $
		osam = insam $
	else	osam = insam + dlim + newsam
endelse
;
return,osam
end
