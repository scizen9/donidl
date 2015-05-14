function snsample,itlist,itran,count=count,union=union
;
common sndb_info
;
ind=-1L
count=0L
;
; check sndat
nsn = n_elements(sndat)
;
; check item list
nits = n_elements(itlist)
if nits le 0 then begin
	print,'SNSAMPLE: - Item list empty'
	return,ind
endif
;
; get sndat items
tags = tag_names(sndat)
tagi = [-1]
nt = 0L
for i=0,nits-1 do begin
	t=where(tags eq strupcase(strtrim(itlist[i],2)), n)
	if n ne 1 then $
		print,'SNSAMPLE: Incorrect or ambiguous item: ',itlist[i] $
	else	nt = nt + 1L
	tagi = [tagi,t[0]]
endfor
if nt ne nits then $
	return,ind
tagi = tagi[1:*]
;
; check ranges
sz = size(itran)
if sz(0) ne 2 or sz(1) ne 2 or sz(2) ne nits then begin
	print,'SNSAMPLE: - Item list, item range mis-match'
	return,ind
endif
;
; sample index
si = intarr(nsn,nits)
;
; loop over item list
for i=0,nits-1 do begin
	t = where(sndat.(tagi(i)) ge itran(0,i) and $
		  sndat.(tagi(i)) le itran(1,i),n)
	if n gt 0 then $
		si(t,i) = 1
endfor	; loop over item list
;
; sum indices
g = intarr(nsn)
for i=0,nsn-1 do $
	g(i) = total(si(i,*))
;
; get the good ones
if keyword_set(union) then $
	ind = where(g gt 0, count) $
else	ind = where(g eq nits, count)
;
return,ind
end
