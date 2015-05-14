pro snprint,indx,items=items
;
common sndb_info
;
; do we want to print items?
if keyword_set(items) then begin
	nits=n_elements(items)
	pitems=strarr(nits)
	pits=intarr(nits) - 1
	if nits gt 0 then begin
		tags = tag_names(sndat)
		for i=0,nits-1 do begin
			t=where(tags eq strupcase(strtrim(items(i),2)), n)
			if n ne 1 then $
				print,'SNPRINT: Incorrect or ambiguous item: ',items(i) $
			else begin
				pits[i]=t(0)
			endelse
			pitems[i] = string(items[i],format='(a-24)')
		endfor
	endif
	good = where(pits ge 0, ngood)
	if ngood gt 0 then begin
		nind=n_elements(indx)
		if nind gt 0 then begin
			for i=0,nind-1 do begin
			    ind = indx[i]
			    for j=0,nits-1 do $
				if pits[j] ge 0 then $
				     print,pitems[j],sndat[ind].(pits[j]) $
				else print,pitems[j]
			endfor
		endif else print,'index list empty'
	endif else print,'No good items left'
endif
;
return
end
