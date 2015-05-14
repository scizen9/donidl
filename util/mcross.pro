function mcross,obj,crossdat=crossdat,silent=silent
;
;	get cross reference for Messier objects
;
; read in cross reference file
;
catfile=!CAT_DATA+'/m-cross.txt'
;
page=strarr(111)
openr,il,catfile,/get_lun
readf,il,page
free_lun,il
;
; setups
mess=''
crossdat=['']
cmp = strtrim(strupcase(obj),2)
;
t=where(strpos(page,obj) ge 0,n)
if n le 0 then begin
	if not keyword_set(silent) then $
		print,obj,' not found'
endif else begin
	for i=0,n-1 do begin
		rec=page(t(i))
		sta=strsplit(rec,',',/extract)
		w=where(strpos(sta,obj) ge 0,nw)
		for j=0,nw-1 do begin
			if strcmp(strtrim(sta(w(j)),2),obj) then begin
				mess=strtrim(sta(0),2) 
				crossdat=sta(1:*)
			endif
		endfor
	endfor
endelse
;
return,mess
end
