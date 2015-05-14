function sqlite_insert,datum
;+
; sqlite_insert - format datum for a sqlite insert command
;-
ret = ''
s=size(datum)
if s[0] eq 0 then begin	; scalar atomic OK
	switch s[1] of
		1:
		2:
		3: begin
			ret = string(datum,format='(i)')
			break
		end
		4:
		5: begin
			ret = string(datum,format='(f)')
			if strpos(ret,'NaN') ge 0 then $
				ret = '-99.0'
			break
		end
		7: begin
			ret = "'" + strtrim(datum,2) + "'"
			break
		end
		12:
		13:
		14:
		15: begin
			ret = string(datum,format='(i)')
			break
		end
	endswitch
endif
return,strtrim(ret,2)
end
