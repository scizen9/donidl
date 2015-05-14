function sqlite_type,ityp
;+
; sqlite_insert - format datum for a sqlite insert command
;-
	ret = ''
	switch ityp of
		1:
		2:
		3: begin
			ret = 'integer'
			break
		end
		4:
		5: begin
			ret = 'real'
			break
		end
		7: begin
			ret = 'text'
			break
		end
		12:
		13:
		14:
		15: begin
			ret = 'integer'
			break
		end
		else: ret = 'numeric'
	endswitch
	return,ret
end
