function sql_type,item
;+
;	sql_type - return the sql type of an item
;-
type=''
t=size(item,/type)
case t of
	1: type = 'integer'
	2: type = 'integer'
	3: type = 'bigint'
	4: type = 'real'
	5: type = 'double precision'
	7: type = 'text'
	12: type = 'integer'
	13: type = 'bigint'
	14: type = 'bigint'
	15: type = 'bigint'
	else: type = ''
endcase
return,type
end
