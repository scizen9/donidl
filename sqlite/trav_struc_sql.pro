function trav_struc_sql, st, cmd
;+
; trav_struc_sql - traverse structure and emit sql create table/column commands
;-
; check size
s = size(st)
;
; if we are atomic, just emit the name
;if s[0] gt 1 then begin
print,scope_varfetch(st,level=(scope_level()-1))
print,s
;
return,1
end
