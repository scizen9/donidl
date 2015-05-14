;+
; SNDB_INSERT_REC
; insert a record into database
;-

function sndb_insert_rec, tabname, instr, DBCONN=dbconn, DBSTR=dbstr, $
	verbose=verbose, silent=silent

  compile_opt idl2

  ; Default status
  status = 1

  ; Check inputs
  if n_params(0) lt 2 then begin
    print,'SNDB_INSERT_REC: Usage - err = sndb_insert_rec( tabname, in_struct, [, DBCONN=dbconn, DBSTR=dbstr, /verbose, /silent] )'
    return,status
  endif

  ; Did we pass a PgSql connection object?
  if not keyword_set(dbconn) then begin

  ; Default database connectioon string
    if n_elements(dbstr) lt 1 then begin
      dbstr = "dbname='sndb'"
    endif else dbstr = dbstr[0]
  
  ; Create a PgSql connection object
    dbconn = obj_new("pgSqlCon", dbstr)

  endif
  
  ; Create an PgSql command object
  cmd = obj_new("pgSqlCmd", dbconn)
  
  ; Get list of input structure item names
  names = tag_names(instr)
  ntg = n_elements(names)

  ; Create the command
  cstr = "INSERT INTO "+tabname+" ("
  for i=0,ntg-2 do cstr = cstr + names[i] + ','
  cstr = cstr + names[i] + ') VALUES ('
  for i=0,ntg-1 do begin
	case size(instr.(i),/type) of
	1:	val = strtrim(string(instr.(i),form='(i)'),2)
	2:	val = strtrim(string(instr.(i),form='(i)'),2)
	3:	val = strtrim(string(instr.(i),form='(i)'),2)
	4:	val = strtrim(string(instr.(i),form='(f)'),2)
	5:	val = strtrim(string(instr.(i),form='(d)'),2)
	7:	val = "'"+strtrim(instr.(i),2)+"'"
	12:	val = strtrim(string(instr.(i),form='(i)'),2)
	else:	val = "NULL"
	endcase
	if i eq (ntg-1) then $
		cstr = cstr + val + ')' $
	else	cstr = cstr + val + ','
  endfor

  ; Execute the command
  print,cstr
  n = cmd -> Execute(cstr,/verbose)

  status = 0

  ; Destroy the objects
  obj_destroy, cmd
  if not keyword_set(dbconn) then obj_destroy, dbconn

  return,status

end
