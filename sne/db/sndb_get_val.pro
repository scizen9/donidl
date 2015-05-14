;+
; SNDB_GET_VAL
; get an item from tabname table in database
;-

function sndb_get_val, id, tabname, colname, $
	DBCONN=dbconn, DBSTR=dbstr, DEFAULT=default, $
	verbose=verbose, silent=silent

  compile_opt idl2

  ; Default value
  if keyword_set(default) then $
	defval = default $
  else	defval = -1

  val = defval	; default result

  ; Check inputs
  if n_params(0) lt 3 then begin
    print,'DB_GET_VAL: Usage - val = sndb_get_item( id, tabname, colname, [, DBCONN=dbconn, DBSTR=dbstr, DEFAULT=default, /verbose, /silent] )'
    return,val
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
  
  ; Create the command
  cstr = "SELECT "+colname+" FROM "+tabname+" WHERE "+tabname+".id = '"+id+"'"
  ; Execute the command
  n = cmd -> Execute(cstr)

  if n lt 1 then begin
    obj_destroy, cmd
    if not keyword_set(dbconn) then obj_destroy, dbconn
    if keyword_set(verbose) and not keyword_set(silent) then $
	    print, 'No rows returned from query'
    return,val
  endif
  
  ; Get the results
  val = cmd->GetField(colname)

  ; Print the results
  if keyword_set(verbose) and not keyword_set(silent) then $
  	print,'retrieved ',n,' values'

  ; Destroy the objects
  obj_destroy, cmd
  if not keyword_set(dbconn) then obj_destroy, dbconn

  return,val

end
