;+
; SNDB_GET_COLUMN
; get a full column from the SN database
;-

function sndb_get_column, tabname, colname, coldat, DBCONN=dbconn, DBSTR=dbstr,$
	verbose=verbose, silent=silent

  compile_opt idl2

  status = 1	; default status
  coldat = -1	; default result

  ; Check inputs
  if n_params(0) lt 2 then begin
    print,'SNDB_GET_COLUMN: Usage - err = db_get_column( tabname, colname, coldat [, DBCONN=dbconn, DBSTR=dbstr, /verbose, /silent]'
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
  
  ; Execute the command
  n = cmd -> Execute("SELECT "+colname+" FROM "+tabname)

  if n lt 1 then begin
    obj_destroy, cmd
    if not keyword_set(dbconn) then obj_destroy, dbconn
    if keyword_set(verbose) and not keyword_set(silent) then $
	    print, 'No rows returned from query'
    return,status
  endif
  
  ; Get the results
  coldat = cmd->GetField(colname)
  status = 0

  ; Print the results
  if keyword_set(verbose) and not keyword_set(silent) then $
  	print,'retrieved ',n,' records'

  ; Destroy the objects
  obj_destroy, cmd
  if not keyword_set(dbconn) then obj_destroy, dbconn

  return,status

end
