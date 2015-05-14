;+
; SNDB_GET_REC
; get a record from database and return it in a structure
;-

function sndb_get_rec, id, tabname, ostr, DBCONN=dbconn, DBSTR=dbstr, $
	verbose=verbose, silent=silent

  compile_opt idl2

  ; Default status
  status = 1

  ; Check inputs
  if n_params(0) lt 2 then begin
    print,'SNDB_GET_REC: Usage - err = sndb_get_rec( id, tabname, out_struct, [, DBCONN=dbconn, DBSTR=dbstr, /verbose, /silent] )'
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
  
  ; Get list of columns in table
  cstr = "SELECT column_name from information_schema.columns where information_schema.columns.table_name = '"+strtrim(tabname,2)+"'"
  n = cmd -> Execute(cstr)

  if n lt 1 then begin
    obj_destroy, cmd
    if not keyword_set(dbconn) then obj_destroy, dbconn 
    if keyword_set(verbose) and not keyword_set(silent) then $
	    print, 'No rows returned from query'
    return,status
  endif
  
  tags = cmd -> GetField('column_name')

  if keyword_set(verbose) and not keyword_set(silent) then $
	  for i=0, n-1 do print, tags[i]

  status = 0

  ; Destroy the objects
  obj_destroy, cmd
  if not keyword_set(dbconn) then obj_destroy, dbconn

  return,status

end
