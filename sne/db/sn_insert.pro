;+
; FUNCTION sn_insert
; insert a base sn record into the database
;-

function sn_insert, id, name, gid, qsn, qgid, type, qtype, lcid, $
	ra, dec, qpid, scid, cz, czerr, off_ew, off_ns, sid, mag, qmag, $
	lid, bandid, optdisc, discdate, maxdate, survey, discoverer, $
	comment, $
	DBCONN=dbconn, DBSTR=dbstr, verbose=verbose, silent=silent

  compile_opt idl2

  status = 1	; default status

  ; Did we pass a PgSql connection object?
  if not keyword_set(dbconn) then begin

  ; Default database connection string
    if n_elements(dbstr) lt 1 then begin
      dbstr = "dbname='sndb'"
    endif else dbstr = dbstr[0]
  
  ; Create a PgSql connection object
    dbconn = obj_new("pgSqlCon", dbstr)

  endif
  
  ; Create an PgSql command object
  cmd = obj_new("pgSqlCmd", dbconn)
  
  ; Execute the command
  n = cmd -> Execute("SELECT id FROM sn.sn WHERE sn.sn.id = '" + $
	  		strtrim(inid,2) + "'")

  if n lt 1 then begin
    obj_destroy, cmd
    if not keyword_set(dbconn) then obj_destroy, dbconn
    if keyword_set(verbose) and not keyword_set(silent) then $
	    print, 'No rows returned from query'
    return,status
  endif
  
  ; Get the results
  name = cmd->GetField('name')
  id   = cmd->GetField('id')
  status = 1

  ; Print the results
  if keyword_set(verbose) and not keyword_set(silent) then $
  	for i=0, n-1 do print, name[i], id[i], format='(A15,I6)'

  ; Destroy the objects
  obj_destroy, cmd
  if not keyword_set(dbconn) then obj_destroy, dbconn

  return,status

end
