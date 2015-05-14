;+
; VS_CONN
; make a connection to visit server
;
; dbconn = vs_conn(dbstr=dbstr)
;-

function vs_conn, DBSTR=dbstr

  compile_opt idl2

; Default database connectioon string
  if n_elements(dbstr) lt 1 then begin
    dbstr = "dbname='gserv' host='nagelfar'"
  endif else dbstr = dbstr[0]
  
; Create a PgSql connection object
  dbconn = obj_new("pgSqlCon", dbstr)

  return,dbconn

end
