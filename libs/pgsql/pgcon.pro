;+
; PGCON
; make a connection to a postgres database
;
; dbconn = pgcon(dbstr=dbstr)
;-

function pgcon, dbase, HOST=host, USER=user, DBSTR=dbstr

  compile_opt idl2

; check input
  if n_params(0) lt 1 then begin
	  print,'PGCON: Usage - dbconn = pgcon( dbase <, HOST=host, USER=user, DBSTR=dbstr>)'
	  return,-1
  endif

; Default database connectioon string
  if n_elements(dbstr) lt 1 then begin
	if keyword_set(HOST) then $
		db_host = " host='"+strtrim(host,2)+"'" $
	else	db_host = ''
	if keyword_set(USER) then $
		db_user = " user='"+strtrim(user,2)+"'" $
	else	db_user = ''
    dbstr = "dbname='"+strtrim(dbase,2)+"'"+db_host+db_user
  endif else dbstr = dbstr[0]
  
; Create a PgSql connection object
  dbconn = obj_new("pgSqlCon", dbstr)

  return,dbconn

end
