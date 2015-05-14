;+
; NAME: mysqlcheck
; 	check that a connection to mysql is open; 
; 	open one if it isn't
;
; INPUTS:
; KEYWORDS:
; 	database	database name to open (default is "Astronomy")
; OUTPUTS:
;
; HISTORY:
; 	Began 2005-10-05 18:47:04 by Marshall Perrin 
;-

PRO mysqlcheck, handle,reopen=reopen,database=database

	if ~(keyword_set(database)) then database="Astronomy"

	if keyword_set(reopen) then begin
		close,handle
		delvarx,handle
	endif
	
	if n_elements(handle) gt 0 then return
	openmysql,handle,database
	

end
