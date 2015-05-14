;+
; NAME: mdb_nameresolve 
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-04-26 23:47:25 by Marshall Perrin 
;-

function mdb_nameresolve,objname,silent=silent,debug=debug
	common mysql, SQLhandle
	mysqlcheck,SQLhandle


	newobjname = strepex(objname,"_"," ",/all)
	; see if there's a match for the name
	if keyword_set(debug) then print, "... SOURCES"
	q = "select name from sources where name="+ quote(newobjname)+";"
	mysqlquery,SQLhandle,q,name,ngood=count,format='A';
	if count eq 1 then return,name[0]

;; 	; otherwise look it up as a synonym
;; 	if keyword_set(debug) then print, "... SOURCES.alternatenames"
;; 	q = "select name,alternatenames from sources where alternatenames like "+$
;; 		quote("%"+newobjname+"%")+";"
;; 		;print,q
;; 	mysqlquery,SQLhandle,q,name,ngood=count,format="A,A"
;; 	if count eq 1 then return,name[0]
;; 
	; otherwise look it up as a synonym from the other table
	;   EXACT MATCH
	if keyword_set(debug) then print, "... ALTERNATENAMES"
	q = "select name,alternatename from alternatenames where alternatename = "+$
		quote(newobjname)+";"
		;print,q
	mysqlquery,SQLhandle,q,name,ngood=count,format="A,A"
	if count eq 1 then return,name[0]
	
	; otherwise is it in the photometry table?
	if keyword_set(debug) then print, "... PHOTOMETRY"
	; DISTINCT is inexplicably not working. ???? MDP 2006-05-15
	;q = "select distinct name from photometry where name ="+$
	q = "select name from photometry where name ="+$
		quote(newobjname)+";"
		;print,q
	mysqlquery,SQLhandle,q,name,ngood=count,format="A,A"
	if count eq 1 then return,name[0]
	v = uniqvals(name)
	if n_elements(v) eq 1 and name[0] ne "" then return ,name[0]

	; otherwise is it in the properties table?
	if keyword_set(debug) then print, "... PROPERTIES"
	q = "select distinct name from otherproperties where name ="+$
		quote(newobjname)+";"
		;print,q
	mysqlquery,SQLhandle,q,name,ngood=count,format="A,A"
	if count eq 1 then return,name[0]


	; TODO
	; add automated checking of HBC and IRAS columns too?
	; add a seperate name resolving table?
	; fail over to actually query simbad if we can't find the
	; answer locally


	message,"Couldn't find matching name for "+newobjname,/info
	if keyword_set(silent) then return,newobjname


	;Comment out this following line to enable update mode:
	;return,newobjname
	
		
	if getyn("Use '"+newobjname+"'?") then return,newobjname
	read,'Enter new object name: ',objname
	;print, "Set OBJNAME= whatever you want to use, then .cont"
	;objname = newobjname
	;stop


	; add this to the database
	if objname ne newobjname and objname ne '' then begin
		q = "insert into alternatenames (name,alternatename) values ("+$
			quote(objname)+", "+quote(newobjname)+");"
		mysqlcmd,SQLhandle,q
		print, "Inserted alias for "+newobjname+" => "+objname+"."
	endif

	return,objname
	
end
