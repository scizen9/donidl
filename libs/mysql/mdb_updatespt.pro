;+
; NAME:  
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-04-26 15:00:31 by Marshall Perrin 
;-


PRO mdb_updatespt
	common mysql, SQLhandle
	mysqlcheck,SQLhandle


	q = "select distinct sources.name,sources.spectraltype from otherproperties,sources where property="Spectral Type" and sources.name =otherproperties.name;"
	mysqlquery,SQLhandle,names

	for i=0L,n_elements(names)-1 do begin
		name=names[i]
		print, "-------------- " +name +" ----------------"
			q="select otherproperties.property, otherproperties.value,"+$
			"otherproperties.reference "+$
			" from otherproperties where otherproperties.name = "+quote(name)+" and otherproperties.property='Spectral Type' order by otherproperties.property, otherproperties.date;"

		print,""	
		mysqlquery,SQLhandle,q,property,value,references,format='A,A,A,A'
		value = strtrim(value,2)
		print,"--Property--", "--Value--", "--Reference--",format='(A20,"          ",A9,"   ",A-30)'
		for i=0,n_elements(property)-1 do begin
			print,property[i] ,value[i],references[i],format='(A20,"          ",A9,"   ",A-30)'
		endfor

		


	endfor 

end
