;+
; NAME:  
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-05-18 15:49:36 by Marshall Perrin 
;-


PRO mdb_checknames

	common mysql, SQLhandle
	mysqlcheck,SQLhandle


	q1 = "select distinct name from otherproperties;"
	mysqlquery,SQLhandle,q1,name,ngood=count,format='A';
	
	q2 = "select distinct name from sources;"
	mysqlquery,SQLhandle,q2,sources,ngood=count,format='A';

	diff = cmset_op(name,"AND",/not2,sources)

	print,"---- names in properties but not in sources: ---- "
	forprint,diff,/text


	stop
	q1 = "select distinct name from photometry;"
	mysqlquery,SQLhandle,q1,name,ngood=count,format='A';
	
	q2 = "select distinct name from sources;"
	mysqlquery,SQLhandle,q2,sources,ngood=count,format='A';

	diff = cmset_op(name,"AND",/not2,sources)

	print,"---- names in photometry but not in sources: ---- "
	forprint,diff,/text


	stop

end
