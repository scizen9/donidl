;+
; NAME: mdb_surveylist
;
; 	Return the list of objects for the HAEBE survey
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
; 	returns list of objects
; 	multiple=	flag (1 or 0) for whether an object is in a multiple
; 	objnames=	list of objects, except without multiples replaced.
;
; HISTORY:
; 	Began 2006-07-23 11:23:07 by Marshall Perrin 
;-

function mdb_surveylist,multiple=multiple,objnames=snames,all=all,ra=sra,dec=sdec,survey=survey,listmult=listmult

	mysqlcheck,SQLhandle

	q1 = " select name,multiple,ra2000,dec2000,objecttype,survey from sources where survey is not null and objecttype!='PPNe' and objecttype!='Planet' and objecttype not like '%Tauri%' order by ra2000; "
	if keyword_set(all) then q1 = " select name,multiple,ra2000,dec2000,objecttype,survey from sources where objecttype like '%HAEBE%' or objecttype like 'High Mass YSO' order by ra2000; "
	
	mysqlquery,SQLhandle,q1,snames, smult,sra,sdec,stype,survey


	if keyword_set(all) then return,snames
	
	; get the proper names. 
	; objectname if single, multiple group name if multiple
	gnames = snames
	wm = where(smult ne "NULL")
	gnames[wm] = smult[wm]

	multiple = (smult ne "NULL")

if ~(keyword_set(listmult)) then begin
	; throw out duplicates (from multiples)
	u = uniq(gnames)
	gnames = gnames[u]
	snames = snames[u]
	multiple = multiple[u]
	sra=sra[u]
	; sort by RA
	s = sort(sra)
	gnames = gnames[s]
	snames = snames[s]
	multiple = multiple[s]
	sra=sra[s]
endif
	


	return, gnames

	


end
