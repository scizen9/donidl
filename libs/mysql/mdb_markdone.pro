;+
; NAME:  
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-05-25 16:46:11 by Marshall Perrin 
;-

PRO mdb_markdone,name0,type,date

	types = ["Lick:","MIR:"]

	; make sure there's no trailing colon since we add that here. 
	type = strepex(type,":","",/all)

	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	name = mdb_nameresolve(name0)
	

	q = "select name,survey from sources where name="+quote(name)+";"
	mysqlquery,SQLhandle,q,	name2,survey


	if survey eq "NULL" or strc(survey) eq '' then begin
		; survey is blank
		newsurvey = type+": "+date
	endif else begin
		; survey is non-null
		parts = strsplit(survey,/extract)
		wlick = where(parts eq "Lick:",lct)
		wmir = where(parts eq "MIR:",mct)
		; the following could be done a LOT more cleverly
		; with some sort of a parser
		if lct gt 0 then begin
			if mct gt 0 then begin
				lickparts = parts[wlick+1:wmir-1]
				mirparts = parts[wmir+1:*]
			endif else begin
				lickparts = parts[wlick+1:*]
				mirparts =['']
			endelse
		endif else begin
			if mct ge 0 then begin
				mirparts = parts[wmir+1:*]
				lickparts =['']
			endif
		endelse
	
		if type eq "Lick" then begin
			wm = where(lickparts eq date,mct)
			if mct eq 0 then lickparts = [lickparts, date]
		endif
		if type eq "MIR" then begin
			wm = where(mirparts eq date,mct)
			if mct eq 0 then mirparts = [mirparts, date]
		endif
		if n_elements(lickparts) eq 0 then lickparts=''
		if n_elements(mirparts) eq 0 then mirparts=''
	
		sl = strjoin(["Lick:",lickparts],' ',/single)
		if strc(sl) eq "Lick:" then sl=""
		sm = strjoin(["MIR:",mirparts],' ',/single)
		if strc(sm) eq "MIR:" then sm=""
		newsurvey = strjoin([sl,sm]," ")
	endelse

	if newsurvey eq survey then return

	q = "update sources set survey="+quote(newsurvey)+" where name="+quote(name)+";"
	;stop
	mysqlcmd,sqlhandle,q

end
