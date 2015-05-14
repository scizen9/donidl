;+
; NAME: mdb_rename 
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-04-20 02:59:41 by Marshall Perrin 
;-

PRO mdb_rename,old,new,alternate=alternate,auto=auto
	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	default = keyword_set(auto) ; default is 0 for manual, 1 for auto)
	print,"going to rename "+old+" to "+new+"."
	if getyn("Continue?",default,noquery=auto) then begin

	q = "update sources set name="+quote(new)+" where name="+quote(old)+";"
	mysqlcmd,SQLhandle,q
	q = "update photometry set name="+quote(new)+" where name="+quote(old)+";"
	mysqlcmd,SQLhandle,q
	q = "update otherproperties set name="+quote(new)+" where name="+quote(old)+";"
	mysqlcmd,SQLhandle,q
	q = "update alternatenames set name="+quote(new)+" where name="+quote(old)+";"
	mysqlcmd,SQLhandle,q
	q = "update observations set name="+quote(new)+" where name="+quote(old)+";"
	mysqlcmd,SQLhandle,q
	q = "update companions set name="+quote(new)+" where name="+quote(old)+";"
	mysqlcmd,SQLhandle,q
				
	print,"Tables {sources,photometry,otherproperties,alternatenames,observations,companions} updated"

	if keyword_set(alternate) then begin
 		 q = "insert ignore into alternatenames (name,alternatename) values ("+$
            quote(new)+", "+quote(old)+");"
        mysqlcmd,SQLhandle,q
        print, "Inserted alias for "+old+" => "+new+"."



	endif
	endif

end
