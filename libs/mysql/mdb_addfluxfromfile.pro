;+
; NAME:  
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-05-18 17:17:00 by Marshall Perrin 
;-


PRO mdb_addfluxfromfile,filename

	tab=string(09b)
	readcol,filename,delim=tab,type,name,band,value,value_error,reference,$
		format="A,A,A,A,A,A"

	n = n_elements(name)

	nan = !values.f_nan
	for i=0L,n-1 do begin
		cmt = quote("ADDPROPFROMFILE: filename "+filename+" ; "+systime())
		my_name = mdb_nameresolve(name[i])
		err = value_error[i]
		if err eq '-' then err=NaN

		if type[i] eq "F" then begin
			flux = value[i]
			flux_error = err
			mag = nan
			mag_error = nan
		endif
		if type[i] eq "M" then begin
			flux = nan
			flux_error = nan
			mag = value[i]
			mag_error = err
		endif
			
		ref = reference[i]
		year = stregex(ref,"[1-2][0-9][0-9][0-9]",/extract)
        if year eq "" then year = '2004'
        date = year+"-01-01" 
	
		mdb_addflux,my_name,band[i],mag,mag_error,flux,flux_error,ref,date=date,comment=cmt

		mdb_sed,name[i],/print
		;stop

	endfor 

end
