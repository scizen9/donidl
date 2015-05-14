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


PRO mdb_addfluxfromfile2, filename,date=date

	tab=string(09b)
	readcol,filename,delim=tab,name,band,mag,mag_error,  flux, flux_error, reference,$
		format="A,A,A,A,A,A,A"

	n = n_elements(name)

	nan = !values.f_nan
	for i=0L,n-1 do begin
		cmt = quote("ADDPROPFROMFILE: filename "+filename+" ; "+systime())
		my_name = mdb_nameresolve(name[i])
		err = flux_error[i]
		if flux_error[i] eq '-' then flux_error[i] = nan
			
		if ~(keyword_set(date)) then begin
			ref = reference[i]
			year = stregex(ref,"[1-2][0-9][0-9][0-9]",/extract)
	        if year eq "" then year = '2004'
	        date = year+"-01-01" 
		endif
	
		mdb_addflux,my_name,band[i],mag[i],mag_error[i],flux[i],flux_error[i],reference[i],date=date,comment=cmt

		mdb_sed,name[i],/print

	endfor 

end
