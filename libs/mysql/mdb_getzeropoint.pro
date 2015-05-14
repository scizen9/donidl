;+
; NAME: mdb_getcoords 
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-05-10 00:58:13 by Marshall Perrin 
;-

function mdb_getzeropoint,inst,band0, my_date, error = zp_err,vega=vega

	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	; TODO this is just a cheap hack to have the code not lock up
	; on narrowband objects. The numbers returned won't really be correct,
	; though.
	band=band0
	if band eq 'BrG-2.16' then band='Ks'
	if band eq 'FeII' then band='H'

	; get all possibly useful zero point records
	q = "SELECT datetime,zeropoint,zeropoint_err,name,comment from zeropoints where instrument="+quote(inst)+" and band="+quote(band)+";"
	mysqlquery,SQLhandle,q,dates,zps,zperrs,names,comments  ,format="A,F,F,A,A",ngood=count

	if count eq 0 then begin
		message,"ERROR: Couldn't find any zero points for that date!"

	endif


	; Figure out which one is closest in time.
	rjds = fltarr(count); reduced julian dates
	for i=0L,count-1 do begin
		rjds[i] = date2rjd(dates[i])
	endfor 
	
	my_rjd = date2rjd(my_date)

	diffs = abs(rjds - my_rjd)
	wbest = (where(diffs eq min(diffs)))[0]
	print, "Requested date is      "+my_date
	print, "Best zero point is on  "+dates[wbest]
	zp_err = zperrs[wbest]
	zeropoint = zps[wbest]
		

	; TODO If there are multiple zero points from the same night, 
	; i.e. diff < 0.5
	; then they should be averaged into a better zp estimate.
	
	w = where(diffs lt 0.5,wcnt)
	if wcnt gt 1 then begin
		print, "Computing mean of "+strc(wcnt)+" available zero points that night."
		meanerr,zps[w],zperrs[w],zeropoint,zp_err
	endif
	

	if arg_present(vega) then begin

		newband = inst+" "+band
		q = "select band,zeropoint_jy from filters where band = "+ quote(newband)+";"
		mysqlquery,SQLhandle,q,  bandname,vega_jy,  format="A,F",ngood=count
		if count ne 1 then message,"Found some number of records other than 1?!"
		vega = vega_jy[0]
	endif

	return,zeropoint[0]

end
