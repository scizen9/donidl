pro sndb_table_sql
;+
; sndb_table_sql - make a table for input to sql database
;-
	common lowz_sne_info
	n = n_elements(sndat)
	tb='	'

	ofil='sntable.sql'
	filestamp,ofil
	openw,ol,ofil,/get_lun

	printf,ol,"COPY sne(plc, id, comment, srv_id, srv_name, type, cz, off_ew, off_ns, filt, mag, mag_type, host, htype, hinc, hpa, hd25, hfuv_exptime, hnuv_exptime, hfuv_int_mag, hfuv_int_magerr, hnuv_int_mag, hnuv_int_magerr) from stdin NULL as '-';"
	
	for i=0L,n-1L do begin
		orec= ''
		rra = sndat[i].ra * !DTOR
		rdc = sndat[i].dec * !DTOR
		if rra lt 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + '(' + string(rra,form='(f15.13)') + $
				', ' + string(rdc,form='(f18.14)') + ')' + tb
		orec = orec + strtrim(sndat[i].id,2) + tb 
		if strpos(sndat[i].srv_name,'IAU') ge 0 then $
			pre = 'SN' $
		else	pre = ''
		orec = orec + pre + strtrim(sndat[i].id,2) + ',' + $
			strtrim(sndat[i].type,2) + ',' + $
			strtrim(sndat[i].host,2) + tb
		orec = orec + strtrim(sndat[i].srv_id,2) + tb
		if strlen(sndat[i].srv_name) le 0 then $
			orec = orec + '-' + tb $
		else	orec = orec + strtrim(sndat[i].srv_name,2) + tb
		orec = orec + strtrim(sndat[i].type,2) + tb
		if sndat[i].cz lt -9000. then $
			orec = orec + '-' + tb $
		else	orec = orec + strtrim(string(sndat[i].cz),2) + tb
		if sndat[i].off_ew lt -9000. then $
			orec = orec + '-' + tb $
		else	orec = orec + strtrim(string(sndat[i].off_ew),2) + tb
		if sndat[i].off_ns lt -9000. then $
			orec = orec + '-' + tb $
		else	orec = orec + strtrim(string(sndat[i].off_ns),2) + tb
		orec = orec + strtrim(sndat[i].filt,2) + tb
		if sndat[i].mag lt 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + strtrim(string(sndat[i].mag),2) + tb
		orec = orec + strtrim(sndat[i].mag_type,2) + tb
		orec = orec + strtrim(sndat[i].host,2) + tb
		orec = orec + strtrim(sndat[i].htype,2) + tb
		if sndat[i].hinc lt 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + strtrim(string(sndat[i].hinc),2) + tb
		if sndat[i].hpa lt 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + strtrim(string(sndat[i].hpa),2) + tb
		if sndat[i].hd25 le 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + strtrim(string(sndat[i].hd25),2) + tb
		if sndat[i].hfuv_exptime le 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + $
			strtrim(string(sndat[i].hfuv_exptime),2) + tb
		if sndat[i].hnuv_exptime le 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + $
			strtrim(string(sndat[i].hnuv_exptime),2) + tb
		if sndat[i].hfuv_int_mag le 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + $
			strtrim(string(sndat[i].hfuv_int_mag),2) + tb
		if sndat[i].hfuv_int_magerr le 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + $
			strtrim(string(sndat[i].hfuv_int_magerr),2) + tb
		if sndat[i].hnuv_int_mag le 0. then $
			orec = orec + '-' + tb $
		else	orec = orec + $
			strtrim(string(sndat[i].hnuv_int_mag),2) + tb
		if sndat[i].hnuv_int_magerr le 0. then $
			orec = orec + '-' $
		else	orec = orec + $
			strtrim(string(sndat[i].hnuv_int_magerr),2)
		printf,ol,orec
	endfor

	printf,ol,'\.'
	printf,ol,'UPDATE sne SET grouping=5;'

	free_lun,ol
	return
end
