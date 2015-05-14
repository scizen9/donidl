function lcur_com_twin,tag
;
COMMON phot_data
COMMON parm_data
COMMON cnts_data
COMMON phase_data
;
;print,'lcur_com_twin: ',tag, t0, t1
case strupcase(strtrim(tag,2)) of
	'JD'    :	return,jd(t0:t1)
	'EPNUM' :	return,epnum(t0:t1)
	'MAGS'  :	return,mags(t0:t1)
	'MERRS' :	return,merrs(t0:t1)
	'TIME'  :	return,time(t0:t1)
	'COUNTS':	return,counts(t0:t1)
	'CERRS' :	return,cerrs(t0:t1)
	'PHASE' :	return,phase(t0:t1)
	'COFFS' :	return,coffs(t0:t1)
	ELSE	:	begin
		print,'LCUR_COM_TWIN: ERROR, non-windowable tag: ',tag
		return,0
	end
endcase
;
; should never get here
return,0
end
