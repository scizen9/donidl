pro eclform_sn
;+
; ECLFORM_SN - create eclstat script for supernovae
;-
;
common lowz_sne_info
;
; SN types
tylist=[ $
	'I', $
	'Ia', $
	'Ic', 'Ib_c', 'Ib', $
	'IIn', 'IIb', $
	'IIL', 'IIP', $
	'II' ]
nty = n_elements(tylist)
;
; loop over types
for it=0,nty-1 do begin
;
; get sne of interest
    s=where(sndat.tyn eq it, nsn)
    if nsn le 0 then begin
	print,'No ',tylist(it),' SNe found: '
    endif else begin
;
; open output files
	openw,2,tylist(it)+'_list.txt'
	openw,3,tylist(it)+'_ecl.txt'
;
	for j=0,nsn-1 do begin
		i=s(j)
;
; get coords
		ctyp=''
;
; first check sn coords
		if sndat(i).ra ge 0. and sndat(i).dec ge -90. then begin
			dra=sndat(i).ra
			ddec=sndat(i).dec
			ctyp='sn'
;
; if not check host coords
		endif else begin
;
; so we use the host coords
	        	if sndat(i).hra ge 0. and sndat(i).hdec ge -90. then $
				begin
				dra=sndat(i).hra
				ddec=sndat(i).hdec
				ctyp='host'
;
; host coords no good either
			endif else begin
				dra = -9.
				ddec= -99.
				ctyp='none'
			endelse
		endelse
		printf,2,sndat(i).id,sndat(i).ra,sndat(i).dec,sndat(i).type, $
			sndat(i).host,sndat(i).hra,sndat(i).hdec, $
			sndat(i).off_ew,sndat(i).off_ns, sndat(i).htype, $
			sndat(i).htyn, sndat(i).cz, $
              form='(a-8,2f13.8,1x,a-8,1x,a-14,2f13.8,2f8.1,1x,a-8,f5.1,f11.3)'
;
; write out eclstat script
		if dra ge 0. then begin
			sra=strtrim(string(dra,form='(f9.5)'),2)
			sdec=strtrim(string(ddec,form='(f9.5)'),2)
		printf,3,'echo '+sndat(i).id+' '+ctyp+' >> '+tylab+'_ecl.out'
		printf,3,'eclstat -csv -cols=info_str,sub_visit,ow,surv_type,'+$
			'qa_grade -pos '+sra+','+sdec+' >> '+tylab+'_ecl.out'
		endif
	endfor
	close,2,3
;
	print,'Found: ',nsn,' SNe'
    endelse	; some SNe were found
endfor	; loop over types
;
return
end
