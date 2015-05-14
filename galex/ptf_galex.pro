pro ptf_galex
;+
; produce a list of galex observations to compare with prf
;-
op=mrdfits('~/ref/operations-review-dat.fits',1,ophdr)	; master list
et=mrdfits('~/ref/mission-etf.fits',1,ethdr)		; start/stop times
td=mrdfits('~/ref/mission-tdb.fits',1,tdhdr)		; position
ai=mrdfits('~/ref/ais-tdb.fits',1,aihdr)		; position for AIS
;
openw,ll,'GALEX-ptf.list',/get_lun
printf,ll,'#Eclipse Target_ID             Survey OW_pos  RA        Dec       Start                  End                    Exposure'
openw,cl,'GALEX-ptf.csv',/get_lun
printf,cl,'Eclipse,Target_ID,Survey,OW_pos,RA,Dec,Start,End,Exposure'
c=','
;
nop=n_elements(op)
for i=0L,nop-1L do begin
;
; skip safing targets
   nuv_exp=op[i].nuv_exp
   fuv_exp=op[i].fuv_exp
   exptim = fix(max([nuv_exp,fuv_exp])+0.5)
   target=op[i].target
   if strpos(target,'SAFE') lt 0 and exptim gt 0 then begin
	eclipse=op[i].eclipse
	print,string(13B),i+1,'/',nop,eclipse,exptim, $
		form='($,a1,i7,a1,i7,2x,i6,2x,i7)'
	survey=op[i].survey
	opwheel=(strtrim(op[i].ow,2) eq 'd') ? 'imaging' : 'grism'
	e=where(et.eclipse_num eq eclipse, en)
	if en ne 1 then begin
		print,' '
		print,'ET err, eclipse: ',eclipse
		start=''
		fin=''
	endif else begin
		e=e[0]
		start=et[e].start_ccsds
		fin=et[e].end_ccsds
	endelse

	;target=strmid(op[i].target,0,strpos(op[i].target,'_',/reverse_search))
	if strpos(target,'AIS') eq 0 then begin
		t=where(strpos(ai.target_id,target) eq 0, nt)
		if nt lt 1 then begin
			print,' '
			print,'AI err, target: ',target
			ra=[-9.99]
			dec=[-99.9]
			nt=1
		endif else begin
			if nt gt 1 then begin
				t=t[1:*]
				nt=nt-1
			endif
			ra=ai[t].ra
			dec=ai[t].dec
			b=where(ra lt 0., nb)
			if nb gt 0 then ra[b] = ra[b] + 360.d0
		endelse
	endif else begin
		t=where(strpos(td.target_id,target) eq 0, nt)
		if nt lt 1 then begin
			print,' '
			print,'TD err, target: ',target
			ra=[-9.99]
			dec=[-99.9]
			nt=1
		endif else begin
			if nt gt 1 then begin
				t=t[1:*]
				nt=nt-1
			endif
			ra=td[t].ra
			dec=td[t].dec
			b=where(ra lt 0., nb)
			if nb gt 0 then ra[b] = ra[b] + 360.d0
		endelse
	endelse
	for j=0,nt-1 do begin
		printf,ll,eclipse,target,survey,opwheel,ra[j],dec[j], $
			start,fin,exptim/nt, $
		      format='(i5,1x,a-27,1x,a3,1x,a7,2f10.5,1x,a22,1x,a22,i5)'
		printf,cl,strtrim(eclipse,2),c,strtrim(target,2),c, $
			strtrim(survey,2),c,strtrim(opwheel,2),c, $
			strtrim(string(ra[j],form='(f10.5)'),2),c, $
			strtrim(string(dec[j],form='(f10.5)'),2),c, $
			strtrim(start,2),c,strtrim(fin,2),c, $
			strtrim(string(exptim/nt,form='(i5)'),2),format='(17a)'
	endfor
    endif	; eliminate SAFE targets
endfor
print,' '
free_lun,ll,cl
;
return
end
