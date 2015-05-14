pro mkdaomasscr,scrfl,mchfile, mch_str, sigl, order, sbox, nsbox, n1box, anstr
;
; make daomaster script files
;
if n_params(0) le 1 then begin
	print,'MKDAOMASSCR - Usage: mkdaomasscr, scr_file, mch_file, mch_str,'+$
		' sigma_lim, order, start_box, n_start_box, n_1px_box, ans_str'
	return
endif
;
openw,sl,scrfl,/get_lun
;
printf,sl,mchfile
printf,sl,mch_str
printf,sl,strn(sigl)
printf,sl,strn(order)
for i=0,nsbox-1 do printf,sl,strn(sbox)
for i=sbox-1,1,-1 do printf,sl,strn(i)
for i=1,n1box-1 do printf,sl,'1'
printf,sl,'0'
for i=0,n_elements(anstr)-1 do printf,sl,anstr(i)
free_lun,sl
;
return
end
