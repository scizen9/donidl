pro sifto_file,snd
;
; write out the sifto fit data to a file to be read in by snmaster routines
;
; snd is the data structure from SNLS with the low-z sne fits
;
file=!SNE_DATA+'snia_lcfit_smpl.dat'
filestamp,file,/verbose
;
openw,ol,file,/get_lun
printf,ol,'#SN    zhel   zcmb   ebmvmw mjdmax  bmag   err   s     err    clr   err        cos     use'
;
nsn = n_elements(snd)
for i=0,nsn-1 do begin
    if finite(snd(i).bmag) then begin
	name = strmid(snd(i).name,2)
	if strlen(name) eq 5 then $
		name = strmid(name,0,4)+strupcase(strmid(name,4,1))
	printf,ol, name, $
		snd(i).redshift_hel, snd(i).redshift_cmb, $
		snd(i).ebmv_mw, snd(i).mjdmax, $
		snd(i).bmag, snd(i).bmag_error, $
		snd(i).stretch, snd(i).stretch_error, $
		snd(i).colour, snd(i).colour_error, 0, 0, $
		format='(a-6,2f7.4,f7.3,f8.1,f7.3,3f6.3,f7.3,f6.3,2i8)'
    endif
endfor
;
free_lun,ol
;
return
end
