pro mktp_input
;+
; make data for input into TPOINT
;-
readcol,'tp_solve.dat',fn,hra,hdec,ra0,dec0,ra1,dec1,format='a,a,a,d,d,d,d'
n=n_elements(ra1)
openw,ol,'tp_input.dat',/get_lun
for i=0,n-1 do begin
	ads = adstring(ra1[i],dec1[i],1)
	printf,ol,fn[i],ads,format='(a,2x,a)'
endfor
free_lun,ol
return
end
