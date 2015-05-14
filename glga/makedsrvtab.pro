pro makedsrvtab,verbose=verbose,local=local
;+
; makedsrvtab - create dsrv_table.dat
;-
common galdb_info
;
if keyword_set(local) then begin
	pfil=!GLGA_DATA+'ellipsepars.dat'
	ofil='dsrv_table.dat'
endif else begin
	pfil=!GLGA_DATA+'master_ellipsepars.dat'
	ofil='master_dsrv_table.dat'
endelse
;
readcol,pfil,gal,ra,dec,smaj,smin,pa,form='a,d,d,f,f,f'
nf = n_elements(ra)
if keyword_set(verbose) then print,'Found this many: ',nf
;
filestamp,ofil,/arch,/verbose
openw,ol,ofil,/get_lun
printf,ol,'# MAKEDSRVTAB - '+systime(0)
if keyword_set(local) then $
	printf,ol,'# LOCAL' $
else	printf,ol,'# MASTER'
printf,ol,'#name                           ra_cen      dec_cen   SemiMajAx   SemiMinAx       PA    FILE'
;
for i=0,nf-1 do begin
	ddir=glga_degdir(ra[i])
	g=gfind(gal[i],count=ng)
	if ng eq 1 then $
		snid = galdat[g].sne $
	else	snid = '-'
	if strlen(strtrim(snid,2)) eq 0 then snid = '-'
	printf,ol,gal[i],ra[i],dec[i],smaj[i],smin[i],pa[i],ddir,snid, $
			format='(a-25,2f13.8,2f12.3,f9.3,4x,a,2x,a)'
        if keyword_set(verbose) then $
		print,string(13B),i+1,'/',nf,gal[i],ddir,'  ', $
			format='($,a1,i5,a,i5,2x,a-26,2x,a,a)'

endfor
free_lun,ol
;
if keyword_set(verbose) then print,' '
;
return
end
