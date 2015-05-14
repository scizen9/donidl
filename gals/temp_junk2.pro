pro temp_junk2
;+
;	update all the gal data in master structure
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; get ones to fix
t=where(galdat.cz gt 0. and galdat.linear_scale lt 0., nt)
for i=0,nt-1 do begin
	p=t[i]
	z = galdat[p].cz / !phys_c
	lumd = sullivanlumdist(z,omega_l=!COSMO_OL, $
		omega_m=!COSMO_OM,h0=!COSMO_H0,/silent)
	galdat[p].linear_scale = $
		( (lumd / (1.+z)^2) / 206265.d0 ) * 1.d6
	print,i+1,nt,galdat[p].cz,galdat[p].linear_scale, $
		form='(i04,1x,i04,2x,f9.2,2x,f9.2)'
endfor
;
; save file
savfile=!GALS_DATA+'/galdb_info.sav'
;
; mv old save file
filestamp,savfile,/verbose
;
; create save file
print,'Saving Galaxy info to: ',savfile
save,galdat,filename=savfile,/verbose
;
return
end
