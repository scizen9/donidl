pro temp_junk
;+
;	update all the gal data in master structure
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; get snhosts
t=where(strlen(galdat.sne) gt 0, nt)
for i=0,nt-1 do begin
	p=t[i]
	galdat[p].sample = gadd_sample(galdat[p].sample,'snhosts')
	sta = strsplit(galdat[p].sne,',',/extract)
	sta = sta[sort(sta)]
	sta = sta[uniq(sta)]
	nstr = n_elements(sta)
	galdat[p].sne = ''
	for j=0,nstr-1 do $
		galdat[p].sne = gadd_sample(galdat[p].sne,sta[j])
	print,i+1,nt,galdat[p].sne,galdat[p].sample, $
		form='(i04,1x,i04,2x,a,2x,a)'
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
