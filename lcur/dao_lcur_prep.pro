pro dao_lcur_prep,root
;
if n_elements(root) le 0 then begin
	root = ''
	read,'dao data set root (no ext): ',root
endif

print,'reading: ',root+'.tfr...'
rddaotfr, root+'.tfr', alflist, id, x, y, indx, offsets

print,'reading: *.alf files...'
rddaoalfs, alflist, indx, mags, merrs, chis, shrps
print,'Done.'

filts = ['b','i','u','v']

dao_force_mags,id,x,y,mags,merrs,chis,shrps,alflist,filts,root,offsets

return
end	; pro dao_lcur_prep
