pro galdb_update, glga=glga, sne=sne, data=data, hleda=hleda, $
	silent=silent, verbose=verbose, nosave=nosave
;+
;	update all the gal data in master structure
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; update GLGA data
if keyword_set(glga) then $
	galdb_updt_glga,verbose=verbose,silent=silent
;
; update SNe data
if keyword_set(sne) then $
	galdb_updt_snhosts,verbose=verbose,silent=silent
;
; update data
if keyword_set(data) then $
	galdb_updt_data,verbose=verbose,silent=silent
;
; sort on RA
galdat = galdat[sort(galdat.ra)]
;
; save file
if not keyword_set(nosave) then begin
	savfile=!GALS_DATA+'/galdb_info.sav'
;
; mv old save file
	filestamp,savfile,/verbose
;
; create save file
	print,'Saving Galaxy info to: ',savfile
	save,galdat,filename=savfile,/verbose
endif
;
return
end
