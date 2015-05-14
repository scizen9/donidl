pro sndb_update, silent=silent, verbose=verbose, snsurvey=snsurvey, $
	site=site, prof=prof, lcfit=lcfit, nosave=nosave
;+
;	update all the sn data in master structure
;-
; common variable for sndat
COMMON sndb_info, sndat
;
; add new IAU sne
sndb_addsne,verbose=verbose,silent=silent
;
; update non IAU SNe
if keyword_set(snsurvey) then $
	sndb_updt_snsurvey,nsne,verbose=verbose,silent=silent
;
; update SN site data
if keyword_set(site) then $
	sndb_updt_site,verbose=verbose,silent=silent
;
; update profile analysis data
if keyword_set(prof) then $
	sndb_updt_prof,verbose=verbose,silent=silent
;
; update light curve data
if keyword_set(lcfit) then $
	sndb_updt_lcfit,verbose=verbose,silent=silent
;
; save file
if not keyword_set(nosave) then begin
	savfile=!SNE_DATA+'/sndb_info.sav'
;
; mv old save file
	filestamp,savfile,/verbose,/arch
;
; create save file
	print,'Saving SNe info to: ',savfile
	save,sndat,filename=savfile,/verbose
endif
;
return
end
