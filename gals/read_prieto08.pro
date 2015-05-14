function read_prieto08,silent=silent,verbose=verbose
;+
;	read in data from Prieto08 and return structure with data
;-
; prdat structure definition
A = { snid: '', snty: '', snra: -9.d0, sndec: -99.d0, $
      host: '', hra: -9.d0, hdec: -99.d0, cz: -9999., $
      sarc: -9.0, skpc: -9.0, babs: -999., oab: -9.0 }
;
; set to one element
prdat=A
;
; get host abs B mags and Oxygen abundance from Prieto et al.
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading metallicities.txt'
if file_test(!SDSS_DATA+'metallicities.txt') then begin
	readcol,!SDSS_DATA+'metallicities.txt',msn,mty,mra,mdec, $
		mhost,mhra,mhdec,sarc,skpc,zed,mb,oab, $
		format='a,a,d,d,a,d,d,f,f,f,f,f',/silent
	nmh = n_elements(oab)
      	prdat = replicate(A,nmh)
	prdat[*].snid = msn
	prdat[*].snty = mty
	prdat[*].snra = mra
	prdat[*].sndec= mdec
	prdat[*].host = mhost
	prdat[*].hra  = mhra
	prdat[*].hdec = mhdec
	prdat[*].cz   = zed * !phys_c
	prdat[*].sarc = sarc
	prdat[*].skpc = skpc
	prdat[*].babs = mb
	prdat[*].oab  = oab
endif else print,'metallicities.txt not found'
;
return,prdat
end
