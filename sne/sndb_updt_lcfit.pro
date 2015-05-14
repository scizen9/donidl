pro sndb_updt_lcfit,silent=silent,verbose=verbose
;+
;	update the light curve fit data
;-
; common variable for sndat
COMMON sndb_info, sndat
;
; get SimpleFit data
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading ',!SNE_DATA+'snia_lcfit_smpl.dat'
finfo = file_info(!SNE_DATA+'snia_lcfit_smpl.dat')
readcols,!SNE_DATA+'snia_lcfit_smpl.dat', $
	sn,zhel,zcmb,ebmvmw,mjdmax,bmag,dbmag,s,ds,clr,dclr, $
	ucos,ustr,/silent,format='a,f,f,f,f,f,f,f,f,f,f,i,i'

np=n_elements(sn)
for j=0L,np-1L do begin
	w=where(strpos(sndat.id,sn[j]) ge 0, n)
	if n eq 0 then begin
		if not keyword_set(silent) then $
		    print,'Asiago entry not found for: ',sn[j]
	endif else begin
		w=w[0]
		if finfo.mtime gt sndat[w].mod_time then $
			sndat[w].mod_time = finfo.mtime
		sndat[w].smpl_str	= s[j]
		sndat[w].smpl_strerr	= ds[j]
		sndat[w].smpl_clr	= clr[j]
		sndat[w].smpl_clrerr	= dclr[j]
		sndat[w].smpl_zhel	= zhel[j]
		sndat[w].smpl_zhelerr	= 0.005
		sndat[w].smpl_zcmb	= zcmb[j]
		sndat[w].smpl_zcmberr	= 0.005
		sndat[w].smpl_bmag	= bmag[j]
		sndat[w].smpl_bmagerr	= dbmag[j]
		sndat[w].smpl_ucos	= ucos[j]
		sndat[w].smpl_ustr	= ustr[j]
	endelse
endfor	; SimpleFit data
;
; get MLCS2K data (from JRK07, table 4)
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading ',!SNE_DATA+'snia_lcfit_mlcs2k.dat'
finfo = file_info(!SNE_DATA+'snia_lcfit_mlcs2k.dat')
readcols,!SNE_DATA+'snia_lcfit_mlcs2k.dat', $
	sn,t0,dt0,mb,dmb,delta,ddelta,a0v,da0v,rv,drv,m0v,dm0v,$
	/silent,format='a,f,f,f,f,f,f,f,f,f,f,f,f'

np=n_elements(sn)
for j=0L,np-1L do begin
	w=where(strpos(sndat.id,sn[j]) ge 0, n)
	if n eq 0 then begin
		if not keyword_set(silent) then $
		    print,'Asiago entry not found for: ',sn[j]
	endif else begin
		w=w[0]
		if finfo.mtime gt sndat[w].mod_time then $
			sndat[w].mod_time = finfo.mtime
		sndat[w].mlcs2k_delta	= delta[j]
		sndat[w].mlcs2k_deltaerr= ddelta[j]
		sndat[w].mlcs2k_a0v	= a0v[j]
		sndat[w].mlcs2k_a0verr	= da0v[j]
		sndat[w].mlcs2k_rv	= rv[j]
		sndat[w].mlcs2k_rverr	= drv[j]
		sndat[w].mlcs2k_m0v	= m0v[j]
		sndat[w].mlcs2k_m0verr	= dm0v[j]
		sndat[w].mlcs2k_mub	= mb[j]
		sndat[w].mlcs2k_muberr	= dmb[j]
	endelse
endfor	; MLCS2K fit data
;
; get CfA3 data (from Hicken et al. 2009, tables 3 and 4)
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading ',!SNE_DATA+'snia_lcfit_cfa3.dat'
finfo = file_info(!SNE_DATA+'snia_lcfit_cfa3.dat')
readcols,!SNE_DATA+'snia_lcfit_cfa3.dat', $
	sn,delta,ddelta,av,dav,bmv,dbmv,mb,dmb,mv,dmv,$
		ebmv,debmv, $
	/silent,format='a,f,f,f,f,f,f,f,f,f,f,f,f'

np=n_elements(sn)
for j=0L,np-1L do begin
	w=where(strpos(sndat.id,sn[j]) ge 0, n)
	if n eq 0 then begin
		if not keyword_set(silent) then $
		    print,'Asiago entry not found for: ',sn[j]
	endif else begin
		w=w[0]
		if finfo.mtime gt sndat[w].mod_time then $
			sndat[w].mod_time = finfo.mtime
		sndat[w].cfa3_delta	= delta[j]
		sndat[w].cfa3_deltaerr	= ddelta[j]
		sndat[w].cfa3_av	= av[j]
		sndat[w].cfa3_averr	= dav[j]
		sndat[w].cfa3_bmv	= bmv[j]
		sndat[w].cfa3_bmverr	= dbmv[j]
		sndat[w].cfa3_bmag	= mb[j]
		sndat[w].cfa3_bmagerr	= dmb[j]
		sndat[w].cfa3_vmag	= mv[j]
		sndat[w].cfa3_vmagerr	= dmv[j]
		sndat[w].cfa3_hebmv	= ebmv[j]
		sndat[w].cfa3_hebmverr	= debmv[j]
	endelse
endfor	; CfA3 fit data
;
return
end
