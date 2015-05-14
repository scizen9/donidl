pro sndb_updt_prof,silent=silent,verbose=verbose
;+
;	update the SN site profile photometry
;-
; common variable for sndat
COMMON sndb_info, sndat
;
; get GALEX prof data
gfile = !SNE_DATA+'galex_prof.dat'
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading ',gfile
if file_test(gfile) then begin
	finfo = file_info(gfile)
	readcols,gfile,sn,host,ffr,fpd,fpde,nfr,npd,npde, $
		form='a,a,f,f,f,f,f,f',silent=silent
;
	np=n_elements(sn)
	for i=0L,np-1L do begin
		w=snfind(sn[i],n)
		if n eq 0 then begin
			if not keyword_set(silent) then $
				print,'entry not found for: ',sn[i]
		endif else begin
			w=w[0]
			if finfo.mtime gt sndat[w].mod_time then $
				sndat[w].mod_time = finfo.mtime
			sndat[w].fuv_frac 	= ffr[i]
			sndat[w].fuv_pdel	= fpd[i]
			sndat[w].fuv_pdelerr	= fpde[i]
			sndat[w].nuv_frac 	= nfr[i]
			sndat[w].nuv_pdel	= npd[i]
			sndat[w].nuv_pdelerr	= npde[i]
		endelse
	endfor
endif else if not keyword_set(silent) then print,'Not found: ',gfile
;
; get SDSS prof data
sfile = !SNE_DATA+'sdss_prof.dat'
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading ',sfile
if file_test(sfile) then begin
	finfo=file_info(sfile)
	readcol,sfile,sn,host,ufr,upd,upde,gfr,gpd,gpde,rfr,rpd,rpde, $
		ifr,ipd,ipde,zfr,zpd,zpde, $
		format='a,a,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f',silent=silent
	nsn=n_elements(ufr)
	for i=0,nsn-1 do begin
		m=snfind(sn[i],n)
		if n le 0 then begin
			if not keyword_set(silent) then $
				print,'sndat entry not found for: ',sn[i]
		endif else begin
			m=m[0]
			if finfo.mtime gt sndat[m].mod_time then $
				sndat[m].mod_time = finfo.mtime
			sndat[m].u_frac		= ufr[i]
			sndat[m].u_pdel		= upd[i]
			sndat[m].u_pdelerr	= upde[i]
			sndat[m].g_frac		= gfr[i]
			sndat[m].g_pdel		= gpd[i]
			sndat[m].g_pdelerr	= gpde[i]
			sndat[m].r_frac		= rfr[i]
			sndat[m].r_pdel		= rpd[i]
			sndat[m].r_pdelerr	= rpde[i]
			sndat[m].i_frac		= ifr[i]
			sndat[m].i_pdel		= ipd[i]
			sndat[m].i_pdelerr	= ipde[i]
			sndat[m].z_frac		= zfr[i]
			sndat[m].z_pdel		= zpd[i]
			sndat[m].z_pdelerr	= zpde[i]
		endelse
	endfor
endif else if not keyword_set(silent) then print,'Not found: ',sfile
;
; get 2MASS profile data
tfile = !SNE_DATA+'2mass_prof.dat'
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading ',tfile
if file_test(tfile) then begin
	finfo=file_info(tfile)
	openr,tl,tfile,/get_lun
	rec=''
	readf,tl,rec
	readf,tl,rec
	while not eof(tl) do begin
		readf,tl,rec
		sta=strsplit(rec,/extract)
		m=snfind(sta[0],n)
		if n le 0 then begin
			if not keyword_set(silent) then $
				print,'sndat entry not found for: ',sta[0]
		endif else begin
			m=m[0]
			if finfo.mtime gt sndat[m].mod_time then $
				sndat[m].mod_time = finfo.mtime
			p=2
			sndat[m].j_frac		= float(sta[p]) & p=p+1
			sndat[m].j_pdel		= float(sta[p]) & p=p+1
			sndat[m].j_pdelerr	= float(sta[p]) & p=p+1
			sndat[m].h_frac		= float(sta[p]) & p=p+1
			sndat[m].h_pdel		= float(sta[p]) & p=p+1
			sndat[m].h_pdelerr	= float(sta[p]) & p=p+1
			sndat[m].k_frac		= float(sta[p]) & p=p+1
			sndat[m].k_pdel		= float(sta[p]) & p=p+1
			sndat[m].k_pdelerr	= float(sta[p]) & p=p+1
		endelse
	endwhile
	free_lun,tl
endif else if not keyword_set(silent) then print,'Not found: ',tfile
;
; get WISE profile data
wfile = !SNE_DATA+'wise_prof.dat'
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading ',wfile
if file_test(wfile) then begin
	finfo=file_info(wfile)
	openr,wl,wfile,/get_lun
	rec=''
	readf,wl,rec
	readf,wl,rec
	while not eof(wl) do begin
		readf,wl,rec
		sta=strsplit(rec,/extract)
		m=snfind(sta[0],n)
		if n le 0 then begin
			if not keyword_set(silent) then $
				print,'sndat entry not found for: ',sta[0]
		endif else begin
			m=m[0]
			if finfo.mtime gt sndat[m].mod_time then $
				sndat[m].mod_time = finfo.mtime
			p=2
			sndat[m].w1_frac	= float(sta[p]) & p=p+1
			sndat[m].w1_pdel	= float(sta[p]) & p=p+1
			sndat[m].w1_pdelerr	= float(sta[p]) & p=p+1
			sndat[m].w2_frac	= float(sta[p]) & p=p+1
			sndat[m].w2_pdel	= float(sta[p]) & p=p+1
			sndat[m].w2_pdelerr	= float(sta[p]) & p=p+1
			sndat[m].w3_frac	= float(sta[p]) & p=p+1
			sndat[m].w3_pdel	= float(sta[p]) & p=p+1
			sndat[m].w3_pdelerr	= float(sta[p]) & p=p+1
			sndat[m].w4_frac	= float(sta[p]) & p=p+1
			sndat[m].w4_pdel	= float(sta[p]) & p=p+1
			sndat[m].w4_pdelerr	= float(sta[p]) & p=p+1
		endelse
	endwhile
	free_lun,wl
endif else if not keyword_set(silent) then print,'Not found: ',wfile
;
return
end
