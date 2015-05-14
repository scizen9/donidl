pro sndb_updt_site,silent=silent,verbose=verbose
;+
;	update the SN site photometry
;-
; common variable for sndat
COMMON sndb_info, sndat
;
; get GALEX site data
gfile = !SNE_DATA+'galex_site.dat'
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading ',gfile
if file_test(gfile) then begin
    finfo = file_info(gfile)
    readcols,gfile, $
	sn,mf0,mfe0,mf1,mfe1,mf2,mfe2,mf3,mfe3,sbf,mn0,mne0,mn1,mne1, $
	mn2,mne2,mn3,mne3,sbn,host,fexpt,nexpt,fbkg,nbkg, silent=silent, $
	format='a,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,a,f,f,f,f'

	np=n_elements(sn)
	for j=0L,np-1L do begin
		w=snfind(sn[j],n)
		if n eq 0 then begin
			if not keyword_set(silent) then $
				print,'entry not found for: ',sn[j]
		endif else begin
			w=w[0]
			if finfo.mtime gt sndat[w].mod_time then $
				sndat[w].mod_time = finfo.mtime
			if sndat[w].mwebmv gt -1. then begin
				fextin = glga_getextin(sndat[w].mwebmv,'FUV', $
					/yuan13)
				nextin = glga_getextin(sndat[w].mwebmv,'NUV', $
					/yuan13)
			endif else begin
				fextin = 0.0
				nextin = 0.0
			endelse
			if mf0[j] gt 0. then begin
		  	sndat[w].fuv_res_mag		= mf0[j] - fextin
		  	sndat[w].fuv_res_magerr		= mfe0[j]
			endif
			if mf1[j] gt 0. then begin
		  	sndat[w].fuv_500pc_mag		= mf1[j] - fextin
		  	sndat[w].fuv_500pc_magerr	= mfe1[j]
			endif
			if mf2[j] gt 0. then begin
		  	sndat[w].fuv_1kpc_mag		= mf2[j] - fextin
		  	sndat[w].fuv_1kpc_magerr	= mfe2[j]
			endif
			if mf3[j] gt 0. then begin
		  	sndat[w].fuv_2kpc_mag		= mf3[j] - fextin
		  	sndat[w].fuv_2kpc_magerr	= mfe3[j]
			endif
			if sbf[j] gt 0. then $
		  	sndat[w].fuv_sbrt		= sbf[j] - fextin
			if mn0[j] gt 0. then begin
		  	sndat[w].nuv_res_mag		= mn0[j] - nextin
		  	sndat[w].nuv_res_magerr		= mne0[j]
			endif
			if mn1[j] gt 0. then begin
		  	sndat[w].nuv_500pc_mag		= mn1[j] - nextin
		  	sndat[w].nuv_500pc_magerr	= mne1[j]
			endif
			if mn2[j] gt 0. then begin
		  	sndat[w].nuv_1kpc_mag		= mn2[j] - nextin
		  	sndat[w].nuv_1kpc_magerr	= mne2[j]
			endif
			if mn3[j] gt 0. then begin
		  	sndat[w].nuv_2kpc_mag		= mn3[j] - nextin
		  	sndat[w].nuv_2kpc_magerr	= mne3[j]
			endif
			if sbn[j] gt 0. then $
		  	sndat[w].nuv_sbrt		= sbn[j] - nextin
		endelse
	endfor	; UV GLGA site data
endif else if not keyword_set(silent) then print,'Not found: ',gfile
;
; get SDSS site data
sfile = !SNE_DATA+'sdss_site.dat'
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading ',sfile
if file_test(sfile) then begin
	finfo=file_info(sfile)
	openr,sl,sfile,/get_lun
	rec=''
	readf,sl,rec
	readf,sl,rec
	while not eof(sl) do begin
		readf,sl,rec
		sta=strsplit(rec,/extract)
		m=snfind(sta[0],n)
		if n le 0 then begin
			if not keyword_set(silent) then $
				print,'sndat entry not found for: ',sta[0]
		endif else begin
			m=m[0]
			if sndat[m].mwebmv gt -1. then begin
				ebv = sndat[m].mwebmv
				exs = [ glga_getextin(ebv,'u',/yuan13), $
					glga_getextin(ebv,'g',/yuan13), $
					glga_getextin(ebv,'r',/yuan13), $
					glga_getextin(ebv,'i',/yuan13), $
					glga_getextin(ebv,'z',/yuan13) ]
			endif else	exs = fltarr(5)
			if finfo.mtime gt sndat[m].mod_time then $
				sndat[m].mod_time = finfo.mtime
			p=1
			sndat[m].u_res_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].u_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].u_500pc_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].u_500pc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].u_1kpc_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].u_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].u_2kpc_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].u_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].u_sbrt		= float(sta[p]) - exs[0] & p=p+1
			sndat[m].g_res_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].g_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].g_500pc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].g_500pc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].g_1kpc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].g_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].g_2kpc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].g_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].g_sbrt		= float(sta[p]) - exs[1] & p=p+1
			sndat[m].r_res_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].r_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].r_500pc_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].r_500pc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].r_1kpc_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].r_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].r_2kpc_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].r_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].r_sbrt		= float(sta[p]) - exs[2] & p=p+1
			sndat[m].i_res_mag	= float(sta[p]) - exs[3] & p=p+1
			sndat[m].i_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].i_500pc_mag	= float(sta[p]) - exs[3] & p=p+1
			sndat[m].i_500pc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].i_1kpc_mag	= float(sta[p]) - exs[3] & p=p+1
			sndat[m].i_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].i_2kpc_mag	= float(sta[p]) - exs[3] & p=p+1
			sndat[m].i_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].i_sbrt		= float(sta[p]) - exs[3] & p=p+1
			sndat[m].z_res_mag	= float(sta[p]) - exs[4] & p=p+1
			sndat[m].z_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].z_500pc_mag	= float(sta[p]) - exs[4] & p=p+1
			sndat[m].z_500pc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].z_1kpc_mag	= float(sta[p]) - exs[4] & p=p+1
			sndat[m].z_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].z_2kpc_mag	= float(sta[p]) - exs[4] & p=p+1
			sndat[m].z_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].z_sbrt		= float(sta[p]) - exs[4] & p=p+1
		endelse
	endwhile
	free_lun,sl
endif else if not keyword_set(silent) then print,'Not found: ',sfile
;
; get 2MASS LGA Site data
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading lga_snan.log'
if file_test(!SNE_DATA+'lga_snan.log') then begin
    finfo = file_info(!SNE_DATA+'lga_snan.log')
    readcols,!SNE_DATA+'lga_snan.log',sn2,$
	mj0,mj0e,mj1,mj1e,mj2,mj2e,mj3,mj3e,sbj,jfrac,jdp,jdpe, $	; J
	mh0,mh0e,mh1,mh1e,mh2,mh2e,mh3,mh3e,sbh,hfrac,hdp,hdpe, $	; H
	mk0,mk0e,mk1,mk1e,mk2,mk2e,mk3,mk3e,sbk,kfrac,kdp,kdpe, $	; K
	format='a,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,'+ $
	       'f,f,f,f,f,f,f,f,f',silent=silent
	n2=n_elements(mk0)
	for i=0L,n2-1L do begin
		m=where(strpos(sndat.id,sn2[i]) ge 0, n)
		if n eq 0 then begin
			if not keyword_set(silent) then $
				print,'2MASS LGA entry not found for: ',sn2[i]
		endif else begin
			m=m[0]
			if finfo.mtime gt sndat[m].mod_time then $
				sndat[m].mod_time = finfo.mtime
			sndat[m].J_frac		= jfrac[i]
			sndat[m].J_pdel		= jdp[i]
			sndat[m].J_pdelerr	= jdpe[i]
			sndat[m].J_res_mag	= mj0[i]
			sndat[m].J_res_magerr	= mj0e[i]
			sndat[m].J_500pc_mag	= mj1[i]
			sndat[m].J_500pc_magerr	= mj1e[i]
			sndat[m].J_1kpc_mag	= mj2[i]
			sndat[m].J_1kpc_magerr	= mj2e[i]
			sndat[m].J_2kpc_mag	= mj3[i]
			sndat[m].J_2kpc_magerr	= mj3e[i]
			sndat[m].J_sbrt		= sbj[i]
			sndat[m].H_frac		= hfrac[i]
			sndat[m].H_pdel		= hdp[i]
			sndat[m].H_pdelerr	= hdpe[i]
			sndat[m].H_res_mag	= mh0[i]
			sndat[m].H_res_magerr	= mh0e[i]
			sndat[m].H_500pc_mag	= mh1[i]
			sndat[m].H_500pc_magerr	= mh1e[i]
			sndat[m].H_1kpc_mag	= mh2[i]
			sndat[m].H_1kpc_magerr	= mh2e[i]
			sndat[m].H_2kpc_mag	= mh3[i]
			sndat[m].H_2kpc_magerr	= mh3e[i]
			sndat[m].H_sbrt		= sbh[i]
			sndat[m].K_frac		= kfrac[i]
			sndat[m].K_pdel		= kdp[i]
			sndat[m].K_pdelerr	= kdpe[i]
			sndat[m].K_res_mag	= mk0[i]
			sndat[m].K_res_magerr	= mk0e[i]
			sndat[m].K_500pc_mag	= mk1[i]
			sndat[m].K_500pc_magerr	= mk1e[i]
			sndat[m].K_1kpc_mag	= mk2[i]
			sndat[m].K_1kpc_magerr	= mk2e[i]
			sndat[m].K_2kpc_mag	= mk3[i]
			sndat[m].K_2kpc_magerr	= mk3e[i]
			sndat[m].K_sbrt		= sbk[i]
		endelse
	endfor
endif else if not keyword_set(silent) then print,'lga_snan.log not found.'
;
; get 2MASS XSC Site data
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading xsc_snan.log'
if file_test(!SNE_DATA+'xsc_snan.log') then begin
    finfo = file_info(!SNE_DATA+'xsc_snan.log')
    readcols,!SNE_DATA+'xsc_snan.log',sn2,$
	mj0,mj0e,mj1,mj1e,mj2,mj2e,mj3,mj3e,sbj,jfrac,jdp,jdpe, $	; J
	mh0,mh0e,mh1,mh1e,mh2,mh2e,mh3,mh3e,sbh,hfrac,hdp,hdpe, $	; H
	mk0,mk0e,mk1,mk1e,mk2,mk2e,mk3,mk3e,sbk,kfrac,kdp,kdpe, $	; K
	format='a,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,'+ $
	       'f,f,f,f,f,f,f,f,f',silent=silent
	n2=n_elements(mk0)
	for i=0L,n2-1L do begin
		m=where(strpos(sndat.id,sn2[i]) ge 0, n)
		if n eq 0 then begin
			if not keyword_set(silent) then $
				print,'2MASS XSC entry not found for: ',sn2[i]
		endif else begin
			m=m[0]
			if finfo.mtime gt sndat[m].mod_time then $
				sndat[m].mod_time = finfo.mtime
			sndat[m].J_frac		= jfrac[i]
			sndat[m].J_pdel		= jdp[i]
			sndat[m].J_pdelerr	= jdpe[i]
			sndat[m].J_res_mag	= mj0[i]
			sndat[m].J_res_magerr	= mj0e[i]
			sndat[m].J_500pc_mag	= mj1[i]
			sndat[m].J_500pc_magerr	= mj1e[i]
			sndat[m].J_1kpc_mag	= mj2[i]
			sndat[m].J_1kpc_magerr	= mj2e[i]
			sndat[m].J_2kpc_mag	= mj3[i]
			sndat[m].J_2kpc_magerr	= mj3e[i]
			sndat[m].J_sbrt		= sbj[i]
			sndat[m].H_frac		= hfrac[i]
			sndat[m].H_pdel		= hdp[i]
			sndat[m].H_pdelerr	= hdpe[i]
			sndat[m].H_res_mag	= mh0[i]
			sndat[m].H_res_magerr	= mh0e[i]
			sndat[m].H_500pc_mag	= mh1[i]
			sndat[m].H_500pc_magerr	= mh1e[i]
			sndat[m].H_1kpc_mag	= mh2[i]
			sndat[m].H_1kpc_magerr	= mh2e[i]
			sndat[m].H_2kpc_mag	= mh3[i]
			sndat[m].H_2kpc_magerr	= mh3e[i]
			sndat[m].H_sbrt		= sbh[i]
			sndat[m].K_frac		= kfrac[i]
			sndat[m].K_pdel		= kdp[i]
			sndat[m].K_pdelerr	= kdpe[i]
			sndat[m].K_res_mag	= mk0[i]
			sndat[m].K_res_magerr	= mk0e[i]
			sndat[m].K_500pc_mag	= mk1[i]
			sndat[m].K_500pc_magerr	= mk1e[i]
			sndat[m].K_1kpc_mag	= mk2[i]
			sndat[m].K_1kpc_magerr	= mk2e[i]
			sndat[m].K_2kpc_mag	= mk3[i]
			sndat[m].K_2kpc_magerr	= mk3e[i]
			sndat[m].K_sbrt		= sbk[i]
		endelse
	endfor
endif else if not keyword_set(silent) then print,'xsc_snan.log not found.'
;
; get 2MASS site data
tfile = !SNE_DATA+'2mass_site.dat'
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
			if sndat[m].mwebmv gt -1. then begin
				ebv = sndat[m].mwebmv
				exs = [ glga_getextin(ebv,'J',/yuan13), $
					glga_getextin(ebv,'H',/yuan13), $
					glga_getextin(ebv,'K',/yuan13) ]
			endif else	exs = fltarr(3)
			if finfo.mtime gt sndat[m].mod_time then $
				sndat[m].mod_time = finfo.mtime
			p=1
			sndat[m].j_res_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].j_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].j_500pc_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].j_500pc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].j_1kpc_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].j_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].j_2kpc_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].j_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].j_sbrt		= float(sta[p]) - exs[0] & p=p+1
			sndat[m].h_res_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].h_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].h_500pc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].h_500pc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].h_1kpc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].h_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].h_2kpc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].h_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].h_sbrt		= float(sta[p]) - exs[1] & p=p+1
			sndat[m].k_res_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].k_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].k_500pc_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].k_500pc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].k_1kpc_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].k_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].k_2kpc_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].k_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].k_sbrt		= float(sta[p]) - exs[2] & p=p+1
		endelse
	endwhile
	free_lun,tl
endif else if not keyword_set(silent) then print,'Not found: ',tfile
;
; get WISE site data
wfile = !SNE_DATA+'wise_site.dat'
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
			if sndat[m].mwebmv gt -1. then begin
				ebv = sndat[m].mwebmv
				exs = [ glga_getextin(ebv,'w1',/yuan13), $
					glga_getextin(ebv,'w2',/yuan13), $
					glga_getextin(ebv,'w3',/yuan13), $
					glga_getextin(ebv,'w4',/yuan13) ]
			endif else	exs = fltarr(4)
			if finfo.mtime gt sndat[m].mod_time then $
				sndat[m].mod_time = finfo.mtime
			p=1
			sndat[m].w1_res_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].w1_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w1_500pc_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].w1_500pc_magerr= float(sta[p]) 	 & p=p+1
			sndat[m].w1_1kpc_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].w1_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w1_2kpc_mag	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].w1_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w1_sbrt	= float(sta[p]) - exs[0] & p=p+1
			sndat[m].w2_res_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w2_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w2_500pc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w2_500pc_magerr= float(sta[p]) 	 & p=p+1
			sndat[m].w2_1kpc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w2_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w2_2kpc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w2_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w2_sbrt	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w3_res_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w3_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w3_500pc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w3_500pc_magerr= float(sta[p]) 	 & p=p+1
			sndat[m].w3_1kpc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w3_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w3_2kpc_mag	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w3_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w3_sbrt	= float(sta[p]) - exs[1] & p=p+1
			sndat[m].w4_res_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].w4_res_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w4_500pc_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].w4_500pc_magerr= float(sta[p]) 	 & p=p+1
			sndat[m].w4_1kpc_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].w4_1kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w4_2kpc_mag	= float(sta[p]) - exs[2] & p=p+1
			sndat[m].w4_2kpc_magerr	= float(sta[p]) 	 & p=p+1
			sndat[m].w4_sbrt	= float(sta[p]) - exs[2] & p=p+1
		endelse
	endwhile
	free_lun,wl
endif else if not keyword_set(silent) then print,'Not found: ',wfile
;
return
end
