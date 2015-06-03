pro ned_phot_abmag,irec,mag,magerr
;
; parse NED photometry record and convert Jy to ABMag
;
	fABnu0 = 3630.78d0	; Jy
	rec=irec
	for i=0,6 do val=gettok(rec,'|')
	if not valid_num(val,fjy) then $ ; flux in Jy
		fjy = -9.
	val=gettok(rec,'|')
	jnk=gettok(val,'-')
	if not valid_num(val,fjyerr) then $	; error in Jy
		fjyerr = -9.
	if fjy gt 0. then begin
		mag = -2.5*alog10(fjy/fABnu0)
		if fjyerr gt 0. then $
			magerr = 1.0857362d0 * (fjyerr/fjy) $
		else	magerr = -9.
	endif else begin
		mag = -99.
		magerr = -9.
	endelse
return
end

pro ned_phot_vegamag,irec,mag,magerr
;
; parse NED photometry record and get vega mag
;
	rec=irec
	for i=0,2 do val=gettok(rec,'|')
	if not valid_num(val,mag) then $	; mag
		mag = -99.
	val=gettok(rec,'|')
	jnk=gettok(val,'-')
	if not valid_num(val,magerr) then $
		magerr = -9.
	val=strtrim(gettok(rec,'|'),2)
	if not strcmp(val,'mag') then begin
		mag = -99.
		magerr = -9.
	endif
return
end

pro get_ned_phot,obj, $
	fuv=fuv,errfuv=errfuv,srcfuv=srcfuv, $
	nuv=nuv,errnuv=errnuv,srcnuv=srcnuv, $
	su_t=su_t,errsu_t=errsu_t,srcsu_t=srcsu_t, $
	sg_t=sg_t,errsg_t=errsg_t,srcsg_t=srcsg_t, $
	sr_t=sr_t,errsr_t=errsr_t,srcsr_t=srcsr_t, $
	si_t=si_t,errsi_t=errsi_t,srcsi_t=srcsi_t, $
	sz_t=sz_t,errsz_t=errsz_t,srcsz_t=srcsz_t, $
	u_t=u_t,erru_t=erru_t,srcu_t=srcu_t, $
	u0_t=u0_t,erru0_t=erru0_t,srcu0_t=srcu0_t, $
	b_t=b_t,errb_t=errb_t,srcb_t=srcb_t, $
	b0_t=b0_t,errb0_t=errb0_t,srcb0_t=srcb0_t, $
	v_t=v_t,errv_t=errv_t,srcv_t=srcv_t, $
	v0_t=v0_t,errv0_t=errv0_t,srcv0_t=srcv0_t, $
	r_t=r_t,errr_t=errr_t,srcr_t=srcr_t, $
	j_t=j_t,errj_t=errj_t,srcj_t=srcj_t, $
	h_t=h_t,errh_t=errh_t,srch_t=srch_t, $
	k_t=k_t,errk_t=errk_t,srck_t=srck_t, $
	iras_12m=iras_12m,erriras_12m=erriras_12m,srciras_12m=srciras_12m, $
	iras_25m=iras_25m,erriras_25m=erriras_25m,srciras_25m=srciras_25m, $
	iras_60m=iras_60m,erriras_60m=erriras_60m,srciras_60m=srciras_60m, $
       iras_100m=iras_100m,erriras_100m=erriras_100m,srciras_100m=srciras_100m,$
	ned_gal=ned_gal,reread=reread,silent=silent
;
; init
ned_gal = 0
;
; see if it's in cache
nedir=!NED_CACHE
fspec = nedir+'/'+obj+'_phot.html'
flist=file_search(fspec,count=nf)
;
; if not, use wget to retrieve NED data
if nf lt 1 or keyword_set(reread) then begin
    cmd='wget "http://ned.ipac.caltech.edu/cgi-bin/nph-datasearch?objname='+$
	    obj+'&meas_type=bot&ebars_spec=ebars&label_spec=no&x_spec=freq&y_spec=Fnu_jy&xr=-1&of=ascii_bar&search_type=Photometry" -O '+nedir+'/'+obj+'_phot.html'
    if not keyword_set(silent) then begin
	    print,'wgetting from NED...'
	    print,cmd
    endif
    spawn,cmd,res,eres
endif
;
; get file
flist=file_search(fspec,count=nf)
if nf eq 1 then begin
;
; read in file
	openr,il,flist[0],/get_lun
	rec=''
	page=''
	while not eof(il) do begin
		readf,il,rec
		page=[page,rec]
	endwhile
	free_lun,il
;
; check output
	if n_elements(page) lt 2 then begin
		if not keyword_set(silent) then $
			print,obj+' file error or not found in NED, recommend using /reread.'
		page=['']
	endif else page=page[1:*]
;
	t=where(strpos(page,'not currently recognized') ge 0, n)
	if n gt 0 then begin
		if not keyword_set(silent) then $
			print,obj+' not found in NED'
		prt = (1 eq 0)
	endif else prt = (1 eq 1)
	if keyword_set(silent) then prt = (1 eq 0)
;
; get data
;
; FUV
	t=where(strpos(page,'FUV (GALEX)') ge 0,n)
	srcfuv=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,fuv,errfuv
		ned_gal = 1
		srcfuv = 'NGA'
	endif else begin
		if prt and keyword_set(fuv) then print,'FUV (GALEX) not found'
		fuv=-99.
		errfuv=-9.
	endelse
;
; NUV
	t=where(strpos(page,'NUV (GALEX)') ge 0,n)
	srcnuv=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,nuv,errnuv
		ned_gal = 1
		srcnuv = 'NGA'
	endif else begin
		if prt and keyword_set(nuv) then print,'NUV (GALEX) not found'
		nuv=-99.
		errnuv=-9.
	endelse
;
; SDSS u_T
	t=where(strpos(page,'u (SDSS Model)') ge 0,n)
	srcsu_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,su_t,errsu_t
		ned_gal = 1
		srcsu_t='SDSS Model'
		t=where(strpos(page,'u (SDSS CModel)') ge 0,n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_abmag,rec,su_t
			ned_gal = 1
		endif
	endif else begin
		if prt and keyword_set(su_t) then print,'SDSS u not found'
		su_t=-99.
		errsu_t=-9.
	endelse
;
; SDSS g_T
	t=where(strpos(page,'g (SDSS Model)') ge 0,n)
	srcsg_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,sg_t,errsg_t
		ned_gal = 1
		srcsg_t='SDSS Model'
		t=where(strpos(page,'g (SDSS CModel)') ge 0,n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_abmag,rec,sg_t
			ned_gal = 1
		endif
	endif else begin
		t=where(strpos(page,'g (SDSS)') ge 0, n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_abmag,rec,sg_t,errsg_t
			ned_gal = 1
			srcsg_t='SDSS'
		endif else begin
			if prt and keyword_set(sg_t) then print,'SDSS g not found'
			sg_t=-99.
			errsg_t=-9.
		endelse
	endelse
;
; SDSS r_T
	t=where(strpos(page,'r (SDSS Model)') ge 0,n)
	srcsr_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,sr_t,errsr_t
		ned_gal = 1
		srcsr_t='SDSS Model'
		t=where(strpos(page,'r (SDSS CModel)') ge 0,n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_abmag,rec,sr_t
			ned_gal = 1
		endif
	endif else begin
		t=where(strpos(page,'r (SDSS)') ge 0, n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_abmag,rec,sr_t,errsr_t
			ned_gal = 1
			srcsr_t='SDSS'
		endif else begin
			if prt and keyword_set(sr_t) then print,'SDSS r not found'
			sr_t=-99.
			errsr_t=-9.
		endelse
	endelse
;
; SDSS i_T
	t=where(strpos(page,'i (SDSS Model)') ge 0,n)
	srcsi_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,si_t,errsi_t
		ned_gal = 1
		srcsi_t='SDSS Model'
		t=where(strpos(page,'i (SDSS CModel)') ge 0,n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_abmag,rec,si_t
			ned_gal = 1
		endif
	endif else begin
		if prt and keyword_set(si_t) then print,'SDSS i not found'
		si_t=-99.
		errsi_t=-9.
	endelse
;
; SDSS z_T
	t=where(strpos(page,'z (SDSS Model)') ge 0,n)
	srcsz_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,sz_t,errsz_t
		ned_gal = 1
		srcsz_t='SDSS Model'
		t=where(strpos(page,'z (SDSS CModel)') ge 0,n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_abmag,rec,sz_t
			ned_gal = 1
		endif
	endif else begin
		if prt and keyword_set(sz_t) then print,'SDSS z not found'
		sz_t=-99.
		errsz_t=-9.
	endelse
;
; U_T
	t=where(strpos(page,'U (U_T)') ge 0,n)
	srcu_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,u_t,erru_t
		ned_gal = 1
		srcu_t='RC3'
	endif else begin
		if prt and keyword_set(u_t) then print,'u_t not found'
		u_t=-99.
		erru_t=-9.
	endelse
;
; U_T0
	t=where(strpos(page,'U (U_T^0)') ge 0,n)
	srcu0_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,u0_t,erru0_t
		ned_gal = 1
		srcu0_t='RC3'
	endif else begin
		if prt and keyword_set(u0_t) then print,'u0_t not found'
		u0_t=-99.
		erru0_t=-9.
	endelse
;
; B_T
	t=where(strpos(page,'B (B_T)') ge 0,n)
	srcb_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,b_t,errb_t
		ned_gal = 1
		srcb_t='RC3'
	endif else begin
	    t=where(strpos(page,'B (Cousins) (B_T)') ge 0, n)
	    if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,b_t,errb_t
		ned_gal = 1
		srcb_t='ESOLV89'
	    endif else begin
		t=where(strpos(page,'B (m_B)') ge 0, n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_vegamag,rec,b_t,errb_t
			ned_gal = 1
			srcb_t='RC3'
		endif else begin
			t=where(strpos(page,'B_J') ge 0 and $
				strpos(page,'2005MNRAS') ge 0, n)
			if n gt 0 then begin
				rec = page[t[0]]
				ned_phot_vegamag,rec,b_t
				ned_gal = 1
				srcb_t='HIPASSIII'
				errb_t = 0.1
			endif else begin
				if prt and keyword_set(b_t) then print,'b_t not found'
				b_t=-99.
				errb_t=-9.
			endelse
		endelse
	    endelse
	endelse
;
; B_T0
	t=where(strpos(page,'B (B_T^0)') ge 0,n)
	srcb0_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,b0_t,errb0_t
		ned_gal = 1
		srcb0_t='RC3'
	endif else begin
		if prt and keyword_set(b0_t) then print,'b0_t not found'
		b0_t=-99.
		errb0_t=-9.
	endelse
;
; V_T
	t=where(strpos(page,'V (V_T)') ge 0,n)
	srcv_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,v_t,errv_t
		ned_gal = 1
		srcv_t='RC3'
	endif else begin
		if prt and keyword_set(v_t) then print,'v_t not found'
		v_t=-99.
		errv_t=-9.
	endelse
;
; V_T0
	t=where(strpos(page,'V (V_T^0)') ge 0,n)
	srcv0_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,v0_t,errv0_t
		ned_gal = 1
		srcv0_t='RC3'
	endif else begin
		if prt and keyword_set(v0_t) then print,'v0_t not found'
		v0_t=-99.
		errv0_t=-9.
	endelse
;
; R_T
	t=where(strpos(page,'R (Cousins) (R_T)') ge 0,n)
	srcr_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,r_t,errr_t
		ned_gal = 1
		srcr_t='ESOLV89'
	endif else begin
		if prt and keyword_set(r_t) then print,'r_t not found'
		r_t=-99.
		errr_t=-9.
	endelse
;
; J_T
	t=where(strpos(page,'J_tot (2MASS LGA)') ge 0,n)
	srcj_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,j_t,errj_t
		ned_gal = 1
		srcj_t='LGA'
	endif else begin
		t=where(strpos(page,'J_total') ge 0,n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_vegamag,rec,j_t,errj_t
			ned_gal = 1
			srcj_t='XSC'
		endif else begin
			if prt and keyword_set(j_t) then print,'j_t not found'
			j_t=-99.
			errj_t=-9.
		endelse
	endelse
;
; H_T
	t=where(strpos(page,'H_tot (2MASS LGA)') ge 0,n)
	srch_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,h_t,errh_t
		ned_gal = 1
		srch_t='LGA'
	endif else begin
		t=where(strpos(page,'H_total') ge 0,n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_vegamag,rec,h_t,errh_t
			ned_gal = 1
			srch_t='XSC'
		endif else begin
			if prt and keyword_set(h_t) then print,'h_t not found'
			h_t=-99.
			errh_t=-9.
		endelse
	endelse
;
; K_T
	t=where(strpos(page,'K_tot (2MASS LGA)') ge 0,n)
	srck_t=''
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_vegamag,rec,k_t,errk_t
		ned_gal = 1
		srck_t='LGA'
	endif else begin
		t=where(strpos(page,'K_s_total') ge 0,n)
		if n gt 0 then begin
			rec = page[t[0]]
			ned_phot_vegamag,rec,k_t,errk_t
			ned_gal = 1
			srck_t='XSC'
		endif else begin
			if prt and keyword_set(k_t) then print,'k_t not found'
			k_t=-99.
			errk_t=-9.
		endelse
	endelse
;
; IRAS 12 mu
	srciras_12m=''
	t=where(strpos(page,'IRAS 12 microns') ge 0 and $
		strpos(page,'1988ApJS') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_12m,erriras_12m
		ned_gal = 1
		srciras_12m='IRASLOG'
	endif else begin
	t=where(strpos(page,'IRAS 12 microns') ge 0 and $
		strpos(page,'1994PrivC') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_12m,erriras_12m
		ned_gal = 1
		srciras_12m='IRASKnapp94'
	endif else begin
	t=where(strpos(page,'IRAS 12 microns') ge 0 and $
		strpos(page,'1990IRASF') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_12m,erriras_12m
		ned_gal = 1
		srciras_12m='IRASFSC'
	endif else begin
	t=where(strpos(page,'IRAS 12 microns') ge 0 and $
		strpos(page,'Total flux') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_12m,erriras_12m
		ned_gal = 1
		srciras_12m='IRASRBGS'
	endif else begin
	t=where(strpos(page,'IRAS 12 microns') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_12m,erriras_12m
		ned_gal = 1
		srciras_12m='IRAS'
	endif else begin
		if prt and keyword_set(iras_12m) then print,'iras_12m not found'
		iras_12m=-99.
		erriras_12m=-9.
	endelse
	endelse
	endelse
	endelse
	endelse
;
; IRAS 25 mu
	srciras_25m=''
	t=where(strpos(page,'IRAS 25 microns') ge 0 and $
		strpos(page,'1988ApJS') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_25m,erriras_25m
		ned_gal = 1
		srciras_25m='IRASLOG'
	endif else begin
	t=where(strpos(page,'IRAS 25 microns') ge 0 and $
		strpos(page,'1994PrivC') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_25m,erriras_25m
		ned_gal = 1
		srciras_25m='IRASKnapp94'
	endif else begin
	t=where(strpos(page,'IRAS 25 microns') ge 0 and $
		strpos(page,'1990IRASF') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_25m,erriras_25m
		ned_gal = 1
		srciras_25m='IRASFSC'
	endif else begin
	t=where(strpos(page,'IRAS 25 microns') ge 0 and $
		strpos(page,'Total flux') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_25m,erriras_25m
		ned_gal = 1
		srciras_25m='IRASRBGS'
	endif else begin
	t=where(strpos(page,'IRAS 25 microns') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_25m,erriras_25m
		ned_gal = 1
		srciras_25m='IRAS'
	endif else begin
		if prt and keyword_set(iras_25m) then print,'iras_25m not found'
		iras_25m=-99.
		erriras_25m=-9.
	endelse
	endelse
	endelse
	endelse
	endelse
;
; IRAS 60 mu
	srciras_60m=''
	t=where(strpos(page,'IRAS 60 microns') ge 0 and $
		strpos(page,'1988ApJS') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_60m,erriras_60m
		ned_gal = 1
		srciras_60m='IRASLOG'
	endif else begin
	t=where(strpos(page,'IRAS 60 microns') ge 0 and $
		strpos(page,'1994PrivC') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_60m,erriras_60m
		ned_gal = 1
		srciras_60m='IRASKnapp94'
	endif else begin
	t=where(strpos(page,'IRAS 60 microns') ge 0 and $
		strpos(page,'1990IRASF') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_60m,erriras_60m
		ned_gal = 1
		srciras_60m='IRASFSC'
	endif else begin
	t=where(strpos(page,'IRAS 60 microns') ge 0 and $
		strpos(page,'Total flux') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_60m,erriras_60m
		ned_gal = 1
		srciras_60m='IRASRBGS'
	endif else begin
	t=where(strpos(page,'IRAS 60 microns') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_60m,erriras_60m
		ned_gal = 1
		srciras_60m='IRAS'
	endif else begin
		if prt and keyword_set(iras_60m) then print,'iras_60m not found'
		iras_60m=-99.
		erriras_60m=-9.
	endelse
	endelse
	endelse
	endelse
	endelse
;
; IRAS 100 mu
	srciras_100m=''
	t=where(strpos(page,'IRAS 100 microns') ge 0 and $
		strpos(page,'1988ApJS') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_100m,erriras_100m
		ned_gal = 1
		srciras_100m='IRASLOG'
	endif else begin
	t=where(strpos(page,'IRAS 100 microns') ge 0 and $
		strpos(page,'1994PrivC') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_100m,erriras_100m
		ned_gal = 1
		srciras_100m='IRASKnapp94'
	endif else begin
	t=where(strpos(page,'IRAS 100 microns') ge 0 and $
		strpos(page,'1990IRASF') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_100m,erriras_100m
		ned_gal = 1
		srciras_100m='IRASFSC'
	endif else begin
	t=where(strpos(page,'IRAS 100 microns') ge 0 and $
		strpos(page,'Total flux') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_100m,erriras_100m
		ned_gal = 1
		srciras_100m='IRASRBGS'
	endif else begin
	t=where(strpos(page,'IRAS 100 microns') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		ned_phot_abmag,rec,iras_100m,erriras_100m
		ned_gal = 1
		srciras_100m='IRAS'
	endif else begin
		if prt and keyword_set(iras_100m) then print,'iras_100m not found'
		iras_100m=-99.
		erriras_100m=-9.
	endelse
	endelse
	endelse
	endelse
	endelse

endif else print,'GET_NED_PHOT - Wrong number of files found: ',nf
;
return
end
