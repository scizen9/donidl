function get_snsam,samnam,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit, $
	merrlim=merrlim,verbose=verbose,silent=silent,nsam=nsam, $
	saminfo=saminfo,limleg=lem,apmags=apmags
;
; get samples 
;
; check input
if n_params(0) lt 1  then begin
	print,'get_snsam,samname,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit,merrlim=merrlim,nsam=nsam,saminfo=saminfo,limleg=limleg,/silent,/verbose'
	nsam=0
	return,-1
endif
;
; check keywords
nleg=1
if keyword_set(czlimit) then begin
	czlim = czlimit
	nleg = nleg + 1
endif else    czlim = 1.d9
if keyword_set(exlimit) then begin
	exlim = exlimit
	nleg = nleg + 1
endif else    exlim = 0.0
if keyword_set(inclimit) then begin
	inclim = inclimit
	nleg = nleg + 1
endif else    inclim = 90.0
if keyword_set(merrlim) then $
	melim = merrlim $
else    melim = 1.
;
; get sample
case strupcase(samnam) of
	'1':	begin
		saminfo='1 - SN Ia, Host int'
		its  = ['cz','tyn']
		itr  = [[-900.0,czlim],[1,1]]
		end
	'2':	begin
		saminfo='2 - ALL SNe (except Type I), Host int'
		its  = ['cz','tyn']
		itr  = [[-900.0,czlim],[1,9]]
		end
	'A':	begin
		if not keyword_set(merrlim) then melim = 0.25
		if keyword_set(apmags) then begin
			saminfo='A - ALL SNe, Host int, FUV, NUV, K, (m-M) AP'
			its  = ['hfuv_ap_mag','hfuv_ap_magerr', $
				'hnuv_ap_mag','hnuv_ap_magerr', $
				'hK_int_mag','hK_int_magerr', $
				'tyn','hmu','cz','hfuv_exptime','hinc']
		endif else begin
			saminfo='A - ALL SNe, Host int, FUV, NUV, K, (m-M)'
			its  = ['hfuv_int_mag','hfuv_int_magerr', $
				'hnuv_int_mag','hnuv_int_magerr', $
				'hK_int_mag','hK_int_magerr', $
				'tyn','hmu','cz','hfuv_exptime','hinc']
		endelse
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[1,9],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'B':	begin
		if not keyword_set(merrlim) then melim = 1.0
		saminfo='B - ALL SNe, SN sites, FUV, NUV, K, (m-M)'
		its  = ['hfuv_1kpc_mag','hfuv_1kpc_magerr', $
			'hnuv_1kpc_mag','hnuv_1kpc_magerr', $
			'hK_1kpc_mag','hK_1kpc_magerr', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[1,9],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'C':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='C - ALL SNe, Host int, FUV, NUV, K, SDSS'
		its  = ['hfuv_int_mag','hfuv_int_magerr', $
			'hnuv_int_mag','hnuv_int_magerr', $
			'hK_int_mag','hK_int_magerr', $
			'hr_int_mag','hr_int_magerr', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[1,9],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'D':	begin
		if not keyword_set(merrlim) then melim = 1.0
		saminfo='D - ALL SNe, SN sites, FUV, NUV, K, SDSS'
		its  = ['hfuv_1kpc_mag','hfuv_1kpc_magerr', $
			'hnuv_1kpc_mag','hnuv_1kpc_magerr', $
			'hK_1kpc_mag','hK_1kpc_magerr', $
			'hr_1kpc_mag','hr_1kpc_magerr', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[1,9],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'E':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='E - SNe Ias, Host int, FUV, NUV, K, (m-M), LC: str, B-V'
		its  = ['hfuv_int_mag','hfuv_int_magerr', $
			'hnuv_int_mag','hnuv_int_magerr', $
			'hK_int_mag','hK_int_magerr', $
			'smpl_str', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,2.], $
			[1,1],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'F':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='F - SNe Ias, Host int, FUV, NUV, K, (m-M), LC: Delta, A0(V)'
		its  = ['hfuv_int_mag','hfuv_int_magerr', $
			'hnuv_int_mag','hnuv_int_magerr', $
			'hK_int_mag','hK_int_magerr', $
			'mlcs2k_delta', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[-5,5.], $
			[1,1],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'G':	begin
		if not keyword_set(merrlim) then melim = 1.0
		saminfo='G - SNe Ias, SN sites, FUV, NUV, K, (m-M), LC: str, B-V'
		its  = ['hfuv_1kpc_mag','hfuv_1kpc_magerr', $
			'hnuv_1kpc_mag','hnuv_1kpc_magerr', $
			'hK_1kpc_mag','hK_1kpc_magerr', $
			'smpl_str', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,2.], $
			[1,1],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'H':	begin
		if not keyword_set(merrlim) then melim = 1.0
		saminfo='H - SNe Ias, SN sites, FUV, NUV, K, (m-M), LC: Delta, A0(V)'
		its  = ['hfuv_1kpc_mag','hfuv_1kpc_magerr', $
			'hnuv_1kpc_mag','hnuv_1kpc_magerr', $
			'hK_1kpc_mag','hK_1kpc_magerr', $
			'mlcs2k_delta', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[-5,5.], $
			[1,1],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'I':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='I - SNe Ias, Host int, FUV, NUV, K, SDSS, LC: str, B-V'
		its  = ['hfuv_int_mag','hfuv_int_magerr', $
			'hnuv_int_mag','hnuv_int_magerr', $
			'hK_int_mag','hK_int_magerr', $
			'hr_int_mag','hr_int_magerr', $
			'smpl_str', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,2.], $
			[1,1],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'J':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='J - SNe Ias, Host int, FUV, NUV, K, SDSS, LC: Delta, A0(V)'
		its  = ['hfuv_int_mag','hfuv_int_magerr', $
			'hnuv_int_mag','hnuv_int_magerr', $
			'hK_int_mag','hK_int_magerr', $
			'hr_int_mag','hr_int_magerr', $
			'mlcs2k_delta', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[-5.,5.], $
			[1,1],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'K':	begin
		if not keyword_set(merrlim) then melim = 1.0
		saminfo='K - SNe Ias, SN sites, FUV, NUV, K, SDSS, LC: str, B-V'
		its  = ['hfuv_1kpc_mag','hfuv_1kpc_magerr', $
			'hnuv_1kpc_mag','hnuv_1kpc_magerr', $
			'hK_1kpc_mag','hK_1kpc_magerr', $
			'hr_1kpc_mag','hr_1kpc_magerr', $
			'smpl_str', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0., 2.], $
			[1,1],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'L':	begin
		if not keyword_set(merrlim) then melim = 1.0
		saminfo='L - SNe Ias, SN sites, FUV, NUV, K, SDSS, LC: Delta, A0(V)'
		its  = ['hfuv_1kpc_mag','hfuv_1kpc_magerr', $
			'hnuv_1kpc_mag','hnuv_1kpc_magerr', $
			'hK_1kpc_mag','hK_1kpc_magerr', $
			'hr_1kpc_mag','hr_1kpc_magerr', $
			'mlcs2k_delta', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[-5.,5.], $
			[1,1],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'M':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='M - ALL SNe, Host int, FUV, NUV, K, Johnson B'
		its  = ['hfuv_int_mag','hfuv_int_magerr', $
			'hnuv_int_mag','hnuv_int_magerr', $
			'hK_int_mag','hK_int_magerr', $
			'hbJ_int_mag','hbJ_int_magerr', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[1,9],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'N':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='N - ALL SNe, Host int, FUV, IRAS 100mu'
		its  = ['hfuv_int_mag','hfuv_int_magerr', $
			'h100m_int_mag','h100m_int_magerr', $
			'tyn','hmu','cz','hfuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[1,9],[0.,99.], $
			[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	'P':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='P - ALL SNe, Host int, FUV, NUV, K'
		its  = ['hK_int_mag', 'hfuv_int_magerr', 'hnuv_int_magerr', $
			'cz']
		itr  = [[0,90.],[0.,melim], [0.,melim], $
			[-900.0,czlim]]
		end
	'Q':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='Q - SN Ia, Host int, NUV, K, no FUV'
		its  = ['hK_int_mag', 'hfuv_int_mag', 'hnuv_int_magerr', $
			'cz','tyn']
		itr  = [[0,90.],[-1000.,-1.],[0.,melim], $
			[-900.0,czlim],[1,1]]
		end
	'S':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='S - SN Ia, Good Stretch'
		its  = ['smpl_ustr','tyn']
		itr  = [[1,1],[1,1]]
		end
	'W':	begin
		if not keyword_set(merrlim) then melim = 0.25
		saminfo='W - ALL SNe, Host int, NUV, SDSS'
		its  = ['hnuv_int_mag','hnuv_int_magerr', $
			'hr_int_mag','hr_int_magerr', $
			'hr_abs_mag','hr_abs_magerr', $
			'tyn','cz','nuv_exptime','hinc']
		itr  = [[0.,30.],[0.,melim], $
			[0.,30.],[0.,melim], $
			[-30.,0.],[0.,melim], $
			[1,9],[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
		end
	else:	begin
		print,'Sample name must be one of:'
		print,'1 - SN Ia, Host int'
		print,'2 - ALL SNe (except Type I), Host int'
		print,'A - ALL SNe, Host int, FUV, NUV, K, (m-M)'
		print,'B - ALL SNe, SN sites, FUV, NUV, K, (m-M)'
		print,'C - ALL SNe, Host int, FUV, NUV, K, SDSS'
		print,'D - ALL SNe, SN sites, FUV, NUV, K, SDSS'
		print,'E - SNe Ias, Host int, FUV, NUV, K, (m-M), LC: str, B-V'
		print,'F - SNe Ias, Host int, FUV, NUV, K, (m-M), LC: Delta, A0(V)'
		print,'G - SNe Ias, SN sites, FUV, NUV, K, (m-M), LC: str, B-V'
		print,'H - SNe Ias, SN sites, FUV, NUV, K, (m-M), LC: Delta, A0(V)'
		print,'I - SNe Ias, Host int, FUV, NUV, K, SDSS, LC: str, B-V'
		print,'J - SNe Ias, Host int, FUV, NUV, K, SDSS, LC: Delta, A0(V)'
		print,'K - SNe Ias, SN sites, FUV, NUV, K, SDSS, LC: str, B-V'
		print,'L - SNe Ias, SN sites, FUV, NUV, K, SDSS, LC: Delta, A0(V)'
		print,'M - ALL SNe, Host int, FUV, NUV, K, Johnson B'
		print,'N - ALL SNe, Host int, FUV, IRAS 100mu'
		print,'P - ALL SNe, Host int, FUV, NUV, K'
		print,'Q - SN Ia, Host int, NUV, K, no FUV'
		print,'S - SN Ia, Good Stretch'
		print,'W - ALL SNe, Host int, NUV, SDSS'
		nsam=0
		return,-1
		end
endcase
;
; set up legend
pleg = 0
lem=strarr(nleg)
if czlim lt 1.d8 then begin
	lem(pleg) = 'cz \leq '+ strtrim(string(czlim,form='(f12.1)'),2)+' km/s'
	pleg = pleg + 1
endif
if exlim gt 0. then begin
	lem(pleg) = 'Exptime > '+ strtrim(string(exlim,form='(f6.1)'),2)+' s'
	pleg = pleg + 1
endif
if inclim lt 90. then begin
	lem(pleg) = 'Host inc \leq '+ $
		strtrim(string(fix(inclim+0.5),form='(i2)'),2)+'^{\circ}'
	pleg = pleg + 1
endif
lem(pleg) = 'Mgerr \leq '+strtrim(string(melim,form='(f4.2)'),2)
;
; get sample
g = snsample(its,itr,count=nsam)
if not keyword_set(silent) and keyword_set(verbose) then begin
	print,saminfo
	print,'Total Sample: ',nsam,' SNe'
endif
;
return,g
end
