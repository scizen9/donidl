pro sndb_data__define
;+
; sndb_data__define - define sn database structure
;
; NOTE: remember to keep structure name on a line by itself 
;	(see struct_init.pro)
;
; Catalog data are first followed by analysis data
;
; see structure snhphsrc for host photometry sources:
;	higher source numbers are more reliable
;
;-
tmp = { sndb_data, $
	id:'', iau_id:'', $		; canonical ID and IAU ID
	srv_id:'', srv_name:'', $	; IDs from surveys (non-IAU)
	type:'', tyn:-1, $		; type string and numerical
	ra:-9.d0, dec:-99.d0, coo_src:'', $
	cz:-9999.d0, $			; radial velocity
	off_ew: -9999., off_ns: -9999., $	; offsets from center in asec
	r_iso:-9., r_equiv:-9., $	; sn radii in host
	filt:'', mag:-9., mag_type:'', $
	mwebmv:-9., $			; Milky Way color excess E(B-V)
	date:'', discoverer:'', $
	host:'', $					; host id
	hra:-9.d0, hdec:-99.d0, hcoo_src:'', $		; host coords
	hmajax:-9., hminax:-9., hpa:-9., hinc:-9., $	; host shape params
	fuv_frac:-9., nuv_frac:-9., $	; host frac light inside sn pos
	u_frac:-9., g_frac:-9., r_frac:-9., $	; SDSS
	i_frac:-9., z_frac:-9., $
	J_frac:-9., H_frac:-9., K_frac:-9., $	; 2MASS
	w1_frac:-9., w2_frac:-9., w3_frac:-9, w4_frac:-9, $	; WISE
	fuv_pdel:-9., fuv_pdelerr:-9., $ ; profile deltas
	nuv_pdel:-9., nuv_pdelerr:-9., $ ; difference between SN site
	u_pdel:-9., u_pdelerr:-9., $	; SDSS	; and profile in mags
	g_pdel:-9., g_pdelerr:-9., $
	r_pdel:-9., r_pdelerr:-9., $
	i_pdel:-9., i_pdelerr:-9., $
	z_pdel:-9., z_pdelerr:-9., $
	J_pdel:-9., J_pdelerr:-9., $	; 2MASS
	H_pdel:-9., H_pdelerr:-9., $
	K_pdel:-9., K_pdelerr:-9., $
	w1_pdel:-9., w1_pdelerr:-9., $	; WISE
	w2_pdel:-9., w2_pdelerr:-9., $
	w3_pdel:-9., w3_pdelerr:-9., $
	w4_pdel:-9., w4_pdelerr:-9., $
	fuv_res_mag:-99., fuv_res_magerr:-9., $ ; NUV resolution aper
	nuv_res_mag:-99., nuv_res_magerr:-9., $
	u_res_mag:-99., u_res_magerr:-9., $	; SDSS
	g_res_mag:-99., g_res_magerr:-9., $
	r_res_mag:-99., r_res_magerr:-9., $
	i_res_mag:-99., i_res_magerr:-9., $
	z_res_mag:-99., z_res_magerr:-9., $
	J_res_mag:-99., J_res_magerr:-9., $	; 2MASS
	H_res_mag:-99., H_res_magerr:-9., $
	K_res_mag:-99., K_res_magerr:-9., $
	w1_res_mag:-99., w1_res_magerr:-9., $	; WISE
	w2_res_mag:-99., w2_res_magerr:-9., $
	w3_res_mag:-99., w3_res_magerr:-9., $
	w4_res_mag:-99., w4_res_magerr:-9., $
	fuv_500pc_mag:-99., fuv_500pc_magerr:-9., $	; 500pc aper
	nuv_500pc_mag:-99., nuv_500pc_magerr:-9., $
	u_500pc_mag:-99., u_500pc_magerr:-9., $	; SDSS
	g_500pc_mag:-99., g_500pc_magerr:-9., $
	r_500pc_mag:-99., r_500pc_magerr:-9., $
	i_500pc_mag:-99., i_500pc_magerr:-9., $
	z_500pc_mag:-99., z_500pc_magerr:-9., $
	J_500pc_mag:-99., J_500pc_magerr:-9., $	; 2MASS
	H_500pc_mag:-99., H_500pc_magerr:-9., $
	K_500pc_mag:-99., K_500pc_magerr:-9., $
	w1_500pc_mag:-99., w1_500pc_magerr:-9., $	; WISE
	w2_500pc_mag:-99., w2_500pc_magerr:-9., $
	w3_500pc_mag:-99., w3_500pc_magerr:-9., $
	w4_500pc_mag:-99., w4_500pc_magerr:-9., $
	fuv_1kpc_mag:-99., fuv_1kpc_magerr:-9., $	; 1kpc aper
	nuv_1kpc_mag:-99., nuv_1kpc_magerr:-9., $
	u_1kpc_mag:-99., u_1kpc_magerr:-9., $	; SDSS
	g_1kpc_mag:-99., g_1kpc_magerr:-9., $
	r_1kpc_mag:-99., r_1kpc_magerr:-9., $
	i_1kpc_mag:-99., i_1kpc_magerr:-9., $
	z_1kpc_mag:-99., z_1kpc_magerr:-9., $
	J_1kpc_mag:-99., J_1kpc_magerr:-9., $	; 2MASS
	H_1kpc_mag:-99., H_1kpc_magerr:-9., $
	K_1kpc_mag:-99., K_1kpc_magerr:-9., $
	w1_1kpc_mag:-99., w1_1kpc_magerr:-9., $	; WISE
	w2_1kpc_mag:-99., w2_1kpc_magerr:-9., $
	w3_1kpc_mag:-99., w3_1kpc_magerr:-9., $
	w4_1kpc_mag:-99., w4_1kpc_magerr:-9., $
	fuv_2kpc_mag:-99., fuv_2kpc_magerr:-9., $	; 2kpc aper
	nuv_2kpc_mag:-99., nuv_2kpc_magerr:-9., $
	u_2kpc_mag:-99., u_2kpc_magerr:-9., $	; SDSS
	g_2kpc_mag:-99., g_2kpc_magerr:-9., $
	r_2kpc_mag:-99., r_2kpc_magerr:-9., $
	i_2kpc_mag:-99., i_2kpc_magerr:-9., $
	z_2kpc_mag:-99., z_2kpc_magerr:-9., $
	J_2kpc_mag:-99., J_2kpc_magerr:-9., $	; 2MASS
	H_2kpc_mag:-99., H_2kpc_magerr:-9., $
	K_2kpc_mag:-99., K_2kpc_magerr:-9., $
	w1_2kpc_mag:-99., w1_2kpc_magerr:-9., $	; WISE
	w2_2kpc_mag:-99., w2_2kpc_magerr:-9., $
	w3_2kpc_mag:-99., w3_2kpc_magerr:-9., $
	w4_2kpc_mag:-99., w4_2kpc_magerr:-9., $
	fuv_sbrt:-99., nuv_sbrt:-99., $
	u_sbrt:-99., g_sbrt:-99., r_sbrt:-99., $	; SDSS
	i_sbrt:-99., z_sbrt:-99., $
	J_sbrt:-99., H_sbrt:-99., K_sbrt:-99., $	; 2MASS
	w1_sbrt:-99., w2_sbrt:-99., w3_sbrt:-99, w4_sbrt:-99, $	; WISE
	smpl_str:-9., smpl_strerr:-9., $	; SN LC SimpleFit fit params
	smpl_clr:-9., smpl_clrerr:-9., $
	smpl_zhel:-9., smpl_zhelerr:-9., $
	smpl_zcmb:-9., smpl_zcmberr:-9., $
	smpl_bmag:-9., smpl_bmagerr:-9., $
	smpl_ucos:0, smpl_ustr:0, $		; Use flags 0 - no, 1 - yes
	mlcs2k_delta:-9., mlcs2k_deltaerr:-9., $; MLCS2K fit params
	mlcs2k_a0v:-9., mlcs2k_a0verr:-9., $
	mlcs2k_rv:-9., mlcs2k_rverr:-9., $
	mlcs2k_m0v:-9., mlcs2k_m0verr:-9., $
	mlcs2k_mub:-9., mlcs2k_muberr:-9., $
	cfa3_delta:-9., cfa3_deltaerr:-9., $	; CfA3 SNeIa MLCS2K2
	cfa3_av:-9., cfa3_averr:-9., $		; R(V) = 1.7
	cfa3_bmv:-9., cfa3_bmverr:-9., $	; at Bmax
	cfa3_bmag:-9., cfa3_bmagerr:-9., $	; not A(V) corrected
	cfa3_vmag:-9., cfa3_vmagerr:-9., $	; not A(V) corrected
	cfa3_hebmv:-9., cfa3_hebmverr:-9., $	; Host color excess
	mod_time:0.d0 $		; time stamp in seconds systime(1)
	}
end
