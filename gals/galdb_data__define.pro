pro galdb_data__define
;+
; galdb_data__define - define galaxy database structure
;
; NOTE: remember to keep structure name on a line by itself 
;	(see struct_init.pro)
;
; see structure snhphsrc for host photometry sources:
;	higher source numbers are more reliable
;
;-
tmp = { galdb_data, $
	id:'', altids:'', $		; IDs (alts are comma sep list)
	sample:'', $			; list of sample names (comma sep)
	sne:'', $			; list of SNe (comma sep, if any)
	ra:-9.d0, dec:-99.d0, coo_src:'', $	; coords (J2000)
	cz:-9999.d0, $			; radial velocity (km/s)
	type:'', tyn:-99.0, $		; type
	inc:-99.0 , pa:-99.0, $		; inclination, PA (degrees)
	majax:-9., minax:-9., $		; major/minor axes (arcsec)
	mu:-9., mwebmv:-99., mag:-9., $	
	ned_gal:0, $			; in NED?
	neighbor:0, $			; Neighbor? 0 - no, 1 - yes
	fuv_int_mag:-99., fuv_int_magerr:-9., $
	nuv_int_mag:-99., nuv_int_magerr:-9., $
	fuv_int_src:0, nuv_int_src:0, $	; source for UV mags
	uJ_int_mag:-99., uJ_int_magerr:-9., uJ_int_src:0, $	; Johnson
	bJ_int_mag:-99., bJ_int_magerr:-9., bJ_int_src:0, $
	vJ_int_mag:-99., vJ_int_magerr:-9., vJ_int_src:0, $
	u_int_mag:-99., u_int_magerr:-9., u_int_src:0, $ ; SDSS
	g_int_mag:-99., g_int_magerr:-9., g_int_src:0, $
	r_int_mag:-99., r_int_magerr:-9., r_int_src:0, $
	i_int_mag:-99., i_int_magerr:-9., i_int_src:0, $
	z_int_mag:-99., z_int_magerr:-9., z_int_src:0, $
	u_abs_mag:-99., u_abs_magerr:-9., u_abs_src:0, $
	g_abs_mag:-99., g_abs_magerr:-9., g_abs_src:0, $
	r_abs_mag:-99., r_abs_magerr:-9., r_abs_src:0, $
	i_abs_mag:-99., i_abs_magerr:-9., i_abs_src:0, $
	z_abs_mag:-99., z_abs_magerr:-9., z_abs_src:0, $
	J_abs_mag:-99., J_abs_magerr:-9., J_abs_src:0, $ ; 2MASS
	H_abs_mag:-99., H_abs_magerr:-9., H_abs_src:0, $
	K_abs_mag:-99., K_abs_magerr:-9., K_abs_src:0, $
	J_int_mag:-99., J_int_magerr:-9., J_int_src:0, $
	H_int_mag:-99., H_int_magerr:-9., H_int_src:0, $
	K_int_mag:-99., K_int_magerr:-9., K_int_src:0, $
	w1_int_mag:-99., w1_int_magerr:-9., w1_int_src:0, $ ; WISE
	w2_int_mag:-99., w2_int_magerr:-9., w2_int_src:0, $
	w3_int_mag:-99., w3_int_magerr:-9., w3_int_src:0, $
	w4_int_mag:-99., w4_int_magerr:-9., w4_int_src:0, $
	i12m_int_mag:-99., i12m_int_magerr:-9., i12m_int_src:0, $ ; IRAS
	i25m_int_mag:-99., i25m_int_magerr:-9., i25m_int_src:0, $
	i60m_int_mag:-99., i60m_int_magerr:-9., i60m_int_src:0, $
	i100m_int_mag:-99., i100m_int_magerr:-9., i100m_int_src:0, $
	B_abs_mag:-99., B_abs_src:0, O_abund:-99., O_abund_src:0, $
	linear_scale:-9., ap_res:-9., ap_500pc:-9., $
	ap_1kpc:-9., ap_2kpc:-9., $
	mod_time:0.d0 $		; time stamp in seconds systime(1)
	}
end
