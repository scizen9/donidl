pro glgadb_data__define
;+
; glgadb_data__define - define glga_status database structure
;
; NOTE: remember to keep structure name on a line by itself 
;	(see struct_init.pro)
;
; for _qa tags the following scheme applies:
;	-1 - qa file, but no images
;	 0 - no qa file
;	 1 - qa file current format and qa complete
;	 2 - qa file current format and qa not complete
;	 3 - qa file old format
;	 4 - qa file unreadable format
;
; Units:
;	ra, dec, pa: decimal degrees
;	majax, minax: arcseconds
;
;-
tmp = { glgadb_data, $
	id:'', type:'-', pgc:-9ll, $
	ra:-9.d0, dec:-99.d0, majax:-9., minax:-9., pa:-9., $
	timestamp:0ll, catalog:'', new_ell:0ll, dss_img:0ll, $
	fuv_img:0ll, nuv_img:0ll, $			; GALEX
	fuv_jpg:0ll, nuv_jpg:0ll, $
	fuv_plot:0ll, nuv_plot:0ll, $
	fuv_ap_a:-9., fuv_ap_mag:-99., fuv_ap_magerr:-9., $
	fuv_asym_a:-9., fuv_asym_mag:-99., fuv_asym_magerr:-9., $
	fuv_tot_a:-9., fuv_tot_mag:-99., fuv_tot_magerr:-9., $
	nuv_ap_a:-9., nuv_ap_mag:-99., nuv_ap_magerr:-9., $
	nuv_asym_a:-9., nuv_asym_mag:-99., nuv_asym_magerr:-9., $
	nuv_tot_a:-9., nuv_tot_mag:-99., nuv_tot_magerr:-9., $
	galex_jpg:0ll, galex_qa:0, galex_phot:0ll, galex_plots:0ll, $
	galex_roi:0ll, galex_pointsrc:0ll, galex_mask:0ll, $
	u_img:0ll, g_img:0ll, r_img:0ll, i_img:0ll, z_img:0ll, $	; SDSS
	u_jpg:0ll, g_jpg:0ll, r_jpg:0ll, i_jpg:0ll, z_jpg:0ll, $
	u_plot:0ll, g_plot:0ll, r_plot:0ll, i_plot:0ll, z_plot:0ll, $
	u_ap_a:-9., u_ap_mag:-99., u_ap_magerr:-9., $
	u_asym_a:-9., u_asym_mag:-99., u_asym_magerr:-9., $
	u_tot_a:-9., u_tot_mag:-99., u_tot_magerr:-9., $
	g_ap_a:-9., g_ap_mag:-99., g_ap_magerr:-9., $
	g_asym_a:-9., g_asym_mag:-99., g_asym_magerr:-9., $
	g_tot_a:-9., g_tot_mag:-99., g_tot_magerr:-9., $
	r_ap_a:-9., r_ap_mag:-99., r_ap_magerr:-9., $
	r_asym_a:-9., r_asym_mag:-99., r_asym_magerr:-9., $
	r_tot_a:-9., r_tot_mag:-99., r_tot_magerr:-9., $
	i_ap_a:-9., i_ap_mag:-99., i_ap_magerr:-9., $
	i_asym_a:-9., i_asym_mag:-99., i_asym_magerr:-9., $
	i_tot_a:-9., i_tot_mag:-99., i_tot_magerr:-9., $
	z_ap_a:-9., z_ap_mag:-99., z_ap_magerr:-9., $
	z_asym_a:-9., z_asym_mag:-99., z_asym_magerr:-9., $
	z_tot_a:-9., z_tot_mag:-99., z_tot_magerr:-9., $
	sdss_roi:0ll, sdss_pointsrc:0ll, sdss_mask:0ll, $
	sdss_jpg:0ll, sdss_qa:0, sdss_phot:0ll, sdss_plots:0ll, $
	j_img:0ll, h_img:0ll, k_img:0ll, $			; 2MASS
	j_jpg:0ll, h_jpg:0ll, k_jpg:0ll, $
	j_plot:0ll, h_plot:0ll, k_plot:0ll, $
	j_ap_a:-9., j_ap_mag:-99., j_ap_magerr:-9., $
	j_asym_a:-9., j_asym_mag:-99., j_asym_magerr:-9., $
	j_tot_a:-9., j_tot_mag:-99., j_tot_magerr:-9., $
	h_ap_a:-9., h_ap_mag:-99., h_ap_magerr:-9., $
	h_asym_a:-9., h_asym_mag:-99., h_asym_magerr:-9., $
	h_tot_a:-9., h_tot_mag:-99., h_tot_magerr:-9., $
	k_ap_a:-9., k_ap_mag:-99., k_ap_magerr:-9., $
	k_asym_a:-9., k_asym_mag:-99., k_asym_magerr:-9., $
	k_tot_a:-9., k_tot_mag:-99., k_tot_magerr:-9., $
	twomass_roi:0ll, twomass_pointsrc:0ll, twomass_mask:0ll, $
	twomass_jpg:0ll, twomass_qa:0, twomass_phot:0ll, twomass_plots:0ll, $
	w1_img:0ll, w2_img:0ll, w3_img:0ll, w4_img:0ll, $	; WISE
	w1_jpg:0ll, w2_jpg:0ll, w3_jpg:0ll, w4_jpg:0ll, $
	w1_plot:0ll, w2_plot:0ll, w3_plot:0ll, w4_plot:0ll, $
	w1_ap_a:-9., w1_ap_mag:-99., w1_ap_magerr:-9., $
	w1_asym_a:-9., w1_asym_mag:-99., w1_asym_magerr:-9., $
	w1_tot_a:-9., w1_tot_mag:-99., w1_tot_magerr:-9., $
	w2_ap_a:-9., w2_ap_mag:-99., w2_ap_magerr:-9., $
	w2_asym_a:-9., w2_asym_mag:-99., w2_asym_magerr:-9., $
	w2_tot_a:-9., w2_tot_mag:-99., w2_tot_magerr:-9., $
	w3_ap_a:-9., w3_ap_mag:-99., w3_ap_magerr:-9., $
	w3_asym_a:-9., w3_asym_mag:-99., w3_asym_magerr:-9., $
	w3_tot_a:-9., w3_tot_mag:-99., w3_tot_magerr:-9., $
	w4_ap_a:-9., w4_ap_mag:-99., w4_ap_magerr:-9., $
	w4_asym_a:-9., w4_asym_mag:-99., w4_asym_magerr:-9., $
	w4_tot_a:-9., w4_tot_mag:-99., w4_tot_magerr:-9., $
	wise_roi:0ll, wise_pointsrc:0ll, wise_mask:0ll, $
	wise_jpg:0ll, wise_qa:0, wise_phot:0ll, wise_plots:0ll, $
	i3p6um_img:0ll, i4p5um_img:0ll, $			; IRAC
	i3p6um_jpg:0ll, i4p5um_jpg:0ll, $
	i3p6um_plot:0ll, i4p5um_plot:0ll, $
	i3p6um_ap_a:-9., i3p6um_ap_mag:-99., i3p6um_ap_magerr:-9., $
	i3p6um_asym_a:-9., i3p6um_asym_mag:-99., i3p6um_asym_magerr:-9., $
	i3p6um_tot_a:-9., i3p6um_tot_mag:-99., i3p6um_tot_magerr:-9., $
	i4p5um_ap_a:-9., i4p5um_ap_mag:-99., i4p5um_ap_magerr:-9., $
	i4p5um_asym_a:-9., i4p5um_asym_mag:-99., i4p5um_asym_magerr:-9., $
	i4p5um_tot_a:-9., i4p5um_tot_mag:-99., i4p5um_tot_magerr:-9., $
	irac_roi:0ll, irac_pointsrc:0ll, irac_mask:0ll, $
	irac_jpg:0ll, irac_qa:0, irac_phot:0ll, irac_plots:0ll $
	}
end
