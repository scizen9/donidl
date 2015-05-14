pro glgadb_updt_status,inid,degdir=degdir,silent=silent,verbose=verbose
;+
; glgadb_status - list the processing status of the hosts inid
;
;	inid	- GLGA id (string)
;-
; common
COMMON galdb_info
COMMON glgadb_info, glgadat
;
; count of data
nh = n_elements(inid)
if not keyword_set(silent) then $
	print,'Checking this many hosts: ',nh
;
; used to generate file names
id = strcompress(inid,/rem)
;
; define glga_status struct
A_struct = {glgadb_data}
A_struct = struct_init(A_struct)
;
; counters
added = 0L
;
; loop over hosts
for i=0,nh-1 do begin
;
; status
	dbstat = ''
;
; get top level data directory
	ddeg = degdir[i]
;
; are we already in glga?
	l = (glfind(id[i],count=nl))[0]
;
; not in glga db so add a record
	if nl le 0 then begin
		glgadat = [glgadat,A_struct]
		l = n_elements(glgadat)-1L
		added = added + 1L
		glgadat[l].id = id[i]
;
; check gals database
		h=gfind(id[i],count=ng,/silent)
;
; in database
		if ng gt 0 then begin
			dbstat = ''
			h=h[0]
			if strlen(strtrim(galdat[h].type,2)) gt 0 then $
				glgadat[l].type = galdat[h].type
; start with literature coords and shape params
			glgadat[l].ra	= galdat[h].ra
			glgadat[l].dec	= galdat[h].dec
			glgadat[l].majax= galdat[h].majax
			glgadat[l].minax= galdat[h].minax
			glgadat[l].pa	= galdat[h].pa
; catalog
			if glgadat[l].majax ge 90. then begin
				glgadat[l].catalog='glga_v1'
			endif else if glgadat[l].majax ge 48 then begin
				glgadat[l].catalog='glga_v2'
			endif else glgadat[l].catalog='extra'
;
; not in database
		endif else begin
			dbstat = 'not found'
			glgadat[l].catalog	= 'extra'
		endelse
	endif 	; end not found in glga database
;
; no anonymous galaxies in catalog regardless of size
	if strmid(strtrim(glgadat[l].id,2),0,1) eq 'A' then $
		glgadat[l].catalog	= 'extra'
;
; general record timestamp
	glgadat[l].timestamp = systime(1)
;
; Coords and shape params
;
; ellipse file is definitive
	ellfile = ddeg+'aux/'+id[i]+'_ellipse.dat'
	glgadat[l].new_ell = (file_info(ellfile)).mtime
	if glgadat[l].new_ell gt 0 then begin
		readcol,ellfile,majd_as,mind_as,ra,dec,pa, $
			form='f,f,d,d,f',/silent
		glgadat[l].ra	= ra
		glgadat[l].dec	= dec
		glgadat[l].majax= majd_as
		glgadat[l].minax= mind_as
		glgadat[l].pa	= pa
	endif 
;
; use ellipsepar files if no ellipse file or literature values (ra lt 0)
	if glgadat[l].ra lt 0 then begin
		eflist = file_search(ddeg+'photometry/'+id[i]+ $
				  '_*_ellipsepar.dat',count=nef)
		if nef ge 1 then begin
			readcol,eflist[0],efra,efdec,efa,efb,efpa, $
				format='d,d,f,f,f',comment='#',/silent
			glgadat[l].ra	= efra
			glgadat[l].dec	= efdec
			glgadat[l].majax= efa*2.0
			glgadat[l].minax= efb*2.0
			glgadat[l].pa	= efpa
		endif
	endif
;
; dss img
	dssfile = ddeg+'dss/fits/'+id[i]+'_dss2_red.fits.gz'
	glgadat[l].dss_img = (file_info(dssfile)).mtime
;
; GALEX imgs
	fuvfile = ddeg+'galex/fits/'+id[i]+'_FUV.fits.gz'
	glgadat[l].fuv_img = (file_info(fuvfile)).mtime
	nuvfile = ddeg+'galex/fits/'+id[i]+'_NUV.fits.gz'
	glgadat[l].nuv_img = (file_info(nuvfile)).mtime
;
; GALEX jpgs
	uvjpg = ddeg+'galex/jpg/'+id[i]+'_FUV.jpg'
	glgadat[l].fuv_jpg = (file_info(uvjpg)).mtime
	uvjpg = ddeg+'galex/jpg/'+id[i]+'_NUV.jpg'
	glgadat[l].nuv_jpg = (file_info(uvjpg)).mtime
	uvjpg = ddeg+'galex/jpg/'+id[i]+'_FUVNUV.jpg'
	glgadat[l].galex_jpg = (file_info(uvjpg)).mtime
;
; GALEX aux
	roifile = ddeg+'aux/'+id[i]+'_galex_roi.dat'
	glgadat[l].galex_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_galex_pointsrc.dat'
	glgadat[l].galex_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_galex_mask.fits.gz'
	glgadat[l].galex_mask = (file_info(mskfile)).mtime
;
; GALEX outputs
	uvqa = ddeg+'galex/fits/'+id[i]+'_qa.txt'
	if file_test(uvqa) then begin
		qa = glga_read_qa_stat(uvqa,stat=stat)
		glgadat[l].galex_qa = stat
	endif
	if glgadat[l].galex_qa eq 2 and $
		not glgadat[l].fuv_img and not glgadat[l].nuv_img then $
	   	glgadat[l].galex_qa = -1
	uvplots = ddeg+'plots/'+id[i]+'_galex_profile.pdf'
	glgadat[l].galex_plots = (file_info(uvplots)).mtime
;
; GALEX photometry
; FUV
	uvphot = ddeg+'photometry/'+id[i]+'_FUV_aperture.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].fuv_ap_a = a
		glgadat[l].fuv_ap_mag = mag
		glgadat[l].fuv_ap_magerr = mag_e
	endif
	uvphot = ddeg+'photometry/'+id[i]+'_FUV_asymptotic.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].fuv_asym_a = a
		glgadat[l].fuv_asym_mag = mag
		glgadat[l].fuv_asym_magerr = mag_e
	endif
	uvphot = ddeg+'photometry/'+id[i]+'_FUV_total.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].fuv_tot_a = a
		glgadat[l].fuv_tot_mag = mag
		glgadat[l].fuv_tot_magerr = mag_e
	endif
	glgadat[l].galex_phot = (file_info(uvphot)).mtime
; NUV
	uvphot = ddeg+'photometry/'+id[i]+'_NUV_aperture.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].nuv_ap_a = a
		glgadat[l].nuv_ap_mag = mag
		glgadat[l].nuv_ap_magerr = mag_e
	endif
	uvphot = ddeg+'photometry/'+id[i]+'_NUV_asymptotic.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].nuv_asym_a = a
		glgadat[l].nuv_asym_mag = mag
		glgadat[l].nuv_asym_magerr = mag_e
	endif
	uvphot = ddeg+'photometry/'+id[i]+'_NUV_total.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].nuv_tot_a = a
		glgadat[l].nuv_tot_mag = mag
		glgadat[l].nuv_tot_magerr = mag_e
	endif
	glgadat[l].galex_phot = (file_info(uvphot)).mtime
;
; SDSS imgs
	ufile = ddeg+'sdss/fits/'+id[i]+'_u.fits.gz'
	glgadat[l].u_img = (file_info(ufile)).mtime
	gfile = ddeg+'sdss/fits/'+id[i]+'_g.fits.gz'
	glgadat[l].g_img = (file_info(gfile)).mtime
	rfile = ddeg+'sdss/fits/'+id[i]+'_r.fits.gz'
	glgadat[l].r_img = (file_info(rfile)).mtime
	ifile = ddeg+'sdss/fits/'+id[i]+'_i.fits.gz'
	glgadat[l].i_img = (file_info(ifile)).mtime
	zfile = ddeg+'sdss/fits/'+id[i]+'_z.fits.gz'
	glgadat[l].z_img = (file_info(zfile)).mtime
;
; SDSS jpgs
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_u.jpg'
	glgadat[l].u_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_g.jpg'
	glgadat[l].g_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_r.jpg'
	glgadat[l].r_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_i.jpg'
	glgadat[l].i_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_z.jpg'
	glgadat[l].z_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_gri.jpg'
	glgadat[l].sdss_jpg = (file_info(sdssjpg)).mtime
;
; SDSS aux
	roifile = ddeg+'aux/'+id[i]+'_sdss_roi.dat'
	glgadat[l].sdss_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_sdss_pointsrc.dat'
	glgadat[l].sdss_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_sdss_mask.fits.gz'
	glgadat[l].sdss_mask = (file_info(mskfile)).mtime
;
; SDSS outputs
	sdssqa = ddeg+'sdss/fits/'+id[i]+'_qa.txt'
	if file_test(sdssqa) then begin
		qa = glga_read_qa_stat(sdssqa,stat=stat)
		glgadat[l].sdss_qa = stat
	endif
	if glgadat[l].sdss_qa eq 2 and $
		not glgadat[l].u_img and not glgadat[l].g_img and $
		not glgadat[l].r_img and not glgadat[l].i_img and $
		not glgadat[l].z_img then glgadat[l].sdss_qa = -1
	sdssplots = ddeg+'plots/'+id[i]+'_sdss_profile.pdf'
	glgadat[l].sdss_plots = (file_info(sdssplots)).mtime
;
; SDSS photometry
; u
	sdssphot = ddeg+'photometry/'+id[i]+'_u_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].u_ap_a = a
		glgadat[l].u_ap_mag = mag
		glgadat[l].u_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_u_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].u_asym_a = a
		glgadat[l].u_asym_mag = mag
		glgadat[l].u_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_u_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].u_tot_a = a
		glgadat[l].u_tot_mag = mag
		glgadat[l].u_tot_magerr = mag_e
	endif
	glgadat[l].sdss_phot = (file_info(sdssphot)).mtime
; g
	sdssphot = ddeg+'photometry/'+id[i]+'_g_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].g_ap_a = a
		glgadat[l].g_ap_mag = mag
		glgadat[l].g_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_g_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].g_asym_a = a
		glgadat[l].g_asym_mag = mag
		glgadat[l].g_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_g_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].g_tot_a = a
		glgadat[l].g_tot_mag = mag
		glgadat[l].g_tot_magerr = mag_e
	endif
	glgadat[l].sdss_phot = (file_info(sdssphot)).mtime
; r
	sdssphot = ddeg+'photometry/'+id[i]+'_r_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].r_ap_a = a
		glgadat[l].r_ap_mag = mag
		glgadat[l].r_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_r_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].r_asym_a = a
		glgadat[l].r_asym_mag = mag
		glgadat[l].r_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_r_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].r_tot_a = a
		glgadat[l].r_tot_mag = mag
		glgadat[l].r_tot_magerr = mag_e
	endif
	glgadat[l].sdss_phot = (file_info(sdssphot)).mtime
; i
	sdssphot = ddeg+'photometry/'+id[i]+'_i_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].i_ap_a = a
		glgadat[l].i_ap_mag = mag
		glgadat[l].i_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_i_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].i_asym_a = a
		glgadat[l].i_asym_mag = mag
		glgadat[l].i_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_i_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].i_tot_a = a
		glgadat[l].i_tot_mag = mag
		glgadat[l].i_tot_magerr = mag_e
	endif
	glgadat[l].sdss_phot = (file_info(sdssphot)).mtime
; z
	sdssphot = ddeg+'photometry/'+id[i]+'_z_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].z_ap_a = a
		glgadat[l].z_ap_mag = mag
		glgadat[l].z_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_z_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].z_asym_a = a
		glgadat[l].z_asym_mag = mag
		glgadat[l].z_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_z_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].z_tot_a = a
		glgadat[l].z_tot_mag = mag
		glgadat[l].z_tot_magerr = mag_e
	endif
	glgadat[l].sdss_phot = (file_info(sdssphot)).mtime
;
; 2MASS imgs
	jfile = ddeg+'2mass/fits/'+id[i]+'_j.fits.gz'
	glgadat[l].j_img = (file_info(jfile)).mtime
	hfile = ddeg+'2mass/fits/'+id[i]+'_h.fits.gz'
	glgadat[l].h_img = (file_info(hfile)).mtime
	kfile = ddeg+'2mass/fits/'+id[i]+'_k.fits.gz'
	glgadat[l].k_img = (file_info(kfile)).mtime
;
; 2MASS jpgs
	twomassjpg = ddeg+'2mass/jpg/'+id[i]+'_j.jpg'
	glgadat[l].j_jpg = (file_info(twomassjpg)).mtime
	twomassjpg = ddeg+'2mass/jpg/'+id[i]+'_h.jpg'
	glgadat[l].h_jpg = (file_info(twomassjpg)).mtime
	twomassjpg = ddeg+'2mass/jpg/'+id[i]+'_k.jpg'
	glgadat[l].k_jpg = (file_info(twomassjpg)).mtime
	twomassjpg = ddeg+'2mass/jpg/'+id[i]+'_jhk.jpg'
	glgadat[l].twomass_jpg = (file_info(twomassjpg)).mtime
;
; 2MASS aux
	roifile = ddeg+'aux/'+id[i]+'_2mass_roi.dat'
	glgadat[l].twomass_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_2mass_pointsrc.dat'
	glgadat[l].twomass_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_2mass_mask.fits.gz'
	glgadat[l].twomass_mask = (file_info(mskfile)).mtime
;
; 2MASS outputs
	twomassqa = ddeg+'2mass/fits/'+id[i]+'_qa.txt'
	if file_test(twomassqa) then begin
		qa = glga_read_qa_stat(twomassqa,stat=stat)
		glgadat[l].twomass_qa = stat
	endif
	if glgadat[l].twomass_qa eq 2 and $
		not glgadat[l].j_img and not glgadat[l].h_img and $
		not glgadat[l].k_img then glgadat[l].twomass_qa = -1
	twomassplots = ddeg+'plots/'+id[i]+'_2mass_profile.pdf'
	glgadat[l].twomass_plots = (file_info(twomassplots)).mtime
;
; 2MASS photometry
; j
	twomassphot = ddeg+'photometry/'+id[i]+'_j_aperture.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].j_ap_a = a
		glgadat[l].j_ap_mag = mag
		glgadat[l].j_ap_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_j_asymptotic.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].j_asym_a = a
		glgadat[l].j_asym_mag = mag
		glgadat[l].j_asym_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_j_total.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].j_tot_a = a
		glgadat[l].j_tot_mag = mag
		glgadat[l].j_tot_magerr = mag_e
	endif
	glgadat[l].twomass_phot = (file_info(twomassphot)).mtime
; h
	twomassphot = ddeg+'photometry/'+id[i]+'_h_aperture.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].h_ap_a = a
		glgadat[l].h_ap_mag = mag
		glgadat[l].h_ap_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_h_asymptotic.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].h_asym_a = a
		glgadat[l].h_asym_mag = mag
		glgadat[l].h_asym_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_h_total.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].h_tot_a = a
		glgadat[l].h_tot_mag = mag
		glgadat[l].h_tot_magerr = mag_e
	endif
	glgadat[l].twomass_phot = (file_info(twomassphot)).mtime
; k
	twomassphot = ddeg+'photometry/'+id[i]+'_k_aperture.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].k_ap_a = a
		glgadat[l].k_ap_mag = mag
		glgadat[l].k_ap_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_k_asymptotic.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].k_asym_a = a
		glgadat[l].k_asym_mag = mag
		glgadat[l].k_asym_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_k_total.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].k_tot_a = a
		glgadat[l].k_tot_mag = mag
		glgadat[l].k_tot_magerr = mag_e
	endif
	glgadat[l].twomass_phot = (file_info(twomassphot)).mtime
;
; WISE imgs
	w1file = ddeg+'wise/fits/'+id[i]+'_w1.fits.gz'
	glgadat[l].w1_img = (file_info(w1file)).mtime
	w2file = ddeg+'wise/fits/'+id[i]+'_w2.fits.gz'
	glgadat[l].w2_img = (file_info(w2file)).mtime
	w3file = ddeg+'wise/fits/'+id[i]+'_w3.fits.gz'
	glgadat[l].w3_img = (file_info(w3file)).mtime
	w4file = ddeg+'wise/fits/'+id[i]+'_w4.fits.gz'
	glgadat[l].w4_img = (file_info(w4file)).mtime
;
; WISE jpgs
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w1.jpg'
	glgadat[l].w1_jpg = (file_info(wisejpg)).mtime
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w2.jpg'
	glgadat[l].w2_jpg = (file_info(wisejpg)).mtime
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w3.jpg'
	glgadat[l].w3_jpg = (file_info(wisejpg)).mtime
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w4.jpg'
	glgadat[l].w4_jpg = (file_info(wisejpg)).mtime
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w123.jpg'
	glgadat[l].wise_jpg = (file_info(wisejpg)).mtime
;
; WISE aux
	roifile = ddeg+'aux/'+id[i]+'_wise_roi.dat'
	glgadat[l].wise_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_wise_pointsrc.dat'
	glgadat[l].wise_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_wise_mask.fits.gz'
	glgadat[l].wise_mask = (file_info(mskfile)).mtime
;
; WISE outputs
	wiseqa = ddeg+'wise/fits/'+id[i]+'_qa.txt'
	if file_test(wiseqa) then begin
		qa = glga_read_qa_stat(wiseqa,stat=stat)
		glgadat[l].wise_qa = stat
	endif
	if glgadat[l].wise_qa eq 2 and $
		not glgadat[l].w1_img and not glgadat[l].w2_img and $
		not glgadat[l].w3_img and not glgadat[l].w4_img then $
		glgadat[l].wise_qa = -1
	wiseplots = ddeg+'plots/'+id[i]+'_wise_profile.pdf'
	glgadat[l].wise_plots = (file_info(wiseplots)).mtime
;
; WISE photometry
; w1
	wisephot = ddeg+'photometry/'+id[i]+'_w1_aperture.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w1_ap_a = a
		glgadat[l].w1_ap_mag = mag
		glgadat[l].w1_ap_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w1_asymptotic.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w1_asym_a = a
		glgadat[l].w1_asym_mag = mag
		glgadat[l].w1_asym_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w1_total.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w1_tot_a = a
		glgadat[l].w1_tot_mag = mag
		glgadat[l].w1_tot_magerr = mag_e
	endif
	glgadat[l].wise_phot = (file_info(wisephot)).mtime
; w2
	wisephot = ddeg+'photometry/'+id[i]+'_w2_aperture.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w2_ap_a = a
		glgadat[l].w2_ap_mag = mag
		glgadat[l].w2_ap_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w2_asymptotic.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w2_asym_a = a
		glgadat[l].w2_asym_mag = mag
		glgadat[l].w2_asym_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w2_total.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w2_tot_a = a
		glgadat[l].w2_tot_mag = mag
		glgadat[l].w2_tot_magerr = mag_e
	endif
	glgadat[l].wise_phot = (file_info(wisephot)).mtime
; w3
	wisephot = ddeg+'photometry/'+id[i]+'_w3_aperture.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w3_ap_a = a
		glgadat[l].w3_ap_mag = mag
		glgadat[l].w3_ap_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w3_asymptotic.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w3_asym_a = a
		glgadat[l].w3_asym_mag = mag
		glgadat[l].w3_asym_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w3_total.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w3_tot_a = a
		glgadat[l].w3_tot_mag = mag
		glgadat[l].w3_tot_magerr = mag_e
	endif
	glgadat[l].wise_phot = (file_info(wisephot)).mtime
; w4
	wisephot = ddeg+'photometry/'+id[i]+'_w4_aperture.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w4_ap_a = a
		glgadat[l].w4_ap_mag = mag
		glgadat[l].w4_ap_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w4_asymptotic.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w4_asym_a = a
		glgadat[l].w4_asym_mag = mag
		glgadat[l].w4_asym_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w4_total.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].w4_tot_a = a
		glgadat[l].w4_tot_mag = mag
		glgadat[l].w4_tot_magerr = mag_e
	endif
	glgadat[l].wise_phot = (file_info(wisephot)).mtime
;
; IRAC imgs
	i3p6umfile = ddeg+'irac/fits/'+id[i]+'_3p6um.fits.gz'
	glgadat[l].i3p6um_img = (file_info(i3p6umfile)).mtime
	i4p5umfile = ddeg+'irac/fits/'+id[i]+'_4p5um.fits.gz'
	glgadat[l].i4p5um_img = (file_info(i4p5umfile)).mtime
;
; IRAC jpgs
	iracjpg = ddeg+'irac/jpg/'+id[i]+'_3p6um.jpg'
	glgadat[l].i3p6um_jpg = (file_info(iracjpg)).mtime
	iracjpg = ddeg+'irac/jpg/'+id[i]+'_4p5um.jpg'
	glgadat[l].i4p5um_jpg = (file_info(iracjpg)).mtime
	iracjpg = ddeg+'irac/jpg/'+id[i]+'_3p6um4p5um.jpg'
	glgadat[l].irac_jpg = (file_info(iracjpg)).mtime
;
; IRAC aux
	roifile = ddeg+'aux/'+id[i]+'_irac_roi.dat'
	glgadat[l].irac_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_irac_pointsrc.dat'
	glgadat[l].irac_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_irac_mask.fits.gz'
	glgadat[l].irac_mask = (file_info(mskfile)).mtime
;
; IRAC outputs
	iracqa = ddeg+'irac/fits/'+id[i]+'_qa.txt'
	if file_test(iracqa) then begin
		qa = glga_read_qa_stat(iracqa,stat=stat)
		glgadat[l].irac_qa = stat
	endif
	if glgadat[l].irac_qa eq 2 and $
		not glgadat[l].i3p6um_img and not glgadat[l].i4p5um_img then $
		glgadat[l].irac_qa = -1
	iracplots = ddeg+'plots/'+id[i]+'_irac_profile.pdf'
	glgadat[l].irac_plots = (file_info(iracplots)).mtime
;
; IRAC photometry
; 3p6um
	iracphot = ddeg+'photometry/'+id[i]+'_3p6um_aperture.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].i3p6um_ap_a = a
		glgadat[l].i3p6um_ap_mag = mag
		glgadat[l].i3p6um_ap_magerr = mag_e
	endif
	iracphot = ddeg+'photometry/'+id[i]+'_3p6um_asymptotic.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].i3p6um_asym_a = a
		glgadat[l].i3p6um_asym_mag = mag
		glgadat[l].i3p6um_asym_magerr = mag_e
	endif
	iracphot = ddeg+'photometry/'+id[i]+'_3p6um_total.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].i3p6um_tot_a = a
		glgadat[l].i3p6um_tot_mag = mag
		glgadat[l].i3p6um_tot_magerr = mag_e
	endif
	glgadat[l].irac_phot = (file_info(iracphot)).mtime
; 4p5um
	iracphot = ddeg+'photometry/'+id[i]+'_4p5um_aperture.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].i4p5um_ap_a = a
		glgadat[l].i4p5um_ap_mag = mag
		glgadat[l].i4p5um_ap_magerr = mag_e
	endif
	iracphot = ddeg+'photometry/'+id[i]+'_4p5um_asymptotic.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].i4p5um_asym_a = a
		glgadat[l].i4p5um_asym_mag = mag
		glgadat[l].i4p5um_asym_magerr = mag_e
	endif
	iracphot = ddeg+'photometry/'+id[i]+'_4p5um_total.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[l].i4p5um_tot_a = a
		glgadat[l].i4p5um_tot_mag = mag
		glgadat[l].i4p5um_tot_magerr = mag_e
	endif
	glgadat[l].irac_phot = (file_info(iracphot)).mtime
;
	if not keyword_set(silent) then $
		print,string(13B),i+1,'/',nh,id[i],glgadat[l].ra,dbstat, $
			format='($,a1,i6,a1,i6,2x,a-25,f13.8,2x,a-10)'
endfor
if not keyword_set(silent) then begin
	print,' '
	print,'Number of records added: ',added
endif
;
return
;
end
