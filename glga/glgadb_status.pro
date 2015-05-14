pro glgadb_status,inid,degdir=degdir,silent=silent,verbose=verbose
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
A = {glgadb_data}
A = struct_init(A)
glgadat=replicate(A,nh)
;
; loop over hosts
for i=0,nh-1 do begin
	glgadat[i].id = id[i]
	ddeg = degdir[i]
;
; check gals database
	h=gfind(id[i],count=ng,/silent)
;
; in database
	if ng gt 0 then begin
		dbstat = ''
		h=h[0]
		if strlen(strtrim(galdat[h].type,2)) gt 0 then $
			glgadat[i].type = galdat[h].type
; start with literature values
		glgadat[i].ra		= galdat[h].ra
		glgadat[i].dec		= galdat[h].dec
		glgadat[i].majax	= galdat[h].majax
		glgadat[i].minax	= galdat[h].minax
		glgadat[i].pa		= galdat[h].pa
; catalog
		if glgadat[i].majax ge 90. then begin
			glgadat[i].catalog='glga_v1'
		endif else if glgadat[i].majax ge 48. then begin
			glgadat[i].catalog='glga_v2'
		endif else glgadat[i].catalog='extra'
;
; not in database
	endif else begin
		dbstat = 'not found'
		glgadat[i].catalog	= 'extra'
		;
		; check ellipsepar files for basic params
		eflist = file_search(ddeg+'photometry/'+id[i]+ $
					  '_*_ellipsepar.dat',count=nef)
		if nef ge 1 then begin
			readcol,eflist[0],efra,efdec,efa,efb,efpa, $
				format='d,d,f,f,f',comment='#',/silent
			glgadat[i].ra	= efra
			glgadat[i].dec	= efdec
			glgadat[i].majax= efa*2.0
			glgadat[i].minax= efb*2.0
			glgadat[i].pa	= efpa
		endif
	endelse
;
; no anonymous galaxies in catalog
	if strmid(strtrim(glgadat[i].id,2),0,1) eq 'A' then $
		glgadat[i].catalog	= 'extra'
;
; general record timestamp
	glgadat[i].timestamp = systime(1)
;
; ellipse file is definitive
	ellfile = ddeg+'aux/'+id[i]+'_ellipse.dat'
	glgadat[i].new_ell = (file_info(ellfile)).mtime
	if glgadat[i].new_ell gt 0 then begin
		readcol,ellfile,majd_as,mind_as,ra,dec,pa, $
			form='f,f,d,d,f',/silent
		glgadat[i].ra	= ra
		glgadat[i].dec	= dec
		glgadat[i].majax= majd_as
		glgadat[i].minax= mind_as
		glgadat[i].pa	= pa
	endif
;
; use ellipsepar files if no ellipse file or literature values (ra lt 0)
	if glgadat[i].ra lt 0 then begin
		eflist = file_search(ddeg+'photometry/'+id[i]+ $
				  '_*_ellipsepar.dat',count=nef)
		if nef ge 1 then begin
			readcol,eflist[0],efra,efdec,efa,efb,efpa, $
				format='d,d,f,f,f',comment='#',/silent
			glgadat[i].ra	= efra
			glgadat[i].dec	= efdec
			glgadat[i].majax= efa*2.0
			glgadat[i].minax= efb*2.0
			glgadat[i].pa	= efpa
		endif
	endif
;
; dss img
	dssfile = ddeg+'dss/fits/'+id[i]+'_dss2_red.fits.gz'
	glgadat[i].dss_img = (file_info(dssfile)).mtime
;
; GALEX imgs
	fuvfile = ddeg+'galex/fits/'+id[i]+'_FUV.fits.gz'
	glgadat[i].fuv_img = (file_info(fuvfile)).mtime
	nuvfile = ddeg+'galex/fits/'+id[i]+'_NUV.fits.gz'
	glgadat[i].nuv_img = (file_info(nuvfile)).mtime
;
; GALEX jpgs
	uvjpg = ddeg+'galex/jpg/'+id[i]+'_FUV.jpg'
	glgadat[i].fuv_jpg = (file_info(uvjpg)).mtime
	uvjpg = ddeg+'galex/jpg/'+id[i]+'_NUV.jpg'
	glgadat[i].nuv_jpg = (file_info(uvjpg)).mtime
	uvjpg = ddeg+'galex/jpg/'+id[i]+'_FUVNUV.jpg'
	glgadat[i].galex_jpg = (file_info(uvjpg)).mtime
;
; GALEX plots
	fuvplot = ddeg+'plots/'+id[i]+'_FUV.pdf'
	glgadat[i].fuv_plot = (file_info(fuvplot)).mtime
	nuvplot = ddeg+'plots/'+id[i]+'_NUV.pdf'
	glgadat[i].nuv_plot = (file_info(nuvplot)).mtime
;
; GALEX aux
	roifile = ddeg+'aux/'+id[i]+'_galex_roi.dat'
	glgadat[i].galex_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_galex_pointsrc.dat'
	glgadat[i].galex_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_galex_mask.fits.gz'
	glgadat[i].galex_mask = (file_info(mskfile)).mtime
;
; GALEX outputs
	uvqa = ddeg+'galex/fits/'+id[i]+'_qa.txt'
	if file_test(uvqa) then begin
		qa = glga_read_qa_stat(uvqa,stat=stat)
		glgadat[i].galex_qa = stat
	endif
	if glgadat[i].galex_qa eq 2 and $
		not glgadat[i].fuv_img and not glgadat[i].nuv_img then $
	   	glgadat[i].galex_qa = -1
	uvplots = ddeg+'plots/'+id[i]+'_galex_profile.pdf'
	glgadat[i].galex_plots = (file_info(uvplots)).mtime
;
; GALEX photometry
; FUV
	uvphot = ddeg+'photometry/'+id[i]+'_FUV_aperture.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].fuv_ap_a = a
		glgadat[i].fuv_ap_mag = mag
		glgadat[i].fuv_ap_magerr = mag_e
	endif
	uvphot = ddeg+'photometry/'+id[i]+'_FUV_asymptotic.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].fuv_asym_a = a
		glgadat[i].fuv_asym_mag = mag
		glgadat[i].fuv_asym_magerr = mag_e
	endif
	uvphot = ddeg+'photometry/'+id[i]+'_FUV_total.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].fuv_tot_a = a
		glgadat[i].fuv_tot_mag = mag
		glgadat[i].fuv_tot_magerr = mag_e
	endif
	glgadat[i].galex_phot = (file_info(uvphot)).mtime
; NUV
	uvphot = ddeg+'photometry/'+id[i]+'_NUV_aperture.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].nuv_ap_a = a
		glgadat[i].nuv_ap_mag = mag
		glgadat[i].nuv_ap_magerr = mag_e
	endif
	uvphot = ddeg+'photometry/'+id[i]+'_NUV_asymptotic.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].nuv_asym_a = a
		glgadat[i].nuv_asym_mag = mag
		glgadat[i].nuv_asym_magerr = mag_e
	endif
	uvphot = ddeg+'photometry/'+id[i]+'_NUV_total.dat'
	if file_test(uvphot) then begin
		readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].nuv_tot_a = a
		glgadat[i].nuv_tot_mag = mag
		glgadat[i].nuv_tot_magerr = mag_e
	endif
	glgadat[i].galex_phot = (file_info(uvphot)).mtime
;
; SDSS imgs
	ufile = ddeg+'sdss/fits/'+id[i]+'_u.fits.gz'
	glgadat[i].u_img = (file_info(ufile)).mtime
	gfile = ddeg+'sdss/fits/'+id[i]+'_g.fits.gz'
	glgadat[i].g_img = (file_info(gfile)).mtime
	rfile = ddeg+'sdss/fits/'+id[i]+'_r.fits.gz'
	glgadat[i].r_img = (file_info(rfile)).mtime
	ifile = ddeg+'sdss/fits/'+id[i]+'_i.fits.gz'
	glgadat[i].i_img = (file_info(ifile)).mtime
	zfile = ddeg+'sdss/fits/'+id[i]+'_z.fits.gz'
	glgadat[i].z_img = (file_info(zfile)).mtime
;
; SDSS jpgs
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_u.jpg'
	glgadat[i].u_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_g.jpg'
	glgadat[i].g_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_r.jpg'
	glgadat[i].r_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_i.jpg'
	glgadat[i].i_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_z.jpg'
	glgadat[i].z_jpg = (file_info(sdssjpg)).mtime
	sdssjpg = ddeg+'sdss/jpg/'+id[i]+'_gri.jpg'
	glgadat[i].sdss_jpg = (file_info(sdssjpg)).mtime
;
; SDSS plots
	uplot = ddeg+'plots/'+id[i]+'_u.pdf'
	glgadat[i].u_plot = (file_info(uplot)).mtime
	gplot = ddeg+'plots/'+id[i]+'_g.pdf'
	glgadat[i].g_plot = (file_info(gplot)).mtime
	rplot = ddeg+'plots/'+id[i]+'_r.pdf'
	glgadat[i].r_plot = (file_info(rplot)).mtime
	iplot = ddeg+'plots/'+id[i]+'_i.pdf'
	glgadat[i].i_plot = (file_info(iplot)).mtime
	zplot = ddeg+'plots/'+id[i]+'_z.pdf'
	glgadat[i].z_plot = (file_info(zplot)).mtime
;
; SDSS aux
	roifile = ddeg+'aux/'+id[i]+'_sdss_roi.dat'
	glgadat[i].sdss_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_sdss_pointsrc.dat'
	glgadat[i].sdss_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_sdss_mask.fits.gz'
	glgadat[i].sdss_mask = (file_info(mskfile)).mtime
;
; SDSS outputs
	sdssqa = ddeg+'sdss/fits/'+id[i]+'_qa.txt'
	if file_test(sdssqa) then begin
		qa = glga_read_qa_stat(sdssqa,stat=stat)
		glgadat[i].sdss_qa = stat
	endif
	if glgadat[i].sdss_qa eq 2 and $
		not glgadat[i].u_img and not glgadat[i].g_img and $
		not glgadat[i].r_img and not glgadat[i].i_img and $
		not glgadat[i].z_img then glgadat[i].sdss_qa = -1
	sdssplots = ddeg+'plots/'+id[i]+'_sdss_profile.pdf'
	glgadat[i].sdss_plots = (file_info(sdssplots)).mtime
;
; SDSS photometry
; u
	sdssphot = ddeg+'photometry/'+id[i]+'_u_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].u_ap_a = a
		glgadat[i].u_ap_mag = mag
		glgadat[i].u_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_u_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].u_asym_a = a
		glgadat[i].u_asym_mag = mag
		glgadat[i].u_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_u_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].u_tot_a = a
		glgadat[i].u_tot_mag = mag
		glgadat[i].u_tot_magerr = mag_e
	endif
	glgadat[i].sdss_phot = (file_info(sdssphot)).mtime
; g
	sdssphot = ddeg+'photometry/'+id[i]+'_g_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].g_ap_a = a
		glgadat[i].g_ap_mag = mag
		glgadat[i].g_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_g_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].g_asym_a = a
		glgadat[i].g_asym_mag = mag
		glgadat[i].g_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_g_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].g_tot_a = a
		glgadat[i].g_tot_mag = mag
		glgadat[i].g_tot_magerr = mag_e
	endif
	glgadat[i].sdss_phot = (file_info(sdssphot)).mtime
; r
	sdssphot = ddeg+'photometry/'+id[i]+'_r_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].r_ap_a = a
		glgadat[i].r_ap_mag = mag
		glgadat[i].r_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_r_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].r_asym_a = a
		glgadat[i].r_asym_mag = mag
		glgadat[i].r_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_r_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].r_tot_a = a
		glgadat[i].r_tot_mag = mag
		glgadat[i].r_tot_magerr = mag_e
	endif
	glgadat[i].sdss_phot = (file_info(sdssphot)).mtime
; i
	sdssphot = ddeg+'photometry/'+id[i]+'_i_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].i_ap_a = a
		glgadat[i].i_ap_mag = mag
		glgadat[i].i_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_i_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].i_asym_a = a
		glgadat[i].i_asym_mag = mag
		glgadat[i].i_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_i_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].i_tot_a = a
		glgadat[i].i_tot_mag = mag
		glgadat[i].i_tot_magerr = mag_e
	endif
	glgadat[i].sdss_phot = (file_info(sdssphot)).mtime
; z
	sdssphot = ddeg+'photometry/'+id[i]+'_z_aperture.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].z_ap_a = a
		glgadat[i].z_ap_mag = mag
		glgadat[i].z_ap_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_z_asymptotic.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].z_asym_a = a
		glgadat[i].z_asym_mag = mag
		glgadat[i].z_asym_magerr = mag_e
	endif
	sdssphot = ddeg+'photometry/'+id[i]+'_z_total.dat'
	if file_test(sdssphot) then begin
		readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].z_tot_a = a
		glgadat[i].z_tot_mag = mag
		glgadat[i].z_tot_magerr = mag_e
	endif
	glgadat[i].sdss_phot = (file_info(sdssphot)).mtime
;
; 2MASS imgs
	jfile = ddeg+'2mass/fits/'+id[i]+'_j.fits.gz'
	glgadat[i].j_img = (file_info(jfile)).mtime
	hfile = ddeg+'2mass/fits/'+id[i]+'_h.fits.gz'
	glgadat[i].h_img = (file_info(hfile)).mtime
	kfile = ddeg+'2mass/fits/'+id[i]+'_k.fits.gz'
	glgadat[i].k_img = (file_info(kfile)).mtime
;
; 2MASS jpgs
	twomassjpg = ddeg+'2mass/jpg/'+id[i]+'_j.jpg'
	glgadat[i].j_jpg = (file_info(twomassjpg)).mtime
	twomassjpg = ddeg+'2mass/jpg/'+id[i]+'_h.jpg'
	glgadat[i].h_jpg = (file_info(twomassjpg)).mtime
	twomassjpg = ddeg+'2mass/jpg/'+id[i]+'_k.jpg'
	glgadat[i].k_jpg = (file_info(twomassjpg)).mtime
	twomassjpg = ddeg+'2mass/jpg/'+id[i]+'_jhk.jpg'
	glgadat[i].twomass_jpg = (file_info(twomassjpg)).mtime
;
; 2MASS plots
	jplot = ddeg+'plots/'+id[i]+'_j.pdf'
	glgadat[i].j_plot = (file_info(jplot)).mtime
	hplot = ddeg+'plots/'+id[i]+'_h.pdf'
	glgadat[i].h_plot = (file_info(hplot)).mtime
	kplot = ddeg+'plots/'+id[i]+'_k.pdf'
	glgadat[i].k_plot = (file_info(kplot)).mtime
;
; 2MASS aux
	roifile = ddeg+'aux/'+id[i]+'_2mass_roi.dat'
	glgadat[i].twomass_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_2mass_pointsrc.dat'
	glgadat[i].twomass_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_2mass_mask.fits.gz'
	glgadat[i].twomass_mask = (file_info(mskfile)).mtime
;
; 2MASS outputs
	twomassqa = ddeg+'2mass/fits/'+id[i]+'_qa.txt'
	if file_test(twomassqa) then begin
		qa = glga_read_qa_stat(twomassqa,stat=stat)
		glgadat[i].twomass_qa = stat
	endif
	if glgadat[i].twomass_qa eq 2 and $
		not glgadat[i].j_img and not glgadat[i].h_img and $
		not glgadat[i].k_img then glgadat[i].twomass_qa = -1
	twomassplots = ddeg+'plots/'+id[i]+'_2mass_profile.pdf'
	glgadat[i].twomass_plots = (file_info(twomassplots)).mtime
;
; 2MASS photometry
; j
	twomassphot = ddeg+'photometry/'+id[i]+'_j_aperture.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].j_ap_a = a
		glgadat[i].j_ap_mag = mag
		glgadat[i].j_ap_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_j_asymptotic.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].j_asym_a = a
		glgadat[i].j_asym_mag = mag
		glgadat[i].j_asym_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_j_total.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].j_tot_a = a
		glgadat[i].j_tot_mag = mag
		glgadat[i].j_tot_magerr = mag_e
	endif
	glgadat[i].twomass_phot = (file_info(twomassphot)).mtime
; h
	twomassphot = ddeg+'photometry/'+id[i]+'_h_aperture.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].h_ap_a = a
		glgadat[i].h_ap_mag = mag
		glgadat[i].h_ap_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_h_asymptotic.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].h_asym_a = a
		glgadat[i].h_asym_mag = mag
		glgadat[i].h_asym_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_h_total.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].h_tot_a = a
		glgadat[i].h_tot_mag = mag
		glgadat[i].h_tot_magerr = mag_e
	endif
	glgadat[i].twomass_phot = (file_info(twomassphot)).mtime
; k
	twomassphot = ddeg+'photometry/'+id[i]+'_k_aperture.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].k_ap_a = a
		glgadat[i].k_ap_mag = mag
		glgadat[i].k_ap_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_k_asymptotic.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].k_asym_a = a
		glgadat[i].k_asym_mag = mag
		glgadat[i].k_asym_magerr = mag_e
	endif
	twomassphot = ddeg+'photometry/'+id[i]+'_k_total.dat'
	if file_test(twomassphot) then begin
		readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].k_tot_a = a
		glgadat[i].k_tot_mag = mag
		glgadat[i].k_tot_magerr = mag_e
	endif
	glgadat[i].twomass_phot = (file_info(twomassphot)).mtime
;
; WISE imgs
	w1file = ddeg+'wise/fits/'+id[i]+'_w1.fits.gz'
	glgadat[i].w1_img = (file_info(w1file)).mtime
	w2file = ddeg+'wise/fits/'+id[i]+'_w2.fits.gz'
	glgadat[i].w2_img = (file_info(w2file)).mtime
	w3file = ddeg+'wise/fits/'+id[i]+'_w3.fits.gz'
	glgadat[i].w3_img = (file_info(w3file)).mtime
	w4file = ddeg+'wise/fits/'+id[i]+'_w4.fits.gz'
	glgadat[i].w4_img = (file_info(w4file)).mtime
;
; WISE jpgs
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w1.jpg'
	glgadat[i].w1_jpg = (file_info(wisejpg)).mtime
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w2.jpg'
	glgadat[i].w2_jpg = (file_info(wisejpg)).mtime
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w3.jpg'
	glgadat[i].w3_jpg = (file_info(wisejpg)).mtime
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w4.jpg'
	glgadat[i].w4_jpg = (file_info(wisejpg)).mtime
	wisejpg = ddeg+'wise/jpg/'+id[i]+'_w123.jpg'
	glgadat[i].wise_jpg = (file_info(wisejpg)).mtime
;
; WISE plots
	w1plot = ddeg+'plots/'+id[i]+'_w1.pdf'
	glgadat[i].w1_plot = (file_info(w1plot)).mtime
	w2plot = ddeg+'plots/'+id[i]+'_w2.pdf'
	glgadat[i].w2_plot = (file_info(w2plot)).mtime
	w3plot = ddeg+'plots/'+id[i]+'_w3.pdf'
	glgadat[i].w3_plot = (file_info(w3plot)).mtime
	w4plot = ddeg+'plots/'+id[i]+'_w4.pdf'
	glgadat[i].w4_plot = (file_info(w4plot)).mtime
;
; WISE aux
	roifile = ddeg+'aux/'+id[i]+'_wise_roi.dat'
	glgadat[i].wise_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_wise_pointsrc.dat'
	glgadat[i].wise_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_wise_mask.fits.gz'
	glgadat[i].wise_mask = (file_info(mskfile)).mtime
;
; WISE outputs
	wiseqa = ddeg+'wise/fits/'+id[i]+'_qa.txt'
	if file_test(wiseqa) then begin
		qa = glga_read_qa_stat(wiseqa,stat=stat)
		glgadat[i].wise_qa = stat
	endif
	if glgadat[i].wise_qa eq 2 and $
		not glgadat[i].w1_img and not glgadat[i].w2_img and $
		not glgadat[i].w3_img and not glgadat[i].w4_img then $
		glgadat[i].wise_qa = -1
	wiseplots = ddeg+'plots/'+id[i]+'_wise_profile.pdf'
	glgadat[i].wise_plots = (file_info(wiseplots)).mtime
;
; WISE photometry
; w1
	wisephot = ddeg+'photometry/'+id[i]+'_w1_aperture.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w1_ap_a = a
		glgadat[i].w1_ap_mag = mag
		glgadat[i].w1_ap_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w1_asymptotic.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w1_asym_a = a
		glgadat[i].w1_asym_mag = mag
		glgadat[i].w1_asym_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w1_total.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w1_tot_a = a
		glgadat[i].w1_tot_mag = mag
		glgadat[i].w1_tot_magerr = mag_e
	endif
	glgadat[i].wise_phot = (file_info(wisephot)).mtime
; w2
	wisephot = ddeg+'photometry/'+id[i]+'_w2_aperture.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w2_ap_a = a
		glgadat[i].w2_ap_mag = mag
		glgadat[i].w2_ap_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w2_asymptotic.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w2_asym_a = a
		glgadat[i].w2_asym_mag = mag
		glgadat[i].w2_asym_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w2_total.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w2_tot_a = a
		glgadat[i].w2_tot_mag = mag
		glgadat[i].w2_tot_magerr = mag_e
	endif
	glgadat[i].wise_phot = (file_info(wisephot)).mtime
; w3
	wisephot = ddeg+'photometry/'+id[i]+'_w3_aperture.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w3_ap_a = a
		glgadat[i].w3_ap_mag = mag
		glgadat[i].w3_ap_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w3_asymptotic.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w3_asym_a = a
		glgadat[i].w3_asym_mag = mag
		glgadat[i].w3_asym_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w3_total.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w3_tot_a = a
		glgadat[i].w3_tot_mag = mag
		glgadat[i].w3_tot_magerr = mag_e
	endif
	glgadat[i].wise_phot = (file_info(wisephot)).mtime
; w4
	wisephot = ddeg+'photometry/'+id[i]+'_w4_aperture.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w4_ap_a = a
		glgadat[i].w4_ap_mag = mag
		glgadat[i].w4_ap_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w4_asymptotic.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w4_asym_a = a
		glgadat[i].w4_asym_mag = mag
		glgadat[i].w4_asym_magerr = mag_e
	endif
	wisephot = ddeg+'photometry/'+id[i]+'_w4_total.dat'
	if file_test(wisephot) then begin
		readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].w4_tot_a = a
		glgadat[i].w4_tot_mag = mag
		glgadat[i].w4_tot_magerr = mag_e
	endif
	glgadat[i].wise_phot = (file_info(wisephot)).mtime
;
; IRAC imgs
	i3p6umfile = ddeg+'irac/fits/'+id[i]+'_3p6um.fits.gz'
	glgadat[i].i3p6um_img = (file_info(i3p6umfile)).mtime
	i4p5umfile = ddeg+'irac/fits/'+id[i]+'_4p5um.fits.gz'
	glgadat[i].i4p5um_img = (file_info(i4p5umfile)).mtime
;
; IRAC jpgs
	iracjpg = ddeg+'irac/jpg/'+id[i]+'_3p6um.jpg'
	glgadat[i].i3p6um_jpg = (file_info(iracjpg)).mtime
	iracjpg = ddeg+'irac/jpg/'+id[i]+'_4p5um.jpg'
	glgadat[i].i4p5um_jpg = (file_info(iracjpg)).mtime
	iracjpg = ddeg+'irac/jpg/'+id[i]+'_3p6um4p5um.jpg'
	glgadat[i].irac_jpg = (file_info(iracjpg)).mtime
;
; IRAC plots
	i3p6umplot = ddeg+'plots/'+id[i]+'_3p6um.pdf'
	glgadat[i].i3p6um_plot = (file_info(i3p6umplot)).mtime
	i4p5umplot = ddeg+'plots/'+id[i]+'_4p5um.pdf'
	glgadat[i].i4p5um_plot = (file_info(i4p5umplot)).mtime
;
; IRAC aux
	roifile = ddeg+'aux/'+id[i]+'_irac_roi.dat'
	glgadat[i].irac_roi = (file_info(roifile)).mtime
	pntfile = ddeg+'aux/'+id[i]+'_irac_pointsrc.dat'
	glgadat[i].irac_pointsrc = (file_info(pntfile)).mtime
	mskfile = ddeg+'aux/'+id[i]+'_irac_mask.fits.gz'
	glgadat[i].irac_mask = (file_info(mskfile)).mtime
;
; IRAC outputs
	iracqa = ddeg+'irac/fits/'+id[i]+'_qa.txt'
	if file_test(iracqa) then begin
		qa = glga_read_qa_stat(iracqa,stat=stat)
		glgadat[i].irac_qa = stat
	endif
	if glgadat[i].irac_qa eq 2 and $
		not glgadat[i].i3p6um_img and not glgadat[i].i4p5um_img then $
		glgadat[i].irac_qa = -1
	iracplots = ddeg+'plots/'+id[i]+'_irac_profile.pdf'
	glgadat[i].irac_plots = (file_info(iracplots)).mtime
;
; IRAC photometry
; 3p6um
	iracphot = ddeg+'photometry/'+id[i]+'_3p6um_aperture.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].i3p6um_ap_a = a
		glgadat[i].i3p6um_ap_mag = mag
		glgadat[i].i3p6um_ap_magerr = mag_e
	endif
	iracphot = ddeg+'photometry/'+id[i]+'_3p6um_asymptotic.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].i3p6um_asym_a = a
		glgadat[i].i3p6um_asym_mag = mag
		glgadat[i].i3p6um_asym_magerr = mag_e
	endif
	iracphot = ddeg+'photometry/'+id[i]+'_3p6um_total.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].i3p6um_tot_a = a
		glgadat[i].i3p6um_tot_mag = mag
		glgadat[i].i3p6um_tot_magerr = mag_e
	endif
	glgadat[i].irac_phot = (file_info(iracphot)).mtime
; 4p5um
	iracphot = ddeg+'photometry/'+id[i]+'_4p5um_aperture.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].i4p5um_ap_a = a
		glgadat[i].i4p5um_ap_mag = mag
		glgadat[i].i4p5um_ap_magerr = mag_e
	endif
	iracphot = ddeg+'photometry/'+id[i]+'_4p5um_asymptotic.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].i4p5um_asym_a = a
		glgadat[i].i4p5um_asym_mag = mag
		glgadat[i].i4p5um_asym_magerr = mag_e
	endif
	iracphot = ddeg+'photometry/'+id[i]+'_4p5um_total.dat'
	if file_test(iracphot) then begin
		readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
		glgadat[i].i4p5um_tot_a = a
		glgadat[i].i4p5um_tot_mag = mag
		glgadat[i].i4p5um_tot_magerr = mag_e
	endif
	glgadat[i].irac_phot = (file_info(iracphot)).mtime
;
	if not keyword_set(silent) then $
		print,string(13B),i+1,'/',nh,id[i],glgadat[i].ra,dbstat, $
			format='($,a1,i6,a1,i6,2x,a-25,f13.8,2x,a-10)'
endfor
if not keyword_set(silent) then begin
	print,' '
endif
;
return
;
end
