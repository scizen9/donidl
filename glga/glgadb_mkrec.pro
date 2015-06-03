function glgadb_mkrec,inid,degdir
;+
; glgadb_mkrec - make a single record in the GLGA info struct database
;
;	inid	- GLGA id (string)
;	degdir	- GLGA data directory: !GLGA_ROOT+'/data/???D/' with ???
;		  appropriate for the RA of the object specified by INID
;-
; common
COMMON galdb_info
COMMON glgadb_info
;
; check inputs
if n_params(0) lt 2 then begin
	print,'Usage: newrec = glgadb_mkrec( <input_id>, <glga_degdir> )'
	return,-1
endif
;
; used to generate file names
id = strcompress(inid,/rem)
;
; define glga_status struct
A = {glgadb_data}
glgarec = struct_init(A)
;
; start filling record
glgarec.id = id
ddeg = degdir
;
; check gals database
h=gfind(id,count=ng,/silent)
;
; in database
if ng gt 0 then begin
	h=h[0]
	if strlen(strtrim(galdat[h].type,2)) gt 0 then $
		glgarec.type = galdat[h].type
; start with literature values
	glgarec.ra	= galdat[h].ra
	glgarec.dec	= galdat[h].dec
	glgarec.majax	= galdat[h].majax
	glgarec.minax	= galdat[h].minax
	glgarec.pa	= galdat[h].pa
	glgarec.pgc     = galdat[h].pgc
; catalog
	if glgarec.majax ge 90. then begin
		glgarec.catalog='glga_v1'
	endif else if glgarec.majax ge 48. then begin
		glgarec.catalog='glga_v2'
	endif else glgarec.catalog='extra'
;
; not in database
endif else begin
	glgarec.catalog	= 'extra'
	;
	; check PGC
	get_ned,get_hl_name(glgarec.id),pgc=pgc,/silent
	if pgc ge 0 then glgarec.pgc = pgc
	;
	; check ellipsepar files for basic params
	eflist = file_search(ddeg+'photometry/'+id+ $
				  '_*_ellipsepar.dat',count=nef)
	if nef ge 1 then begin
		readcol,eflist[0],efra,efdec,efa,efb,efpa, $
			format='d,d,f,f,f',comment='#',/silent
		glgarec.ra	= efra
		glgarec.dec	= efdec
		glgarec.majax	= efa*2.0
		glgarec.minax	= efb*2.0
		glgarec.pa	= efpa
	endif
endelse
;
; no anonymous galaxies in catalog
if strmid(strtrim(glgarec.id,2),0,1) eq 'A' then $
	glgarec.catalog	= 'extra'
;
; ellipse file is definitive
ellfile = ddeg+'aux/'+id+'_ellipse.dat'
glgarec.new_ell = (file_info(ellfile)).mtime
if glgarec.new_ell gt 0 then begin
	readcol,ellfile,majd_as,mind_as,ra,dec,pa, form='f,f,d,d,f',/silent
	glgarec.ra	= ra
	glgarec.dec	= dec
	glgarec.majax	= majd_as
	glgarec.minax	= mind_as
	glgarec.pa	= pa
endif
;
; use ellipsepar files if no ellipse file or literature values (ra lt 0)
if glgarec.ra lt 0 then begin
	eflist = file_search(ddeg+'photometry/'+id+ $
			  '_*_ellipsepar.dat',count=nef)
	if nef ge 1 then begin
		readcol,eflist[0],efra,efdec,efa,efb,efpa, $
			format='d,d,f,f,f',comment='#',/silent
		glgarec.ra	= efra
		glgarec.dec	= efdec
		glgarec.majax	= efa*2.0
		glgarec.minax	= efb*2.0
		glgarec.pa	= efpa
	endif
endif
;
; dss img
dssfile = ddeg+'dss/fits/'+id+'_dss2_red.fits.gz'
glgarec.dss_img = (file_info(dssfile)).mtime
;
; GALEX imgs
fuvfile = ddeg+'galex/fits/'+id+'_FUV.fits.gz'
glgarec.fuv_img = (file_info(fuvfile)).mtime
nuvfile = ddeg+'galex/fits/'+id+'_NUV.fits.gz'
glgarec.nuv_img = (file_info(nuvfile)).mtime
;
; GALEX jpgs
uvjpg = ddeg+'galex/jpg/'+id+'_FUV.jpg'
glgarec.fuv_jpg = (file_info(uvjpg)).mtime
uvjpg = ddeg+'galex/jpg/'+id+'_NUV.jpg'
glgarec.nuv_jpg = (file_info(uvjpg)).mtime
uvjpg = ddeg+'galex/jpg/'+id+'_FUVNUV.jpg'
glgarec.galex_jpg = (file_info(uvjpg)).mtime
;
; GALEX plots
fuvplot = ddeg+'plots/'+id+'_FUV.pdf'
glgarec.fuv_plot = (file_info(fuvplot)).mtime
nuvplot = ddeg+'plots/'+id+'_NUV.pdf'
glgarec.nuv_plot = (file_info(nuvplot)).mtime
;
; GALEX aux
roifile = ddeg+'aux/'+id+'_galex_roi.dat'
glgarec.galex_roi = (file_info(roifile)).mtime
pntfile = ddeg+'aux/'+id+'_galex_pointsrc.dat'
glgarec.galex_pointsrc = (file_info(pntfile)).mtime
mskfile = ddeg+'aux/'+id+'_galex_mask.fits.gz'
glgarec.galex_mask = (file_info(mskfile)).mtime
;
; GALEX outputs
uvqa = ddeg+'galex/fits/'+id+'_qa.txt'
if file_test(uvqa) then begin
	qa = glga_read_qa_stat(uvqa,stat=stat)
	glgarec.galex_qa = stat
endif
if glgarec.galex_qa eq 2 and $
	not glgarec.fuv_img and not glgarec.nuv_img then $
   	glgarec.galex_qa = -1
uvplots = ddeg+'plots/'+id+'_galex_profile.pdf'
glgarec.galex_plots = (file_info(uvplots)).mtime
;
; GALEX photometry
; FUV
uvphot = ddeg+'photometry/'+id+'_FUV_aperture.dat'
if file_test(uvphot) then begin
	readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.fuv_ap_a = a
	glgarec.fuv_ap_mag = mag
	glgarec.fuv_ap_magerr = mag_e
endif
uvphot = ddeg+'photometry/'+id+'_FUV_asymptotic.dat'
if file_test(uvphot) then begin
	readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.fuv_asym_a = a
	glgarec.fuv_asym_mag = mag
	glgarec.fuv_asym_magerr = mag_e
endif
uvphot = ddeg+'photometry/'+id+'_FUV_total.dat'
if file_test(uvphot) then begin
	readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.fuv_tot_a = a
	glgarec.fuv_tot_mag = mag
	glgarec.fuv_tot_magerr = mag_e
endif
glgarec.galex_phot = (file_info(uvphot)).mtime
; NUV
uvphot = ddeg+'photometry/'+id+'_NUV_aperture.dat'
if file_test(uvphot) then begin
	readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.nuv_ap_a = a
	glgarec.nuv_ap_mag = mag
	glgarec.nuv_ap_magerr = mag_e
endif
uvphot = ddeg+'photometry/'+id+'_NUV_asymptotic.dat'
if file_test(uvphot) then begin
	readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.nuv_asym_a = a
	glgarec.nuv_asym_mag = mag
	glgarec.nuv_asym_magerr = mag_e
endif
uvphot = ddeg+'photometry/'+id+'_NUV_total.dat'
if file_test(uvphot) then begin
	readcol,uvphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.nuv_tot_a = a
	glgarec.nuv_tot_mag = mag
	glgarec.nuv_tot_magerr = mag_e
endif
glgarec.galex_phot = (file_info(uvphot)).mtime
;
; SDSS imgs
ufile = ddeg+'sdss/fits/'+id+'_u.fits.gz'
glgarec.u_img = (file_info(ufile)).mtime
gfile = ddeg+'sdss/fits/'+id+'_g.fits.gz'
glgarec.g_img = (file_info(gfile)).mtime
rfile = ddeg+'sdss/fits/'+id+'_r.fits.gz'
glgarec.r_img = (file_info(rfile)).mtime
ifile = ddeg+'sdss/fits/'+id+'_i.fits.gz'
glgarec.i_img = (file_info(ifile)).mtime
zfile = ddeg+'sdss/fits/'+id+'_z.fits.gz'
glgarec.z_img = (file_info(zfile)).mtime
;
; SDSS jpgs
sdssjpg = ddeg+'sdss/jpg/'+id+'_u.jpg'
glgarec.u_jpg = (file_info(sdssjpg)).mtime
sdssjpg = ddeg+'sdss/jpg/'+id+'_g.jpg'
glgarec.g_jpg = (file_info(sdssjpg)).mtime
sdssjpg = ddeg+'sdss/jpg/'+id+'_r.jpg'
glgarec.r_jpg = (file_info(sdssjpg)).mtime
sdssjpg = ddeg+'sdss/jpg/'+id+'_i.jpg'
glgarec.i_jpg = (file_info(sdssjpg)).mtime
sdssjpg = ddeg+'sdss/jpg/'+id+'_z.jpg'
glgarec.z_jpg = (file_info(sdssjpg)).mtime
sdssjpg = ddeg+'sdss/jpg/'+id+'_gri.jpg'
glgarec.sdss_jpg = (file_info(sdssjpg)).mtime
;
; SDSS plots
uplot = ddeg+'plots/'+id+'_u.pdf'
glgarec.u_plot = (file_info(uplot)).mtime
gplot = ddeg+'plots/'+id+'_g.pdf'
glgarec.g_plot = (file_info(gplot)).mtime
rplot = ddeg+'plots/'+id+'_r.pdf'
glgarec.r_plot = (file_info(rplot)).mtime
iplot = ddeg+'plots/'+id+'_i.pdf'
glgarec.i_plot = (file_info(iplot)).mtime
zplot = ddeg+'plots/'+id+'_z.pdf'
glgarec.z_plot = (file_info(zplot)).mtime
;
; SDSS aux
roifile = ddeg+'aux/'+id+'_sdss_roi.dat'
glgarec.sdss_roi = (file_info(roifile)).mtime
pntfile = ddeg+'aux/'+id+'_sdss_pointsrc.dat'
glgarec.sdss_pointsrc = (file_info(pntfile)).mtime
mskfile = ddeg+'aux/'+id+'_sdss_mask.fits.gz'
glgarec.sdss_mask = (file_info(mskfile)).mtime
;
; SDSS outputs
sdssqa = ddeg+'sdss/fits/'+id+'_qa.txt'
if file_test(sdssqa) then begin
	qa = glga_read_qa_stat(sdssqa,stat=stat)
	glgarec.sdss_qa = stat
endif
if glgarec.sdss_qa eq 2 and $
	not glgarec.u_img and not glgarec.g_img and $
	not glgarec.r_img and not glgarec.i_img and $
	not glgarec.z_img then glgarec.sdss_qa = -1
sdssplots = ddeg+'plots/'+id+'_sdss_profile.pdf'
glgarec.sdss_plots = (file_info(sdssplots)).mtime
;
; SDSS photometry
; u
sdssphot = ddeg+'photometry/'+id+'_u_aperture.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.u_ap_a = a
	glgarec.u_ap_mag = mag
	glgarec.u_ap_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_u_asymptotic.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.u_asym_a = a
	glgarec.u_asym_mag = mag
	glgarec.u_asym_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_u_total.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.u_tot_a = a
	glgarec.u_tot_mag = mag
	glgarec.u_tot_magerr = mag_e
endif
glgarec.sdss_phot = (file_info(sdssphot)).mtime
; g
sdssphot = ddeg+'photometry/'+id+'_g_aperture.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.g_ap_a = a
	glgarec.g_ap_mag = mag
	glgarec.g_ap_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_g_asymptotic.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.g_asym_a = a
	glgarec.g_asym_mag = mag
	glgarec.g_asym_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_g_total.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.g_tot_a = a
	glgarec.g_tot_mag = mag
	glgarec.g_tot_magerr = mag_e
endif
glgarec.sdss_phot = (file_info(sdssphot)).mtime
; r
sdssphot = ddeg+'photometry/'+id+'_r_aperture.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.r_ap_a = a
	glgarec.r_ap_mag = mag
	glgarec.r_ap_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_r_asymptotic.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.r_asym_a = a
	glgarec.r_asym_mag = mag
	glgarec.r_asym_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_r_total.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.r_tot_a = a
	glgarec.r_tot_mag = mag
	glgarec.r_tot_magerr = mag_e
endif
glgarec.sdss_phot = (file_info(sdssphot)).mtime
; i
sdssphot = ddeg+'photometry/'+id+'_i_aperture.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.i_ap_a = a
	glgarec.i_ap_mag = mag
	glgarec.i_ap_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_i_asymptotic.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.i_asym_a = a
	glgarec.i_asym_mag = mag
	glgarec.i_asym_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_i_total.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.i_tot_a = a
	glgarec.i_tot_mag = mag
	glgarec.i_tot_magerr = mag_e
endif
glgarec.sdss_phot = (file_info(sdssphot)).mtime
; z
sdssphot = ddeg+'photometry/'+id+'_z_aperture.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.z_ap_a = a
	glgarec.z_ap_mag = mag
	glgarec.z_ap_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_z_asymptotic.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.z_asym_a = a
	glgarec.z_asym_mag = mag
	glgarec.z_asym_magerr = mag_e
endif
sdssphot = ddeg+'photometry/'+id+'_z_total.dat'
if file_test(sdssphot) then begin
	readcol,sdssphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.z_tot_a = a
	glgarec.z_tot_mag = mag
	glgarec.z_tot_magerr = mag_e
endif
glgarec.sdss_phot = (file_info(sdssphot)).mtime
;
; 2MASS imgs
jfile = ddeg+'2mass/fits/'+id+'_j.fits.gz'
glgarec.j_img = (file_info(jfile)).mtime
hfile = ddeg+'2mass/fits/'+id+'_h.fits.gz'
glgarec.h_img = (file_info(hfile)).mtime
kfile = ddeg+'2mass/fits/'+id+'_k.fits.gz'
glgarec.k_img = (file_info(kfile)).mtime
;
; 2MASS jpgs
twomassjpg = ddeg+'2mass/jpg/'+id+'_j.jpg'
glgarec.j_jpg = (file_info(twomassjpg)).mtime
twomassjpg = ddeg+'2mass/jpg/'+id+'_h.jpg'
glgarec.h_jpg = (file_info(twomassjpg)).mtime
twomassjpg = ddeg+'2mass/jpg/'+id+'_k.jpg'
glgarec.k_jpg = (file_info(twomassjpg)).mtime
twomassjpg = ddeg+'2mass/jpg/'+id+'_jhk.jpg'
glgarec.twomass_jpg = (file_info(twomassjpg)).mtime
;
; 2MASS plots
jplot = ddeg+'plots/'+id+'_j.pdf'
glgarec.j_plot = (file_info(jplot)).mtime
hplot = ddeg+'plots/'+id+'_h.pdf'
glgarec.h_plot = (file_info(hplot)).mtime
kplot = ddeg+'plots/'+id+'_k.pdf'
glgarec.k_plot = (file_info(kplot)).mtime
;
; 2MASS aux
roifile = ddeg+'aux/'+id+'_2mass_roi.dat'
glgarec.twomass_roi = (file_info(roifile)).mtime
pntfile = ddeg+'aux/'+id+'_2mass_pointsrc.dat'
glgarec.twomass_pointsrc = (file_info(pntfile)).mtime
mskfile = ddeg+'aux/'+id+'_2mass_mask.fits.gz'
glgarec.twomass_mask = (file_info(mskfile)).mtime
;
; 2MASS outputs
twomassqa = ddeg+'2mass/fits/'+id+'_qa.txt'
if file_test(twomassqa) then begin
	qa = glga_read_qa_stat(twomassqa,stat=stat)
	glgarec.twomass_qa = stat
endif
if glgarec.twomass_qa eq 2 and $
	not glgarec.j_img and not glgarec.h_img and $
	not glgarec.k_img then glgarec.twomass_qa = -1
twomassplots = ddeg+'plots/'+id+'_2mass_profile.pdf'
glgarec.twomass_plots = (file_info(twomassplots)).mtime
;
; 2MASS photometry
; j
twomassphot = ddeg+'photometry/'+id+'_j_aperture.dat'
if file_test(twomassphot) then begin
	readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.j_ap_a = a
	glgarec.j_ap_mag = mag
	glgarec.j_ap_magerr = mag_e
endif
twomassphot = ddeg+'photometry/'+id+'_j_asymptotic.dat'
if file_test(twomassphot) then begin
	readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.j_asym_a = a
	glgarec.j_asym_mag = mag
	glgarec.j_asym_magerr = mag_e
endif
twomassphot = ddeg+'photometry/'+id+'_j_total.dat'
if file_test(twomassphot) then begin
	readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.j_tot_a = a
	glgarec.j_tot_mag = mag
	glgarec.j_tot_magerr = mag_e
endif
glgarec.twomass_phot = (file_info(twomassphot)).mtime
; h
twomassphot = ddeg+'photometry/'+id+'_h_aperture.dat'
if file_test(twomassphot) then begin
	readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.h_ap_a = a
	glgarec.h_ap_mag = mag
	glgarec.h_ap_magerr = mag_e
endif
twomassphot = ddeg+'photometry/'+id+'_h_asymptotic.dat'
if file_test(twomassphot) then begin
	readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.h_asym_a = a
	glgarec.h_asym_mag = mag
	glgarec.h_asym_magerr = mag_e
endif
twomassphot = ddeg+'photometry/'+id+'_h_total.dat'
if file_test(twomassphot) then begin
	readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.h_tot_a = a
	glgarec.h_tot_mag = mag
	glgarec.h_tot_magerr = mag_e
endif
glgarec.twomass_phot = (file_info(twomassphot)).mtime
; k
twomassphot = ddeg+'photometry/'+id+'_k_aperture.dat'
if file_test(twomassphot) then begin
	readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.k_ap_a = a
	glgarec.k_ap_mag = mag
	glgarec.k_ap_magerr = mag_e
endif
twomassphot = ddeg+'photometry/'+id+'_k_asymptotic.dat'
if file_test(twomassphot) then begin
	readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.k_asym_a = a
	glgarec.k_asym_mag = mag
	glgarec.k_asym_magerr = mag_e
endif
twomassphot = ddeg+'photometry/'+id+'_k_total.dat'
if file_test(twomassphot) then begin
	readcol,twomassphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.k_tot_a = a
	glgarec.k_tot_mag = mag
	glgarec.k_tot_magerr = mag_e
endif
glgarec.twomass_phot = (file_info(twomassphot)).mtime
;
; WISE imgs
w1file = ddeg+'wise/fits/'+id+'_w1.fits.gz'
glgarec.w1_img = (file_info(w1file)).mtime
w2file = ddeg+'wise/fits/'+id+'_w2.fits.gz'
glgarec.w2_img = (file_info(w2file)).mtime
w3file = ddeg+'wise/fits/'+id+'_w3.fits.gz'
glgarec.w3_img = (file_info(w3file)).mtime
w4file = ddeg+'wise/fits/'+id+'_w4.fits.gz'
glgarec.w4_img = (file_info(w4file)).mtime
;
; WISE jpgs
wisejpg = ddeg+'wise/jpg/'+id+'_w1.jpg'
glgarec.w1_jpg = (file_info(wisejpg)).mtime
wisejpg = ddeg+'wise/jpg/'+id+'_w2.jpg'
glgarec.w2_jpg = (file_info(wisejpg)).mtime
wisejpg = ddeg+'wise/jpg/'+id+'_w3.jpg'
glgarec.w3_jpg = (file_info(wisejpg)).mtime
wisejpg = ddeg+'wise/jpg/'+id+'_w4.jpg'
glgarec.w4_jpg = (file_info(wisejpg)).mtime
wisejpg = ddeg+'wise/jpg/'+id+'_w123.jpg'
glgarec.wise_jpg = (file_info(wisejpg)).mtime
;
; WISE plots
w1plot = ddeg+'plots/'+id+'_w1.pdf'
glgarec.w1_plot = (file_info(w1plot)).mtime
w2plot = ddeg+'plots/'+id+'_w2.pdf'
glgarec.w2_plot = (file_info(w2plot)).mtime
w3plot = ddeg+'plots/'+id+'_w3.pdf'
glgarec.w3_plot = (file_info(w3plot)).mtime
w4plot = ddeg+'plots/'+id+'_w4.pdf'
glgarec.w4_plot = (file_info(w4plot)).mtime
;
; WISE aux
roifile = ddeg+'aux/'+id+'_wise_roi.dat'
glgarec.wise_roi = (file_info(roifile)).mtime
pntfile = ddeg+'aux/'+id+'_wise_pointsrc.dat'
glgarec.wise_pointsrc = (file_info(pntfile)).mtime
mskfile = ddeg+'aux/'+id+'_wise_mask.fits.gz'
glgarec.wise_mask = (file_info(mskfile)).mtime
;
; WISE outputs
wiseqa = ddeg+'wise/fits/'+id+'_qa.txt'
if file_test(wiseqa) then begin
	qa = glga_read_qa_stat(wiseqa,stat=stat)
	glgarec.wise_qa = stat
endif
if glgarec.wise_qa eq 2 and $
	not glgarec.w1_img and not glgarec.w2_img and $
	not glgarec.w3_img and not glgarec.w4_img then $
	glgarec.wise_qa = -1
wiseplots = ddeg+'plots/'+id+'_wise_profile.pdf'
glgarec.wise_plots = (file_info(wiseplots)).mtime
;
; WISE photometry
; w1
wisephot = ddeg+'photometry/'+id+'_w1_aperture.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w1_ap_a = a
	glgarec.w1_ap_mag = mag
	glgarec.w1_ap_magerr = mag_e
endif
wisephot = ddeg+'photometry/'+id+'_w1_asymptotic.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w1_asym_a = a
	glgarec.w1_asym_mag = mag
	glgarec.w1_asym_magerr = mag_e
endif
wisephot = ddeg+'photometry/'+id+'_w1_total.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w1_tot_a = a
	glgarec.w1_tot_mag = mag
	glgarec.w1_tot_magerr = mag_e
endif
glgarec.wise_phot = (file_info(wisephot)).mtime
; w2
wisephot = ddeg+'photometry/'+id+'_w2_aperture.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w2_ap_a = a
	glgarec.w2_ap_mag = mag
	glgarec.w2_ap_magerr = mag_e
endif
wisephot = ddeg+'photometry/'+id+'_w2_asymptotic.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w2_asym_a = a
	glgarec.w2_asym_mag = mag
	glgarec.w2_asym_magerr = mag_e
endif
wisephot = ddeg+'photometry/'+id+'_w2_total.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w2_tot_a = a
	glgarec.w2_tot_mag = mag
	glgarec.w2_tot_magerr = mag_e
endif
glgarec.wise_phot = (file_info(wisephot)).mtime
; w3
wisephot = ddeg+'photometry/'+id+'_w3_aperture.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w3_ap_a = a
	glgarec.w3_ap_mag = mag
	glgarec.w3_ap_magerr = mag_e
endif
wisephot = ddeg+'photometry/'+id+'_w3_asymptotic.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w3_asym_a = a
	glgarec.w3_asym_mag = mag
	glgarec.w3_asym_magerr = mag_e
endif
wisephot = ddeg+'photometry/'+id+'_w3_total.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w3_tot_a = a
	glgarec.w3_tot_mag = mag
	glgarec.w3_tot_magerr = mag_e
endif
glgarec.wise_phot = (file_info(wisephot)).mtime
; w4
wisephot = ddeg+'photometry/'+id+'_w4_aperture.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w4_ap_a = a
	glgarec.w4_ap_mag = mag
	glgarec.w4_ap_magerr = mag_e
endif
wisephot = ddeg+'photometry/'+id+'_w4_asymptotic.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w4_asym_a = a
	glgarec.w4_asym_mag = mag
	glgarec.w4_asym_magerr = mag_e
endif
wisephot = ddeg+'photometry/'+id+'_w4_total.dat'
if file_test(wisephot) then begin
	readcol,wisephot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.w4_tot_a = a
	glgarec.w4_tot_mag = mag
	glgarec.w4_tot_magerr = mag_e
endif
glgarec.wise_phot = (file_info(wisephot)).mtime
;
; IRAC imgs
i3p6umfile = ddeg+'irac/fits/'+id+'_3p6um.fits.gz'
glgarec.i3p6um_img = (file_info(i3p6umfile)).mtime
i4p5umfile = ddeg+'irac/fits/'+id+'_4p5um.fits.gz'
glgarec.i4p5um_img = (file_info(i4p5umfile)).mtime
;
; IRAC jpgs
iracjpg = ddeg+'irac/jpg/'+id+'_3p6um.jpg'
glgarec.i3p6um_jpg = (file_info(iracjpg)).mtime
iracjpg = ddeg+'irac/jpg/'+id+'_4p5um.jpg'
glgarec.i4p5um_jpg = (file_info(iracjpg)).mtime
iracjpg = ddeg+'irac/jpg/'+id+'_3p6um4p5um.jpg'
glgarec.irac_jpg = (file_info(iracjpg)).mtime
;
; IRAC plots
i3p6umplot = ddeg+'plots/'+id+'_3p6um.pdf'
glgarec.i3p6um_plot = (file_info(i3p6umplot)).mtime
i4p5umplot = ddeg+'plots/'+id+'_4p5um.pdf'
glgarec.i4p5um_plot = (file_info(i4p5umplot)).mtime
;
; IRAC aux
roifile = ddeg+'aux/'+id+'_irac_roi.dat'
glgarec.irac_roi = (file_info(roifile)).mtime
pntfile = ddeg+'aux/'+id+'_irac_pointsrc.dat'
glgarec.irac_pointsrc = (file_info(pntfile)).mtime
mskfile = ddeg+'aux/'+id+'_irac_mask.fits.gz'
glgarec.irac_mask = (file_info(mskfile)).mtime
;
; IRAC outputs
iracqa = ddeg+'irac/fits/'+id+'_qa.txt'
if file_test(iracqa) then begin
	qa = glga_read_qa_stat(iracqa,stat=stat)
	glgarec.irac_qa = stat
endif
if glgarec.irac_qa eq 2 and $
	not glgarec.i3p6um_img and not glgarec.i4p5um_img then $
	glgarec.irac_qa = -1
iracplots = ddeg+'plots/'+id+'_irac_profile.pdf'
glgarec.irac_plots = (file_info(iracplots)).mtime
;
; IRAC photometry
; 3p6um
iracphot = ddeg+'photometry/'+id+'_3p6um_aperture.dat'
if file_test(iracphot) then begin
	readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.i3p6um_ap_a = a
	glgarec.i3p6um_ap_mag = mag
	glgarec.i3p6um_ap_magerr = mag_e
endif
iracphot = ddeg+'photometry/'+id+'_3p6um_asymptotic.dat'
if file_test(iracphot) then begin
	readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.i3p6um_asym_a = a
	glgarec.i3p6um_asym_mag = mag
	glgarec.i3p6um_asym_magerr = mag_e
endif
iracphot = ddeg+'photometry/'+id+'_3p6um_total.dat'
if file_test(iracphot) then begin
	readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.i3p6um_tot_a = a
	glgarec.i3p6um_tot_mag = mag
	glgarec.i3p6um_tot_magerr = mag_e
endif
glgarec.irac_phot = (file_info(iracphot)).mtime
; 4p5um
iracphot = ddeg+'photometry/'+id+'_4p5um_aperture.dat'
if file_test(iracphot) then begin
	readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.i4p5um_ap_a = a
	glgarec.i4p5um_ap_mag = mag
	glgarec.i4p5um_ap_magerr = mag_e
endif
iracphot = ddeg+'photometry/'+id+'_4p5um_asymptotic.dat'
if file_test(iracphot) then begin
	readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.i4p5um_asym_a = a
	glgarec.i4p5um_asym_mag = mag
	glgarec.i4p5um_asym_magerr = mag_e
endif
iracphot = ddeg+'photometry/'+id+'_4p5um_total.dat'
if file_test(iracphot) then begin
	readcol,iracphot,a,mag,mag_e,form='f,f,f',comment='#',/silent
	glgarec.i4p5um_tot_a = a
	glgarec.i4p5um_tot_mag = mag
	glgarec.i4p5um_tot_magerr = mag_e
endif
glgarec.irac_phot = (file_info(iracphot)).mtime
;
; general record timestamp
glgarec.timestamp = systime(1)
;
return,glgarec
;
end
