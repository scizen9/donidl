pro glgadb_print,gstat,outfile=outfile,list=list
;+
; glgadb_print - print gstat structure results
;
; INPUTS:
;	gstat	- glga status struct (see glga_status__define.pro)
;
; KEYWORDS:
;	outfile	- set to print output to file, otherwise to screen
;	list	- set to just print a list of galaxy properties:
;			id,ra,dec,majax,minax,pa,ty without the status info
;-
; count entries
ns = n_elements(gstat)
;
; check keywords
if keyword_set(outfile) then begin
	filestamp,outfile
	openw,ol,outfile,/get_lun
	wrt2file = (1 eq 1)
endif else	wrt2file = (1 eq 0)
;
;
hdr1 = '#                                (J2000)                  (arcmin)      (deg)                         GALEX                     SDSS                            2MASS                       WISE                          IRAC'
hdr2 = '#ID                         ra           dec           majax    minax    pa     type       ell dss  | f n r p m jpg qa ph pl  | u g r i z r p m jpg qa ph pl  | j h k r p m jpg qa ph pl  | 1 2 3 4 r p m jpg qa ph pl  | 3 4 r p m jpg qa ph pl'
hdr3 = '#                                (J2000)                  (arcmin)      (deg)'
hdr4 = '#ID                         ra           dec           majax    minax    pa     type'
if keyword_set(list) then begin
	hdra = hdr3
	hdrb = hdr4
endif else begin
	hdra = hdr1
	hdrb = hdr2
endelse
if wrt2file then begin
	printf,ol,hdra
	printf,ol,hdrb
endif else begin
	print,hdra
	print,hdrb
endelse
;
for i=0,ns-1 do begin
    if keyword_set(list) then begin
	out = string(gstat[i].id, gstat[i].ra, gstat[i].dec, $
		gstat[i].majax/60., gstat[i].minax/60., $
		gstat[i].pa, gstat[i].type, $
		format='(a-25,2f13.8,3f9.3,2x,a-8)')
    endif else begin
	out = string( $
		gstat[i].id, gstat[i].ra, gstat[i].dec, $
		gstat[i].majax/60., gstat[i].minax/60., $
		gstat[i].pa, gstat[i].type, $
		gstat[i].new_ell, gstat[i].dss_img, $
		gstat[i].fuv_img, gstat[i].nuv_img, $
		gstat[i].galex_roi, gstat[i].galex_pointsrc, $
		gstat[i].galex_mask, $
		gstat[i].galex_jpg, gstat[i].galex_qa, $
		gstat[i].galex_phot, gstat[i].galex_plots, $
		gstat[i].u_img, gstat[i].g_img, gstat[i].r_img, $
		gstat[i].i_img, gstat[i].z_img, $
		gstat[i].sdss_roi, gstat[i].sdss_pointsrc, $
		gstat[i].sdss_mask, $
		gstat[i].sdss_jpg, gstat[i].sdss_qa, $
		gstat[i].sdss_phot, gstat[i].sdss_plots, $
		gstat[i].j_img, gstat[i].h_img, gstat[i].k_img, $
		gstat[i].twomass_roi, gstat[i].twomass_pointsrc, $
		gstat[i].twomass_mask, $
		gstat[i].twomass_jpg, gstat[i].twomass_qa, $
		gstat[i].twomass_phot, gstat[i].twomass_plots, $
		gstat[i].w1_img, gstat[i].w2_img, $
		gstat[i].w3_img, gstat[i].w4_img, $
		gstat[i].wise_roi, gstat[i].wise_pointsrc, $
		gstat[i].wise_mask, $
		gstat[i].wise_jpg, gstat[i].wise_qa, $
		gstat[i].wise_phot, gstat[i].wise_plots, $
		gstat[i].i3p6um_img, gstat[i].i4p5um_img, $
		gstat[i].irac_roi, gstat[i].irac_pointsrc, $
		gstat[i].irac_mask, $
		gstat[i].irac_jpg, gstat[i].irac_qa, $
		gstat[i].irac_phot, gstat[i].irac_plots, $
		format='(a-25,2f13.8,3f9.3,2x,a-8,2x,2i4,3x, 2i2,3i2,i4,3i3,3x, 5i2,3i2,i4,3i3,3x, 3i2,3i2,i4,3i3,3x, 4i2,3i2,i4,3i3,3x, 2i2,3i2,i4,3i3)')
    endelse
    if wrt2file then $
		printf,ol,out $
    else	print,out
endfor
;
if wrt2file then free_lun,ol
;
return
end
