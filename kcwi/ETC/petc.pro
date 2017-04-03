	pro petc, h=h, slicer, grating, grat_wave, f_lam_index, seeing, exposure_time, ccd_bin,  mag_AB=mag_AB, flux=flux, Nframes=Nframes, nas=nas, $
		ccd_speed=ccd_speed, spatial_bin=spatial_bin, spectral_bin=spectral_bin, sb=sb, plotout=plotout, emline_width=emline_width
	if keyword_set(h) then begin
	print,'ketc, slicer, grating, ref_wave, f_lam_index, seeing, exposure time, ccd_bin, mag_AB=mag_AB, flux=flux, Nframes=Nframes, nas=nas, $'
	print,'   spatial_bin=spatial_bin, spectral_bin=spectral_bin, sb=sb, plotout=plotout, emline_width=emline_width'
	print
        print,"slicer = 'L'"
        print,"grating = 'MEDREZ, BLUE, YELLOW, RED'"
       	print,'3800. < ref_wave < 7900.'
	print,'f_lam_index: source f_lam ~ lam^f_lam_index'
        print,'exposure_time = seconds for source image (total) for all frames'
        print,'seeing = arcsec'
        print,'/mag_AB = continuum AB magnitude at wavelength (ref_wave)'
	print,'/flux = POINT: erg cm^-2 s^-1 Å^1 (continuum source [total]); erg cm^-2 s^1 (point line source [total]) [emline = width in Å];'
        print,'       = EXTENDED: erg cm^-2 s^-1 Å^1 arcsec^-2 (continuum source [total]); erg cm^-2 s^1 arcsec^-2 (point line source [total]) [emline = width in Å];'
        print,'/sb = m_AB in mag arcsec^2; flux = cgs arcsec^-2'
        print,'/emline_width = flux is for an emission line, not continuum flux (only works for flux), and emission line width is emline_width Å'
        print,'/spatial_bin = [dx,dy] bin in arcsec x arcsec for binning extended emission flux. if /sb keyword then default is 1 x 1 arcsec^2'
        print,'/spectral_bin = Å to bin for S/N calculation'
        print,"ccd_bin = '1x1','2x2'"
;       ccd_speed = 'FAST','SLOW' [Default=SLOW] ; NOT IMPLEMENTED YET
	print,'/nas=minutes per exposure -- assume exposure_time is total, account for 1/2 Source, 1/2 Sky, Sky subtraction noise, overhead'
	print,'/Nframes = number of frames (default is 1)'
	return
	endif

;       INPUT
;       slicer = 'L,M,S'
;       grating = 'BH1, BH2, BH3, BM, BL'
;       3400. < grat_wave < 6000.
;       /mag_AB = continuum AB magnitude at wavelength
;       /flux = POINT: erg cm^-2 s^-1 Å^1 (continuum source [total]); erg cm^-2 s^1 (point line source [total]) [emline = width in Å]; 
;	 	= EXTENDED: erg cm^-2 s^-1 Å^1 arcsec^-2 (continuum source [total]); erg cm^-2 s^1 arcsec^-2 (point line source [total]) [emline = width in Å];
;       /sb = m_AB in mag arcsec^2; flux = cgs arcsec^-2
;	/emline_width = flux is for an emission line, not continuum flux (only works for flux), and emission line width is emline_width Å
;	/spatial_bin = [dx,dy] bin in arcsec x arcsec for binning extended emission flux. if /sb keyword then default is 1 x 1 arcsec^2
;	/spectral_bin = Å to bin for S/N calculation
;       ccd_bin = '1x1','2x2'
;       ccd_speed = 'FAST','SLOW' [Default=SLOW] ; NOT IMPLEMENTED YET
;       exposure_time = seconds for source image (total) for all frames
;       seeing = arcsec
;       /nas=minutes per exposure -- assume exposure_time is total, account for 1/2 Source, 1/2 Sky, Sky subtraction noise, overhead
;	/Nframes = number of frames (default is 1)
;
;	OUTPUT
;       p_A = is the total cgs flux of point or extended object
;	SNR vs. wavelength; Object cts vs. wavelength, Sky cts vs. wavelength; Read noise "cts"; contrast vs. wavelength
;
;	NOTES
;		1. converts everything to a surface brightness / Å
;
	bin_factor = 1.
        ccd_bin = '2x2'
	if ccd_bin eq '2x2' then bin_factor = 0.25
	if ccd_bin eq '2x2' and slicer eq 'S' then print,'******** WARNING: DO NOT USE 2x2 BINNING WITH SMALL SLICER'
	read_noise = 2.7 ; electrons
	Nf = 1.
	if keyword_set(Nframes) then Nf = Nframes
	
        chsz = 3
        nas_overhead = 10. ; seconds per half cycle
        seeing1 = seeing
        seeing2 = seeing
	pixels_per_arcsec = 1./0.28
        slicer='L'
        if slicer eq 'L' then begin
		seeing2 = 2.6
		snr_spatial_bin = seeing1*seeing2
		pixels_spectral = 8
		arcsec_per_slice = 2.6
	endif
        if slicer eq 'M' then begin
		seeing2 = max([0.69,seeing])
		snr_spatial_bin = seeing1*seeing2
		pixels_spectral = 4
		arcsec_per_slice = 0.69
	endif
	if slicer eq 'S' then begin
		seeing2 = seeing
		snr_spatial_bin = seeing1*seeing2
		pixels_spectral = 2
		arcsec_per_slice = 0.35
	endif
	N_slices = seeing/arcsec_per_slice

	if keyword_set(spatial_bin) then begin
		N_slices = spatial_bin[1]/arcsec_per_slice
		snr_spatial_bin = spatial_bin[0]*spatial_bin[1]
	endif
	pixels_spatial_bin = pixels_per_arcsec * N_slices
		
        grating=strupcase(grating)
	if grating eq 'MEDREZ' then begin
		A_per_pixel = 0.27
	endif
	if grating eq 'BLUE' or grating eq 'YELLOW' or grating eq 'RED' then begin
		A_per_pixel = 0.12
	endif

	print,'f_lam ~ lam^',f_lam_index
	print,'reference wavelength =',grat_wave
	print,'SLICER: ',slicer
	print,'GRATING: ',grating
	print,'SEEING: ',seeing, ' arcsec'
	print,'Å/pixel:',A_per_pixel
	print,'spectral pixels in 1 spectral resolution element = ',pixels_spectral
	A_per_spectral_bin = pixels_spectral*A_per_pixel
	print,'Å/resolution element:',A_per_spectral_bin
	if keyword_set(spectral_bin) then begin
		snr_spectral_bin = spectral_bin
	endif else begin
		snr_spectral_bin = A_per_spectral_bin
	endelse
	print,'Å/SNR bin:',snr_spectral_bin
	pixels_per_snr_spec_bin = snr_spectral_bin/A_per_pixel
	print,'Pixels/Spectral SNR bin:',pixels_per_snr_spec_bin
	print,'SNR Spatial Bin [arcsec^2]: ',snr_spatial_bin
	print,'SNR Spatial Bin [pixels^2]: ',pixels_spatial_bin

	flux1 = 0
	if keyword_set(flux) then flux1 = flux
	if keyword_set(flux) and keyword_set(emline_width) then flux1 = flux/emline_width
	if ~keyword_set(flux) and keyword_set(emline_width) then begin
		print,'Dont use mag_AB for emission line'
		return
	endif
	if keyword_set(mag_AB) then begin
		flux1 = (10^(-0.4*(mag_AB+48.6)))*(3.d18/grat_wave)/grat_wave
	endif

	make_obj,flux1,grat_wave, f_lam_index,w, p_A

	if keyword_set(mag_AB) and ~keyword_set(sb) then begin
		flux_input = 'mag_AB'
		print,'OBJECT mag:',mag_AB,' ',flux_input
	endif
	if keyword_set(mag_AB) and keyword_set(sb) then begin
		flux_input = 'mag_AB / arcsec^2'
		print,'OBJECT mag:',mag_AB,' ',flux_input
	endif

	if keyword_set(flux) and ~keyword_set(sb) and ~keyword_set(emline_width) then flux_input = 'erg cm^-2 s^-1 Å^-1'
	if keyword_set(flux) and ~keyword_set(sb) and keyword_set(emline_width) then flux_input = 'erg cm^-2 s^-1 in '+string(format='(f4.1)',emline_width)+' Å'
	if keyword_set(flux) and keyword_set(sb) and ~keyword_set(emline_width) then flux_input = 'erg cm^-2 s^-1 Å^-1 arcsec^-2'
	if keyword_set(flux) and keyword_set(sb) and keyword_set(emline_width) then flux_input = 'erg cm^-2 s^-1 arcsec^-2 in '+string(format='(f4.1)',emline_width)+' Å'
	if keyword_set(flux) then print,'OBJECT Flux ',flux,' ',flux_input
	if keyword_Set(emline_width) then print,'EMISSION LINE OBJECT --> flux is not per unit Å'
		
;	Calculate Read Noise
;	Need to determine pixels per spectral and spatial bin
;	1 unbinned pixel = 0.147 arcsec in both dimensions (no significant difference in grating angles
;	1 pixel spectral depends on grating: slicer BH L = 8 CCD pixels per spectral element = 4500/4500 = 1Å; M = 0.5Å; S = 0.25Å
;	1 pixel spectral depends on grating: slicer BM L = 8 CCD pixels per spectral element = 4500/2000 = 2.4Å; M = 1.2Å; S = 0.6Å
;	1 pixel spectral depends on grating: slicer BL L = 8 CCD pixels per spectral element = 4500/900 = 5Å; M = 2.5Å; S = 1.25Å

	t_exp = exposure_time
        ; pcwi fudge factors. 
        pcwi_area_corr=0.25 ; telescope area correction
        pcwi_sky_corr=2.5; sky surface brightness correction (estimate 1 mag)
	if ~keyword_set(nas) then begin
		c_o = obj_cts(w,p_A,grating,t_exp)*snr_spatial_bin*snr_spectral_bin
		c_s = sky_cts(w,grating,exposure_time)*snr_spatial_bin*snr_spectral_bin
		c_r = Nf*read_noise^2*pixels_per_snr_spec_bin*pixels_spatial_bin*bin_factor
                c_o=c_o*pcwi_area_corr
                c_s=c_s*pcwi_area_corr*pcwi_sky_corr
		snr = c_o/sqrt(c_s+c_o+c_r)
	endif
	if keyword_set(nas) then begin
		n_cyc = fix((exposure_time-nas_overhead)/2./(nas+nas_overhead)+0.5)
		total_exposure = (2*n_cyc*(nas+nas_overhead))+nas_overhead
		print,'NAS: Rounding up to ',n_cyc, ' Cycles of NAS for total exposure of',total_exposure,' s'
		t_exp = n_cyc*nas
		c_o = obj_cts(w,p_A,grating,t_exp)*snr_spatial_bin*snr_spectral_bin
		c_s = sky_cts(w,grating,t_exp)*snr_spatial_bin*snr_spectral_bin
		c_r = 2.*Nf*read_noise^2*pixels_per_snr_spec_bin*pixels_spatial_bin*bin_factor
                c_o=c_o*pcwi_area_corr
                c_s=c_s*pcwi_area_corr*pcwi_sky_corr
		snr = c_o/sqrt(2.*c_s+c_o+c_r)
	endif

	y0 = 0.1
	y1 = 0.9
	dy = (y1-y0)/6.
	chsz=1.
	window,0,title='SNR',ysize=900
        plot,w,snr,ytitle='SNR /'+string(format='(f4.1)',snr_spectral_bin)+' Å',charsize=chsz,title=plotout,pos=[0.15,y1-dy,0.9,y1],xrange=[grat_wave-500,grat_wave+500]
        plot,w,c_o,ytitle='Obj cts /'+string(format='(f4.1)',snr_spectral_bin)+' Å',charsize=chsz,pos=[0.15,y1-2*dy,0.9,y1-dy],/noerase,xrange=[grat_wave-500,grat_wave+500]
        plot,w,c_s,ytitle='Sky cts /'+string(format='(f4.1)',snr_spectral_bin)+' Å',charsize=chsz,pos=[0.15,y1-3*dy,0.9,y1-2*dy],/noerase,xrange=[grat_wave-500,grat_wave+500]
        plot,w,c_r*replicate(1.,n_elements(w)),ytitle='Read Noise cts /'+string(format='(f4.1)',snr_spectral_bin)+' Å',charsize=chsz,pos=[0.15,y1-4*dy,0.9,y1-3*dy],/noerase,yrange=[0.,2.*c_r],xrange=[grat_wave-500,grat_wave+500]
        plot,w,c_o/c_s,ytitle='Obj cts / Sky cts',charsize=chsz,pos=[0.15,y1-5*dy,0.9,y1-4*dy],/noerase,xrange=[grat_wave-500,grat_wave+500]
        plot,w,p_A,ytitle='Flux [ph cm^-2 s^-1 Å^-1]',charsize=chsz,xtitle='Wavelength (Å)',pos=[0.15,y1-6*dy,0.9,y1-5*dy],/noerase,xrange=[grat_wave-500,grat_wave+500]

	if keyword_set(plotout) then begin
	chsz = 0.8
	ps_start,file=plotout+'.eps'
	device,/inches,xsize=8,ysize=10,xoffset=0,yoffset=0
        plot,w,snr,ytitle='SNR /'+string(format='(f4.1)',snr_spectral_bin)+' Å',charsize=chsz,title=plotout,pos=[0.15,y1-dy,0.9,y1],xtickname=replicate(' ',20)
        plot,w,c_o,ytitle='Obj cts /'+string(format='(f4.1)',snr_spectral_bin)+' Å',charsize=chsz,pos=[0.15,y1-2*dy,0.9,y1-dy],/noerase,xtickname=replicate(' ',20)
        plot,w,c_s,ytitle='Sky cts /'+string(format='(f4.1)',snr_spectral_bin)+' Å',charsize=chsz,pos=[0.15,y1-3*dy,0.9,y1-2*dy],/noerase,xtickname=replicate(' ',20)
        plot,w,c_r*replicate(1.,n_elements(w)),ytitle='Read Noise cts /'+string(format='(f4.1)',snr_spectral_bin)+' Å',charsize=chsz,pos=[0.15,y1-4*dy,0.9,y1-3*dy],/noerase,xtickname=replicate(' ',20),yrange=[0.,2.*c_r]
        plot,w,c_o/c_s,ytitle='Obj cts / Sky cts',charsize=chsz,pos=[0.15,y1-5*dy,0.9,y1-4*dy],/noerase,xtickname=replicate(' ',20)
        plot,w,p_A,ytitle='Flux [ph cm^-2 s^-1 Å^-1]',charsize=chsz,xtitle='Wavelength (Å)',pos=[0.15,y1-6*dy,0.9,y1-5*dy],/noerase
	ps_end
	endif
	return
	end
