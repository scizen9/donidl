;+
;NAME
; read_megacamthroughput
;PURPOSE
; reads megacam throughput
;USAGE
; read_megacamthroughput,wave,response
;OPTIONAL INPUTS
; filterairmass    Airmass at which to apply atmosphere. Ignored if
;                   noatm set.  The default value is 1.2
;OUTPUTS
; wave        Wavelength vector
; response    Response vector
;KEYWORDS
; noatm       Don't include atmospheric transmission
;COMMON BLOCKS
; none
;AUTHOR
; Mark Sullivan
;-
PRO read_megacamthroughput,megacam_response_wave,megacam_response,$
  NOATM=noatm,FILTERAIRMASS=filterairmass

basedir = !PHOT_DATA

; primary mirror
filename=basedir+'/CFHT_Primary_Transmission.txt'
readcol,filename,wave_reflectivity,reflectivity,/SILENT
wave_reflectivity=wave_reflectivity*10.d0
maxreflectivity=max(reflectivity)
reflectivity=reflectivity/maxreflectivity

; optics
filename=basedir+'/CFHT_MegaPrime_Transmission.txt'
readcol,filename,wave_transmission,transmission,/SILENT
wave_transmission=wave_transmission*10.d0 ;;Convert to Angstroms
maxtransmission=max(transmission)
transmission=transmission/maxtransmission

; CCD QE
filename=basedir+'/megacam_average_qe.txt'
readcol,filename,wave_qe,qe,/SILENT
wave_qe=wave_qe*10.d0  ;;Convert to Angstroms
maxqe=max(qe)
qe=qe/maxqe
wave_qe=[wave_qe[0]-100.,wave_qe,wave_qe[N_ELEMENTS(wave_qe)-1]+100.]
qe=[0.,qe,0.]

megacam_response_wave=[wave_reflectivity,wave_transmission,wave_qe]

;;Build the list of unique wavelengths in sorted order
sort_index=SORT(megacam_response_wave)
megacam_response_wave=megacam_response_wave[sort_index]
unique_index=REM_DUP(megacam_response_wave)
megacam_response_wave=megacam_response_wave[unique_index]

megacam_response=DBLARR(N_ELEMENTS(megacam_response_wave))
allwave_reflectivity=DBLARR(N_ELEMENTS(megacam_response_wave))
allwave_transmission=DBLARR(N_ELEMENTS(megacam_response_wave))
allwave_qe=DBLARR(N_ELEMENTS(megacam_response_wave))

;;Linearly interpolate all responses onto this wavelength grid
LINTERP,wave_reflectivity,reflectivity,megacam_response_wave,$
  allwave_reflectivity
LINTERP,wave_transmission,transmission,megacam_response_wave,$
  allwave_transmission
LINTERP,wave_qe,qe,megacam_response_wave,allwave_qe

;;Combine mirror,optics,qe
megacam_response=allwave_reflectivity * allwave_transmission * $
                 allwave_qe

IF ~ KEYWORD_SET( noatm ) THEN BEGIN
   IF(N_ELEMENTS(filterairmass) EQ 0)THEN filterairmass=1.2d0
   IF(filterairmass LT 0.)THEN BEGIN
      PRINT,'WARNING: filterairmass less than zero. Setting to 0.'
      filterairmass=0.
   ENDIF
   ;;Atmospheric transmission. Although the file is called extinction,
   ;;in fact it lists the transmission fraction from the effect of extinction
   ;;This is for airmass 1 I think

   ;;Here we assume the 'Bouger law' -- that extinction by the
   ;; atmosphere is proportional to exp( airmass ).  In magnitude
   ;; space this means that ext = k * X, where X is the airmass
    atmextfile = basedir + '/MaunaKea.Extinction'
    readcol,atmextfile,atm_extinc_wave,atm_extinc,FORMAT='d,d',/SILENT
    atm_extinc=atm_extinc^filterairmass
    
    ;;Apply this to the megacam response after linearly interpolating
    allwave_atm_ext=INTERPOL( atm_extinc,atm_extinc_wave,$
                              megacam_response_wave )
    allwave_atm_ext >= 0.0 ;;Set any negative points to zero
    megacam_response *= allwave_atm_ext

    ;;Atmospheric absorption from H2O+O2 molecules etc. The file 
    ;; lists the LOG(transmission).  Telluric features don't obey
    ;; the 'Bouger law', but suffer from a 'curve-of-growth'.  There
    ;; is no good way to handle this, but a standard approach is
    ;; the so called 'square root law', which is to assume that
    ;; the transmission is roughly proportional to exp( sqrt(airmass) )
    ;; Here we follow Wade & Horne 1988, ApJ 324, 411, and use
    ;; exp( airmass^0.6 )
    ;;Our input file is for airmass 1.
    atmabsfile = basedir + '/spect.atm_abscor.txt'
    readcol,atmabsfile,atm_tell_wave,atm_tell,FORMAT='d,d',/SILENT
    atm_tell=10^(atm_tell) ;;Convert from log10 to absolute transmission
    atm_tell=atm_tell^(filterairmass^0.6)

    ;;This has a complicated shape, so we use spline interpolation
    ;;rather than linear interpolation
    allwave_tell = INTERPOL(atm_tell,atm_tell_wave,megacam_response_wave,$
                            /SPLINE)
    allwave_tell >= 0.0 ;;Set any negative points to zero
    megacam_response *= allwave_tell
ENDIF

;;Renormalize response -- we only care about the shape
maxmegacam_response=MAX(megacam_response)
megacam_response=megacam_response/maxmegacam_response

RETURN
END
