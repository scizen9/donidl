;+
;NAME
; read_vega
;PURPOSE
; Read the VEGA SED (spectral energy distribution)
;USAGE
; vega_struct = read_vega()
;OPTIONAL INPUTS
; vegasedtype    Type of Vega SED to use.  Current options are:
;                 "stis","kim","k93","c95", "lcb", "stisold" and "hayes85".  
;                 The default is stis.
; Vmag           V magnitude of Vega (def: 0.023)
;KEYWORDS
; reread         Force a read from the text/fits file rather than the
;                 restore from an IDL savefile
; fullwave       Do not do a wavelength cut on the part of the
;                 spectrum that is carried forward. The default is to cut the spectrum
;                 from 1000A to 29010A.
; verbose        Verbose output
;NOTES
; The returnes SED for vega is in F_lambda units
;AUTHOR
; Mark Sullivan
;-
FUNCTION read_vega,VERBOSE=verbose,REREAD=reread,$
                   VEGASEDTYPE=vegasedtype,VMAG=vmag,$
                   FULLWAVE=fullwave

; V mag of Vega comes from Bohlin 2004 STIS spectrum
; updated by Bohlin (2006).  However, we don't really
; fully believe in this value, since it uses the Cohen filters
IF(N_ELEMENTS(fullwave) EQ 0)THEN fullwave=0b
IF N_ELEMENTS(vmag) EQ 0 THEN vmag=0.023d0

basedir = !PHOT_DATA

vega_directory = basedir+'/vega/'

;;The absolute SED of Vega is, not surprisingly, not perfectly
;; determined.  There are two issues here: first, the shape
;; of the spectrum.  Second, the absolute normalization of
;; said spectrum.  The first is handled by choosing which of
;; the files below to read in, the second by setting Vmag,
;; which is supposed to represent the V band magnitude of Vega.
;; Of course, since Vega defines the usual system in which
;; B=V=R=I, etc., this is the magnitude in all filters.
;;Right now the updated Bohlin value (0.023 mag) is used in all cases.  This
;; is based on the Cohen et al. 2003 Landolt V bandpass and a STIS spectrum
;;Note that a more standard value is 0.03, but this is not really that
;; well known.

IF ~ KEYWORD_SET( vegasedtype ) THEN vegasedtype = "stis"
vegasedtype = STRLOWCASE( vegasedtype )
fitsfile = 0b

CASE vegasedtype OF
    "kim" : BEGIN
        ;;Vega spectrum from Alex Kim.  Uncertain provenance
        tag = 'From Alex Kim'
        wavfac = 10.0 ;;Wav in nm
        vegafile = vega_directory + 'vega_kim.dat'
    END
    "lcb" : BEGIN
        ;;Vega spectrum Lejeune et al. 1997, supplied with PEGASE.2
        tag = 'From PEAGSE.2 (LCB 1997)'
        wavfac = 1.0 ;;Wav in nm
        vegafile = vega_directory + 'VegaLCB.dat'
    END
    "k93" : BEGIN
        ;;Vega spectrum from k93.  
        ;; k93 is Kurucz 1993 models for an A0V star
        tag = 'Kurucz 1993 models of AOV star'
        wavfac = 1.0
        vegafile = vega_directory + 'vega_k93.dat'
    END
    "c95" : BEGIN
        ;;Vega spectrum from c95.  Unfortunately, what c95 is is unlear.
        ;; It _may_ refer to Casteli & Kurucz, 1994, A&A 281, 817
        tag = 'Casteli & Kurucz, 1994, A&A 281, 817 (probably)'
        wavfac = 1.0
        vegafile = vega_directory + 'vega_c95.dat'
    END
    "stis" : BEGIN
        ;;Vega spectrum from HST STIS observations, updated
        ;; for CTE correction and other changes.  Some discussion
        ;; can be found in astro-ph/0608715 (Bohlin)
        tag = 'Bohlin, 2006, astro-ph/0608715'
        wavfac = 1.0
        vegafile=vega_directory+'alpha_lyr_stis_003.fits'
        fitsfile = 1b
    END
    "stisold" : BEGIN
        ;;Vega spectrum from HST STIS observations:
        ;; Bohlin & Gilliland, 2004, AJ, 127, 3508.
        tag = 'Bohlin & Gilliland, 2004, AJ, 127, 3508 (STIS)'
        wavfac = 1.0
        vegafile=vega_directory+'alpha_lyr_stis_002.fits'
        fitsfile = 1b
    END
    "hayes85" : BEGIN
        ;;Vega SED from Hayes 1985, IAUS 111, p 255
        ;; with his value of f_lambda = 4.65e-9 at 5000 A
        tag = 'Hayes 1985, IAUS 111, p 255'
        wavfac = 1.0 ;;in angstroms
        vegafile = vega_directory+'vega_Hayes85.dat'
    END
    ELSE : BEGIN
        PRINT,"ERROR in read_vega: Unknown vegasedtype: ",vegasedtype
        RETURN,0
    ENDELSE
ENDCASE

;;Decide whether or not to restore from a save file
;;In order to restore, the sav file must exists, reread can't
;; be set, and the save file has to be more recent than the 
;; text file.
savname = vegafile + '.sav'
IF(fullwave)THEN savname=vegafile + '.fullwave.sav'
IF FILE_TEST( savname, /READ, /REGULAR ) AND $
  (~ KEYWORD_SET(reread)) THEN BEGIN
    savinfo = FILE_INFO( savname )
    IF ~ FILE_TEST( vegafile, /READ, /REGULAR ) THEN BEGIN
        PRINT,"ERROR in read_vega: can't find input file: ",vegafile
        RETURN,0
    ENDIF
    txtinfo = FILE_INFO( vegafile )
    ;;Test the time of last modification
    IF savinfo.mtime GE txtinfo.mtime THEN restoresave=1b ELSE BEGIN
        IF KEYWORD_SET( verbose ) THEN $
          PRINT,"Save file is older than text file -- rereading"
        restoresave=0b
    ENDELSE
ENDIF ELSE restoresave=0b

IF restoresave THEN BEGIN
    RESTORE,savname,VERBOSE=verbose 
    vega.vmag = vmag
ENDIF ELSE BEGIN

    IF fitsfile THEN BEGIN
        star = mrdfits( vegafile, 1,/SILENT )
        vega_wave = DOUBLE( star.wavelength )
        vega_flux = DOUBLE( star.flux )
        DELVARX,star
    ENDIF ELSE BEGIN
        READCOL,vegafile,vega_wave,vega_flux,format='d,d',/silent
    ENDELSE

    vega_wave *= wavfac ;;Convert to Angstroms

    IF(~ fullwave) THEN BEGIN
       ;;Only interested between 1000 Angstroms and 29000 Angstroms
       w=WHERE(vega_wave GE 1000.d0 AND vega_wave LE 29010.d0, npoints)
       
       IF (npoints EQ 0) THEN $
          MESSAGE,"ERROR in read_vega() : No points in useful wavelength range"
    ENDIF ELSE BEGIN
       w=WHERE(vega_wave GE 0.d0 AND vega_wave LE 1.e30, npoints)
    ENDELSE
    vega = { wave: DBLARR(npoints), flux: DBLARR(npoints),$
             Vmag: vmag, npoints:0L, sedtype: vegasedtype,$
             minwave: !VALUES.F_NAN, maxwave: !VALUES.F_NAN, $
             tag: tag }
    vega.npoints = npoints
    vega.wave = vega_wave[w]
    vega.flux = vega_flux[w]
    vega.minwave = MIN( vega.wave )
    vega.maxwave = MAX( vega.wave )
    SAVE,vega,FILENAME=savname,VERBOSE=VERBOSE
ENDELSE

IF (KEYWORD_SET(verbose)) THEN PRINT,vega.npoints,$
  ' VEGA SED points read in'

RETURN,vega

END
