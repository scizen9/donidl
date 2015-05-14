FUNCTION GALEXSpectrum::AngularSeparation, ra0, dec0, ra1, dec1

sep = SIN( dec0 )*SIN( dec1 ) + COS( dec0 )*COS( dec1 )*COS( ra1-ra0 )

RETURN, ACOS( sep )

END

PRO GALEXSpectrum::AnnotateImageStrip, band, lambda

IF band EQ 'FUV' THEN offset = self.fuvLambdaOffset $
  ELSE offset = self.nuvLambdaOffset

; Label the wavelength along the top of the image strip.

x = self->WavelengthToOffset( band, 1, lambda ) - offset
y = !Y.CRANGE[1]
FOR i = 0, N_ELEMENTS( x ) - 1 DO $
  XYOUTS, x[i], y, STRING( lambda[i], format='(i4)' ), orientation=90., $
          color=fsc_color( 'Red' )

x = self->WavelengthToOffset( band, 2, lambda ) - offset
y = !Y.CRANGE[1]
FOR i = 0, N_ELEMENTS( x ) - 1 DO $
  XYOUTS, x[i], y, STRING( lambda[i], format='(i4)' ), orientation=90., $
          color=fsc_color( 'Green' )

x = self->WavelengthToOffset( band, 3, lambda ) - offset
y = !Y.CRANGE[1]
FOR i = 0, N_ELEMENTS( x ) - 1 DO $
  XYOUTS, x[i], y, STRING( lambda[i], format='(i4)' ), orientation=90., $
          color=fsc_color( 'Blue' )

y = 0.5*(!Y.CRANGE[0] + !Y.CRANGE[1])
OPLOT, !X.CRANGE, [y, y], linestyle=2, color=fsc_color( 'Purple' )

XYOUTS, 820, -20., STRING( self.tile, self.currentSpecID, format='(%"%s %d")' ), $
        alignment=0., color=fsc_color( 'Purple' )

END

PRO GALEXSpectrum::AsciiFile

fileName = STRING( self.tile, self.currentSpecID, format='(%"%s_%d.txt")' )
OPENW, unit, fileName, /get_lun

spec = (*self.ptrGsp)[self.currentIndex]

lambda = spec.zero + FINDGEN(  spec.numpt + 1 ) * spec.disp

flux = spec.obj * (6.6e-27 * 3.e18 / lambda)
err = spec.objerr * (6.6e-27 * 3.e18 / lambda)

FOR i = 0, N_ELEMENTS( lambda ) - 1 DO PRINTF, unit, lambda[i], flux[i], err[i]

FREE_LUN, unit

END

FUNCTION GALEXSpectrum::ComputeBackground, band

IF band EQ 'FUV' THEN BEGIN

    IF NOT PTR_VALID( self.ptrFuvStrip ) THEN RETURN, 0

    strip = *self.ptrFuvStrip
    
    lambda = 1400. + FINDGEN( 400 )
    offset = self->WavelengthToOffset( 'FUV', 2, lambda ) - self.fuvLambdaOffset

    smoothingLength = 50

ENDIF ELSE BEGIN

    IF NOT PTR_VALID( self.ptrNuvStrip ) THEN RETURN, 0

    strip = *self.ptrNuvStrip

    lambda = 1800. + FINDGEN( 1200 )
    offset = self->WavelengthToOffset( 'NUV', 1, lambda ) - self.nuvLambdaOffset

    smoothingLength = 100

ENDELSE

i = offset[0]
j = offset[N_ELEMENTS( offset ) - 1]

bottom = TOTAL( strip[i:j, 0:27], 2 )
top = TOTAL( strip[i:j, 53:79], 2 )
background = (bottom + top) / 55.
background = INTERPOL( background, i + FINDGEN( j-i+1 ), offset )

spectrum = TOTAL( strip[i:j, 28:52], 2 ) / 25.
spectrum = INTERPOL( spectrum, i + FINDGEN( j-i+1 ), offset )

use = WHERE( FINITE( background ), nUse )
IF nUse GT 25 THEN BEGIN
    sigmaLarge = STDDEV( background, /nan )
    sigmaSmall = STDDEV( background - MEDSMOOTH( background, smoothingLength ), /nan )
ENDIF ELSE BEGIN
    sigmaLarge = -1
    sigmaSmall = -1
ENDELSE

IF band EQ 'FUV' THEN BEGIN
    
    self.ptrFuvBackground = PTR_NEW( background )
    self.ptrFuvSpectrum = PTR_NEW( spectrum )
    self.fuvSigmaLarge = sigmaLarge
    self.fuvSigmaSmall = sigmaSmall

ENDIF ELSE BEGIN

    self.ptrNuvBackground = PTR_NEW( background )
    self.ptrNuvSpectrum = PTR_NEW( spectrum )
    self.nuvSigmaLarge = sigmaLarge
    self.nuvSigmaSmall = sigmaSmall

ENDELSE

RETURN, 1

END

PRO GALEXSpectrum::ComputeSNR, fuvSNR=fuvSNR, nuvSNR=nuvSNR

spec = (*self.ptrGsp)[self.currentIndex]

lambda = spec.zero + FINDGEN(  spec.numpt + 1 ) * spec.disp

flux = spec.obj * (6.6e-27 * 3.e18 / lambda)
err = spec.objerr * (6.6e-27 * 3.e18 / lambda)

; FUV
good = WHERE( 1440 LE lambda AND lambda LE 1660 )
fuvSNR = MEDIAN( flux[good]/err[good] )

; NUV
good = WHERE( 2200 LE lambda AND lambda LE 2600 )
nuvSNR = MEDIAN( flux[good]/err[good] )

END

PRO GALEXSpectrum::FITSFile, hdrStruct

fileName = STRING( self.tile, self.currentSpecID, format='(%"%s_%d.fits")' )

spec = (*self.ptrGsp)[self.currentIndex]

lambda = spec.zero + FINDGEN(  spec.numpt + 1 ) * spec.disp

flux = spec.obj * (6.6e-27 * 3.e18 / lambda)
err = spec.objerr * (6.6e-27 * 3.e18 / lambda)

s = {lambda: lambda, flux: flux, err: err}

IF N_ELEMENTS( hdrStruct ) NE 0 THEN BEGIN
    MKHDR, hdr, ''
    FXADDPAR, hdr, 'OBSDATIM',self.obsdate,' Obs date, time (UT:YYMMDDTHHMMSSZ)'
    FXADDPAR, hdr, 'ECLIPSE',self.eclipse,' Eclipse number'
    FXADDPAR, hdr, 'VISIT',self.visit,' Visit number'
    FXADDPAR, hdr, 'SUBVIS',self.subvis,' Subvisit number'
    FXADDPAR, hdr, 'EXPOSURE',self.exposure, ' Exposure (s)'

    names = TAG_NAMES( hdrStruct )
    FOR i = 0, N_ELEMENTS( names ) - 1 DO BEGIN
        val = hdrStruct.(i)
        IF SIZE( val, /type ) EQ 1 THEN val = FIX( val )
        FXADDPAR, hdr, names[i], val
    ENDFOR

    MWRFITS, undefined, fileName, hdr, /create
ENDIF

IF FILE_TEST( fileName ) THEN MWRFITS, s, fileName ELSE $
  MWRFITS, s, fileName, /create

END

FUNCTION GALEXSpectrum::GetStampData, band, width

IF band EQ 'FUV' THEN BEGIN
    IF NOT PTR_VALID( self.ptrFuvHdr ) THEN RETURN, 0
    img = *self.ptrFuvImg
    hdr = *self.ptrFuvHdr
ENDIF ELSE BEGIN
    IF NOT PTR_VALID( self.ptrNuvHdr ) THEN RETURN, 0
    img = *self.ptrNuvImg
    hdr = *self.ptrNuvHdr
ENDELSE

sz = SIZE( img )

ra = (*self.ptrGsp)[self.currentIndex].alpha_j2000
dec = (*self.ptrGsp)[self.currentIndex].delta_j2000

raMin = ra - width / COS( dec*!DTOR ) / 2.
raMax = ra + width / COS( dec*!DTOR ) / 2.

decMin = dec - width / 2.
decMax = dec + width / 2.

ADXY, hdr, [raMin, raMax], [decMin, decMax], x, y
x[1] = (x[1] > 0)
x[0] = (x[0] < (sz[1]-1))
y[0] = (y[0] > 0)
y[1] = (y[1] < (sz[2]-1))

stamp = img[x[1]:x[0], y[0]:y[1]]
min = MIN( stamp, max=max )

IF band EQ 'FUV' THEN BEGIN
    self.ptrFuvStamp = PTR_NEW( stamp, /no_copy )
    self.fuvStampMin = min
    self.fuvStampMax = max
    self.fuvStampHistEqual = 0B
ENDIF ELSE BEGIN
    self.ptrNuvStamp = PTR_NEW( stamp, /no_copy )
    self.nuvStampMin = min
    self.nuvStampMax = max
    self.nuvStampHistEqual = 0B
ENDELSE

RETURN, 1

END

FUNCTION GALEXSpectrum::GetStripData, band

IF band EQ 'FUV' THEN BEGIN
    ptrGsax = self.ptrFuvGsax
    unit = self.fuvPrcUnit
    offset = self.fuvOffset
    skip = self.fuvSkip
ENDIF ELSE BEGIN
    ptrGsax = self.ptrNuvGsax
    unit = self.nuvPrcUnit
    offset = self.nuvOffset
    skip = self.nuvSkip
ENDELSE

IF NOT PTR_VALID( ptrGsax ) THEN RETURN, 0
IF unit EQ 0 THEN RETURN, 0

i = WHERE( (*ptrGsax).id EQ self.currentSpecID, n )
IF n NE 1 THEN BEGIN
    MESSAGE, 'Unable to find requested spectrum in the GSAX file.', /informational
    RETURN, 0
ENDIF

loc = offset + ((*ptrGsax)[i].img_index - 1) * skip
POINT_LUN, unit, loc

dataStruct = MRDFITS( unit, 0, hdr, /silent )
nColumns = FXPAR( hdr, 'PRI_NC' )
nRows = FXPAR( hdr, 'PRI_NR' )
dataZero = FXPAR( hdr, 'DATZERO' )
dataScale = FXPAR( hdr, 'DATSCALE' )
rspZero = FXPAR( hdr, 'RSPZERO' )
rspScale = FXPAR( hdr, 'RSPSCALE' )
lambdaOffset = FXPAR( hdr, 'ARCSEC1' )

img = (dataStruct.dat * dataScale + dataZero) / (dataStruct.rsp * rspScale + rspZero)
img = REFORM( img, nColumns, nRows )

min = MIN( img, max=max, /nan )

IF band EQ 'FUV' THEN BEGIN
    self.ptrFuvStrip = PTR_NEW( img, /no_copy )
    self.fuvLambdaOffset = lambdaOffset
    self.fuvStripMin = min
    self.fuvStripMax = max
    self.fuvStripHistEqual = 0B
ENDIF ELSE BEGIN
    self.ptrNuvStrip = PTR_NEW( img, /no_copy )
    self.nuvLambdaOffset = lambdaOffset
    self.nuvStripMin = min
    self.nuvStripMax = max
    self.nuvStripHistEqual = 0B
ENDELSE

RETURN, 1

END

FUNCTION GALEXSpectrum::GSViewerTable, fuvLimit=fuvLimit, nuvLimit=nuvLimit, $
  radiusLimit=radiusLimit

IF N_ELEMENTS( radiusLimit ) EQ 0 THEN radiusLimit = 1.5

gsp = *self.ptrGsp

struct = { id: 0L, $
           ra: 0.d, $
           dec: 0.d, $
           fuv: !VALUES.F_NAN, $
           nuv: !VALUES.F_NAN, $
           color: !VALUES.F_NAN, $
           fuv_quality: -1, $
           nuv_quality: -1, $
           redshift: !VALUES.F_NAN, $
           fuv_snr: !VALUES.F_NAN, $
           nuv_snr: !VALUES.F_NAN, $
           fuv_fwhm: !VALUES.F_NAN, $
           nuv_fwhm: !VALUES.F_NAN, $
           radius: !VALUES.F_NAN, $
           GGOID_1: 0UL, $
           GGOID_2: 0UL, $
           Notable: 0B }

; Convert limits from magnitude limits to flux limits.
IF N_ELEMENTS( fuvLimit ) EQ 1 THEN fuvLimit = 10^(0.4*(18.82 - fuvLimit)) $
    ELSE fuvLimit = -1000.
IF N_ELEMENTS( nuvLimit ) EQ 1 THEN nuvLimit = 10^(0.4*(20.08 - nuvLimit)) $
    ELSE nuvLimit = -1000.

; Compute radius from center of field.
radius = self->AngularSeparation( gsp.alpha_j2000 * !DTOR, gsp.delta_j2000 * !DTOR, $
                                  self.ra0 * !DTOR, self.dec0 * !DTOR ) * !RADEG

keep = WHERE( fuvLimit LE gsp.fuv AND nuvLimit LE gsp.nuv AND $
              radius LE radiusLimit, nKeep )
IF nKeep LT 1 THEN BEGIN
    MESSAGE, 'Your magnitude and/or radius limits have cut out all the objects.', /info
    RETURN, -1
ENDIF

gsp = gsp[keep]

struct = REPLICATE( struct, nKeep )

struct.id = gsp.id
struct.ra = gsp.alpha_j2000
struct.dec = gsp.delta_j2000
struct.fuv_snr = gsp.median_s2n[1]
struct.nuv_snr = gsp.median_s2n[0]
struct.radius = radius[keep]
struct.ggoid_1 = gsp.ggoid_d[0]
struct.ggoid_2 = gsp.ggoid_d[1]

good = WHERE( gsp.fuv NE -99., nGood )
IF nGood GT 0 THEN struct[good].fuv = -2.5*ALOG10( gsp[good].fuv ) + 18.82

good = WHERE( gsp.nuv NE -99., nGood )
IF nGood GT 0 THEN struct[good].nuv = -2.5*ALOG10( gsp[good].nuv ) + 20.08

struct.color = struct.fuv - struct.nuv

IF PTR_VALID( self.ptrFuvGsax ) AND PTR_VALID( self.ptrNuvGsax ) THEN BEGIN

    fuvGsax = *self.ptrFuvGsax
    nuvGsax = *self.ptrNuvGsax

    FOR i = 0, N_ELEMENTS( struct ) - 1 DO BEGIN

        id = WHERE( struct[i].id EQ nuvGsax.id, count )
        IF count EQ 1 THEN BEGIN
            struct[i].fuv_fwhm = (fuvGsax[id].fwhm GT 0.) ? fuvGsax[id].fwhm : !VALUES.F_NAN
            struct[i].nuv_fwhm = (nuvGsax[id].fwhm GT 0.) ? nuvGsax[id].fwhm : !VALUES.F_NAN
        ENDIF

    ENDFOR

ENDIF

RETURN, struct

END

PRO GALEXSpectrum::NewSpectrum, currentIndex=currentIndex, currentSpecID=currentSpecID

PTR_FREE, self.ptrFuvStrip, self.ptrNuvStrip, self.ptrFuvStamp, self.ptrNuvStamp
PTR_FREE, self.ptrFuvBackground, self.ptrFuvSpectrum
PTR_FREE, self.ptrNuvBackground, self.ptrNuvSpectrum

IF N_ELEMENTS( currentIndex ) NE 0 THEN BEGIN
    currentIndex = currentIndex > 0
    currentIndex = currentIndex < (N_ELEMENTS( *self.ptrGsp ) - 1)
    self.currentIndex = currentIndex
    self.currentSpecID = (*self.ptrGsp)[self.currentIndex].id
ENDIF

IF N_ELEMENTS( currentSpecID ) NE 0 THEN BEGIN
    index = WHERE( (*self.ptrGsp).id EQ currentSpecID, n )
    IF n EQ 1 THEN BEGIN
        self.currentSpecID = currentSpecId
        self.currentIndex = index
    ENDIF ELSE MESSAGE, 'Oops.  Unable to find requested specID in GSP file.', /informational
ENDIF

x = self->GetStripData( 'FUV' )
x = self->GetStripData( 'NUV' )

x = self->ComputeBackground( 'FUV' )
x = self->ComputeBackground( 'NUV' )

END

PRO GALEXSpectrum::NextSpectrum

self->NewSpectrum, currentIndex=(self.currentIndex + 1)

END

FUNCTION GALEXSpectrum::OpenPrcFile, file, offset=offset, skip=skip

IF NOT FILE_TEST( file ) THEN RETURN, -1

unit = FXPOSIT( file, 1, /readonly, /silent)
POINT_LUN, -unit, offset

unit = FXPOSIT( file, 2, /readonly, /silent)
POINT_LUN, -unit, loc
skip = loc - offset

RETURN, unit

END

PRO GALEXSpectrum::PlotBackground, noerase=noerase, position=position

IF NOT PTR_VALID( self.ptrFuvBackground ) THEN x = self->ComputeBackground( 'FUV' )
IF NOT PTR_VALID( self.ptrNuvBackground ) THEN x = self->ComputeBackground( 'NUV' )

IF N_ELEMENTS( position ) EQ 0 THEN position=[0.1, 0.1, 0.95, 0.95]

IF !D.NAME EQ 'PS' THEN thick = 2. ELSE thick = 1.

height = position[3] - position[1]

positionNuv = [position[0], position[1] + 0.55*height, position[2], position[3]]
positionFuv = [position[0], position[1], position[2], position[1] + 0.45*height]

; NUV
IF PTR_VALID( self.ptrNuvBackground ) AND PTR_VALID( self.ptrNuvSpectrum ) THEN BEGIN
    lambda = 1800 + FINDGEN( 1200 )
    background = *self.ptrNuvBackground
    spectrum = *self.ptrNuvSpectrum
    yMin = MIN( [background, spectrum], max=yMax )

    PLOT, lambda, spectrum, psym=10, xstyle=1, yrange=[0.95*yMin, 1.05*yMax], ystyle=1, $
          xtitle='Wavelength (' + STRING( 197B ) + ')', ytitle='Flux (counts/sec)', $
          xmargin=[15, 3], position=positionNuv, noerase=KEYWORD_SET( noerase ), thick=thick, $
          xthick=thick, ythick=thick
    OPLOT, lambda, background, psym=10, color=fsc_color( 'Red' ), thick=thick
    OPLOT, lambda, REPLICATE( MEDIAN( background ), N_ELEMENTS( background ) ), $
           color=fsc_color( 'Purple' ), linestyle=2, thick=thick

; cts = (*self.ptrGsp)[self.currentIndex].nuv
; PRINT, cts, N_ELEMENTS( lambda )
; IF cts NE -99. THEN $
;   OPLOT, !X.CRANGE, [cts, cts], color=fsc_color( 'Blue' ), linestyle=1, thick=thick
; cts = (*self.ptrGsp)[self.currentIndex].nuv1s / 3.5 + $
;       (*self.ptrGsp)[self.currentIndex].background[0]
; OPLOT, !X.CRANGE, [cts, cts], color=fsc_color( 'Blue' ), linestyle=1, thick=thick

    XYOUTS, 1850., 0.95*yMin + 0.8*(1.05*yMax-0.95*yMin), 'NUV', color=fsc_color( 'Red' )
    XYOUTS, 2850,  0.95*yMin + 0.8*(1.05*yMax-0.95*yMin), $
            STRING( self.nuvSigmaLarge / self.nuvSigmaSmall, format='(%"%8.2f")' ), $
            color=fsc_color( 'Purple' )
ENDIF

; FUV
IF PTR_VALID( self.ptrFuvBackground ) AND PTR_VALID( self.ptrFuvSpectrum ) THEN BEGIN
    lambda = 1400 + FINDGEN( 400 )
    background = *self.ptrFuvBackground
    spectrum = *self.ptrFuvSpectrum
    yMin = MIN( [background, spectrum], max=yMax )

    PLOT, lambda, spectrum, psym=10, xstyle=1, yrange=[0.95*yMin, 1.05*yMax], ystyle=1, $
          xtitle='Wavelength (' + STRING( 197B ) + ')', ytitle='Flux (counts/sec)', $
          xmargin=[15, 3], position=positionFuv, /noerase, thick=thick, $
          xthick=thick, ythick=thick
    OPLOT, lambda, background, psym=10, color=fsc_color( 'Red' )
    OPLOT, lambda, REPLICATE( MEDIAN( background ), N_ELEMENTS( background ) ), $
           color=fsc_color( 'Purple' ), linestyle=2, thick=thick

; cts = (*self.ptrGsp)[self.currentIndex].fuv2s / 3.5
; OPLOT, !X.CRANGE, [cts, cts], color=fsc_color( 'Blue' ), linestyle=1, thick=thick

    XYOUTS, 1420., 0.95*yMin + 0.8*(1.05*yMax-0.95*yMin), 'FUV', color=fsc_color( 'Blue' )
    XYOUTS, 1750,  0.95*yMin + 0.8*(1.05*yMax-0.95*yMin), $
            STRING( self.fuvSigmaLarge / self.fuvSigmaSmall, format='(%"%8.2f")' ), $
            color=fsc_color( 'Purple' )
ENDIF

END

PRO GALEXSpectrum::PlotImage, band, width, histEqual=histEqual, limits=limits, $
  noerase=noerase, position=position, scalebar=scalebar

IF NOT KEYWORD_SET( noerase ) THEN ERASE

IF NOT PTR_VALID( self.ptrFuvStamp ) THEN x = self->GetStampData( 'FUV', width )
IF NOT PTR_VALID( self.ptrNuvStamp ) THEN x = self->GetStampData( 'NUV', width )

IF band EQ 'FUV' THEN BEGIN
    IF NOT PTR_VALID( self.ptrFuvStamp ) THEN RETURN
    stamp = *self.ptrFuvStamp
    min = self.fuvStampMin
    max = self.fuvStampMax
    self.fuvStampHistEqual = KEYWORD_SET( histEqual )
ENDIF ELSE BEGIN
    IF NOT PTR_VALID( self.ptrNuvStamp ) THEN RETURN
    stamp = *self.ptrNuvStamp
    min = self.nuvStampMin
    max = self.nuvStampMax
    self.nuvStampHistEqual = KEYWORD_SET( histEqual )
ENDELSE

IF KEYWORD_SET( histEqual ) THEN BEGIN

    stamp = HIST_EQUAL( stamp )

ENDIF ELSE BEGIN

    IF N_ELEMENTS( limits ) EQ 2 THEN BEGIN
        m0 = limits[0]*(max - min) + min
        m1 = limits[1]*(max - min) + min
    ENDIF ELSE BEGIN
        m0 = min
        m1 = max
    ENDELSE

    IF m0 LT m1 THEN BEGIN
        stamp = BYTSCL( stamp, min=m0, max=m1 )
    ENDIF ELSE BEGIN
        stamp = BYTSCL( stamp, min=m1, max=m0 )
        stamp = 255B - stamp
    ENDELSE

ENDELSE

TVIMAGE, stamp, position=position, /keep_aspect_ratio, /nointerpolation

IF band EQ 'FUV' THEN BEGIN
    XYOUTS, position[0] + 0.1*(position[2]-position[0]), $
            position[1] + 0.8*(position[3]-position[1]), $
            'FUV', /normal, color=fsc_color( 'Blue' ), charsize=2., charthick=2.
ENDIF ELSE BEGIN
    XYOUTS, position[0] + 0.1*(position[2]-position[0]), $
            position[1] + 0.8*(position[3]-position[1]), $
            'NUV', /normal, color=fsc_color( 'Red' ), charsize=2., charthick=2.
ENDELSE

IF KEYWORD_SET( scalebar ) THEN BEGIN
    sz = SIZE( stamp )
    arcsecPerNorm = 1.5 * sz[1] / (position[2] - position[0])
    x0 = position[0] + 0.1 * (position[2]-position[0])
    x1 = x0 + scalebar / arcsecPerNorm
    y0 = position[1] + 0.1 * (position[3]-position[1])
    y1 = position[1] + 0.04 * (position[3]-position[1])
    PLOTS, [x0, x1], [y0, y0], /normal, thick=2., color=fsc_color( 'Red' )
    XYOUTS, 0.5 * (x0+x1), y1, STRING( scalebar, format='(%"%4.1f\"")' ), alignment=0.5, $
            /normal, color=fsc_color( 'Red' )
ENDIF

END

PRO GALEXSpectrum::PlotImageStrip, band, annotate=annotate, histEqual=histEqual, $
  limits=limits, noerase=noerase, position=position

IF band EQ 'FUV' THEN BEGIN
    IF NOT PTR_VALID( self.ptrFuvStrip ) THEN x = self->GetStripData( 'FUV' )
    IF NOT PTR_VALID( self.ptrFuvStrip ) THEN RETURN
    strip = *self.ptrFuvStrip
    lambda = 1300. + FINDGEN( 8 )*100.
    min = self.fuvStripMin
    max = self.fuvStripMax
    self.fuvStripHistEqual = KEYWORD_SET( histEqual )
ENDIF ELSE BEGIN
    IF NOT PTR_VALID( self.ptrNuvStrip ) THEN x = self->GetStripData( 'NUV' )
    IF NOT PTR_VALID( self.ptrNuvStrip ) THEN RETURN
    strip = *self.ptrNuvStrip
    lambda = 1700. + FINDGEN( 14 )*100.
    min = self.nuvStripMin
    max = self.nuvStripMax
    self.nuvStripHistEqual = KEYWORD_SET( histEqual )
ENDELSE

IF min EQ max THEN RETURN

sz = SIZE( strip )

PLOT, FINDGEN( sz[1] ), FINDGEN( sz[2] ), /nodata, xstyle=1, ystyle=1, /isotropic, $
      position=position, noerase=noerase

good = WHERE( FINITE( strip[*, 0:sz[2]-2] ), nGood )
IF nGood LT 100 THEN RETURN

IF KEYWORD_SET( histEqual ) THEN BEGIN

    strip = HIST_EQUAL( strip )

ENDIF ELSE BEGIN

    IF N_ELEMENTS( limits ) EQ 2 THEN BEGIN
        m0 = limits[0]*(max - min) + min
        m1 = limits[1]*(max - min) + min
    ENDIF ELSE BEGIN
        m0 = min
        m1 = max
    ENDELSE

    IF m0 LT m1 THEN BEGIN
        strip = BYTSCL( strip, min=m0, max=m1 )
    ENDIF ELSE BEGIN
        strip = BYTSCL( strip, min=m1, max=m0 )
        strip = 255B - strip
    ENDELSE

ENDELSE

TVIMAGE, strip, /overplot, /nointerpolation

IF KEYWORD_SET( annotate ) THEN BEGIN
    PLOT, FINDGEN( sz[1] ), FINDGEN( sz[2] ), /nodata, /noerase, xstyle=1, ystyle=1, $
          /isotropic, position=position, xtitle='Spectral Direction (arcsec)', $
          ytitle='Spatial Direction (arcsec)'
    self->AnnotateImageStrip, band, lambda
ENDIF

END

PRO GALEXSpectrum::PlotProfile, noerase=noerase, position=position

IF N_ELEMENTS( position ) EQ 0 THEN position = [0.1, 0.1, 0.95, 0.95]

IF !D.NAME EQ 'PS' THEN thick = 2. ELSE thick = 1.

width = position[2] - position[0]
positionFuv = [position[0], position[1], position[0] + 0.42*width, position[3]]
positionNuv = [position[0] + 0.58*width, position[1], position[2], position[3]]

; NUV
IF PTR_VALID( self.ptrNuvStrip ) THEN BEGIN
    strip = *self.ptrNuvStrip

    sz = SIZE( strip )
    p0 = MEDIAN( strip[150:233, 0:sz[2]-2], dimension=1 )
    p1 = MEDIAN( strip[234:317, 0:sz[2]-2], dimension=1 )
    p2 = MEDIAN( strip[318:399, 0:sz[2]-2], dimension=1 )
    yMin = MIN( [p0, p1, p2], max=yMax )

    PLOT, p0, psym=10, yrange=[0.95*yMin, 1.05*yMax], ystyle=1, $
          xtitle='Spatial Direction (arcsec)', noerase=noerase, position=positionNuv, $
          thick=thick, xthick=thick, ythick=thick
    OPLOT, p0, psym=10, color=fsc_color( 'Blue' ), thick=thick
    OPLOT, p1, psym=10, color=fsc_color( 'Green' ), thick=thick
    OPLOT, p2, psym=10, color=fsc_color( 'Red' ), thick=thick

    XYOUTS, 10, !Y.CRANGE[0] + 0.9*(!Y.CRANGE[1]-!Y.CRANGE[0]), 'NUV', color=fsc_color( 'Red' )
    y = !Y.CRANGE[0] + 0.05*(!Y.CRANGE[1]-!Y.CRANGE[0])
    PLOTS, [0, 27], [y, y], color=fsc_color( 'Purple' ), thick=thick
    PLOTS, [53, 79], [y, y], color=fsc_color( 'Purple' ), thick=thick
ENDIF

; FUV
IF PTR_VALID( self.ptrFuvStrip ) THEN BEGIN
    strip = *self.ptrFuvStrip

    sz = SIZE( strip )
    p0 = MEDIAN( strip[200:299, 0:sz[2]-2], dimension=1 )
    p1 = MEDIAN( strip[300:399, 0:sz[2]-2], dimension=1 )
    p2 = MEDIAN( strip[400:499, 0:sz[2]-2], dimension=1 )
    yMin = MIN( [p0, p1, p2], max=yMax )

    PLOT, p0, psym=10, yrange=[0.95*yMin, 1.05*yMax], ystyle=1, $
          xtitle='Spatial Direction (arcsec)', /noerase, position=positionFuv, $
          thick=thick, xthick=thick, ythick=thick
    OPLOT, p0, psym=10, color=fsc_color( 'Blue' ), thick=thick
    OPLOT, p1, psym=10, color=fsc_color( 'Green' ), thick=thick
    OPLOT, p2, psym=10, color=fsc_color( 'Red' ), thick=thick

    XYOUTS, 10, !Y.CRANGE[0] + 0.9*(!Y.CRANGE[1]-!Y.CRANGE[0]), 'FUV', $
            color=fsc_color( 'Blue' )
    y = !Y.CRANGE[0] + 0.05*(!Y.CRANGE[1]-!Y.CRANGE[0])
    PLOTS, [0, 27], [y, y], color=fsc_color( 'Purple' ), thick=thick
    PLOTS, [53, 79], [y, y], color=fsc_color( 'Purple' ), thick=thick
ENDIF


!P.MULTI = 0

END

PRO GALEXSpectrum::PlotSpectrum, autoScale=autoScale, fnu=fnu, limits=limits, $
  noerase=noerase, optimal=optimal, position=position, subtitle=subtitle, smooth=smooth, $
  title=title

spec = (*self.ptrGsp)[self.currentIndex]

lambda = spec.zero + FINDGEN(  spec.numpt + 1 ) * spec.disp

IF KEYWORD_SET( optimal ) THEN BEGIN
    flux = spec.opx
    err = spec.opxerr
ENDIF ELSE BEGIN
    flux = spec.obj
    err = spec.opxerr
ENDELSE

flux = flux * (6.6e-27 * 3.e18 / lambda)
err = err * (6.6e-27 * 3.e18 / lambda)

xtitle = 'Wavelength (' + STRING( 197B ) + ')'
ytitle = 'Flux Density (ergs cm!U-2!N s!U-1!N ' + STRING( 197B ) + '!U-1!N)'

IF !D.NAME EQ 'PS' THEN thick = 2. ELSE thick = 1.

IF KEYWORD_SET( fnu ) THEN BEGIN
    factor = lambda^2 / 3.e18
    flux *= factor
    err *= factor
    ytitle = 'Flux Density (ergs cm!U-2!N s!U-1!N Hz!U-1!N)'
ENDIF

IF N_ELEMENTS( smooth ) NE 0 THEN BEGIN
    IF smooth GT 1 THEN flux = SMOOTH( flux, smooth, /edge_truncate )
ENDIF

IF KEYWORD_SET( autoScale ) THEN BEGIN

    x0 = MIN( lambda, max=x1 )

    good = WHERE( (1450. LE lambda AND lambda LE 1700.) OR $
                  (2000. LE lambda AND lambda LE 2800.), n )

    med = MEDIAN( flux[good] )
    sigma = STDDEV( flux[good], /double )

    y0 = (med - 3.*sigma) < 0.
    y1 = med + 7.*sigma

    limits = [x0, y0, x1, y1]

ENDIF ELSE IF N_ELEMENTS( limits ) NE 4 THEN BEGIN

    x0 = MIN( lambda, max=x1 )
    y0 = MIN( flux, max=y1 )
    limits = [x0, y0, x1, y1]

ENDIF

PLOT, lambda, flux, psym=10, noerase=noerase, $
      xrange=[limits[0], limits[2]], xstyle=1, xmargin=[15, 3], xtitle=xtitle, $
      yrange=[limits[1], limits[3]], ystyle=1, ymargin=[4, 4], ytitle=ytitle, $
      position=position, subtitle=subtitle, thick=thick, title=title, $
      xthick=thick, ythick=thick
OPLOT, lambda, err, psym=10, color=fsc_color( 'Red' ), thick=thick
OPLOT, !X.CRANGE, [0., 0.], linestyle=2, color=fsc_color( 'Purple' ), thick=thick

END

PRO GALEXSpectrum::PlotSummary, infoString=infoString, verString=verString

IF N_ELEMENTS( infoString ) EQ 0 THEN infoString=''
IF N_ELEMENTS( verString ) EQ 0 THEN verString=''

ERASE

XYOUTS, 0.02, 0.05, verString, orientation=90., /normal
XYOUTS, 0.1, 0.97, infoString, alignment=0., /normal
XYOUTS, 0.52,0.94, STRING( self.obsdate, self.eclipse, self.visit, self.subvis,$
	self.exposure, format='(%"%s  Ecl:%d  Vis:%d  Svs:%d  Exp:%10.1f")' ), $
	/normal

self->PlotSpectrum, /autoscale, /noerase, position=[0.1, 0.7, 0.5, 0.95]

LOADCT, 0, /silent
self->PlotImage, 'FUV', 60./3600., /noerase, position=[0.55, 0.7, 0.75, 0.9], scalebar=10.

LOADCT, 0, /silent
self->PlotImage, 'NUV', 60./3600., /noerase, position=[0.75, 0.7, 0.95, 0.9], scalebar=10.

self->PlotBackground, /noerase, position=[0.1, 0.45, 0.5, 0.65]

self->PlotProfile, /noerase, position=[0.57, 0.45, 0.97, 0.65]

LOADCT, 0, /silent
self->PlotImageStrip, 'NUV', /noerase, position=[0.1, 0.25, 0.95, 0.45], /annotate, /histEqual

LOADCT, 0, /silent
self->PlotImageStrip, 'FUV', /noerase, position=[0.1, 0.05, 0.95, 0.25], /annotate, /histEqual

END

PRO GALEXSpectrum::PreviousSpectrum

self->NewSpectrum, currentIndex=(self.currentIndex - 1)

END

FUNCTION GALEXSpectrum::WavelengthToOffset, band, order, lambda

IF band EQ 'FUV' THEN BEGIN
    CASE order OF
        1: coeff = [-4.288e3, 6.719, -3.643e-3, 6.897e-7]
        2: coeff = [-4.531e3, 7.386, -3.925e-3, 7.475e-7]
        3: coeff = [-4.466e3, 7.484, -3.863e-3, 7.356e-7]
        4: coeff = [-4.373e3, 7.464, -3.681e-3, 6.890e-7]
        ELSE: coeff=[0., 0., 0., 0.]
    ENDCASE
ENDIF ELSE BEGIN
    CASE order OF
        1: coeff = [-8.821e2, 7.936e-1, -2.038e-4, 2.456e-8]
        2: coeff = [-8.142e2, 9.161e-1, -1.638e-4, 1.867e-8]
        3: coeff = [-9.182e2, 1.279, -2.330e-4, 2.859e-8]
        ELSE: coeff=[0., 0., 0., 0.]
    ENDCASE
ENDELSE

RETURN, coeff[0] + coeff[1]*lambda + coeff[2]*lambda^2 + coeff[3]*lambda^3

END

; Below here are the standard required IDL object routines.

PRO GALEXSpectrum::CleanUp

PTR_FREE, self.ptrGsp, self.ptrFuvGsax, self.ptrNuvGsax, self.ptrFuvStrip, self.ptrNuvStrip
PTR_FREE, self.ptrFuvImg, self.ptrFuvHdr, self.ptrNuvImg, self.ptrNuvHdr
PTR_FREE, self.ptrFuvStamp, self.ptrNuvStamp
PTR_FREE, self.ptrFuvBackground, self.ptrFuvSpectrum
PTR_FREE, self.ptrNuvBackground, self.ptrNuvSpectrum

IF self.fuvPrcUnit GT 0 THEN FREE_LUN, self.fuvPrcUnit
IF self.nuvPrcUnit GT 0 THEN FREE_LUN, self.nuvPrcUnit

END

PRO GALEXSpectrum::GetProperty, currentIndex=currentIndex, currentSpecID=currentSpecID, $
  fuvImageStrip=fuvImageStrip, fuvSigmaSmall=fuvSigmaSmall, fuvSigmaLarge=fuvSigmaLarge, $
  nuvImageStrip=nuvImageStrip, nuvSigmaSmall=nuvSigmaSmall, nuvSigmaLarge=nuvSigmaLarge, $
  ra0=ra0, dec0=dec0, tile=tile, obsdate=obsdate, eclipse=eclipse, visit=visit, $
  subvis=subvis, exposure=exposure

IF ARG_PRESENT( currentIndex ) THEN currentIndex = self.currentIndex

IF ARG_PRESENT( currentSpecID ) THEN currentSpecID = self.currentSpecID

IF ARG_PRESENT( fuvImageStrip ) THEN fuvImageStrip = *self.ptrFuvStrip

IF ARG_PRESENT( fuvSigmaLarge ) THEN fuvSigmaLarge = self.fuvSigmaLarge

IF ARG_PRESENT( fuvSigmaSmall ) THEN fuvSigmaSmall = self.fuvSigmaSmall

IF ARG_PRESENT( nuvImageStrip ) THEN nuvImageStrip = *self.ptrNuvStrip

IF ARG_PRESENT( nuvSigmaLarge ) THEN nuvSigmaLarge = self.nuvSigmaLarge

IF ARG_PRESENT( nuvSigmaSmall ) THEN nuvSigmaSmall = self.nuvSigmaSmall

IF ARG_PRESENT( ra0 ) THEN ra0 = self.ra0

IF ARG_PRESENT( dec0 ) THEN dec0 = self.dec0

IF ARG_PRESENT( tile ) THEN tile = self.tile

IF ARG_PRESENT( obsdate ) THEN obsdate = self.obsdate

IF ARG_PRESENT( eclipse ) THEN eclipse = self.eclipse

IF ARG_PRESENT( visit ) THEN visit = self.visit

IF ARG_PRESENT( subvis ) THEN subvis = self.subvis

IF ARG_PRESENT( exposure ) THEN exposure = self.exposure

END


PRO GALEXSpectrum::SetProperty, currentIndex=currentIndex, currentSpecID=currentSpecID

IF N_ELEMENTS( currentIndex ) NE 0 THEN self->NewSpectrum, currentIndex=currentIndex

IF N_ELEMENTS( currentSpecID ) NE 0 THEN self->NewSpectrum, currentSpecID=currentSpecID

END

FUNCTION GALEXSpectrum::Init, grismPath, imagePath, noStamp=noStamp, nuvOnly=nuvOnly, $
  visit=visit

self.grismPath = grismPath
self.imagePath = imagePath

; Get the 1D spectra.
gspFile = FILE_SEARCH( self.grismPath + '*-xg-gsp.fit*', count=n )
IF n NE 1 THEN BEGIN
    PRINT, self.grismPath, format='(%"The grism path is %s")'
    MESSAGE, 'Oops.  Expected 1 GSP file, found: ' + strtrim(string(n),2), $
	    /informational
    RETURN, 0
ENDIF

hdr = HEADFITS( gspFile[0] )

self.tile = STRTRIM( FXPAR( hdr, 'TILENAME' ), 2 )
self.visit = FXPAR( hdr, 'VISIT' )
self.subvis = FXPAR( hdr, 'SUBVIS' )
self.eclipse = FXPAR( hdr, 'ECLIPSE' )
self.ra0 = FXPAR( hdr, 'RAO' )
self.dec0 = FXPAR( hdr, 'DECO' )
if KEYWORD_SET( visit ) then begin
	self.exposure = FXPAR( hdr, 'EXPTIME' )
	self.obsdate = STRTRIM( FXPAR( hdr, 'OBSDATIM' ), 2 )
endif else begin
	self.exposure = FXPAR( hdr, 'TOTTIME' )
	self.obsdate = 'COADDED'
endelse

gsp = MRDFITS( gspFile[0], 1, /silent )
self.ptrGsp = PTR_NEW( gsp, /no_copy )

; Open the PRC or PRI files.
IF KEYWORD_SET( visit ) THEN $
  fuvPrcFile = FILE_SEARCH( self.grismPath + '*-fg-pri.fit*', count=n ) $
ELSE fuvPrcFile = FILE_SEARCH( self.grismPath + '*-fg-prc.fit*', count=n )
IF n EQ 1 THEN BEGIN
    self.fuvPrcUnit = self->OpenPrcFile( fuvPrcFile, offset=offset, skip=skip )
    self.fuvOffset = offset
    self.fuvSkip = skip
ENDIF ELSE BEGIN
    IF NOT nuvOnly THEN BEGIN
        MESSAGE, 'Oops.  Expected 1 FUV PRI/PRC file, found: ' + $
		strtrim(string(n),2), /informational
        RETURN, 0
    ENDIF
ENDELSE

IF KEYWORD_SET( visit ) THEN $
  nuvPrcFile = FILE_SEARCH( self.grismPath + '*-ng-pri.fit*', count=n ) $
ELSE nuvPrcFile = FILE_SEARCH( self.grismPath + '*-ng-prc.fit*', count=n )
IF n EQ 1 THEN BEGIN
    self.nuvPrcUnit = self->OpenPrcFile( nuvPrcFile, offset=offset, skip=skip )
    self.nuvOffset = offset
    self.nuvSkip = skip
ENDIF ELSE BEGIN
    MESSAGE, 'Oops.  Expected 1 NUV PRI/PRC file, found: ' + $
	    strtrim(string(n),2), /informational
    RETURN, 0
ENDELSE

; Open the GSAX files.
fuvGsaxFile = FILE_SEARCH( self.grismPath + '*-fg-gsax.fit*', count=n )
IF n EQ 1 THEN BEGIN
    tmp = MRDFITS( fuvGsaxFile[0], 1, /silent )
    self.ptrFuvGsax = PTR_NEW( tmp, /no_copy )
ENDIF

nuvGsaxFile = FILE_SEARCH( self.grismPath + '*-ng-gsax.fit*', count=n )
IF n EQ 1 THEN BEGIN
    tmp = MRDFITS( nuvGsaxFile[0], 1, /silent )
    self.ptrNuvGsax = PTR_NEW( tmp, /no_copy )
ENDIF

; Open the direct images, unless we shouldn't.
IF NOT KEYWORD_SET( noStamp ) THEN BEGIN

    fuvImgFile = FILE_SEARCH( self.imagePath + '*-fd-int.fit*', count=n )
    IF n EQ 1 THEN BEGIN
        fuvImg = MRDFITS( fuvImgFile[0], 0, fuvHdr, /silent )
        self.ptrFuvImg = PTR_NEW( fuvImg, /no_copy )
        self.ptrFuvHdr = PTR_NEW( fuvHdr, /no_copy )
    ENDIF ELSE BEGIN
        IF NOT nuvOnly THEN BEGIN
            PRINT, self.imagePath
            MESSAGE, 'Oops.  Expected 1 FUV image file, found: ' + $
		    strtrim(string(n),2), /informational
            RETURN, 0
        ENDIF 
    ENDELSE 

    nuvImgFile = FILE_SEARCH( self.imagePath + '*-nd-int.fit*', count=n )

    IF n EQ 1 THEN BEGIN
        nuvImg = MRDFITS( nuvImgFile[0], 0, nuvHdr, /silent )
        self.ptrNuvImg = PTR_NEW( nuvImg, /no_copy )
        self.ptrNuvHdr = PTR_NEW( nuvHdr, /no_copy )
    ENDIF ELSE BEGIN
        MESSAGE, 'Oops.  Expected 1 NUV image file, found: ' + $
		strtrim(string(n),2), /informational
        RETURN, 0
    ENDELSE 

ENDIF

self->SetProperty, currentIndex=0

RETURN, 1

END

PRO GALEXSpectrum__define

define = { GALEXSpectrum, $
           grismPath: '', $
           imagePath: '', $
           tile: '', $
           visit: 0, $
           subvis: 0, $
           eclipse: 0L, $
           exposure: 0., $
           obsdate: '', $
           ra0: 0.d, $
           dec0: 0.d, $
           ptrGsp: PTR_NEW(), $
           ptrFuvGsax: PTR_NEW(), $
           ptrNuvGsax: PTR_NEW(), $
           fuvPrcUnit: 0, $
           fuvOffset: 0L, $
           fuvSkip: 0L, $
           fuvLambdaOffset: 0., $
           nuvPrcUnit: 0, $
           nuvOffset: 0L, $
           nuvSkip: 0L, $
           nuvLambdaOffset: 0., $
           ptrFuvStrip: PTR_NEW(), $
           ptrNuvStrip: PTR_NEW(), $
           ptrFuvImg: PTR_NEW(), $
           ptrFuvHdr: PTR_NEW(), $
           ptrNuvImg: PTR_NEW(), $
           ptrNuvHdr: PTR_NEW(), $
           ptrFuvStamp: PTR_NEW(), $
           ptrNuvStamp: PTR_NEW(), $
           fuvStripMin: 0., $
           fuvStripMax: 0., $
           fuvStripHistEqual: 0B, $
           nuvStripMin: 0., $
           nuvStripMax: 0., $
           nuvStripHistEqual: 0B, $
           fuvStampMin: 0., $
           fuvStampMax: 0., $
           fuvStampHistEqual: 0B, $
           nuvStampMin: 0., $
           nuvStampMax: 0., $
           nuvStampHistEqual: 0B, $
           ptrFuvBackground: PTR_NEW(), $
           ptrFuvSpectrum: PTR_NEW(), $
           fuvSigmaLarge: 0., $
           fuvSigmaSmall: 0., $
           ptrNuvBackground: PTR_NEW(), $
           ptrNuvSpectrum: PTR_NEW(), $
           nuvSigmaLarge: 0., $
           nuvSigmaSmall: 0., $
           currentSpecID: 0L, $
           currentIndex: 0L }

END

