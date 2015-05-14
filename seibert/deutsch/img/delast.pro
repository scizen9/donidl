pro delast,h
;+
; NAME:
;   DELAST
; PURPOSE:
;   Deletes all astrometric information from a header.  Mainly useful for
;   deleting screwy "astrometry" that IRAF delights in fouling up the
;   header with (e.g., "PIXEL", "SYSTEM", etc. CTYPEs)
; CALLING SEQEUNCE:
;   delast,header
;-

  sxdelpar,h,['CTYPE1','CTYPE2','CRVAL1','CRVAL2','CRPIX1','CRPIX2', $
    'CD1_1','CD1_2','CD2_1','CD2_2','CDELT1','CDELT2','CROTA1','CROTA2', $
    'CD001001','CD001002','CD002001','CD002002']

  print,'That pesky astrometry is GONE!'

  return

end
