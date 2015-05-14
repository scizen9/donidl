	function get_extinction,r,d
;------------------------------------------------------------------------------
;+
; NAME:
;   dust_getval
;
; PURPOSE:
;   Read values from BH files or our dust maps.
;
; CALLING SEQUENCE:
;   value = dust_getval( [ gall, galb, infile=infile, skipline=skipline, $
;    outfile=outfile, map=map, interp=interp, noloop=noloop, verbose=verbose, $
;    ipath=ipath, bhpath=bhpath ] )
;
; INPUTS:
;
; OPTIONAL INPUTS:
;   gall:       Galactic longitude(s) in degrees
;   galb:       Galactic latitude(s) in degrees
;   map:        Set to one of the following (default is 'Ebv'):
;               BH  : Burstein-Heiles 4*E(B-V)
;               I100: 100-micron map in MJy/Sr
;               X   : X-map, temperature-correction factor
;               T   : Temperature map in degrees Kelvin for n=2 emissivity
;               IX  : Temperature-corrected 100-micron map in MJy/Sr
;               Ebv : E(B-V) in magnitudes
;               mask: 8-bit mask
;   infile:     If set, then read GALL and GALB from this file
;   skipline:   Number of lines to skip at the top of the input file
;   outfile:    If set, then write results to this file
;   interp:     Set this flag to return a linearly interpolated value
;               from the 4 nearest pixels.
;               This is disabled if map='mask'.
;   noloop:     Set this flag to read all values at once without a FOR loop.
;               This is a faster option for reading a large number of values,
;               but requires reading an entire FITS image into memory.
;               (Actually, the smallest possible sub-image is read.)
;   verbose:    Set this flag for verbose output, printing pixel coordinates
;               and map values.  Setting NOLOOP disables this option.
;   ipath:      Path name for dust maps; default to path set by the
;               environment variable $DUST_DIR/maps, or to the current
;               directory.
;   bhpath:     Path name for BH maps
;
; OUTPUTS:
;   value:      Value(s) from Lambert-projection maps.
	glactc,r,d,2000.,gl,gb,1,/degree
	ebv = dust_getval(gl,gb,ipath='/home/galex/cal/plan/dust/')
	fl = 1.
	ccm_unred,2300.,fl,ebv,fu
	Anuv = -2.5*alog10(fl/fu)
	ccm_unred,1600.,fl,ebv,fu
	Afuv = -2.5*alog10(fl/fu)
	return,[Afuv,Anuv,ebv]
	end
	
