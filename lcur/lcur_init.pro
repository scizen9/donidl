pro lcur_init
;
COMMON phot_data, pfile, filt, ntmax, ntpts, jd, epnum, mags, merrs, merrlim
COMMON parm_data, jdran, jdzero, twin, t0, t1, def_pran, pran, def_fran, fran
COMMON cnts_data, nmax, npts, time, counts, cerrs
COMMON scar_data, nifmax, nif, nu, pd, px, faps, $
		  npkmax, npks, pkis, pkerrs, $
		  signi, noise, fmin, fmax
COMMON phase_data, period, phase, phinc, do_bin, bins, do_weight, coffs, do_sub
COMMON dao_data,  dao_filt, dao_nmax, dao_npts, dao_nsmax, dao_nstars,  $
		  dao_files, dao_mags, dao_merrs, dao_id, dao_x, dao_y, $
		  dao_rnei, dao_nframes, dao_chi, dao_sharp, dao_var, dao_varo,$
		  dao_blunder, dao_varth, dao_neith, dao_nvar, dao_cid, dao_cx,$
		  dao_cy, dao_cvar, dao_cvaro, dao_nflim, dao_enframes
COMMON dao_imdata, dao_im, dao_imhdr, dao_imgno
;
; initialize phot_data
pfile	= ''
filt	= ''				; filter in which observations were made
ntmax	= 100000				; maximum total points
ntpts	= 0				; number of total points read in
jd 	= dblarr(ntmax)			; julian day of each point
epnum	= intarr(ntmax)			; epoch number
mags 	= fltarr(ntmax)			; magnitude of each point
merrs	= fltarr(ntmax)			; magnitude errors of each point
merrlim = 0.1				; magnitude error limit
;
; initialize parm_data
jdran	= dblarr(2)			; total jd range of data
jdzero	= 0.d0				; jd zero point offset
twin	= dblarr(2)			; time window in julian days (beg, end)
t0	= 0				; beginning vector index for time window
t1	= 0				; end vector index for time window
def_pran= (1 eq 1)			; bool: use default period range?
pran	= fltarr(2)			; user specified period range
def_fran = (1 eq 1)			; bool: use default frq range for plot?
fran	= fltarr(2)			; user specified frq range
;
; initialize cnts_data
nmax	= ntmax				; maximum number of points (ntmax)
npts	= 0				; number of points
time	= dblarr(nmax)			; time (jd)
counts	= fltarr(nmax)			; adjusted arbitrary counts
cerrs	= fltarr(nmax)			; counts errors
;
; initialize scar_data
nifmax	= ntmax				; maximum number of indep. frequencies
nif	= 0				; actual number of indep. frequencies
nu	= fltarr(nifmax)		; normal frequency (omega/2*!DPI)
pd	= fltarr(nifmax)		; period for each frequency
px	= fltarr(nifmax)		; power at each frequency
faps	= fltarr(nifmax)		; false alarm prob. at each frequency

npkmax	= 10				; max number of interesting peaks
npks	= 0				; actual number of interesting peaks
pkis	= lonarr(npkmax)		; indices of interesting peaks
pkerrs	= fltarr(npkmax)		; errors on peaks (15% half width)

signi	= 0.				; power thresh at given FAP
noise	= 0.				; variance of original data
fmin	= 0.				; minimum frequency
fmax	= 0.				; maximum frequency
;
; initialize phase_data
period 	= 0.d0				; period in days
phase	= fltarr(nmax)			; phase vector for counts data
phinc	= 0.001				; phase increment (days)
do_bin	= (1 eq 0)			; bin in phase?
bins	= 0				; number of phase bins
do_weight = (1 eq 0)			; weighted binning?
coffs	= fltarr(nmax)			; count offsets for removing signal
do_sub	= (1 eq 0)			; do subtraction of offsets?
;
; initialize dao_data
dao_filt	= ''			; filter of observations
dao_nmax	= 200			; max number of epochs
dao_npts	= 0			; number of points
dao_nsmax	= 100000L		; max number of stars
dao_nstars	= 0			; actual number of stars
dao_files	= strarr(dao_nmax)	; image file names
dao_mags	= fltarr(dao_nmax, dao_nsmax)	; magnitude of each point
dao_merrs	= fltarr(dao_nmax, dao_nsmax)	; magnitude errors of each point
dao_id		= lonarr(dao_nsmax)	; star id
dao_x		= fltarr(dao_nsmax)	; x position
dao_y		= fltarr(dao_nsmax)	; y position
dao_rnei	= fltarr(dao_nsmax)	; offset to nearest neighbor
dao_nframes	= intarr(dao_nsmax)	; n frames star observed in
dao_chi		= fltarr(dao_nsmax)	; chi
dao_sharp	= fltarr(dao_nsmax)	; shape parameter
dao_var		= fltarr(dao_nsmax)-1.	; variability
dao_varo	= fltarr(dao_nsmax)-1.	; variability (original value)
dao_blunder	= fltarr(dao_nsmax)	; blunder index:frac of positive resids
dao_varth	= fltarr(2)		; variability threshhold (lo, hi)
dao_neith	= 0.0			; nearest neighbor threshhold (r pix)
dao_nvar	= 0L			; number of vars selected
dao_cid		= 0L			; current id
dao_cx		= 0.0			; current x
dao_cy		= 0.0			; current y
dao_cvar	= 0.0			; current variability
dao_cvaro	= 0.0			; current variability (original value)
dao_nflim	= 0			; must have at least this many frames
dao_enframes	=intarr(dao_nsmax)	; number of frames after thresshold
					; with merrlim
;
; initialize dao_imdata
dao_im		= intarr(2047,4096)	; image
dao_imhdr	= strarr(360)
dao_imgno	= 0			; image number (-1 for no image)
;
; initialize plots
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
;
return
end
