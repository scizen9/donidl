function lcur_com_get,tag
;
COMMON phot_data ; pfile, filt, ntmax, ntpts, jd, epnum, mags, merrs, merrlim
COMMON parm_data ; jdran, jdzero, twin, t0, t1, def_pran, pran, def_fran, fran
COMMON cnts_data ; nmax, npts, time, counts, cerrs
COMMON scar_data ; nifmax, nif, nu, pd, px, faps, $
		 ; npkmax, npks, pkis, pkerrs, $
		 ; signi, noise, fmin, fmax
COMMON phase_data ; period, phase, phinc, do_bin, bins, do_weight, coffs, do_sub
COMMON dao_data  ; dao_filt, dao_nmax, dao_npts, dao_nsmax, dao_nstars,  $
                 ; dao_files, dao_mags, dao_merrs, dao_id, dao_x, dao_y, $
		 ; dao_rnei, dao_nframes, dao_chi, dao_sharp, dao_var,dao_varo,$
		 ; dao_blunder, dao_varth, dao_neith, dao_nvar, dao_cid,dao_cx,$
		 ; dao_cy, dao_cvar, dao_cvaro, dao_nflim, dao_enframes
COMMON dao_imdata ; dao_im, dao_imhdr, dao_imgno

;
; check tag
;print,'lcur_com_get: ',tag
case strupcase(strtrim(tag,2)) of

; phot_data

	'PFILE' : return,pfile
	'FILT'  : return,filt
	'NTMAX' : return,ntmax
	'NTPTS' : return,ntpts
	'JD'    : return,jd(0:(ntpts-1))
	'EPNUM' : return,epnum(0:(ntpts-1))
	'MAGS'  : return,mags(0:(ntpts-1))
	'MERRS' : return,merrs(0:(ntpts-1))
	'MERRLIM': return,merrlim
		
; parm_data
	
	'JDRAN' : return,jdran
	'JDZERO': return,jdzero
	'TWIN'  : return,twin
	'T0'    : return,t0
	'T1'    : return,t1
	'DEF_PRAN': return,def_pran
	'PRAN'  : return,pran
	'DEF_FRAN': return,def_fran
	'FRAN'  : return,fran

; cnts_data
	'NMAX'  : return,nmax
	'NPTS'  : return,npts
	'TIME'  : return,time(0:(npts-1))
	'COUNTS': return,counts(0:(npts-1))
	'CERRS' : return,cerrs(0:(npts-1))

; scar_data
	'NIFMAX': return,nifmax
	'NIF'   : return,nif
	'NU'    : return,nu(0:(nif-1))
	'PD'    : return,pd(0:(nif-1))
	'PX'    : return,px(0:(nif-1))
	'FAPS'  : return,faps(0:(nif-1))
	'NPKMAX': return,npkmax
	'NPKS'  : return,npks
	'PKIS'  : return,pkis(0:(npks-1))
	'PKERRS': return,pkerrs(0:(npks-1))
	'SIGNI' : return,signi
	'NOISE' : return,noise
	'FMIN'  : return,fmin
	'FMAX'  : return,fmax

; phase_data
	'PERIOD': return,period
	'PHASE' : return,phase(0:(npts-1))
	'PHINC' : return,phinc
	'DO_BIN': return,do_bin
	'BINS'  : return,bins
	'DO_WEIGHT': return,do_weight
	'COFFS' : return,coffs(0:(npts-1))
	'DO_SUB': return,do_sub

; dao_data
	'DAO_FILT': return, dao_filt
	'DAO_NMAX': return, dao_nmax
	'DAO_NPTS': return, dao_npts
	'DAO_NSMAX': return, dao_nsmax
	'DAO_NSTARS': return, dao_nstars
	'DAO_FILES': return, dao_files(0:(dao_npts-1))
	'DAO_MAGS': return, dao_mags(0:(dao_npts-1), 0:(dao_nstars-1))
	'DAO_MERRS': return, dao_merrs(0:(dao_npts-1), 0:(dao_nstars-1))
	'DAO_ID': return, dao_id(0:(dao_nstars-1))
	'DAO_X': return, dao_x(0:(dao_nstars-1))
	'DAO_Y': return, dao_y(0:(dao_nstars-1))
	'DAO_RNEI': return, dao_rnei(0:(dao_nstars-1))
	'DAO_NFRAMES': return, dao_nframes(0:(dao_nstars-1))
	'DAO_ENFRAMES': return, dao_enframes(0:(dao_nstars-1))
	'DAO_CHI': return, dao_chi(0:(dao_nstars-1))
	'DAO_SHARP': return, dao_sharp(0:(dao_nstars-1))
	'DAO_VAR': return, dao_var(0:(dao_nstars-1))
	'DAO_VARO': return, dao_varo(0:(dao_nstars-1))
	'DAO_BLUNDER': return, dao_blunder(0:(dao_nstars-1))
	'DAO_VARTH': return, dao_varth
	'DAO_NEITH': return, dao_neith
	'DAO_NVAR': return, dao_nvar
	'DAO_CID': return, dao_cid
	'DAO_CX': return, dao_cx
	'DAO_CY': return, dao_cy
	'DAO_CVAR': return, dao_cvar
	'DAO_CVARO': return, dao_cvaro
	'DAO_NFLIM': return, dao_nflim

; dao_imdata
	'DAO_IM': return, daoim
	'DAO_IMGNO': return, dao_imgno

	else:	begin
		print,'LCUR_COM_GET: ERROR, unknown tag: ',tag
		return,-1
	end
endcase
;
; should never get here
return,-1
end
