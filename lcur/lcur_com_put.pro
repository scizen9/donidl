function lcur_com_put,tag,val
;
COMMON phot_data ; pfile, filt, ntmax, ntpts, jd, epnum, mags, merrs, merrlim
COMMON parm_data ; jdran, jdzero, twin, t0, t1, def_pran, pran, def_fran, fran
COMMON cnts_data ; nmax, npts, time, counts, cerrs
COMMON scar_data ; nifmax, nif, nu, pd, px, faps, $
		 ; npkmax, npks, pkis, pkerrs, $
		 ; signi, noise, fmin, fmax
COMMON phase_data ; period, phase, phep, phinc, do_bin, bins, do_weight, $
		  ; coffs, do_sub
COMMON dao_data  ; dao_filt, dao_nmax, dao_npts, dao_nsmax, dao_nstars,  $
                 ; dao_files, dao_mags, dao_merrs, dao_id, dao_x, dao_y, $
		 ; dao_nframes, dao_chi, dao_sharp, dao_var, dao_varo, $
		 ; dao_blunder, dao_varth, dao_nvar, dao_cid, dao_cx, dao_cy, $
		 ; dao_cvar, dao_cvaro, dao_nflim, dao_enframes
COMMON dao_imdata; dao_im, dao_imhdr, dao_imgno
;
; get size
s = size(val)
dimen = s(0)
nn = -1
case dimen of
	0: n = s(2)
	1: n = s(1)
	2: begin
		n = s(1)
		nn = s(2)
	end
	else: begin
		print,'LCUR_COM_PUT: Error - invalid dimension: ',dimen
		return,0
	end
endcase
;
; check for empty val
if ( n le 0 ) then begin
	print,'LCUR_COM_PUT: ERROR, no elements for tag: ',tag
	return,0
endif
;
; check for too many points
switch strupcase(strtrim(tag,2)) of

; check arrays (dao_nsmax, dao_nmax)
	'DAO_MAGS':
	'DAO_MERRS': begin
		if ( n gt dao_nmax or nn gt dao_nsmax ) then begin
			print,'LCUR_COM_PUT: ERROR, too many points, ', $
				strn(n),', ',strn(nn), ', for tag: ',tag, $
				', max = ', dao_nsmax, dao_nmax
			stop
			return,0
		endif
		break
	end

; check vectors (ntmax)
	'JD': 
	'EPNUM':
	'MAGS':
	'MERRS': begin
		if ( n gt ntmax ) then begin
			print,'LCUR_COM_PUT: ERROR, too many points, ', $
				strn(n), ', for tag: ',tag,', max = ', $
				strn(ntmax)
			return,0
		endif
		break
	end

; check vectors (nmax)
	'TIME':
	'COUNTS':
	'CERRS':
	'PHASE':
	'PHEP':
	'COFFS': begin
		if ( n gt nmax ) then begin
			print,'LCUR_COM_PUT: ERROR, too many points, ', $
				strn(n), ', for tag: ',tag,', max = ',strn(nmax)
			return,0
		endif
		break
	end

; check vectors (nifmax)
	'NU':
	'PD':
	'PX':
	'FAPS': begin
		if ( n gt nifmax ) then begin
			print,'LCUR_COM_PUT: ERROR, too many points, ', $
				strn(n), ', for tag: ',tag,', max = ', $
				strn(nifmax)
			return,0
		endif
		break
	end

; check vectors (npkmax)
	'PKIS':
	'PKERRS': begin
		if ( n gt npkmax ) then begin
			print,'LCUR_COM_PUT: ERROR, too many points, ', $
				strn(n),', for tag: ',tag,', max = ', $
				strn(npkmax)
			return,0
		endif
		break
	end

; check vectors (dao_nsmax)
	'DAO_X':
	'DAO_Y':
	'DAO_RNEI':
	'DAO_NFRAMES':
	'DAO_ENFRAMES':
	'DAO_CHI':
	'DAO_SHARP':
	'DAO_VAR':
	'DAO_VARO':
	'DAO_BLUNDER':
	'DAO_ID': begin
		if ( n gt dao_nsmax ) then begin
			print,'LCUR_COM_PUT: ERROR, too many points, ', $
				strn(n),', for tag: ',tag,', max = ', $
				strn(dao_nsmax)
			return,0
		endif
		break
	end

; check vectors (dao_nmax)
	'DAO_FILES': begin
		if ( n gt dao_nmax ) then begin
			print,'LCUR_COM_PUT: ERROR, too many points, ', $
				strn(n),', for tag: ',tag,', max = ', $
				strn(dao_nmax)
			return,0
		endif
		break
	end

; check two element parameters
	'DAO_VARTH':
	'JDRAN':
	'TWIN':
	'PRAN':
	'FRAN': begin
		if ( n gt 2 ) then begin
			print,'LCUR_COM_PUT: ERROR, too many points, ', $
				strn(n), ', for tag: ',tag,', max = 2'
			return,0
		endif
		break
	end

; check dao_im
	'DAO_IM': begin
		print,'LCUR_COM_PUT: use COMMON dao_imdata to update dao_im'
		return,0
		break
	end

; all the rest should be a single value
	ELSE: begin
		if ( n ne 1 ) then begin
			print,'LCUR_COM_PUT: ERROR, wrong number of points, ', $
				strn(n), ', for tag: ',tag,', should be 1'
			return,0
		endif
	end

endswitch
;
;print,'lcur_com_put: ',tag
;
; check tag
case strupcase(strtrim(tag,2)) of

; phot_data

	'PFILE' : pfile = val
	'FILT'  : filt = val
	'NTMAX' : print,tag,' is read only ',ntmax
	'NTPTS' : ntpts = val < ntmax > 0
	'JD'    : begin & ntpts = n<ntmax & jd(0:(ntpts-1)) = val(*) & end
	'EPNUM' : begin & ntpts = n<ntmax & epnum(0:(ntpts-1)) = val(*) & end
	'MAGS'  : begin & ntpts = n<ntmax & mags(0:(ntpts-1)) = val(*) & end
	'MERRS' : begin & ntpts = n<ntmax & merrs(0:(ntpts-1)) = val(*) & end
	'MERRLIM': merrlim = val
		
; parm_data
	
	'JDRAN' : jdran = val
	'JDZERO': jdzero = val
	'TWIN'  : twin(0:1) = val(0:1)
	'T0'    : t0 = val
	'T1'    : t1 = val
	'DEF_PRAN': def_pran = val
	'PRAN'  : pran(0:1) = val(0:1)
	'DEF_FRAN': def_fran = val
	'FRAN'  : fran(0:1) = val(0:1)

; cnts_data
	'NMAX'  : print,tag,' is read only ',nmax
	'NPTS'  : npts = val < nmax > 0
	'TIME'  : begin & npts = n<nmax & time(0:(npts-1)) = val(*) & end
	'COUNTS': begin & npts = n<nmax & counts(0:(npts-1)) = val(*) & end
	'CERRS' : begin & npts = n<nmax & cerrs(0:(npts-1)) = val(*) & end

; scar_data
	'NIFMAX': print,tag,' is read only ',nifmax
	'NIF'   : nif = val < nifmax > 0
	'NU'    : begin & nif = n<nifmax & nu(0:(nif-1)) = val(*) & end
	'PD'    : begin & nif = n<nifmax & pd(0:(nif-1)) = val(*) & end
	'PX'    : begin & nif = n<nifmax & px(0:(nif-1)) = val(*) & end
	'FAPS'  : begin & nif = n<nifmax & faps(0:(nif-1)) = val(*) & end
	'NPKMAX': print,tag,' is read only ',npkmax
	'NPKS'  : npks = val
	'PKIS'  : begin & npks = n<npkmax & pkis(0:(npks-1)) = val(*) & end
	'PKERRS': begin & npks = n<npkmax & pkerrs(0:(npks-1)) = val(*) & end
	'SIGNI' : signi = val
	'NOISE' : noise = val
	'FMIN'  : fmin = val
	'FMAX'  : fmax = val

; phase_data
	'PERIOD': period = val
	'PHASE' : begin & npts = n<nmax & phase(0:(npts-1)) = val(*) & end
	'PHEP'  : begin & npts = n<nmax & phep(0:(npts-1)) = val(*) & end
	'PHINC' : phinc = val
	'DO_BIN': do_bin = val
	'BINS'  : bins = val
	'DO_WEIGHT': do_weight = val
	'COFFS' : begin & npts = n<nmax & coffs(0:(npts-1)) = val(*) & end
	'DO_SUB': do_sub = val

; dao_data
	'DAO_FILT': dao_filt = val
	'DAO_NMAX': print,tag,' is read only ',dao_nmax
	'DAO_NPTS': dao_npts = val < dao_nmax > 0
	'DAO_NSMAX': print,tag,' is read only ',dao_nsmax
	'DAO_NSTARS': dao_nstars = val < dao_nsmax > 0
	'DAO_FILES':	begin 
				dao_npts = n<dao_nmax
				dao_files(0:(dao_npts-1)) = val(*)
			end
	'DAO_MAGS':	begin
				dao_npts = n<dao_nmax
				dao_nstars = nn<dao_nsmax
				dao_mags(0:(dao_npts-1),0:(dao_nstars-1)) $
					= val(*,*)
			end
	'DAO_MERRS':	begin
				dao_npts = n<dao_nmax
				dao_nstars = nn<dao_nsmax
				dao_merrs(0:(dao_npts-1),0:(dao_nstars-1)) $
					= val(*,*)
			end
	'DAO_ID':	begin 
				dao_nstars = n<dao_nsmax
				dao_id(0:(dao_nstars-1)) = val(*)
			end
	'DAO_X':	begin 
				dao_nstars = n<dao_nsmax
				dao_x(0:(dao_nstars-1)) = val(*)
			end
	'DAO_Y':	begin 
				dao_nstars = n<dao_nsmax
				dao_y(0:(dao_nstars-1)) = val(*)
			end
	'DAO_RNEI':	begin 
				dao_nstars = n<dao_nsmax
				dao_rnei(0:(dao_nstars-1)) = val(*)
			end
	'DAO_ENFRAMES':	begin 
				dao_nstars = n<dao_nsmax
				dao_enframes(0:(dao_nstars-1)) = val(*)
			end
	'DAO_NFRAMES':	begin 
				dao_nstars = n<dao_nsmax
				dao_nframes(0:(dao_nstars-1)) = val(*)
			end
	'DAO_CHI':	begin 
				dao_nstars = n<dao_nsmax
				dao_chi(0:(dao_nstars-1)) = val(*)
			end
	'DAO_SHARP':	begin 
				dao_nstars = n<dao_nsmax
				dao_sharp(0:(dao_nstars-1)) = val(*)
			end
	'DAO_VAR':	begin 
				dao_nstars = n<dao_nsmax
				dao_var(0:(dao_nstars-1)) = val(*)
			end
	'DAO_VARO':	begin 
				dao_nstars = n<dao_nsmax
				dao_varo(0:(dao_nstars-1)) = val(*)
			end
	'DAO_BLUNDER':	begin 
				dao_nstars = n<dao_nsmax
				dao_blunder(0:(dao_nstars-1)) = val(*)
			end
	'DAO_VARTH':	dao_varth = val
	'DAO_NEITH':	dao_neith = val
	'DAO_NVAR':	dao_nvar = val
	'DAO_CID':	dao_cid = val
	'DAO_CX':	dao_cx = val
	'DAO_CY':	dao_cy = val
	'DAO_CVAR':	dao_cvar = val
	'DAO_CVARO':	dao_cvaro = val
	'DAO_NFLIM':	dao_nflim = val

	'DAO_IMGNO':	dao_imgno = val

	else:	begin
		print,'LCUR_COM_GET: ERROR, unknown tag: ',tag
		return,0
	end
endcase
;
; successful return
return,1
end	; function lcur_com_put
