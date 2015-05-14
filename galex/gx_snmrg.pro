pro gx_snmrg
;
; Merge statistics on SNe
;
; read log file
readcols,'snan.log',sn,mgf0,mgfe0,mgf1,mgfe1,mgf2,mgfe2,mgf3,mgfe3,sbf, $
	mgn0,mgne0,mgn1,mgne1,mgn2,mgne2,mgn3,mgne3,sbn, $
	host,srv,expt,snx,sny,bckg,/silent,$
	format='a,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,a,a,f,f,f,f'
nr=n_elements(sn)
;
; open mrg file
lfile='snmrg.log'
filestamp,lfile
openw,ol,lfile,/get_lun
printf,ol,'# SNMRG run on '+systime(0)
printf,ol,'# SN          mF   mFerr   mF.5  mFerr    mF1  mFerr    mF2  mFerr   sbF     mN   mNerr   mN.5  mNerr    mN1  mNerr    mN2  mNerr   sbN   host               fexpt    nexpt   nf   nn     Fbackg     Nbackg'
;
; photometry constants
sbrto=3.4787	; 2.5*alog10( !pi * (5.6/2.)^2 ) convert to surface brightness
		; in mag/square arcsec
;
; loop over sne
for i=0,nr-1 do begin
	if strlen(sn(i)) ge 5 then begin
		w=where(strpos(sn,sn(i)) ge 0, nsn)
		if nsn ge 1 then begin
;
; mag with NUV resolution aperture
; NUV
			gx_avmags,1,mgn0(w),mgne0(w),expt(w), $
				    wmgn0,  wmgne0,  nexpt, nan, nbackg, $
				    background=bckg(w), /error_weights, /silent
			wsbn = wmgn0 + sbrto	; surface brightness mag/arcsc^2
;
; FUV
			gx_avmags,2,mgf0(w),mgfe0(w),expt(w), $
				    wmgf0,  wmgfe0,  fexpt, naf, fbackg, $
				    background=bckg(w), /error_weights, /silent
			wsbf = wmgf0 + sbrto	; surface brightness mag/arcsc^2
;
; mag with 500pc aperture
; NUV
			gx_avmags,1,mgn1(w),mgne1(w),expt(w), $
				    wmgn1,  wmgne1, $
				    background=bckg(w), /error_weights, /silent
;
; FUV
			gx_avmags,2,mgf1(w),mgfe1(w),expt(w), $
				    wmgf1,  wmgfe1, $
				    background=bckg(w), /error_weights, /silent
;
; mag with 1kpc aperture
; NUV
			gx_avmags,1,mgn2(w),mgne2(w),expt(w), $
				    wmgn2,  wmgne2, $
				    background=bckg(w), /error_weights, /silent
;
; FUV
			gx_avmags,2,mgf2(w),mgfe2(w),expt(w), $
				    wmgf2,  wmgfe2, $
				    background=bckg(w), /error_weights, /silent
;
; mag with 2kpc aperture
; NUV
			gx_avmags,1,mgn3(w),mgne3(w),expt(w), $
				    wmgn3,  wmgne3, $
				    background=bckg(w), /error_weights, /silent
;
; FUV
			gx_avmags,2,mgf3(w),mgfe3(w),expt(w), $
				    wmgf3,  wmgfe3, $
				    background=bckg(w), /error_weights, /silent
			a = w(0)
;
; write out results
    			printf,ol,sn(a),wmgf0,wmgfe0,wmgf1,wmgfe1,wmgf2,wmgfe2,$
				wmgf3,wmgfe3,wsbf,wmgn0,wmgne0,wmgn1,wmgne1, $
				wmgn2,wmgne2,wmgn3,wmgne3,wsbn, $
				host(a),fexpt,nexpt,naf,nan,fbackg,nbackg, $
			format='(a-10,18f7.2,2x,a-15,2f9.1,2i5,2x,e9.3,2x,e9.3)'
	    		sn(w) = ''
		endif
	endif
endfor
;
free_lun,ol
;
return
end
